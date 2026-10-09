;;; exwm-cc-polkit.el --- Opt-in PolicyKit agent for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `exwm-cc-polkit-mode' is off by default.  Turning it on registers
;; this Emacs as the PolicyKit authentication agent for
;; `XDG_SESSION_ID'.  That replaces the agent the desktop already
;; registered.  Turning the mode off unregisters this agent and does
;; not start the previous one again.
;;
;; The conversation with polkit-agent-helper-1 follows the helper
;; protocol: the helper writes PAM_* lines escaped with g_strescape,
;; and the response is the raw string plus a newline.  A password is
;; read with `read-passwd' from a timer, never from a process filter,
;; and it is not stored.

;;; Code:

(require 'cl-lib)
(require 'exwm-cc-lib)

(defconst exwm-cc-polkit-service "org.freedesktop.PolicyKit1")
(defconst exwm-cc-polkit-path "/org/freedesktop/PolicyKit1/Authority")
(defconst exwm-cc-polkit-iface "org.freedesktop.PolicyKit1.Authority")
(defconst exwm-cc-polkit-agent-path "/org/freedesktop/ExwmCc/AuthenticationAgent")
(defconst exwm-cc-polkit-agent-iface "org.freedesktop.PolicyKit1.AuthenticationAgent")
(defconst exwm-cc-polkit-helper "/usr/lib/polkit-1/polkit-agent-helper-1")
(defconst exwm-cc-polkit-socket "/run/polkit/agent-helper.socket")

(defvar exwm-cc-polkit--registrations nil
  "D-Bus method registrations for the authentication agent.")

(defvar exwm-cc-polkit--sessions (make-hash-table :test #'equal)
  "Authentication sessions keyed by cookie.")

(defvar exwm-cc-polkit--guard nil
  "Non-nil while the minor mode is correcting a failed registration.")

(cl-defstruct exwm-cc-polkit-session
  cookie process buffers lines done gained cancelled)

(defun exwm-cc-polkit-unescape (string)
  "Undo g_strescape on STRING, as g_strcompress does.
Recognizes \\, \\n, \\r, \\t, \\b, \\f, \\v, and octal \\nnn."
  (let ((i 0)
        (n (length string))
        (chars nil))
    (while (< i n)
      (let ((char (aref string i)))
        (setq i (1+ i))
        (if (and (eq char ?\\) (< i n))
            (let ((next (aref string i)))
              (cond
               ((eq next ?n) (push ?\n chars) (setq i (1+ i)))
               ((eq next ?r) (push ?\r chars) (setq i (1+ i)))
               ((eq next ?t) (push ?\t chars) (setq i (1+ i)))
               ((eq next ?b) (push ?\b chars) (setq i (1+ i)))
               ((eq next ?f) (push ?\f chars) (setq i (1+ i)))
               ((eq next ?v) (push ?\v chars) (setq i (1+ i)))
               ((and (>= next ?0) (<= next ?7))
                (let ((val 0)
                      (k 0))
                  (while (and (< k 3) (< i n))
                    (setq next (aref string i))
                    (if (and (>= next ?0) (<= next ?7))
                        (setq val (+ (* val 8) (- next ?0))
                              i (1+ i)
                              k (1+ k))
                      (setq k 3)))
                  (push (logand val 255) chars)))
               (t (push next chars) (setq i (1+ i)))))
          (push char chars))))
    (concat (nreverse chars))))

(defun exwm-cc-polkit-parse-line (line)
  "Parse one helper LINE into (KIND . TEXT), or nil.
KIND is `echo-off', `echo-on', `error', `info', `success', or
`failure'.  TEXT is the unescaped message."
  (let ((text (exwm-cc-polkit-unescape (string-trim-right line "\n"))))
    (cond
     ((string-prefix-p "PAM_PROMPT_ECHO_OFF " text)
      (cons 'echo-off (substring text (length "PAM_PROMPT_ECHO_OFF "))))
     ((string-prefix-p "PAM_PROMPT_ECHO_ON " text)
      (cons 'echo-on (substring text (length "PAM_PROMPT_ECHO_ON "))))
     ((string-prefix-p "PAM_ERROR_MSG " text)
      (cons 'error (substring text (length "PAM_ERROR_MSG "))))
     ((string-prefix-p "PAM_TEXT_INFO " text)
      (cons 'info (substring text (length "PAM_TEXT_INFO "))))
     ((string-prefix-p "SUCCESS" text) (cons 'success ""))
     ((string-prefix-p "FAILURE" text) (cons 'failure ""))
     (t nil))))

(defun exwm-cc-polkit-unix-uid (tree)
  "Return the first unix-user uid found in identity TREE, or nil."
  (let (uid)
    (cl-labels
        ((walk (value)
           (cond
            ((and (consp value) (equal (car value) "uid") (null uid))
             (let ((number (exwm-cc-dbus-unwrap (cdr value))))
               (cond ((integerp number) (setq uid number))
                     ((and (consp number) (integerp (car number)))
                      (setq uid (car number))))))
            ((consp value)
             (walk (car value))
             (unless uid (walk (cdr value)))))))
      (walk tree))
    uid))

(defun exwm-cc-polkit--user-name (uid)
  "Return the login name for UID, or nil."
  (cond ((not (integerp uid)) nil)
        ((eql uid (user-uid)) (user-login-name))
        (t (let ((res (exwm-cc-command-output "getent" "passwd"
                                              (format "%d" uid))))
             (and (eql (car res) 0)
                  (car (split-string (cdr res) ":" t)))))))

(defun exwm-cc-polkit--subject (session-id)
  "Return the PolicyKit subject struct for SESSION-ID."
  `(:struct "unix-session"
            (:array (:dict-entry "session-id"
                                 (:variant :string ,session-id)))))

(defun exwm-cc-polkit-register-session (session-id)
  "Register this process as the agent for SESSION-ID.
The methods must already be exported on `exwm-cc-polkit-agent-path'."
  (dbus-call-method
   :system exwm-cc-polkit-service exwm-cc-polkit-path exwm-cc-polkit-iface
   "RegisterAuthenticationAgent"
   :timeout 10000
   (exwm-cc-polkit--subject session-id)
   (or (getenv "LANG") "C")
   exwm-cc-polkit-agent-path))

(defun exwm-cc-polkit-unregister-session (session-id)
  "Unregister the agent registered for SESSION-ID."
  (dbus-call-method
   :system exwm-cc-polkit-service exwm-cc-polkit-path exwm-cc-polkit-iface
   "UnregisterAuthenticationAgent"
   :timeout 10000
   (exwm-cc-polkit--subject session-id)
   exwm-cc-polkit-agent-path))

(defun exwm-cc-polkit--unexport ()
  "Drop exported agent methods and kill helper processes."
  (dolist (object exwm-cc-polkit--registrations)
    (condition-case nil
        (dbus-unregister-object object)
      (error nil)))
  (setq exwm-cc-polkit--registrations nil)
  (maphash (lambda (_cookie session) (exwm-cc-polkit--kill session))
           exwm-cc-polkit--sessions)
  (clrhash exwm-cc-polkit--sessions))

(defun exwm-cc-polkit--export ()
  "Export BeginAuthentication and CancelAuthentication.
Registration uses the Emacs bus connection's unique name.  This
does not register the process as the session agent."
  (exwm-cc-polkit--unexport)
  (let (objects)
    (unwind-protect
        (progn
          (push (dbus-register-method
                 :system nil exwm-cc-polkit-agent-path exwm-cc-polkit-agent-iface
                 "BeginAuthentication" #'exwm-cc-polkit--begin t)
                objects)
          (push (dbus-register-method
                 :system nil exwm-cc-polkit-agent-path exwm-cc-polkit-agent-iface
                 "CancelAuthentication" #'exwm-cc-polkit--cancel t)
                objects)
          (setq exwm-cc-polkit--registrations objects)
          (setq objects nil))
      (dolist (object objects)
        (condition-case nil
            (dbus-unregister-object object)
          (error nil))))))

(defun exwm-cc-polkit--kill (session)
  "Stop SESSION's helper and drop its buffers."
  (when (exwm-cc-polkit-session-p session)
    (let ((proc (exwm-cc-polkit-session-process session)))
      (when (processp proc)
        (when (process-live-p proc)
          (unless (eq (process-type proc) 'network)
            (condition-case nil
                (signal-process proc 'SIGTERM)
              (error nil)))
          (delete-process proc))
        (let ((buf (process-buffer proc)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))
        (dolist (buf (process-get proc 'exwm-cc-buffers))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))
    (dolist (buf (exwm-cc-polkit-session-buffers session))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (setf (exwm-cc-polkit-session-buffers session) nil)))

(defun exwm-cc-polkit--attach (proc session)
  "Parse helper output from PROC into SESSION."
  (let ((pending ""))
    (set-process-filter
     proc
     (lambda (_proc chunk)
       (setq pending (concat pending chunk))
       (let (break)
         (while (setq break (string-search "\n" pending))
           (let ((line (substring pending 0 break)))
             (setq pending (substring pending (1+ break)))
             (setf (exwm-cc-polkit-session-lines session)
                   (nconc (exwm-cc-polkit-session-lines session)
                          (list line))))))))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (unless (process-live-p process)
         (unless (exwm-cc-polkit-session-done session)
           (setf (exwm-cc-polkit-session-done session) t)))))))

(defun exwm-cc-polkit--spawn (user)
  "Start the helper for USER and return the process.
The socket is preferred when it exists.  A failed socket falls
back to the setuid helper.  The caller writes the cookie."
  (let ((proc nil))
    (when (file-exists-p exwm-cc-polkit-socket)
      (condition-case err
          (setq proc (make-network-process
                      :name "exwm-cc-polkit"
                      :family 'local
                      :service exwm-cc-polkit-socket
                      :coding 'utf-8-unix
                      :noquery t))
        (error
         (message "PolicyKit socket unavailable: %s" (error-message-string err))
         (setq proc nil))))
    (if (processp proc)
        (progn
          (process-send-string proc (concat user "\n"))
          proc)
      (unless (file-executable-p exwm-cc-polkit-helper)
        (error "PolicyKit agent helper is not installed"))
      (let ((stdout (generate-new-buffer " *exwm-cc-polkit*"))
            (stderr (generate-new-buffer " *exwm-cc-polkit-err*")))
        (condition-case err
            (let ((proc (make-process
                         :name "exwm-cc-polkit"
                         :command (list exwm-cc-polkit-helper user)
                         :connection-type 'pipe
                         :coding 'utf-8-unix
                         :noquery t
                         :buffer stdout
                         :stderr stderr)))
              (process-put proc 'exwm-cc-buffers (list stdout stderr))
              proc)
          (error
           (kill-buffer stdout)
           (kill-buffer stderr)
           (signal (car err) (cdr err))))))))

(defun exwm-cc-polkit--answer (session echo-off prompt)
  "Ask the user for a helper prompt and write the response.
ECHO-OFF uses `read-passwd'.  The response is not stored.  This
runs from a timer so it is not called inside the process filter."
  (let ((proc (exwm-cc-polkit-session-process session))
        (waiting t))
    (run-with-timer
     0 nil
     (lambda ()
       (unwind-protect
           (condition-case nil
               (let ((answer (if echo-off
                                 (read-passwd
                                  (if (string-empty-p prompt) "Password: " prompt))
                               (read-string prompt))))
                 (when (and (process-live-p proc)
                            (not (exwm-cc-polkit-session-cancelled session)))
                   (process-send-string proc (concat answer "\n"))))
             (quit
              (setf (exwm-cc-polkit-session-cancelled session) t
                    (exwm-cc-polkit-session-done session) t)
              (exwm-cc-polkit--kill session)))
         (setq waiting nil))))
    (let ((deadline (+ (float-time) 180)))
      (while (and waiting
                  (not (exwm-cc-polkit-session-done session))
                  (< (float-time) deadline))
        (accept-process-output nil 0.1)))))

(defun exwm-cc-polkit--drain (session)
  "Handle helper lines already queued on SESSION."
  (while (and (exwm-cc-polkit-session-lines session)
              (not (exwm-cc-polkit-session-done session)))
    (let* ((line (pop (exwm-cc-polkit-session-lines session)))
           (parsed (exwm-cc-polkit-parse-line line)))
      (pcase (car parsed)
        ('echo-off (exwm-cc-polkit--answer session t (cdr parsed)))
        ('echo-on (exwm-cc-polkit--answer session nil (cdr parsed)))
        ('error (message "PolicyKit: %s" (cdr parsed)))
        ('info (message "PolicyKit: %s" (cdr parsed)))
        ('success
         (setf (exwm-cc-polkit-session-gained session) t
               (exwm-cc-polkit-session-done session) t))
        ('failure
         (setf (exwm-cc-polkit-session-done session) t))
        (_
         (message "PolicyKit helper sent an unknown line")
         (setf (exwm-cc-polkit-session-done session) t))))))

(defun exwm-cc-polkit--begin (_action-id message _icon _details cookie identities)
  "Handle BeginAuthentication for COOKIE.
MESSAGE is shown in the echo area.  IDENTITIES must contain a
unix-user.  The method returns only after the helper finishes."
  (cond
   ((not (stringp cookie))
    '(:error "org.freedesktop.PolicyKit1.Error.Failed" "Missing cookie"))
   ((gethash cookie exwm-cc-polkit--sessions)
    '(:error "org.freedesktop.PolicyKit1.Error.Failed"
             "Authentication is already in progress"))
   (t
    (let* ((uid (exwm-cc-polkit-unix-uid identities))
           (user (exwm-cc-polkit--user-name uid)))
      (if (not user)
          '(:error "org.freedesktop.PolicyKit1.Error.Failed"
                   "No unix-user identity to authenticate")
        (let ((session (make-exwm-cc-polkit-session :cookie cookie))
              (result '(:error "org.freedesktop.PolicyKit1.Error.Failed"
                               "Authentication failed")))
          (unwind-protect
              (condition-case err
                  (let ((proc (exwm-cc-polkit--spawn user)))
                    (setf (exwm-cc-polkit-session-process session) proc)
                    (exwm-cc-polkit--attach proc session)
                    (process-send-string proc (concat cookie "\n"))
                    (puthash cookie session exwm-cc-polkit--sessions)
                    (when (and (stringp message) (not (string-empty-p message)))
                      (message "%s" message))
                    (let ((deadline (+ (float-time) 180)))
                      (while (and (not (exwm-cc-polkit-session-done session))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.2)
                        (exwm-cc-polkit--drain session)))
                    (unless (exwm-cc-polkit-session-done session)
                      (exwm-cc-polkit--kill session)
                      (setf (exwm-cc-polkit-session-done session) t))
                    (setq result
                          (cond
                           ((exwm-cc-polkit-session-gained session) :ignore)
                           ((exwm-cc-polkit-session-cancelled session)
                            '(:error "org.freedesktop.PolicyKit1.Error.Cancelled"
                                     "Authentication dismissed"))
                           (t result))))
                (error
                 (setq result
                       (list :error "org.freedesktop.PolicyKit1.Error.Failed"
                             (error-message-string err)))))
            (exwm-cc-polkit--kill session)
            (remhash cookie exwm-cc-polkit--sessions))
          result))))))

(defun exwm-cc-polkit--cancel (cookie)
  "Stop the helper conversation identified by COOKIE."
  (let ((session (and (stringp cookie)
                      (gethash cookie exwm-cc-polkit--sessions))))
    (when session
      (setf (exwm-cc-polkit-session-cancelled session) t
            (exwm-cc-polkit-session-done session) t)
      (exwm-cc-polkit--kill session)))
  :ignore)

(defun exwm-cc-polkit--enable ()
  "Export the agent and register it for this session."
  (let ((session (getenv "XDG_SESSION_ID")))
    (unless (and (stringp session) (not (string-empty-p session)))
      (error "XDG_SESSION_ID is not set"))
    (unless (or (file-exists-p exwm-cc-polkit-socket)
                (file-executable-p exwm-cc-polkit-helper))
      (error "PolicyKit agent helper is not installed"))
    (exwm-cc-polkit--export)
    (condition-case err
        (exwm-cc-polkit-register-session session)
      (error
       (exwm-cc-polkit--unexport)
       (signal (car err) (cdr err))))))

(defun exwm-cc-polkit--disable ()
  "Unregister the agent for this session and drop its methods."
  (when exwm-cc-polkit--registrations
    (let ((session (getenv "XDG_SESSION_ID")))
      (when (and (stringp session) (not (string-empty-p session)))
        (condition-case err
            (exwm-cc-polkit-unregister-session session)
          (error (message "PolicyKit unregister: %s"
                          (error-message-string err)))))))
  (exwm-cc-polkit--unexport))

;;;###autoload
(define-minor-mode exwm-cc-polkit-mode
  "Register Emacs as the PolicyKit agent for this session.
Off by default.  Enabling this replaces the desktop's
authentication agent.  Disabling it unregisters this agent and
does not restart the previous one."
  :global t
  :group 'exwm-cc
  (unless exwm-cc-polkit--guard
    (if exwm-cc-polkit-mode
        (condition-case err
            (exwm-cc-polkit--enable)
          (error
           (exwm-cc-polkit--disable)
           (setq exwm-cc-polkit--guard t)
           (setq exwm-cc-polkit-mode nil)
           (setq exwm-cc-polkit--guard nil)
           (signal (car err) (cdr err))))
      (exwm-cc-polkit--disable))))

(defun exwm-cc-polkit-toggle ()
  "Turn `exwm-cc-polkit-mode' on or off after a confirmation."
  (interactive)
  (if exwm-cc-polkit-mode
      (when (yes-or-no-p "Unregister the Emacs PolicyKit agent? This does not restart the previous agent. ")
        (exwm-cc-polkit-mode -1))
    (when (yes-or-no-p "Replace the session PolicyKit agent with Emacs? ")
      (exwm-cc-polkit-mode 1))))

(provide 'exwm-cc-polkit)

;;; exwm-cc-polkit.el ends here
