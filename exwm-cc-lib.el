;;; exwm-cc-lib.el --- Shared helpers for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; D-Bus unwrapping, process helpers, and the tabulated-list parent mode
;; shared by the control center sections.  `dbus-get-property' returns a
;; byte array as a list of integers and a variant as a one-element list.
;; `GetManagedObjects' adds another list around each dict.  The unwrappers
;; below accept both shapes.

;;; Code:

(require 'dbus)
(require 'tabulated-list)
(require 'cl-lib)

(defgroup exwm-cc nil
  "Control center for an EXWM session, or any Emacs with D-Bus."
  :group 'unix
  :prefix "exwm-cc-")

(defconst exwm-cc-empty-dict '(:array :signature "{sv}")
  "Empty D-Bus dictionary of string keys and variant values.")

(defvar exwm-cc-sections nil
  "Control center sections, in display order.
Each element is (ID NAME SUMMARY OPEN).  SUMMARY and OPEN are
functions.  SUMMARY returns one status line.  OPEN shows the section.")

(defun exwm-cc-add-section (id name summary open)
  "Register section ID named NAME.
SUMMARY and OPEN are functions.  A repeated ID replaces the old entry
and keeps its position."
  (let ((entry (list id name summary open)))
    (if (assoc id exwm-cc-sections #'equal)
        (setq exwm-cc-sections
              (mapcar (lambda (old)
                        (if (equal (car old) id) entry old))
                      exwm-cc-sections))
      (setq exwm-cc-sections (append exwm-cc-sections (list entry))))))

(defun exwm-cc-safely (fn)
  "Call FN and return its string, or the error message."
  (condition-case err
      (let ((value (funcall fn)))
        (if (stringp value) value (format "%s" value)))
    (error (error-message-string err))))

(defun exwm-cc-dbus-present-p (bus service)
  "Return non-nil when SERVICE is owned on BUS.
ListNames is used instead of Ping.  Ping can autoload a service,
and NetworkManager's Ping reply is not a presence test."
  (and (featurep 'dbusbind)
       (condition-case nil
           (member service (dbus-list-names bus))
         (error nil))))

(defun exwm-cc--byte-array-p (value)
  "Return non-nil when VALUE is a list of integers."
  (and (consp value)
       (integerp (car value))
       (cl-every #'integerp value)))

(defun exwm-cc-bytes-to-string (bytes)
  "Decode BYTES as UTF-8.  A trailing NUL is dropped."
  (when (and bytes (eq (car (last bytes)) 0))
    (setq bytes (butlast bytes)))
  (if (null bytes)
      ""
    (decode-coding-string (apply #'unibyte-string bytes) 'utf-8 t)))

(defun exwm-cc-dbus-unwrap (value)
  "Return the Lisp value inside a D-Bus variant or byte array VALUE.
A list of more than one integer is a byte array.  A one-element list
is a variant wrapper.  A list of byte arrays is a list of strings."
  (cond
   ((and (exwm-cc--byte-array-p value) (> (length value) 1))
    (exwm-cc-bytes-to-string value))
   ((and (consp value) (null (cdr value)))
    (exwm-cc-dbus-unwrap (car value)))
   ((and (consp value)
         (cl-every #'exwm-cc--byte-array-p value))
    (mapcar #'exwm-cc-bytes-to-string value))
   (t value)))

(defun exwm-cc-dbus-prop (bus service path interface property)
  "Return PROPERTY of INTERFACE on PATH, unwrapped, or nil."
  (condition-case nil
      (exwm-cc-dbus-unwrap
       (dbus-get-property bus service path interface property))
    (error nil)))

(defun exwm-cc-dbus-managed (bus service path)
  "Return ((OBJECT-PATH IFACE ...) ...) from the object manager at PATH."
  (mapcar
   (lambda (obj)
     (cons (car obj) (mapcar #'car (cadr obj))))
   (dbus-call-method bus service path
                     "org.freedesktop.DBus.ObjectManager"
                     "GetManagedObjects")))

(defun exwm-cc-dbus-alist (bus service path interface)
  "Return INTERFACE properties on PATH as an unwrapped alist, or nil."
  (condition-case nil
      (mapcar (lambda (cell)
                (cons (car cell) (exwm-cc-dbus-unwrap (cdr cell))))
              (dbus-get-all-properties bus service path interface))
    (error nil)))

(defun exwm-cc-object-paths (value)
  "Return VALUE as a list of D-Bus object paths.
A method that returns one path comes back as a string, not a list."
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((and (listp value) (cl-every #'stringp value)) value)
        (t nil)))

(defun exwm-cc-number (value)
  "Return VALUE as a number, or nil.
A D-Bus uint64 may arrive as a cons (HIGH . LOW)."
  (cond ((numberp value) value)
        ((and (consp value) (integerp (car value)) (integerp (cdr value)))
         (+ (ash (car value) 32) (cdr value)))
        (t nil)))

(defun exwm-cc-require-program (program)
  "Signal a user error when PROGRAM is not on `exec-path'."
  (unless (executable-find program)
    (user-error "%s is not installed" program)))

(defun exwm-cc-as-list (value)
  "Return VALUE as a list of strings.  Nil becomes nil."
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((and (listp value) (cl-every #'stringp value)) value)
        (t nil)))

(defun exwm-cc-format-size (bytes)
  "Format BYTES as a short size, or an empty string."
  (if (not (numberp bytes))
      ""
    (let ((units '("B" "KB" "MB" "GB" "TB"))
          (n (float bytes)))
      (while (and (>= n 1024) (cdr units))
        (setq n (/ n 1024.0)
              units (cdr units)))
      (if (equal (car units) "B")
          (format "%.0f B" n)
        (format (if (>= n 10) "%.0f %s" "%.1f %s") n (car units))))))

(defun exwm-cc-command-output (program &rest args)
  "Run PROGRAM with ARGS.  Return (EXIT-CODE . OUTPUT), including stderr."
  (with-temp-buffer
    (cons (apply #'call-process program nil (list t t) nil args)
          (buffer-string))))

(defun exwm-cc-start (name command on-exit)
  "Run COMMAND asynchronously.  ON-EXIT is called with (EXIT-CODE OUTPUT).
The output buffer is killed afterwards.  COMMAND is a list of strings."
  (let ((buf (generate-new-buffer (format " *exwm-cc-%s*" name))))
    (make-process
     :name (format "exwm-cc-%s" name)
     :command command
     :buffer buf
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((code (process-exit-status proc))
               (text (with-current-buffer (process-buffer proc)
                       (buffer-string))))
           (kill-buffer (process-buffer proc))
           (funcall on-exit code text)))))))

(defvar-local exwm-cc-list-refresh nil
  "Function that refills the current control-center list.")

(defun exwm-cc-refresh-buffer (name)
  "Refill the control-center buffer NAME when it is live."
  (let ((buf (get-buffer name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when exwm-cc-list-refresh
          (funcall exwm-cc-list-refresh))))))

(defun exwm-cc-list-row ()
  "Return the plist stored as the id of the row at point."
  (tabulated-list-get-id))

(defun exwm-cc-list-refresh ()
  "Refill the current list from `exwm-cc-list-refresh'."
  (interactive)
  (if exwm-cc-list-refresh
      (funcall exwm-cc-list-refresh)
    (user-error "This list has no refresh function")))

(define-derived-mode exwm-cc-list-mode tabulated-list-mode "CC"
  "Parent mode for EXWM control center lists.

\\{exwm-cc-list-mode-map}"
  (setq tabulated-list-padding 1
        truncate-lines t)
  (setq-local revert-buffer-function #'exwm-cc-list-revert))

(defun exwm-cc-list-revert (&rest _)
  "Revert adapter for `exwm-cc-list-mode'."
  (exwm-cc-list-refresh))

(define-key exwm-cc-list-mode-map (kbd "g") #'exwm-cc-list-refresh)
(define-key exwm-cc-list-mode-map (kbd "q") #'quit-window)

(defun exwm-cc-pop-list (name mode refresh)
  "Show buffer NAME in MODE and call REFRESH to fill it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (eq major-mode mode)
        (funcall mode))
      (setq-local exwm-cc-list-refresh refresh)
      (funcall refresh))
    (pop-to-buffer buf)))

(defun exwm-cc-fill-list (rows columns)
  "Replace the tabulated list with ROWS.
ROWS is a list of plists.  COLUMNS is a list of functions from a
plist to a string.  Point stays on the same id when that id remains."
  (let ((at (tabulated-list-get-id)))
    (setq tabulated-list-entries
          (mapcar (lambda (row)
                    (list row
                          (apply #'vector
                                 (mapcar (lambda (fn) (or (funcall fn row) ""))
                                         columns))))
                  rows))
    (tabulated-list-print t)
    (when at
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) at))
                  (not (eobp)))
        (forward-line 1)))))

(provide 'exwm-cc-lib)

;;; exwm-cc-lib.el ends here
