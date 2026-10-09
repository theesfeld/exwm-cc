;;; exwm-cc-power.el --- UPower and session lock for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Batteries and peripheral cells come from UPower.  The power profile
;; comes from org.freedesktop.UPower.PowerProfiles, with
;; net.hadess.PowerProfiles as a fallback.  Locking the session calls
;; logind LockSession and asks first.
;;
;; Keys:
;;   1        power-saver
;;   2        balanced
;;   3        performance
;;   L        lock this session
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-power-service "org.freedesktop.UPower")
(defconst exwm-cc-power-path "/org/freedesktop/UPower")
(defconst exwm-cc-power-device-iface "org.freedesktop.UPower.Device")
(defconst exwm-cc-power-buffer "*EXWM Power*")

(defconst exwm-cc-power-types
  '((0 . "unknown") (1 . "line") (2 . "battery") (3 . "ups")
    (4 . "monitor") (5 . "mouse") (6 . "keyboard") (7 . "pda")
    (8 . "phone"))
  "UPower device type numbers.")

(defconst exwm-cc-power-states
  '((0 . "unknown") (1 . "charging") (2 . "discharging") (3 . "empty")
    (4 . "full") (5 . "pending charge") (6 . "pending discharge"))
  "UPower device state numbers.")

(defun exwm-cc-power--profiles-target ()
  "Return (SERVICE PATH INTERFACE) for the power-profiles daemon, or nil."
  (cond
   ((exwm-cc-dbus-present-p :system "org.freedesktop.UPower.PowerProfiles")
    '("org.freedesktop.UPower.PowerProfiles"
      "/org/freedesktop/UPower/PowerProfiles"
      "org.freedesktop.UPower.PowerProfiles"))
   ((exwm-cc-dbus-present-p :system "net.hadess.PowerProfiles")
    '("net.hadess.PowerProfiles"
      "/net/hadess/PowerProfiles"
      "net.hadess.PowerProfiles"))
   (t nil)))

(defun exwm-cc-power-profile-names ()
  "Return the available power profile names."
  (let ((target (exwm-cc-power--profiles-target))
        (names nil))
    (when target
      (cl-labels
          ((walk (value)
             (cond
              ((and (consp value) (equal (car value) "Profile"))
               (let ((name (exwm-cc-dbus-unwrap (cdr value))))
                 (when (stringp name)
                   (push name names))))
              ((consp value)
               (walk (car value))
               (walk (cdr value))))))
        (walk (dbus-get-property :system (nth 0 target) (nth 1 target)
                                 (nth 2 target) "Profiles"))))
    (nreverse (delete-dups names))))

(defun exwm-cc-power-active-profile ()
  "Return the active power profile, or nil when none is set."
  (let ((target (exwm-cc-power--profiles-target)))
    (when target
      (let ((name (exwm-cc-dbus-prop :system (nth 0 target) (nth 1 target)
                                     (nth 2 target) "ActiveProfile")))
        (and (stringp name) name)))))

(defun exwm-cc-power-devices ()
  "Return a plist for every enumerated UPower device."
  (unless (exwm-cc-dbus-present-p :system exwm-cc-power-service)
    (error "UPower is not running"))
  (mapcar
   (lambda (path)
     (let ((props (exwm-cc-dbus-alist :system exwm-cc-power-service path
                                      exwm-cc-power-device-iface)))
       (list :path path
             :name (file-name-nondirectory path)
             :kind (or (alist-get (exwm-cc-number (cdr (assoc "Type" props)))
                                  exwm-cc-power-types nil nil #'eql)
                       "unknown")
             :state (or (alist-get (exwm-cc-number (cdr (assoc "State" props)))
                                   exwm-cc-power-states nil nil #'eql)
                        "unknown")
             :percent (exwm-cc-number (cdr (assoc "Percentage" props)))
             :model (let ((model (cdr (assoc "Model" props))))
                      (if (stringp model) model "")))))
   (exwm-cc-object-paths
    (dbus-call-method :system exwm-cc-power-service exwm-cc-power-path
                      exwm-cc-power-service "EnumerateDevices"))))

(defun exwm-cc-power--percent (value)
  "Format a battery percentage VALUE."
  (if (numberp value) (format "%.0f%%" value) ""))

(defun exwm-cc-power-summary ()
  "Return one status line for the power section."
  (if (not (exwm-cc-dbus-present-p :system exwm-cc-power-service))
      "UPower is not running"
    (condition-case err
        (let* ((rows (exwm-cc-power-devices))
               (bats (cl-remove-if-not
                      (lambda (row) (equal (plist-get row :kind) "battery"))
                      rows))
               (others (cl-count-if
                        (lambda (row)
                          (not (member (plist-get row :kind) '("battery" "line"))))
                        rows))
               (parts (mapcar
                       (lambda (row)
                         (format "%s %s %s"
                                 (plist-get row :name)
                                 (exwm-cc-power--percent (plist-get row :percent))
                                 (plist-get row :state)))
                       bats))
               (profile (exwm-cc-power-active-profile)))
          (when (> others 0)
            (setq parts (append parts (list (format "%d peripherals" others)))))
          (when profile
            (setq parts (append parts (list profile))))
          (if parts (mapconcat #'identity parts ", ") "no batteries"))
      (error (error-message-string err)))))

(defun exwm-cc-power--fill ()
  "Fill the power device list."
  (setq tabulated-list-format [("Device" 22 t)
                               ("Kind" 12 t)
                               ("State" 18 t)
                               ("Charge" 8 t)
                               ("Model" 24 t)])
  (tabulated-list-init-header)
  (setq mode-line-format
        (list "" mode-line-front-space mode-line-buffer-identification
              "  "
              (format "profile:%s" (or (exwm-cc-power-active-profile) "n/a"))
              "  " mode-line-end-spaces))
  (exwm-cc-fill-list
   (exwm-cc-power-devices)
   (list (lambda (row) (plist-get row :name))
         (lambda (row) (plist-get row :kind))
         (lambda (row) (plist-get row :state))
         (lambda (row) (exwm-cc-power--percent (plist-get row :percent)))
         (lambda (row) (plist-get row :model)))))

(defvar exwm-cc-power-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "1") #'exwm-cc-power-saver)
    (define-key map (kbd "2") #'exwm-cc-power-balanced)
    (define-key map (kbd "3") #'exwm-cc-power-performance)
    (define-key map (kbd "L") #'exwm-cc-lock)
    map)
  "Keymap for the power list.")

(define-derived-mode exwm-cc-power-mode exwm-cc-list-mode "CC-Power"
  "UPower devices and the session power profile.

\\{exwm-cc-power-mode-map}")

(defun exwm-cc-power ()
  "Show UPower devices."
  (interactive)
  (unless (exwm-cc-dbus-present-p :system exwm-cc-power-service)
    (user-error "UPower is not running"))
  (exwm-cc-pop-list exwm-cc-power-buffer #'exwm-cc-power-mode #'exwm-cc-power--fill))

(defun exwm-cc-power-set-profile (name)
  "Set the active power profile to NAME."
  (interactive)
  (let ((target (exwm-cc-power--profiles-target)))
    (unless target
      (user-error "Power profiles are not available"))
    (unless (member name (exwm-cc-power-profile-names))
      (user-error "Unknown profile %s" name))
    (condition-case err
        (progn
          (dbus-set-property :system (nth 0 target) (nth 1 target) (nth 2 target)
                             "ActiveProfile" name)
          (message "Power profile: %s" name)
          (when (eq major-mode 'exwm-cc-power-mode)
            (exwm-cc-list-refresh)))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-power-saver ()
  "Select the power-saver profile."
  (interactive)
  (exwm-cc-power-set-profile "power-saver"))

(defun exwm-cc-power-balanced ()
  "Select the balanced profile."
  (interactive)
  (exwm-cc-power-set-profile "balanced"))

(defun exwm-cc-power-performance ()
  "Select the performance profile."
  (interactive)
  (exwm-cc-power-set-profile "performance"))

(defun exwm-cc-lock ()
  "Lock the current logind session.
This asks for confirmation and then calls LockSession.  It does
not lock anything when the confirmation is declined."
  (interactive)
  (let ((id (getenv "XDG_SESSION_ID")))
    (unless (and (stringp id) (not (string-empty-p id)))
      (user-error "XDG_SESSION_ID is not set"))
    (when (yes-or-no-p (format "Lock session %s? " id))
      (condition-case err
          (progn
            (dbus-call-method :system "org.freedesktop.login1"
                              "/org/freedesktop/login1"
                              "org.freedesktop.login1.Manager"
                              "LockSession" id)
            (message "Lock requested"))
        (error (user-error "%s" (error-message-string err)))))))

(exwm-cc-add-section 'power "Power" #'exwm-cc-power-summary #'exwm-cc-power)

(provide 'exwm-cc-power)

;;; exwm-cc-power.el ends here
