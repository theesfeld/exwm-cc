;;; exwm-cc-bt.el --- BlueZ for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Adapter power, discovery, and device connect, disconnect, pair, and
;; trust go through BlueZ on the system bus.  This package does not
;; call AgentManager1.RequestDefaultAgent.  Pairing uses Device1.Pair
;; and the session's existing agent, usually blueman, shows the dialog.
;;
;; Keys:
;;   c        connect
;;   d        disconnect
;;   p        pair
;;   t        toggle trust
;;   s        start or stop discovery
;;   P        toggle adapter power
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-bt-service "org.bluez")
(defconst exwm-cc-bt-buffer "*EXWM Bluetooth*")

(defcustom exwm-cc-bt-agent nil
  "When non-nil, refuse to pair rather than replace the session agent.
The in-Emacs BlueZ agent is not implemented.  Leave this nil so
Device1.Pair uses the agent that is already registered."
  :type 'boolean
  :group 'exwm-cc)

(defun exwm-cc-bt--objects ()
  "Return BlueZ managed objects as (PATH IFACE ...)."
  (exwm-cc-dbus-managed :system exwm-cc-bt-service "/"))

(defun exwm-cc-bt-adapters ()
  "Return a plist for every BlueZ adapter."
  (delq nil
        (mapcar
         (lambda (obj)
           (when (member "org.bluez.Adapter1" (cdr obj))
             (let ((props (exwm-cc-dbus-alist :system exwm-cc-bt-service
                                              (car obj) "org.bluez.Adapter1")))
               (list :path (car obj)
                     :name (or (cdr (assoc "Name" props)) (car obj))
                     :address (or (cdr (assoc "Address" props)) "")
                     :powered (eq (cdr (assoc "Powered" props)) t)
                     :discovering (eq (cdr (assoc "Discovering" props)) t)))))
         (exwm-cc-bt--objects))))

(defun exwm-cc-bt--device-p (path)
  "Return non-nil when PATH is a BlueZ device and not a GATT child."
  (and (stringp path)
       (string-match-p "\\`/org/bluez/hci[0-9]+/dev_[0-9A-Fa-f_]+\\'" path)))

(defun exwm-cc-bt-devices ()
  "Return a plist for every BlueZ device."
  (delq nil
        (mapcar
         (lambda (obj)
           (when (and (member "org.bluez.Device1" (cdr obj))
                      (exwm-cc-bt--device-p (car obj)))
             (let* ((props (exwm-cc-dbus-alist :system exwm-cc-bt-service
                                               (car obj) "org.bluez.Device1"))
                    (name (cdr (assoc "Alias" props)))
                    (fallback (cdr (assoc "Name" props)))
                    (address (cdr (assoc "Address" props))))
               (list :path (car obj)
                     :name (or (and (stringp name) name)
                               (and (stringp fallback) fallback)
                               (and (stringp address) address)
                               (car obj))
                     :address (if (stringp address) address "")
                     :paired (eq (cdr (assoc "Paired" props)) t)
                     :connected (eq (cdr (assoc "Connected" props)) t)
                     :trusted (eq (cdr (assoc "Trusted" props)) t)
                     :rssi (exwm-cc-number (cdr (assoc "RSSI" props)))))))
         (exwm-cc-bt--objects))))

(defun exwm-cc-bt--status (row)
  "Return the status text for device ROW."
  (mapconcat #'identity
             (delq nil
                   (list (and (plist-get row :paired) "paired")
                         (and (plist-get row :connected) "connected")
                         (and (plist-get row :trusted) "trusted")))
             ", "))

(defun exwm-cc-bt-summary ()
  "Return one status line for the bluetooth section."
  (if (not (exwm-cc-dbus-present-p :system exwm-cc-bt-service))
      "BlueZ is not running"
    (condition-case err
        (let* ((adapters (exwm-cc-bt-adapters))
               (adapter (car adapters))
               (connected (length (cl-remove-if-not
                                   (lambda (row) (plist-get row :connected))
                                   (exwm-cc-bt-devices)))))
          (cond ((null adapter) "no adapter")
                ((plist-get adapter :powered)
                 (format "%s on, %d connected"
                         (file-name-nondirectory (plist-get adapter :path))
                         connected))
                (t (format "%s off"
                           (file-name-nondirectory (plist-get adapter :path))))))
      (error (error-message-string err)))))

(defun exwm-cc-bt--fill ()
  "Fill the bluetooth device list."
  (setq tabulated-list-format [("Name" 28 t)
                               ("Address" 18 t)
                               ("Status" 28 t)
                               ("RSSI" 6 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-bt-devices)
   (list (lambda (row) (plist-get row :name))
         (lambda (row) (plist-get row :address))
         #'exwm-cc-bt--status
         (lambda (row)
           (let ((rssi (plist-get row :rssi)))
             (if (numberp rssi) (format "%d" rssi) ""))))))

(defvar exwm-cc-bt-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "c") #'exwm-cc-bt-connect)
    (define-key map (kbd "d") #'exwm-cc-bt-disconnect)
    (define-key map (kbd "p") #'exwm-cc-bt-pair)
    (define-key map (kbd "t") #'exwm-cc-bt-trust)
    (define-key map (kbd "s") #'exwm-cc-bt-scan)
    (define-key map (kbd "P") #'exwm-cc-bt-power)
    map)
  "Keymap for the bluetooth list.")

(define-derived-mode exwm-cc-bt-mode exwm-cc-list-mode "CC-BT"
  "BlueZ devices.

\\{exwm-cc-bt-mode-map}")

(defun exwm-cc-bt ()
  "Show BlueZ devices."
  (interactive)
  (unless (exwm-cc-dbus-present-p :system exwm-cc-bt-service)
    (user-error "BlueZ is not running"))
  (exwm-cc-pop-list exwm-cc-bt-buffer #'exwm-cc-bt-mode #'exwm-cc-bt--fill))

(defun exwm-cc-bt--row ()
  "Return the device plist at point."
  (or (exwm-cc-list-row)
      (user-error "No device on this line")))

(defun exwm-cc-bt--device-call (method)
  "Call Device1 METHOD on the device at point."
  (let ((path (plist-get (exwm-cc-bt--row) :path)))
    (condition-case err
        (progn
          (dbus-call-method :system exwm-cc-bt-service path
                            "org.bluez.Device1" method
                            :timeout 45000)
          (message "Bluetooth %s" method)
          (exwm-cc-list-refresh))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-bt-connect ()
  "Connect the device at point."
  (interactive)
  (exwm-cc-bt--device-call "Connect"))

(defun exwm-cc-bt-disconnect ()
  "Disconnect the device at point."
  (interactive)
  (exwm-cc-bt--device-call "Disconnect"))

(defun exwm-cc-bt-pair ()
  "Pair the device at point.
The session's existing BlueZ agent presents the dialog.  This
command does not register an agent."
  (interactive)
  (when exwm-cc-bt-agent
    (user-error "Leave `exwm-cc-bt-agent' nil; this package does not replace the BlueZ agent"))
  (exwm-cc-bt--device-call "Pair"))

(defun exwm-cc-bt-trust ()
  "Toggle Trusted on the device at point."
  (interactive)
  (let* ((row (exwm-cc-bt--row))
         (path (plist-get row :path))
         (trusted (not (plist-get row :trusted))))
    (condition-case err
        (progn
          (dbus-set-property :system exwm-cc-bt-service path
                             "org.bluez.Device1" "Trusted" trusted)
          (message "Trusted %s" (if trusted "on" "off"))
          (exwm-cc-list-refresh))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-bt--adapter ()
  "Return the adapter plist for the device at point, or the first adapter."
  (let* ((row (exwm-cc-list-row))
         (path (and row (plist-get row :path)))
         (prefix (and (stringp path)
                      (string-match "\\`\\(/org/bluez/hci[0-9]+\\)" path)
                      (match-string 1 path)))
         (adapters (exwm-cc-bt-adapters)))
    (or (and prefix (cl-find prefix adapters
                             :key (lambda (item) (plist-get item :path))
                             :test #'equal))
        (car adapters)
        (user-error "No Bluetooth adapter"))))

(defun exwm-cc-bt-scan ()
  "Start or stop discovery on the current adapter."
  (interactive)
  (let* ((adapter (exwm-cc-bt--adapter))
         (method (if (plist-get adapter :discovering)
                     "StopDiscovery"
                   "StartDiscovery")))
    (condition-case err
        (progn
          (dbus-call-method :system exwm-cc-bt-service
                            (plist-get adapter :path)
                            "org.bluez.Adapter1" method)
          (message "Bluetooth %s" method)
          (run-with-timer 2 nil #'exwm-cc-refresh-buffer exwm-cc-bt-buffer))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-bt-power ()
  "Toggle power on the current adapter."
  (interactive)
  (let* ((adapter (exwm-cc-bt--adapter))
         (on (not (plist-get adapter :powered))))
    (condition-case err
        (progn
          (dbus-set-property :system exwm-cc-bt-service
                             (plist-get adapter :path)
                             "org.bluez.Adapter1" "Powered" on)
          (message "Bluetooth power %s" (if on "on" "off"))
          (exwm-cc-list-refresh))
      (error (user-error "%s" (error-message-string err))))))

(exwm-cc-add-section 'bluetooth "Bluetooth" #'exwm-cc-bt-summary #'exwm-cc-bt)

(provide 'exwm-cc-bt)

;;; exwm-cc-bt.el ends here
