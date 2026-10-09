;;; exwm-cc-nm.el --- NetworkManager for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Device and access-point state, scan, disconnect, and the wifi radio
;; go through NetworkManager's D-Bus API.  Connecting to a network and
;; bringing a saved connection up go through nmcli, asynchronously,
;; because AddAndActivateConnection's nested dictionaries are a poor fit
;; for a call that must not freeze a stock EXWM command loop.
;;
;; The PSK is an argument of that nmcli process for as long as the
;; process runs.  It is not written into messages or stored on a row.
;;
;; Keys in the device list:
;;   RET      access points for a wifi device
;;   c        connect
;;   a        activate a saved connection
;;   k        disconnect
;;   s        scan
;;   r        toggle the wifi radio
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-nm-service "org.freedesktop.NetworkManager")
(defconst exwm-cc-nm-path "/org/freedesktop/NetworkManager")
(defconst exwm-cc-nm-iface "org.freedesktop.NetworkManager")
(defconst exwm-cc-nm-device-iface "org.freedesktop.NetworkManager.Device")
(defconst exwm-cc-nm-wifi-iface "org.freedesktop.NetworkManager.Device.Wireless")
(defconst exwm-cc-nm-ap-iface "org.freedesktop.NetworkManager.AccessPoint")
(defconst exwm-cc-nm-active-iface "org.freedesktop.NetworkManager.Connection.Active")
(defconst exwm-cc-nm-buffer "*EXWM Network*")
(defconst exwm-cc-nm-ap-buffer "*EXWM Access Points*")

(defcustom exwm-cc-nm-show-all nil
  "When non-nil, list loopback and dummy NetworkManager devices."
  :type 'boolean
  :group 'exwm-cc)

(defun exwm-cc-nm-type-name (type)
  "Return a short name for NetworkManager device TYPE."
  (alist-get type '((1 . "ethernet")
                    (2 . "wifi")
                    (5 . "bluetooth")
                    (8 . "modem")
                    (10 . "bond")
                    (11 . "vlan")
                    (13 . "bridge")
                    (14 . "generic")
                    (16 . "tun")
                    (22 . "dummy")
                    (29 . "wireguard")
                    (30 . "wifi-p2p")
                    (32 . "loopback"))
             "other" nil #'eql))

(defun exwm-cc-nm-state-name (state)
  "Return a short name for NetworkManager device STATE."
  (alist-get state '((10 . "unmanaged")
                     (20 . "unavailable")
                     (30 . "disconnected")
                     (40 . "prepare")
                     (50 . "config")
                     (60 . "need auth")
                     (70 . "ip config")
                     (90 . "secondaries")
                     (100 . "connected")
                     (110 . "deactivating")
                     (120 . "failed"))
             "unknown" nil #'eql))

(defun exwm-cc-nm-security (flags wpa rsn)
  "Return open, WEP, or WPA from access-point flag values.
FLAGS is the Privacy bitfield.  Non-zero WPA or RSN means WPA."
  (cond ((or (and (integerp wpa) (> wpa 0))
             (and (integerp rsn) (> rsn 0)))
         "WPA")
        ((and (integerp flags) (> (logand flags 1) 0))
         "WEP")
        (t "open")))

(defun exwm-cc-nm-ssid (value)
  "Return the access-point SSID in VALUE, or \"(hidden)\"."
  (let ((text (cond ((stringp value) value)
                    ((integerp value)
                     (exwm-cc-bytes-to-string (list value)))
                    ((and (consp value) (cl-every #'integerp value))
                     (exwm-cc-bytes-to-string value))
                    (t ""))))
    (if (or (null text) (string-empty-p text))
        "(hidden)"
      text)))

(defun exwm-cc-nm-connect-command (ssid ifname password)
  "Build the nmcli command that connects to SSID.
IFNAME and PASSWORD are optional.  PASSWORD is omitted when nil
or empty, which is how an open network is joined."
  (let ((cmd (list "nmcli" "-w" "25" "device" "wifi" "connect" ssid)))
    (when (and (stringp password) (not (string-empty-p password)))
      (setq cmd (append cmd (list "password" password))))
    (when (and (stringp ifname) (not (string-empty-p ifname)))
      (setq cmd (append cmd (list "ifname" ifname))))
    cmd))

(defun exwm-cc-nm-activate-command (uuid)
  "Build the nmcli command that activates the connection UUID."
  (list "nmcli" "-w" "25" "connection" "up" "uuid" uuid))

(defun exwm-cc-nm-parse-connections (text)
  "Parse terse nmcli connection TEXT.
Each element is (NAME UUID TYPE).  A colon inside NAME is kept."
  (delq nil
        (mapcar
         (lambda (line)
           (when (string-match
                  "\\`\\(.*\\):\\([0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}\\):\\([^:]+\\)\\'"
                  line)
             (list (match-string 1 line)
                   (match-string 2 line)
                   (match-string 3 line))))
         (split-string text "\n" t))))

(defun exwm-cc-nm--present-p ()
  "Return non-nil when NetworkManager answers on the system bus."
  (exwm-cc-dbus-present-p :system exwm-cc-nm-service))

(defun exwm-cc-nm--call (method &rest args)
  "Call NetworkManager manager METHOD with ARGS."
  (apply #'dbus-call-method :system exwm-cc-nm-service exwm-cc-nm-path
         exwm-cc-nm-iface method args))

(defun exwm-cc-nm--active-name (path)
  "Return the Id of active connection PATH, or an empty string."
  (if (or (not (stringp path)) (string-empty-p path) (equal path "/"))
      ""
    (let ((id (exwm-cc-dbus-prop :system exwm-cc-nm-service path
                                 exwm-cc-nm-active-iface "Id")))
      (if (stringp id) id ""))))

(defun exwm-cc-nm-devices ()
  "Return a plist for every NetworkManager device that should be shown."
  (unless (exwm-cc-nm--present-p)
    (error "NetworkManager is not running"))
  (delq nil
        (mapcar
         (lambda (path)
           (let* ((props (exwm-cc-dbus-alist :system exwm-cc-nm-service path
                                             exwm-cc-nm-device-iface))
                  (type (exwm-cc-number (cdr (assoc "DeviceType" props))))
                  (state (exwm-cc-number (cdr (assoc "State" props))))
                  (iface (cdr (assoc "Interface" props)))
                  (active (cdr (assoc "ActiveConnection" props))))
             (when (and (integerp type)
                        (or exwm-cc-nm-show-all (not (memq type '(22 32)))))
               (list :path path
                     :iface (if (stringp iface) iface path)
                     :type type
                     :type-name (exwm-cc-nm-type-name type)
                     :state state
                     :state-name (exwm-cc-nm-state-name state)
                     :connection (exwm-cc-nm--active-name active)
                     :active (and (stringp active) (not (equal active "/"))
                                  active)))))
         (exwm-cc-object-paths (exwm-cc-nm--call "GetDevices")))))

(defun exwm-cc-nm-wireless-enabled ()
  "Return non-nil when NetworkManager's wifi radio is on."
  (eq (dbus-get-property :system exwm-cc-nm-service exwm-cc-nm-path
                         exwm-cc-nm-iface "WirelessEnabled")
      t))

(defun exwm-cc-nm-summary ()
  "Return one status line for the network section."
  (if (not (exwm-cc-nm--present-p))
      "NetworkManager is not running"
    (condition-case err
        (let* ((rows (exwm-cc-nm-devices))
               (up (cl-remove-if-not
                    (lambda (row) (eql (plist-get row :state) 100))
                    rows))
               (parts (mapcar
                       (lambda (row)
                         (let ((name (plist-get row :connection))
                               (iface (plist-get row :iface)))
                           (if (and (stringp name) (not (string-empty-p name))
                                    (not (equal name iface)))
                               (format "%s %s" iface name)
                             iface)))
                       up)))
          (cond (parts (mapconcat #'identity parts ", "))
                ((exwm-cc-nm-wireless-enabled) "no active connection")
                (t "wifi off")))
      (error (error-message-string err)))))

(defun exwm-cc-nm--fill ()
  "Fill the device list from NetworkManager."
  (setq tabulated-list-format [("Device" 16 t)
                               ("Type" 12 t)
                               ("State" 14 t)
                               ("Connection" 28 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-nm-devices)
   (list (lambda (row) (plist-get row :iface))
         (lambda (row) (plist-get row :type-name))
         (lambda (row) (plist-get row :state-name))
         (lambda (row) (plist-get row :connection)))))

(defvar exwm-cc-nm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "RET") #'exwm-cc-nm-access-points)
    (define-key map (kbd "c") #'exwm-cc-nm-connect)
    (define-key map (kbd "a") #'exwm-cc-nm-activate)
    (define-key map (kbd "k") #'exwm-cc-nm-disconnect)
    (define-key map (kbd "s") #'exwm-cc-nm-scan)
    (define-key map (kbd "r") #'exwm-cc-nm-radio)
    map)
  "Keymap for the NetworkManager device list.")

(define-derived-mode exwm-cc-nm-mode exwm-cc-list-mode "CC-Net"
  "NetworkManager devices.

\\{exwm-cc-nm-mode-map}")

(defun exwm-cc-nm ()
  "Show NetworkManager devices."
  (interactive)
  (unless (exwm-cc-nm--present-p)
    (user-error "NetworkManager is not running"))
  (exwm-cc-pop-list exwm-cc-nm-buffer #'exwm-cc-nm-mode #'exwm-cc-nm--fill))

(defun exwm-cc-nm--row ()
  "Return the device or access-point plist at point."
  (or (exwm-cc-list-row)
      (user-error "No row on this line")))

(defun exwm-cc-nm-access-points ()
  "Show access points for the wifi device at point."
  (interactive)
  (let* ((row (exwm-cc-nm--row))
         (path (plist-get row :path))
         (iface (plist-get row :iface)))
    (unless (eql (plist-get row :type) 2)
      (user-error "%s is not a wifi device" iface))
    (exwm-cc-nm--pop-aps path iface)))

(defun exwm-cc-nm--ap-rows (path iface)
  "Return access-point plists for wifi device PATH named IFACE."
  (let ((paths (exwm-cc-object-paths
                (exwm-cc-dbus-prop :system exwm-cc-nm-service path
                                   exwm-cc-nm-wifi-iface "AccessPoints"))))
    (sort (copy-sequence
           (delq nil
                 (mapcar
                  (lambda (ap)
                    (let ((props (exwm-cc-dbus-alist
                                  :system exwm-cc-nm-service ap
                                  exwm-cc-nm-ap-iface)))
                      (when props
                        (list :path ap
                              :iface iface
                              :ssid (exwm-cc-nm-ssid (cdr (assoc "Ssid" props)))
                              :security (exwm-cc-nm-security
                                         (exwm-cc-number (cdr (assoc "Flags" props)))
                                         (exwm-cc-number (cdr (assoc "WpaFlags" props)))
                                         (exwm-cc-number (cdr (assoc "RsnFlags" props))))
                              :strength (exwm-cc-number (cdr (assoc "Strength" props)))
                              :frequency (exwm-cc-number (cdr (assoc "Frequency" props)))))))
                  paths)))
          (lambda (a b)
            (> (or (plist-get a :strength) 0)
               (or (plist-get b :strength) 0))))))

(defun exwm-cc-nm--fill-aps (path iface)
  "Fill the access-point list for device PATH named IFACE."
  (setq tabulated-list-format [("SSID" 28 t)
                               ("Security" 10 t)
                               ("Signal" 8 t)
                               ("MHz" 8 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-nm--ap-rows path iface)
   (list (lambda (row) (plist-get row :ssid))
         (lambda (row) (plist-get row :security))
         (lambda (row)
           (let ((n (plist-get row :strength)))
             (if (numberp n) (format "%d%%" n) "")))
         (lambda (row)
           (let ((n (plist-get row :frequency)))
             (if (numberp n) (format "%d" n) ""))))))

(defun exwm-cc-nm--pop-aps (path iface)
  "Pop the access-point buffer for device PATH named IFACE."
  (exwm-cc-pop-list exwm-cc-nm-ap-buffer #'exwm-cc-nm-ap-mode
                    (lambda () (exwm-cc-nm--fill-aps path iface))))

(defvar exwm-cc-nm-ap-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "c") #'exwm-cc-nm-connect)
    map)
  "Keymap for the access-point list.")

(define-derived-mode exwm-cc-nm-ap-mode exwm-cc-list-mode "CC-AP"
  "NetworkManager access points.

\\{exwm-cc-nm-ap-mode-map}")

(defun exwm-cc-nm--finish (code text ssid)
  "Report nmcli exit CODE and TEXT for SSID, then refresh."
  (message "Network: %s"
           (if (eql code 0)
               (format "finished %s" ssid)
             (string-trim text)))
  (exwm-cc-refresh-buffer exwm-cc-nm-buffer)
  (exwm-cc-refresh-buffer exwm-cc-nm-ap-buffer))

(defun exwm-cc-nm-connect ()
  "Connect to the access point at point, or prompt for an SSID.
The nmcli process is asynchronous.  An empty password joins an
open network."
  (interactive)
  (exwm-cc-require-program "nmcli")
  (let* ((row (exwm-cc-list-row))
         (ssid (if (and row (plist-get row :ssid))
                   (plist-get row :ssid)
                 (read-string "SSID: ")))
         (iface (and row (plist-get row :iface)))
         (security (and row (plist-get row :security)))
         (password (unless (equal security "open")
                     (read-passwd "Password (empty if the network is open): "))))
    (when (or (not (stringp ssid)) (string-empty-p ssid))
      (user-error "SSID is empty"))
    (when (equal ssid "(hidden)")
      (setq ssid (read-string "Hidden SSID: ")))
    (unless (and (stringp password) (not (string-empty-p password)))
      (setq password nil))
    (exwm-cc-start
     "nm-connect"
     (exwm-cc-nm-connect-command ssid iface password)
     (lambda (code text) (exwm-cc-nm--finish code text ssid)))
    (setq password nil)
    (message "Connecting to %s" ssid)))

(defun exwm-cc-nm-saved ()
  "Return (NAME UUID TYPE) for each saved NetworkManager connection."
  (exwm-cc-require-program "nmcli")
  (let ((res (exwm-cc-command-output
              "nmcli" "-t" "-f" "NAME,UUID,TYPE" "connection" "show")))
    (unless (eql (car res) 0)
      (error "%s" (string-trim (cdr res))))
    (exwm-cc-nm-parse-connections (cdr res))))

(defun exwm-cc-nm-activate ()
  "Activate a saved NetworkManager connection."
  (interactive)
  (let* ((rows (exwm-cc-nm-saved))
         (labels (mapcar (lambda (row)
                           (format "%s (%s)" (nth 0 row) (nth 1 row)))
                         rows))
         (choice (completing-read "Connection: " labels nil t))
         (uuid (and (string-match "(\\([^)]+\\))\\'" choice)
                    (match-string 1 choice))))
    (unless uuid
      (user-error "No connection selected"))
    (exwm-cc-start "nm-up" (exwm-cc-nm-activate-command uuid)
                   (lambda (code text) (exwm-cc-nm--finish code text choice)))
    (message "Activating %s" choice)))

(defun exwm-cc-nm-disconnect ()
  "Deactivate the connection on the device at point."
  (interactive)
  (let* ((row (exwm-cc-nm--row))
         (active (plist-get row :active)))
    (unless (stringp active)
      (user-error "%s has no active connection" (plist-get row :iface)))
    (condition-case err
        (progn
          (exwm-cc-nm--call "DeactivateConnection" :object-path active)
          (message "Disconnected %s" (plist-get row :iface))
          (exwm-cc-list-refresh))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-nm-scan ()
  "Ask the wifi device at point to scan."
  (interactive)
  (let* ((row (exwm-cc-nm--row))
         (path (plist-get row :path))
         (iface (plist-get row :iface)))
    (unless (or (eql (plist-get row :type) 2) (plist-get row :ssid))
      (user-error "%s is not a wifi device" iface))
    (when (plist-get row :ssid)
      (user-error "Press s in the device list to scan"))
    (condition-case err
        (progn
          (dbus-call-method :system exwm-cc-nm-service path
                            exwm-cc-nm-wifi-iface "RequestScan"
                            exwm-cc-empty-dict)
          (message "Scanning on %s" iface)
          (run-with-timer 3 nil #'exwm-cc-refresh-buffer exwm-cc-nm-ap-buffer))
      (error (user-error "%s" (error-message-string err))))))

(defun exwm-cc-nm-radio ()
  "Toggle the NetworkManager wifi radio."
  (interactive)
  (condition-case err
      (let ((on (not (exwm-cc-nm-wireless-enabled))))
        (dbus-set-property :system exwm-cc-nm-service exwm-cc-nm-path
                           exwm-cc-nm-iface "WirelessEnabled" on)
        (message "Wifi %s" (if on "on" "off"))
        (exwm-cc-list-refresh))
    (error (user-error "%s" (error-message-string err)))))

(exwm-cc-add-section 'network "Network" #'exwm-cc-nm-summary #'exwm-cc-nm)

(provide 'exwm-cc-nm)

;;; exwm-cc-nm.el ends here
