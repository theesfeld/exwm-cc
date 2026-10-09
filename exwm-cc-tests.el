;;; exwm-cc-tests.el --- Tests for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Parser tests run without a bus.  Integration tests only read, and
;; they skip when a service is absent.  Nothing here connects,
;; disconnects, mounts, changes a power profile, changes RandR, locks
;; the session, or enables `exwm-cc-polkit-mode'.  The PolicyKit probe
;; calls RegisterAuthenticationAgent with session id "does-not-exist"
;; and expects an error.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'exwm-cc)

(defconst exwm-cc-tests-xrandr
  "Screen 0: minimum 320 x 200, current 8000 x 1800, maximum 16384 x 16384
eDP-1 connected primary 2880x1800+0+0 (normal left inverted right x axis y axis) 302mm x 189mm
   2880x1800    120.00*+  60.00
   1920x1080    60.00
HDMI-1 disconnected (normal left inverted right x axis y axis)
DP-1 connected 5120x1440+2880+0 (normal left inverted right x axis y axis) 1190mm x 340mm
   5120x1440    143.99*+ 119.98    59.99
   3840x1080    119.96 + 143.98    59.98
DP-2 disconnected (normal left inverted right x axis y axis)
")

(defconst exwm-cc-tests-wpctl
  "PipeWire 'pipewire-0' [1.6.9, tj@bliss, cookie:1]
 └─ Clients:
        32. WirePlumber                         [1.6.9, tj@bliss, pid:898]

Audio
 ├─ Devices:
 │      72. Meteor Lake-P HD Audio Controller   [alsa]
 │
 ├─ Sinks:
 │      75. Meteor Lake-P HD Audio Controller Speaker [vol: 0.60]
 │      63. Muted Sink                          [vol: 0.40] MUTED
 │  *  205. RØDE NT-USB Mini Analog Stereo     [vol: 0.85]
 │
 ├─ Sources:
 │  *  142. RØDE NT-USB Mini Mono              [vol: 1.00]
 │
 ├─ Filters:
 │     103. ee_soe_limiter                                               [(null)]
 │
 └─ Streams:
       199. Brave
            100. output_FR       > Easy Effects Sink:playback_FR	[active]

Video
 ├─ Sources:
 │  *   43. Integrated Camera (V4L2)
")

(defun exwm-cc-tests-find (rows key value)
  "Return the plist in ROWS whose KEY equals VALUE."
  (cl-find value rows :key (lambda (row) (plist-get row key)) :test #'equal))

(ert-deftest exwm-cc-unwrap-byte-array ()
  (should (equal (exwm-cc-dbus-unwrap '(67 105 116)) "Cit")))

(ert-deftest exwm-cc-unwrap-variant-string ()
  (should (equal (exwm-cc-dbus-unwrap '("vfat")) "vfat")))

(ert-deftest exwm-cc-unwrap-one-integer ()
  (should (equal (exwm-cc-dbus-unwrap '(66305)) 66305)))

(ert-deftest exwm-cc-unwrap-path-array ()
  (should (equal (exwm-cc-dbus-unwrap '(((47 100 101 118 47 115 100 97 0))))
                 "/dev/sda")))

(ert-deftest exwm-cc-unwrap-path-list ()
  (should (equal (exwm-cc-dbus-unwrap '((47 97 0) (47 98 0)))
                 '("/a" "/b"))))

(ert-deftest exwm-cc-section-replace-keeps-order ()
  (let ((exwm-cc-sections nil))
    (exwm-cc-add-section 'a "A" #'ignore #'ignore)
    (exwm-cc-add-section 'b "B" #'ignore #'ignore)
    (exwm-cc-add-section 'a "A2" #'ignore #'ignore)
    (should (equal (mapcar #'car exwm-cc-sections) '(a b)))
    (should (equal (nth 1 (car exwm-cc-sections)) "A2"))))

(ert-deftest exwm-cc-section-order ()
  (should (equal (mapcar #'car exwm-cc-sections)
                 '(network audio power bluetooth disks displays wireguard))))

(ert-deftest exwm-cc-nm-security-labels ()
  (should (equal (exwm-cc-nm-security 0 0 0) "open"))
  (should (equal (exwm-cc-nm-security 1 0 0) "WEP"))
  (should (equal (exwm-cc-nm-security 1 256 0) "WPA"))
  (should (equal (exwm-cc-nm-security 0 0 8) "WPA")))

(ert-deftest exwm-cc-nm-ssid-shapes ()
  (should (equal (exwm-cc-nm-ssid '(67 105 116)) "Cit"))
  (should (equal (exwm-cc-nm-ssid 67) "C"))
  (should (equal (exwm-cc-nm-ssid nil) "(hidden)")))

(ert-deftest exwm-cc-nm-connect-command-shape ()
  (should (equal (exwm-cc-nm-connect-command "Cafe" "wlan0" "secret")
                 '("nmcli" "-w" "25" "device" "wifi" "connect"
                   "Cafe" "password" "secret" "ifname" "wlan0")))
  (should (equal (exwm-cc-nm-connect-command "Cafe" "wlan0" nil)
                 '("nmcli" "-w" "25" "device" "wifi" "connect"
                   "Cafe" "ifname" "wlan0")))
  (should (equal (exwm-cc-nm-activate-command "abc")
                 '("nmcli" "-w" "25" "connection" "up" "uuid" "abc"))))

(ert-deftest exwm-cc-nm-parse-connections-keeps-colons ()
  (let ((rows (exwm-cc-nm-parse-connections
               "Citywide:Wifi:11111111-1111-1111-1111-111111111111:802-11-wireless\n")))
    (should (equal (car rows)
                   '("Citywide:Wifi"
                     "11111111-1111-1111-1111-111111111111"
                     "802-11-wireless")))))

(ert-deftest exwm-cc-audio-parse-fixture ()
  (let ((rows (exwm-cc-audio-parse exwm-cc-tests-wpctl))
        (ids nil))
    (dolist (row rows) (push (plist-get row :id) ids))
    (should (equal (sort (copy-sequence ids) #'string<)
                   '("100" "142" "199" "205" "63" "75")))
    (let ((sink (exwm-cc-tests-find rows :id "205")))
      (should (plist-get sink :default))
      (should (equal (plist-get sink :section) "sink"))
      (should (equal (plist-get sink :volume) "0.85"))
      (should-not (plist-get sink :muted))
      (should (string-match-p "RØDE" (plist-get sink :name))))
    (let ((muted (exwm-cc-tests-find rows :id "63")))
      (should (plist-get muted :muted))
      (should (equal (plist-get muted :volume) "0.40")))
    (should (equal (plist-get (exwm-cc-tests-find rows :id "199") :section) "stream"))
    (should (equal (plist-get (exwm-cc-tests-find rows :id "142") :section) "source"))
    (should-not (exwm-cc-tests-find rows :id "103"))
    (should-not (exwm-cc-tests-find rows :id "43"))
    (should-not (exwm-cc-tests-find rows :id "32"))))

(ert-deftest exwm-cc-display-parse-fixture ()
  (let* ((rows (exwm-cc-display-parse exwm-cc-tests-xrandr))
         (edp (exwm-cc-tests-find rows :name "eDP-1"))
         (hdmi (exwm-cc-tests-find rows :name "HDMI-1"))
         (dp (exwm-cc-tests-find rows :name "DP-1")))
    (should (plist-get edp :connected))
    (should (plist-get edp :primary))
    (should (equal (plist-get edp :geometry) "2880x1800+0+0"))
    (should (equal (plist-get edp :current) "2880x1800"))
    (should-not (plist-get hdmi :connected))
    (should (plist-get dp :connected))
    (should-not (plist-get dp :primary))
    (should (equal (plist-get dp :geometry) "5120x1440+2880+0"))
    (should (equal (plist-get dp :current) "5120x1440"))))

(ert-deftest exwm-cc-wg-redacts-secrets ()
  (let ((parsed (exwm-cc-wg-parse
                 "interface: wg0\n  public key: abc=\n  private key: SECRET\n  preshared key: ALSO\n")))
    (should (equal (plist-get parsed :interfaces) '("wg0")))
    (should (string-match-p "public key: abc=" (plist-get parsed :text)))
    (should-not (string-match-p "SECRET" (plist-get parsed :text)))
    (should-not (string-match-p "ALSO" (plist-get parsed :text)))
    (should (string-match-p "(hidden)" (plist-get parsed :text)))
    (should (equal exwm-cc-wg-args '("show")))))

(ert-deftest exwm-cc-wg-empty ()
  (let ((parsed (exwm-cc-wg-parse "")))
    (should-not (plist-get parsed :interfaces))))

(ert-deftest exwm-cc-polkit-parse-helper-lines ()
  (should (equal (exwm-cc-polkit-parse-line "PAM_PROMPT_ECHO_OFF Password: ")
                 '(echo-off . "Password: ")))
  (should (equal (exwm-cc-polkit-parse-line "PAM_PROMPT_ECHO_ON login: ")
                 '(echo-on . "login: ")))
  (should (equal (exwm-cc-polkit-parse-line "PAM_ERROR_MSG Failed")
                 '(error . "Failed")))
  (should (equal (exwm-cc-polkit-parse-line "PAM_TEXT_INFO hello\\nworld")
                 '(info . "hello\nworld")))
  (should (equal (exwm-cc-polkit-parse-line "PAM_PROMPT_ECHO_ON octal \\101")
                 '(echo-on . "octal A")))
  (should (equal (exwm-cc-polkit-parse-line "SUCCESS") '(success . "")))
  (should (equal (exwm-cc-polkit-parse-line "FAILURE\n") '(failure . "")))
  (should-not (exwm-cc-polkit-parse-line "NOT A LINE")))

(ert-deftest exwm-cc-polkit-finds-unix-uid ()
  (should (eql (exwm-cc-polkit-unix-uid '(("unix-user" (("uid" 1000))))) 1000))
  (should (eql (exwm-cc-polkit-unix-uid '(("unix-user" ("uid" (1000))))) 1000))
  (should-not (exwm-cc-polkit-unix-uid '(("unix-group" (("gid" 0)))))))

(ert-deftest exwm-cc-polkit-stays-off ()
  (should-not exwm-cc-polkit-mode))

(ert-deftest exwm-cc-polkit-export-roundtrip ()
  "Exporting methods must not turn the agent mode on."
  (skip-unless (exwm-cc-dbus-present-p :system "org.freedesktop.PolicyKit1"))
  (should-not exwm-cc-polkit-mode)
  (unwind-protect
      (progn
        (exwm-cc-polkit--export)
        (should exwm-cc-polkit--registrations)
        (should-not exwm-cc-polkit-mode))
    (exwm-cc-polkit--unexport))
  (should-not exwm-cc-polkit--registrations)
  (should-not exwm-cc-polkit-mode))

(ert-deftest exwm-cc-polkit-register-rejects-unknown-session ()
  "A missing session id must be rejected, and the live agent stays put."
  (skip-unless (exwm-cc-dbus-present-p :system "org.freedesktop.PolicyKit1"))
  (should (not (equal (getenv "XDG_SESSION_ID") "does-not-exist")))
  (should-not exwm-cc-polkit-mode)
  (condition-case err
      (progn
        (exwm-cc-polkit-register-session "does-not-exist")
        (condition-case nil
            (exwm-cc-polkit-unregister-session "does-not-exist")
          (error nil))
        (ert-fail "RegisterAuthenticationAgent accepted session does-not-exist"))
    (error
     (let ((msg (error-message-string err)))
       (message "polkit probe: %s" msg)
       (should-not (string-match-p
                    "Wrong type argument\\|InvalidArgs\\|UnknownMethod\\|UnknownInterface\\|InvalidSignature"
                    msg))
       (should (string-match-p "[Ss]ession" msg)))))
  (should-not exwm-cc-polkit-mode)
  (should-not exwm-cc-polkit--registrations))

(ert-deftest exwm-cc-live-network ()
  (skip-unless (exwm-cc-dbus-present-p :system "org.freedesktop.NetworkManager"))
  (let ((rows (exwm-cc-nm-devices))
        (summary (exwm-cc-nm-summary)))
    (should (exwm-cc-tests-find rows :iface "wlan0"))
    (should (stringp summary))
    (message "exwm-cc network: %s" summary)))

(ert-deftest exwm-cc-live-audio ()
  (skip-unless (executable-find "wpctl"))
  (let ((summary (exwm-cc-audio-summary)))
    (should (stringp summary))
    (should-not (string-empty-p summary))
    (message "exwm-cc audio: %s" summary)))

(ert-deftest exwm-cc-live-power ()
  (skip-unless (exwm-cc-dbus-present-p :system "org.freedesktop.UPower"))
  (let ((bats (cl-remove-if-not
               (lambda (row) (equal (plist-get row :kind) "battery"))
               (exwm-cc-power-devices))))
    (should bats)
    (should (numberp (plist-get (car bats) :percent))))
  (skip-unless (exwm-cc-power--profiles-target))
  (should (stringp (exwm-cc-power-active-profile)))
  (should (member "performance" (exwm-cc-power-profile-names)))
  (message "exwm-cc power: %s" (exwm-cc-power-summary)))

(ert-deftest exwm-cc-live-bluetooth ()
  (skip-unless (exwm-cc-dbus-present-p :system "org.bluez"))
  (should (cl-some (lambda (obj) (string-match-p "hci0" (car obj)))
                   (exwm-cc-dbus-managed :system "org.bluez" "/")))
  (message "exwm-cc bluetooth: %s" (exwm-cc-bt-summary)))

(ert-deftest exwm-cc-live-disks ()
  (skip-unless (exwm-cc-dbus-present-p :system "org.freedesktop.UDisks2"))
  (let ((objs (exwm-cc-dbus-managed :system "org.freedesktop.UDisks2"
                                    "/org/freedesktop/UDisks2")))
    (should (cl-some (lambda (obj)
                       (member "org.freedesktop.UDisks2.Block" (cdr obj)))
                     objs)))
  (message "exwm-cc disks: %s" (exwm-cc-disk-summary)))

(ert-deftest exwm-cc-live-displays ()
  (skip-unless (and (executable-find "xrandr") (getenv "DISPLAY")))
  (let ((rows (exwm-cc-display-query)))
    (should (exwm-cc-tests-find rows :name "eDP-1")))
  (message "exwm-cc displays: %s" (exwm-cc-display-summary)))

(ert-deftest exwm-cc-live-wireguard ()
  (skip-unless (executable-find "wg"))
  (let ((summary (exwm-cc-wg-summary)))
    (should (stringp summary))
    (message "exwm-cc wireguard: %s" summary)))

(ert-deftest exwm-cc-dashboard-renders ()
  (exwm-cc)
  (with-current-buffer "*EXWM Control Center*"
    (should (eq major-mode 'exwm-cc-mode))
    (dolist (title '("Network" "Audio" "Power" "Bluetooth" "Disks"
                     "Displays" "WireGuard" "Polkit"))
      (goto-char (point-min))
      (should (search-forward title nil t)))
    (should (search-forward "agent off" nil t)))
  (should-not exwm-cc-polkit-mode))

(provide 'exwm-cc-tests)

;;; exwm-cc-tests.el ends here
