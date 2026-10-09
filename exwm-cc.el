;;; exwm-cc.el --- Control center for an EXWM session -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/theesfeld/exwm-cc

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; One buffer for the desktop services an EXWM user otherwise leaves
;; Emacs to reach: NetworkManager, PipeWire, UPower, BlueZ, UDisks2,
;; RandR outputs, and WireGuard.  EXWM itself is not required.  The
;; D-Bus panels work in any Emacs built with D-Bus.
;;
;; PolicyKit is a minor mode, off by default.  See `exwm-cc-polkit-mode'.
;;
;;   M-x exwm-cc
;;
;; Keys in the dashboard:
;;   RET      open the section at point
;;   g        refresh
;;   L        lock this session
;;   A        toggle the PolicyKit agent
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)
(require 'exwm-cc-nm)
(require 'exwm-cc-audio)
(require 'exwm-cc-power)
(require 'exwm-cc-bt)
(require 'exwm-cc-disk)
(require 'exwm-cc-display)
(require 'exwm-cc-wg)
(require 'exwm-cc-polkit)

(defconst exwm-cc-buffer-name "*EXWM Control Center*")

(defun exwm-cc-refresh ()
  "Redraw the control center dashboard."
  (interactive)
  (with-current-buffer (get-buffer-create exwm-cc-buffer-name)
    (unless (eq major-mode 'exwm-cc-mode)
      (exwm-cc-mode))
    (let ((inhibit-read-only t)
          (id (get-text-property (point) 'exwm-cc-id))
          (at (line-beginning-position)))
      (erase-buffer)
      (insert "RET open    g refresh    L lock    A polkit agent    q quit\n\n")
      (dolist (section exwm-cc-sections)
        (let ((start (point)))
          (insert (format "%-14s %s\n"
                          (nth 1 section)
                          (exwm-cc-safely (nth 2 section))))
          (put-text-property start (1- (point)) 'exwm-cc-id (nth 0 section))))
      (let ((start (point)))
        (insert (format "%-14s %s\n"
                        "Polkit"
                        (if exwm-cc-polkit-mode
                            "agent on"
                          "agent off")))
        (put-text-property start (1- (point)) 'exwm-cc-id 'polkit))
      (goto-char (point-min))
      (when id
        (while (and (not (eq (get-text-property (point) 'exwm-cc-id) id))
                    (not (eobp)))
          (forward-line 1))
        (unless (eq (get-text-property (point) 'exwm-cc-id) id)
          (goto-char (min at (point-max))))))))

(defun exwm-cc-open ()
  "Open the dashboard section at point."
  (interactive)
  (let ((id (get-text-property (point) 'exwm-cc-id)))
    (cond
     ((eq id 'polkit) (exwm-cc-agent))
     (id (let ((section (assoc id exwm-cc-sections)))
           (unless section
             (user-error "Unknown section"))
           (funcall (nth 3 section))))
     (t (user-error "No section on this line")))))

(defun exwm-cc-agent ()
  "Toggle the PolicyKit agent and refresh the dashboard."
  (interactive)
  (exwm-cc-polkit-toggle)
  (when (get-buffer exwm-cc-buffer-name)
    (with-current-buffer exwm-cc-buffer-name
      (when (eq major-mode 'exwm-cc-mode)
        (exwm-cc-refresh)))))

(defvar exwm-cc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exwm-cc-open)
    (define-key map (kbd "g") #'exwm-cc-refresh)
    (define-key map (kbd "L") #'exwm-cc-lock)
    (define-key map (kbd "A") #'exwm-cc-agent)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the EXWM control center dashboard.")

(define-derived-mode exwm-cc-mode special-mode "EXWM-CC"
  "Dashboard for the EXWM control center.

\\{exwm-cc-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (exwm-cc-refresh))))

;;;###autoload
(defun exwm-cc ()
  "Open the EXWM control center."
  (interactive)
  (let ((buf (get-buffer-create exwm-cc-buffer-name)))
    (with-current-buffer buf
      (exwm-cc-refresh))
    (pop-to-buffer buf)))

(provide 'exwm-cc)

;;; exwm-cc.el ends here
