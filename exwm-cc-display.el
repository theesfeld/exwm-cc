;;; exwm-cc-display.el --- RandR outputs for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Output state is parsed from `xrandr --query'.  Every change asks
;; for confirmation before xrandr runs, because a bad mode can blank
;; the displays.  EXWM itself does not set RandR modes.
;;
;; Keys:
;;   e        enable with --auto
;;   d        disable
;;   p        make primary
;;   r        rotate
;;   o        place relative to another output
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-display-buffer "*EXWM Displays*")

(defun exwm-cc-display--rates (text)
  "Return rate plists parsed from a mode line's TEXT."
  (delq nil
        (mapcar
         (lambda (token)
           (when (string-match "\\`[0-9.]+" token)
             (list :hz (replace-regexp-in-string "[*+]" "" token)
                   :current (and (string-match-p "\\*" token) t)
                   :preferred (and (string-match-p "\\+" token) t))))
         (split-string text))))

(defun exwm-cc-display-parse (text)
  "Parse xrandr --query TEXT into output plists.
Each plist has :name, :connected, :primary, :geometry, :current,
and :modes.  A mode plist has :name, :rates, :current, and
:preferred."
  (let ((rows nil)
        (header nil)
        (modes nil))
    (cl-labels
        ((finish ()
           (when header
             (let ((current (cl-find-if (lambda (mode) (plist-get mode :current))
                                        modes)))
               (push (append header
                             (list :modes (nreverse modes)
                                   :current (and current (plist-get current :name))))
                     rows))
             (setq header nil modes nil))))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-match
           (rx bos
               (group (one-or-more (not (any " "))))
               " "
               (group (or "connected" "disconnected" "unknown"))
               (optional " primary")
               (optional " "
                         (group (one-or-more digit) "x" (one-or-more digit)
                                "+" (one-or-more digit) "+" (one-or-more digit))))
           line)
          (finish)
          (let ((name (match-string 1 line))
                (state (match-string 2 line))
                (geometry (match-string 3 line)))
            (setq header
                  (list :name name
                        :connected (equal state "connected")
                        :primary (and (string-match-p " primary" line) t)
                        :geometry (or geometry "")))))
         ((and header
               (string-match
                (rx bos "   "
                    (group (one-or-more digit) "x" (one-or-more digit)
                           (optional "i"))
                    (one-or-more blank)
                    (group (one-or-more not-newline)))
                line))
          (let* ((name (match-string 1 line))
                 (rates (exwm-cc-display--rates (match-string 2 line))))
            (push (list :name name
                        :rates rates
                        :current (and (cl-some (lambda (rate) (plist-get rate :current))
                                               rates)
                                      t)
                        :preferred (and (cl-some (lambda (rate) (plist-get rate :preferred))
                                                rates)
                                       t))
                  modes)))))
      (finish))
    (nreverse rows)))

(defun exwm-cc-display-query ()
  "Return output plists from the live xrandr query."
  (exwm-cc-require-program "xrandr")
  (let ((res (exwm-cc-command-output "xrandr" "--query")))
    (unless (eql (car res) 0)
      (error "%s" (string-trim (cdr res))))
    (exwm-cc-display-parse (cdr res))))

(defun exwm-cc-display-summary ()
  "Return one status line for the display section."
  (if (not (executable-find "xrandr"))
      "xrandr is not installed"
    (condition-case err
        (let ((parts (delq nil
                           (mapcar
                            (lambda (row)
                              (when (plist-get row :connected)
                                (format "%s%s %s"
                                        (plist-get row :name)
                                        (if (plist-get row :primary) "*" "")
                                        (or (plist-get row :current)
                                            (plist-get row :geometry)))))
                            (exwm-cc-display-query)))))
          (if parts (mapconcat #'identity parts ", ") "no connected outputs"))
      (error (error-message-string err)))))

(defun exwm-cc-display--status (row)
  "Return the status text for output ROW."
  (cond ((and (plist-get row :connected) (plist-get row :primary)) "primary")
        ((plist-get row :connected) "connected")
        (t "disconnected")))

(defun exwm-cc-display--fill ()
  "Fill the display list."
  (setq tabulated-list-format [("Output" 12 t)
                               ("Status" 14 t)
                               ("Mode" 16 t)
                               ("Position" 22 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-display-query)
   (list (lambda (row) (plist-get row :name))
         #'exwm-cc-display--status
         (lambda (row) (or (plist-get row :current) ""))
         (lambda (row) (plist-get row :geometry)))))

(defvar exwm-cc-display-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "e") #'exwm-cc-display-enable)
    (define-key map (kbd "d") #'exwm-cc-display-off)
    (define-key map (kbd "p") #'exwm-cc-display-primary)
    (define-key map (kbd "r") #'exwm-cc-display-rotate)
    (define-key map (kbd "o") #'exwm-cc-display-place)
    map)
  "Keymap for the display list.")

(define-derived-mode exwm-cc-display-mode exwm-cc-list-mode "CC-Display"
  "RandR outputs from xrandr.

\\{exwm-cc-display-mode-map}")

(defun exwm-cc-display ()
  "Show RandR outputs."
  (interactive)
  (exwm-cc-require-program "xrandr")
  (exwm-cc-pop-list exwm-cc-display-buffer #'exwm-cc-display-mode
                    #'exwm-cc-display--fill))

(defun exwm-cc-display--row ()
  "Return the output plist at point."
  (or (exwm-cc-list-row)
      (user-error "No output on this line")))

(defun exwm-cc-display-apply (args)
  "Run xrandr with ARGS after a confirmation.
ARGS is a list of strings.  Declining the prompt does nothing."
  (exwm-cc-require-program "xrandr")
  (when (yes-or-no-p (format "Run xrandr %s? " (string-join args " ")))
    (let ((res (apply #'exwm-cc-command-output "xrandr" args)))
      (unless (eql (car res) 0)
        (user-error "%s" (string-trim (cdr res))))
      (message "xrandr finished")
      (when (eq major-mode 'exwm-cc-display-mode)
        (exwm-cc-list-refresh)))))

(defun exwm-cc-display-enable ()
  "Enable the output at point with xrandr --auto."
  (interactive)
  (exwm-cc-display-apply
   (list "--output" (plist-get (exwm-cc-display--row) :name) "--auto")))

(defun exwm-cc-display-off ()
  "Disable the output at point."
  (interactive)
  (exwm-cc-display-apply
   (list "--output" (plist-get (exwm-cc-display--row) :name) "--off")))

(defun exwm-cc-display-primary ()
  "Make the output at point the primary output."
  (interactive)
  (exwm-cc-display-apply
   (list "--output" (plist-get (exwm-cc-display--row) :name) "--primary")))

(defun exwm-cc-display-rotate ()
  "Rotate the output at point."
  (interactive)
  (let ((name (plist-get (exwm-cc-display--row) :name))
        (rotation (completing-read "Rotation: "
                                   '("normal" "left" "right" "inverted")
                                   nil t)))
    (exwm-cc-display-apply (list "--output" name "--rotate" rotation))))

(defun exwm-cc-display-place ()
  "Place the output at point relative to another output."
  (interactive)
  (let* ((name (plist-get (exwm-cc-display--row) :name))
         (others (mapcar (lambda (row) (plist-get row :name))
                         (exwm-cc-display-query)))
         (other (completing-read "Relative to: " others nil t))
         (where (completing-read "Placement: "
                                 '("left-of" "right-of" "above" "below" "same-as")
                                 nil t)))
    (exwm-cc-display-apply
     (list "--output" name (concat "--" where) other))))

(exwm-cc-add-section 'displays "Displays" #'exwm-cc-display-summary #'exwm-cc-display)

(provide 'exwm-cc-display)

;;; exwm-cc-display.el ends here
