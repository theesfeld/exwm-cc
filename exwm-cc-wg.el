;;; exwm-cc-wg.el --- WireGuard status for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, comm

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Status comes from `wg show'.  `wg show dump' is never used, because
;; that format prints the private key.  Private and preshared key lines
;; are removed even if a future wg version prints them in the human
;; output.

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-wg-buffer "*EXWM WireGuard*")

(defconst exwm-cc-wg-args '("show")
  "Arguments passed to wg.
This stays `show'.  `show dump' prints the interface private key.")

(defun exwm-cc-wg-redact (text)
  "Remove private and preshared key lines from wg TEXT."
  (mapconcat
   (lambda (line)
     (if (string-match
          "\\`[[:space:]]*\\(private key\\|preshared key\\):\\(.*\\)\\'"
          line)
         (format "%s: (hidden)" (match-string 1 line))
       line))
   (split-string text "\n")
   "\n"))

(defun exwm-cc-wg-parse (text)
  "Return (:text REDACTED :interfaces NAMES) for wg TEXT."
  (let ((redacted (exwm-cc-wg-redact text))
        (names nil))
    (dolist (line (split-string redacted "\n" t))
      (when (string-match "\\`interface:[[:space:]]*\\(.+\\)\\'" line)
        (push (string-trim (match-string 1 line)) names)))
    (list :text redacted :interfaces (nreverse names))))

(defun exwm-cc-wg--read ()
  "Return the parsed result of `wg show'."
  (exwm-cc-require-program "wg")
  (let ((res (apply #'exwm-cc-command-output "wg" exwm-cc-wg-args)))
    (unless (eql (car res) 0)
      (error "%s" (string-trim (cdr res))))
    (exwm-cc-wg-parse (cdr res))))

(defun exwm-cc-wg-summary ()
  "Return one status line for the WireGuard section."
  (if (not (executable-find "wg"))
      "wg is not installed"
    (condition-case err
        (let ((names (plist-get (exwm-cc-wg--read) :interfaces)))
          (if names
              (mapconcat #'identity names ", ")
            "no interfaces"))
      (error (error-message-string err)))))

(defvar exwm-cc-wg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'exwm-cc-wg)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the WireGuard status buffer.")

(define-derived-mode exwm-cc-wg-mode special-mode "CC-WG"
  "WireGuard status from `wg show'.

\\{exwm-cc-wg-mode-map}"
  (setq truncate-lines t))

(defun exwm-cc-wg ()
  "Show `wg show' with private keys removed."
  (interactive)
  (exwm-cc-require-program "wg")
  (let* ((parsed (exwm-cc-wg--read))
         (text (plist-get parsed :text))
         (buf (get-buffer-create exwm-cc-wg-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'exwm-cc-wg-mode)
        (exwm-cc-wg-mode))
      (let ((inhibit-read-only t)
            (at (point)))
        (erase-buffer)
        (if (string-match-p "\\`[[:space:]]*\\'" text)
            (insert "No WireGuard interfaces.\n")
          (insert text)
          (unless (eq (char-before) ?\n)
            (insert "\n")))
        (goto-char (min at (point-max)))))
    (pop-to-buffer buf)))

(exwm-cc-add-section 'wireguard "WireGuard" #'exwm-cc-wg-summary #'exwm-cc-wg)

(provide 'exwm-cc-wg)

;;; exwm-cc-wg.el ends here
