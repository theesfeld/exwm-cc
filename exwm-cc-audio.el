;;; exwm-cc-audio.el --- PipeWire mixer for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware, multimedia

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Sinks, sources, and streams come from `wpctl status'.  The volume
;; keys call `wpctl'.  pw-dump is intentionally not used: the status
;; text is enough and is easy to test.
;;
;; Keys:
;;   d        set the default sink or source
;;   m        toggle mute
;;   + / -    raise or lower the volume
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-audio-buffer "*EXWM Audio*")

(defcustom exwm-cc-audio-step 0.05
  "Volume change applied by the + and - keys, as a PipeWire fraction."
  :type 'number
  :group 'exwm-cc)

(defun exwm-cc-audio--node (line)
  "Return a node plist when LINE is a wpctl node, or nil.
The plist has :default, :id, and :name.  Volume and mute are added
by the caller from the name text."
  (when (string-match
         (rx bos
             (zero-or-more (not (any "*\n0-9")))
             (optional (group "*") (zero-or-more (any " \t")))
             (group (one-or-more digit))
             "."
             (one-or-more (any " \t"))
             (group (one-or-more not-newline))
             eos)
         line)
    (list :default (and (match-beginning 1) t)
          :id (match-string 2 line)
          :rest (match-string 3 line))))

(defun exwm-cc-audio--clean-name (rest)
  "Return (NAME VOLUME MUTED) parsed from a wpctl node REST."
  (let ((volume nil)
        (muted nil)
        (name rest))
    (when (string-match "\\[vol: \\([0-9.]+\\)\\]\\( MUTED\\)?" name)
      (setq volume (match-string 1 name)
            muted (match-beginning 2)))
    (setq name (replace-regexp-in-string "\\[vol: [^]]*\\]\\( MUTED\\)?" "" name))
    (setq name (replace-regexp-in-string "\\[[^]]*\\]" "" name))
    (setq name (string-trim (replace-regexp-in-string "[[:space:]]+" " " name)))
    (list name volume (and muted t))))

(defun exwm-cc-audio-parse (text)
  "Parse wpctl status TEXT into node plists.
Only Audio sinks, sources, and streams are returned.  Video nodes
are ignored.  Each plist has :section, :id, :name, :volume,
:muted, and :default."
  (let ((area nil)
        (section nil)
        (rows nil))
    (dolist (line (split-string text "\n"))
      (cond
       ((string-match-p "\\`Video\\'\\|\\`Settings\\'" line)
        (setq area nil section nil))
       ((string-match-p "\\`Audio\\'" line)
        (setq area t section nil))
       ((not area) nil)
       ((string-match-p "Sinks:" line) (setq section "sink"))
       ((string-match-p "Sources:" line) (setq section "source"))
       ((string-match-p "Streams:" line) (setq section "stream"))
       ((string-match-p "Devices:\\|Filters:" line) (setq section nil))
       (section
        (let ((node (exwm-cc-audio--node line)))
          (when node
            (let ((cleaned (exwm-cc-audio--clean-name (plist-get node :rest))))
              (push (list :section section
                          :id (plist-get node :id)
                          :name (nth 0 cleaned)
                          :volume (nth 1 cleaned)
                          :muted (nth 2 cleaned)
                          :default (and (plist-get node :default) t))
                    rows)))))))
    (nreverse rows)))

(defun exwm-cc-audio--status ()
  "Return wpctl status text."
  (exwm-cc-require-program "wpctl")
  (let ((res (exwm-cc-command-output "wpctl" "status")))
    (unless (eql (car res) 0)
      (error "%s" (string-trim (cdr res))))
    (cdr res)))

(defun exwm-cc-audio-nodes ()
  "Return the current audio nodes."
  (exwm-cc-audio-parse (exwm-cc-audio--status)))

(defun exwm-cc-audio--percent (volume)
  "Format a wpctl VOLUME fraction as a percentage."
  (if (and (stringp volume) (not (string-empty-p volume)))
      (format "%d%%" (round (* 100 (string-to-number volume))))
    ""))

(defun exwm-cc-audio-summary ()
  "Return one status line for the audio section."
  (if (not (executable-find "wpctl"))
      "wpctl is not installed"
    (condition-case err
        (let* ((rows (exwm-cc-audio-nodes))
               (sinks (cl-remove-if-not
                       (lambda (row) (equal (plist-get row :section) "sink"))
                       rows))
               (pick (or (cl-find-if (lambda (row) (plist-get row :default)) sinks)
                         (car sinks))))
          (if (not pick)
              "no sinks"
            (string-trim
             (format "%s %s"
                     (plist-get pick :name)
                     (exwm-cc-audio--percent (plist-get pick :volume))))))
      (error (error-message-string err)))))

(defun exwm-cc-audio--flags (row)
  "Return the flag text for audio ROW."
  (mapconcat #'identity
             (delq nil
                   (list (and (plist-get row :default) "default")
                         (and (plist-get row :muted) "muted")))
             ", "))

(defun exwm-cc-audio--fill ()
  "Fill the audio list from wpctl."
  (setq tabulated-list-format [("Kind" 8 t)
                               ("Id" 6 t)
                               ("Name" 52 t)
                               ("Volume" 8 t)
                               ("Flags" 14 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-audio-nodes)
   (list (lambda (row) (plist-get row :section))
         (lambda (row) (plist-get row :id))
         (lambda (row) (plist-get row :name))
         (lambda (row) (exwm-cc-audio--percent (plist-get row :volume)))
         #'exwm-cc-audio--flags)))

(defvar exwm-cc-audio-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "d") #'exwm-cc-audio-default)
    (define-key map (kbd "m") #'exwm-cc-audio-mute)
    (define-key map (kbd "+") #'exwm-cc-audio-up)
    (define-key map (kbd "-") #'exwm-cc-audio-down)
    map)
  "Keymap for the audio list.")

(define-derived-mode exwm-cc-audio-mode exwm-cc-list-mode "CC-Audio"
  "PipeWire sinks, sources, and streams.

\\{exwm-cc-audio-mode-map}")

(defun exwm-cc-audio ()
  "Show PipeWire sinks, sources, and streams."
  (interactive)
  (exwm-cc-require-program "wpctl")
  (exwm-cc-pop-list exwm-cc-audio-buffer #'exwm-cc-audio-mode #'exwm-cc-audio--fill))

(defun exwm-cc-audio--row ()
  "Return the audio plist at point."
  (or (exwm-cc-list-row)
      (user-error "No audio node on this line")))

(defun exwm-cc-audio--wpctl (args)
  "Run wpctl with ARGS and refresh the list."
  (exwm-cc-require-program "wpctl")
  (let ((res (apply #'exwm-cc-command-output "wpctl" args)))
    (unless (eql (car res) 0)
      (user-error "%s" (string-trim (cdr res))))
    (exwm-cc-list-refresh)))

(defun exwm-cc-audio-default ()
  "Make the node at point the default."
  (interactive)
  (exwm-cc-audio--wpctl (list "set-default" (plist-get (exwm-cc-audio--row) :id))))

(defun exwm-cc-audio-mute ()
  "Toggle mute on the node at point."
  (interactive)
  (exwm-cc-audio--wpctl
   (list "set-mute" (plist-get (exwm-cc-audio--row) :id) "toggle")))

(defun exwm-cc-audio-up ()
  "Raise the volume of the node at point."
  (interactive)
  (exwm-cc-audio--wpctl
   (list "set-volume" (plist-get (exwm-cc-audio--row) :id)
         (format "%s+" exwm-cc-audio-step))))

(defun exwm-cc-audio-down ()
  "Lower the volume of the node at point."
  (interactive)
  (exwm-cc-audio--wpctl
   (list "set-volume" (plist-get (exwm-cc-audio--row) :id)
         (format "%s-" exwm-cc-audio-step))))

(exwm-cc-add-section 'audio "Audio" #'exwm-cc-audio-summary #'exwm-cc-audio)

(provide 'exwm-cc-audio)

;;; exwm-cc-audio.el ends here
