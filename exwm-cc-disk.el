;;; exwm-cc-disk.el --- UDisks2 for the EXWM control center -*- lexical-binding: t -*-

;; Copyright (C) 2026 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Keywords: unix, hardware

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Filesystems, swap, and locked volumes come from UDisks2.  Mount,
;; unmount, unlock, and eject are D-Bus method calls.  Unmount and
;; eject ask first.  The size shown is Block.Size, because
;; Filesystem.Size is zero for some FAT volumes.
;;
;; Keys:
;;   m        mount
;;   u        unmount
;;   l        unlock
;;   e        eject
;;   g        refresh
;;   q        quit

;;; Code:

(require 'exwm-cc-lib)

(defconst exwm-cc-disk-service "org.freedesktop.UDisks2")
(defconst exwm-cc-disk-path "/org/freedesktop/UDisks2")
(defconst exwm-cc-disk-buffer "*EXWM Disks*")

(defun exwm-cc-disk--string (value)
  "Return VALUE when it is a string, or an empty string."
  (if (stringp value) value ""))

(defun exwm-cc-disk--mounts (value)
  "Return mount points in VALUE as a list of strings."
  (exwm-cc-as-list (exwm-cc-dbus-unwrap value)))

(defun exwm-cc-disk-volumes ()
  "Return a plist for every filesystem, swap space, and locked volume."
  (unless (exwm-cc-dbus-present-p :system exwm-cc-disk-service)
    (error "UDisks2 is not running"))
  (delq nil
        (mapcar
         (lambda (obj)
           (let* ((path (car obj))
                  (ifaces (cdr obj))
                  (fs (member "org.freedesktop.UDisks2.Filesystem" ifaces))
                  (swap (member "org.freedesktop.UDisks2.Swapspace" ifaces))
                  (enc (member "org.freedesktop.UDisks2.Encrypted" ifaces)))
             (when (or fs swap enc)
               (let* ((block (exwm-cc-dbus-alist
                              :system exwm-cc-disk-service path
                              "org.freedesktop.UDisks2.Block"))
                      (fs-props (and fs
                                     (exwm-cc-dbus-alist
                                      :system exwm-cc-disk-service path
                                      "org.freedesktop.UDisks2.Filesystem")))
                      (mounts (exwm-cc-disk--mounts
                               (cdr (assoc "MountPoints" fs-props))))
                      (device (exwm-cc-disk--string (cdr (assoc "Device" block))))
                      (drive (cdr (assoc "Drive" block))))
                 (list :path path
                       :device (if (string-empty-p device) path device)
                       :label (exwm-cc-disk--string (cdr (assoc "IdLabel" block)))
                       :fstype (cond
                                (swap "swap")
                                ((and enc (not fs)) "locked")
                                (t (exwm-cc-disk--string (cdr (assoc "IdType" block)))))
                       :size (exwm-cc-number (cdr (assoc "Size" block)))
                       :mounts mounts
                       :filesystem (and fs t)
                       :encrypted (and enc t)
                       :drive (and (stringp drive) (not (equal drive "/")) drive))))))
         (exwm-cc-dbus-managed :system exwm-cc-disk-service exwm-cc-disk-path))))

(defun exwm-cc-disk-summary ()
  "Return one status line for the disk section."
  (if (not (exwm-cc-dbus-present-p :system exwm-cc-disk-service))
      "UDisks2 is not running"
    (condition-case err
        (let* ((rows (exwm-cc-disk-volumes))
               (mounted (length (cl-remove-if-not
                                 (lambda (row) (plist-get row :mounts))
                                 rows))))
          (format "%d volumes, %d mounted" (length rows) mounted))
      (error (error-message-string err)))))

(defun exwm-cc-disk--fill ()
  "Fill the disk list."
  (setq tabulated-list-format [("Device" 22 t)
                               ("Type" 10 t)
                               ("Size" 8 t)
                               ("Mount" 32 t)
                               ("Label" 16 t)])
  (tabulated-list-init-header)
  (exwm-cc-fill-list
   (exwm-cc-disk-volumes)
   (list (lambda (row) (plist-get row :device))
         (lambda (row) (plist-get row :fstype))
         (lambda (row) (exwm-cc-format-size (plist-get row :size)))
         (lambda (row) (mapconcat #'identity (plist-get row :mounts) ", "))
         (lambda (row) (plist-get row :label)))))

(defvar exwm-cc-disk-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exwm-cc-list-mode-map)
    (define-key map (kbd "m") #'exwm-cc-disk-mount)
    (define-key map (kbd "u") #'exwm-cc-disk-unmount)
    (define-key map (kbd "l") #'exwm-cc-disk-unlock)
    (define-key map (kbd "e") #'exwm-cc-disk-eject)
    map)
  "Keymap for the disk list.")

(define-derived-mode exwm-cc-disk-mode exwm-cc-list-mode "CC-Disk"
  "UDisks2 filesystems, swap, and locked volumes.

\\{exwm-cc-disk-mode-map}")

(defun exwm-cc-disk ()
  "Show UDisks2 volumes."
  (interactive)
  (unless (exwm-cc-dbus-present-p :system exwm-cc-disk-service)
    (user-error "UDisks2 is not running"))
  (exwm-cc-pop-list exwm-cc-disk-buffer #'exwm-cc-disk-mode #'exwm-cc-disk--fill))

(defun exwm-cc-disk--row ()
  "Return the volume plist at point."
  (or (exwm-cc-list-row)
      (user-error "No volume on this line")))

(defun exwm-cc-disk--call (path interface method &rest args)
  "Call UDisks METHOD on PATH and refresh."
  (condition-case err
      (let ((result (apply #'dbus-call-method :system exwm-cc-disk-service path
                           interface method args)))
        (exwm-cc-list-refresh)
        result)
    (error (user-error "%s" (error-message-string err)))))

(defun exwm-cc-disk-mount ()
  "Mount the filesystem at point."
  (interactive)
  (let ((row (exwm-cc-disk--row)))
    (unless (plist-get row :filesystem)
      (user-error "%s has no filesystem" (plist-get row :device)))
    (let ((where (exwm-cc-disk--call (plist-get row :path)
                                     "org.freedesktop.UDisks2.Filesystem"
                                     "Mount" exwm-cc-empty-dict)))
      (message "Mounted %s%s"
               (plist-get row :device)
               (if (stringp where) (format " at %s" where) "")))))

(defun exwm-cc-disk-unmount ()
  "Unmount the filesystem at point."
  (interactive)
  (let* ((row (exwm-cc-disk--row))
         (mounts (mapconcat #'identity (plist-get row :mounts) ", ")))
    (unless (plist-get row :filesystem)
      (user-error "%s has no filesystem" (plist-get row :device)))
    (when (yes-or-no-p
           (format "Unmount %s%s? "
                   (plist-get row :device)
                   (if (string-empty-p mounts) "" (format " (%s)" mounts))))
      (exwm-cc-disk--call (plist-get row :path)
                          "org.freedesktop.UDisks2.Filesystem"
                          "Unmount" exwm-cc-empty-dict)
      (message "Unmounted %s" (plist-get row :device)))))

(defun exwm-cc-disk-unlock ()
  "Unlock the encrypted volume at point.
The passphrase is read with `read-passwd' and passed only as the
Unlock argument."
  (interactive)
  (let ((row (exwm-cc-disk--row)))
    (unless (plist-get row :encrypted)
      (user-error "%s is not encrypted" (plist-get row :device)))
    (let ((pass (read-passwd (format "Passphrase for %s: " (plist-get row :device)))))
      (unwind-protect
          (progn
            (exwm-cc-disk--call (plist-get row :path)
                                "org.freedesktop.UDisks2.Encrypted"
                                "Unlock" pass exwm-cc-empty-dict)
            (message "Unlocked %s" (plist-get row :device)))
        (setq pass nil)))))

(defun exwm-cc-disk-eject ()
  "Eject the drive that holds the volume at point."
  (interactive)
  (let* ((row (exwm-cc-disk--row))
         (drive (plist-get row :drive)))
    (unless (stringp drive)
      (user-error "%s has no ejectable drive" (plist-get row :device)))
    (when (yes-or-no-p (format "Eject the drive for %s? " (plist-get row :device)))
      (exwm-cc-disk--call drive "org.freedesktop.UDisks2.Drive"
                          "Eject" exwm-cc-empty-dict)
      (message "Eject requested"))))

(exwm-cc-add-section 'disks "Disks" #'exwm-cc-disk-summary #'exwm-cc-disk)

(provide 'exwm-cc-disk)

;;; exwm-cc-disk.el ends here
