;;; Code lifted from bbatsov's prelude
(defvar current-user (getenv "USER"))
;; -- info
(message "whoami: %s!" current-user)

;; -- all subdirs
(defun recursive-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
       (add-to-list 'load-path name)
       (recursive-add-subfolders-to-load-path name)))))

;; -- where to root
(defvar emacsd-dir (file-name-directory load-file-name)
  "The root dir of the Emacs config.")

;; -- config in root
(defvar config-dir (expand-file-name  "config" emacsd-dir)
  "This directory houses all configuration  modules.")

;; -- add config directory and all its subdirs to Emacs's `load-path'
(add-to-list 'load-path config-dir)
(recursive-add-subfolders-to-load-path config-dir)
(message "load-path: %s!" load-path)

;; -- configurations
(require 'ui)
(require 'editing)
(require 'packages)
(require 'fortran)
(require 'general)
(require 'custom-key-set)
(require 'ido-init)
(require 'yorick)
(require 'hippie-expand)
(require 'auto-complete)
