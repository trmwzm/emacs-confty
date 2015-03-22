;; -----------------------------------------------------------------------------
;; fortran/f90
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist
             '("\\.[fF]\\(03\\|95\\|pp\\)\\'" . f90-mode))
;;(defalias 'fortran-mode 'f90-mode)

(add-hook 'f90-mode-hook
          (lambda ()
            ;; These are not default.
            (abbrev-mode 1)             ; turn on abbreviation mode
            (f90-add-imenu-menu)        ; extra menu with functions etc.
            (if f90-auto-keyword-case   ; change case of all keywords on startup
                (f90-change-keywords f90-auto-keyword-case))))

;;; fortran.el ends here
(provide 'fortran)
