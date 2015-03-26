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

;; load module python/anaconda3
;; fix fortran-tags.py shebang
;; fortran-find-tag issuance generate
;; find -name '*.f90' | xargs fortran-tags.py -g
;; (setq fortran-tags-path "~/my-project/FORTAGS")

(add-hook 'f90-mode-hook
          (lambda ()
  	         (local-set-key (kbd "M-.") 'fortran-find-tag)
	         (local-set-key (kbd "M-*") 'fortran-pop-tag-mark)
	         (local-set-key (kbd "M-n") 'fortran-goto-next)
             (local-set-key (kbd "M-s g") 'fortran-find-proc-calls)
	    	 (local-set-key (kbd "M-s s") 'fortran-find-proc-calls-sub)
	    	 (local-set-key (kbd "M-s f") 'fortran-find-proc-calls-func)
	    	 (local-set-key (kbd "M-s t") 'fortran-find-proc-calls-type)))

;;; fortran.el ends here
(provide 'fortran)
