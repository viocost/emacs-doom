(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(package-selected-packages
   '(company-tabnine projectile-ripgrep ripgrep undo-tree company-jedi jedi))
 '(safe-local-variable-values
   '((magit-todos-exclude-globs "*.map" "node_modules" "public"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'projectile-ripgrep 'disabled nil)

(defun test-dnd (event)
  (messge "Test dnd!")
  )

(define-key org-mode-map (kbd "<drag-n-drop>") 'test-dnd)
