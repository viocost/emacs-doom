;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "Source Code Pro" :size 16))


(setq display-line-numbers-type 'relative)


;; Full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(def-package-hook! deft
  :pre-config
  (setq deft-extensions '("txt" "md" "org" "tex"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-text-mode 'org-mode)
  (setq deft-extensions '("org"))
  (setq deft-new-file-format "%Y-%m-%dT%H%M")
  (setq deft-org-mode-title-prefix t)
  (setq deft-directory "~/notes"))

(after! '(treemacs dired)
  (treemacs-icons-dired-mode))

(after! winum-mode
  (winum-mode))

(map! (:map override
        :i "C-f" #'evil-forward-char
        :i "C-b" #'evil-backward-char
        [f8] #'treemacs)

      (:prefix "SPC"
        :n "be" #'eval-buffer
        :n "1" #'winum-select-window-1
        :n "2" #'winum-select-window-2
        :n "3" #'winum-select-window-3
        :n "4" #'winum-select-window-4
        :n "5" #'winum-select-window-5
        :n "6" #'winum-select-window-6))

