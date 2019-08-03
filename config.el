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


(after! winum-mode
  (winum-mode))
