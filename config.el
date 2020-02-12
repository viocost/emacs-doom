;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "Source Code Pro" :size 16))


(setq display-line-numbers-type 'relative)


;; Full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package! deft
  :init
  (setq deft-directory "~/notes")
  (setq deft-extensions '("txt" "md" "org" "tex"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-text-mode 'org-mode)
  (setq deft-extensions '("org"))
  (setq deft-new-file-format "%Y-%m-%dT%H%M")
  (setq deft-directory "~/notes")
  (setq deft-org-mode-title-prefix t))


(after! '(treemacs dired)
  (treemacs-icons-dired-mode))

(use-package! winum
  :config
  (winum-mode))

(map! (:map override
        :i  "C-f" #'right-char
        :i  "C-b" #'left-char
        :nv "C-j" #'evil-mc-make-cursor-move-next-line
        :nv "C-k" #'evil-mc-make-cursor-move-prev-line
        :nv "C-S-j" #'evil-mc-make-and-goto-next-match
        :nv "C-S-k" #'evil-mc-make-and-goto-prev-match
        [f8] #'treemacs)

      (:prefix "SPC"
        :n "1" #'winum-select-window-1
        :n "2" #'winum-select-window-2
        :n "3" #'winum-select-window-3
        :n "4" #'winum-select-window-4
        :n "5" #'winum-select-window-5
        :n "6" #'winum-select-window-6

        ;; describe
        :n "df" #'describe-function
        :n "dp" #'describe-package
        :n "dv" #'describe-variable
        :n "dk" #'describe-key
        :n "dm" #'describe-mode
       
        ;;projectile
        :n "ps" #'projectile-save-project-buffers
        ))

(map!   :map python-mode-map
        :mode python-mode
        :prefix "SPC"
        :n "be" #'python-shell-send-buffer)

(map!   :map emacs-lisp-mode-map
        :mode emacs-lisp-mode
        :prefix "SPC"
        :n "be" #'eval-buffer)

(setq org-log-done 'time)

;; transparency
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;; setting default transaprency to 85
(transparency 85)
