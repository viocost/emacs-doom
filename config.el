;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Konstantin Y. Rybakov"
      user-mail-address "viocost@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
;;(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; OLD CONFIG

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

(setq doom-theme 'doom-acario-dark)
