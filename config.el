;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Konstantin Y. Rybakov"
      user-mail-address "viocost@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Source Code Pro" :size 16))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ORG MODE
;;code execution in babel
;; All options: https://orgmode.org/manual/Results-of-Evaluation.html#Results-of-Evaluation
(setq org-babel-default-header-args
       '((:session . "none")
        (:results . "replace output") ;; this is how to be in scripting mode when evaluating the code
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")))


(setq projectile-project-search-path '("~/projects" "~/cs"))
(projectile-discover-projects-in-search-path)

(after! '(treemacs dired)
  (treemacs-icons-dired-mode))

;; KEYS
(map! (:map override
        :i  "C-f" #'right-char
        :i  "C-b" #'left-char
        :nv "C-j" #'evil-mc-make-cursor-move-next-line
        :nv "C-k" #'evil-mc-make-cursor-move-prev-line
        :nv "C-S-j" #'evil-mc-make-and-goto-next-match
        :nv "C-S-k" #'evil-mc-make-and-goto-prev-match
        [S-right] #'evil-window-increase-width
        [S-left] #'evil-window-decrease-width
        [S-up] #'evil-window-increase-height
        [S-down] #'evil-window-decrease-height
        [f8] #'treemacs
        "C-;" #'iedit-mode)

      (:prefix "SPC"
        :n "1" #'winum-select-window-1
        :n "2" #'winum-select-window-2
        :n "3" #'winum-select-window-3
        :n "4" #'winum-select-window-4
        :n "5" #'winum-select-window-5
        :n "6" #'winum-select-window-6
        :n "U" #'smerge-keep-upper
        :n "N" #'smerge-next
        :n "P" #'smerge-prev
        :n "B" #'smerge-keep-lower

        ;; refactoring
        :n "rf" #'iedit-restrict-function
        :n "rgs" #'+default/search-project
        :n "rgp" #'+default/search-project-for-symbol-at-point

        ;; describe
        :n "dd" #'deft
        :n "dn" #'org-roam-find-file
        :n "dg" #'open-org-roam-graph
        :n "di" #'org-roam-insert
        ;;:n "dff" #'zetteldeft-find-file               ;;:n "dn" #'zetteldeft-new-file
        ;;:n "dft" #'zetteldeft-search-tag              ;;:n "dff" #'zetteldeft-find-file
        ;;:n "dlf" #'zetteldeft-follow-link             ;;:n "dft" #'zetteldeft-search-tag
        ;;:n "dli" #'zetteldeft-insert-list-links       ;;:n "dlf" #'zetteldeft-follow-link
        ;;:n "dli" #'zetteldeft-insert-list-links

        ;;projectile
        :n "ps" #'projectile-save-project-buffers
        ))

(map! :map org-mode-map
      :mode org-mode
      :prefix "SPC"
      :n "lp" #'org-latex-preview-in-buffer
      :n "lu" #'org-latex-disable-preview-in-buffer )

(map!   :map emacs-lisp-mode-map
        :mode emacs-lisp-mode
        :prefix "SPC"
        :n "be" #'eval-buffer)


(map!    :map typescript-mode-map
         :mode typescript-mode
         :n "gd" #'xref-find-definitions
         :n "C-o" #'xref-pop-marker-stack )

(map!   :map js2-mode-map
        :mode js2-mode
        (:prefix "SPC"
         :n "eb"  #'nodejs-repl-send-buffer
         :n "el"  #'nodejs-repl-send-line
         :v "er"  #'nodejs-repl-send-region))
