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

(setq doom-themes-treemacs-theme "doom-colors")
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

        ;; knowledge base
        :n "dd" #'deft
        :n "dn" #'org-roam-node-insert
        :n "dc" #'org-roam-capture
        :n "dg" #'org-roam-ui-open
        :n "di" #'org-id-get-create
        :n "df" #'org-roam-node-find
        :n "dz" #'org-roam-ui-node-zoom
        :n "ds" #'org-roam-db-sync
        :n "dta" #'org-roam-tag-add
        :n "dtr" #'org-roam-tag-remove

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


;; (map!    :map typescript-mode-map
;;          :mode typescript-mode
;;          :n "gd" #'xref-find-definitions
;;          :n "C-o" #'xref-pop-marker-stack )

(map!   :map js2-mode-map
        :mode js2-mode
        (:prefix "SPC"
         :n "eb"  #'nodejs-repl-send-buffer
         :n "el"  #'nodejs-repl-send-line
         :v "er"  #'nodejs-repl-send-region))

(add-hook! 'vue-mode-hook 'lsp)


;; ORG ROAM
;;

;; (use-package! org-roam-protocol)
;;
;;
;; (setq org-roam-directory "~/notes"
;;       org-roam-tag-sources '(prop vanilla)
;;       org-roam-link-auto-replace t
;;       org-roam-completion-system 'ido
;;       org-roam-db-location "~/notes/.cache.db"
;;       org-roam-capture-templates '(("d" "default" plain (function org-roam--capture-get-point)
;;                                    "%?"
;;                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;                                    :head "#+TITLE: ${title}\n#+ROAM_TAGS: unprocessed unfinished\n#+SOURCES: \nLINKS:\n\n"
;;                                    :unnarrowed t)))
;; (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8090
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files t
;;         org-attach-id-dir "~/notes/.attach"
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20)
;;
;; ;;  Setting up deft for note taking and a couple of helper functions
;;  (use-package! deft
;;    :init
;;    ;;(setq deft-directory "~/notes")
;;    (setq deft-directory "~/notes")
;;    (setq deft-text-mode 'org-mode)
;;    (setq deft-use-filename-as-title nil)
;;    (setq deft-extensions '("md" "org")))
;;
;; (after! '(treemacs dired)
;;   (treemacs-icons-dired-mode))

(setq org-roam-directory (file-truename "~/org-roam"))
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)
(setq org-roam-database-connector 'sqlite3)


(setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-attach-id-dir "/home/kostia/org-roam")


(map! :map org-mode-map
        :mode org-mode
        :prefix "SPC"
        :n  "sY"  #'org-download-screenshot
        :n  "sy"  #'org-download-yank )

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

(setq company-tooltip-limit 15)
(setq company-show-quick-access t)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
;; Default indentation level
(setq sgml-basic-offset 2)

(setq tab-width 2
        tab-width 2
        c-basic-offset 2
        coffee-tab-width 2
        javascript-2-level 2
        js-2-level 2
        js2-basic-offset 2
        web-mode-markup-2-offset 2
        web-mode-css-2-offset 2
        web-mode-code-2-offset 2
        css-2-offset 2
        standard-indent 2
        evil-shift-width 2
        rust-indent-offset 2)

(setq magit-ediff-dwim-show-on-hunks t)

;; DEFT
(setq deft-directory "~/org-roam")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title nil)
(setq deft-extensions '("md" "org"))

(setq deft-file-limit 30)
(defun cm/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
 (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
 (if begin
  (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
  (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-strip-summary-regexp
  (concat "\\("
  "[\n\t]" ;; blank
  "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
  "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
  "\\)"))

(set-face-attribute 'org-level-1 nil  :height 1.7 )
(set-face-attribute 'org-level-2 nil  :height 1.3 )
(set-face-attribute 'org-level-3 nil  :height 1.2 )
(set-face-attribute 'org-level-4 nil  :height 1.1 )
(set-face-attribute 'org-level-5 nil  :height 1 )
(set-face-attribute 'org-level-6 nil  :height 0.9 )
(set-face-attribute 'org-level-7 nil  :height 0.8 )
(set-face-attribute 'org-level-8 nil  :height 0.7 )
