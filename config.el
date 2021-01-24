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

;;org-roam

(setq org-roam-directory "~/notes"
      org-roam-tag-sources '(prop vanilla)
      org-roam-completion-system 'ido
      org-roam-capture-templates '(("d" "default" plain (function org-roam--capture-get-point)
                                   "%?"
                                   :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                   :head "#+TITLE: ${title}\n#+ROAM_TAGS: unprocessed unfinished\n#+SOURCES: \nLINKS:\n\n"
                                   :unnarrowed t)))

(require 'org-roam-protocol)


(setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)

;; If another instance of emacs is already running - the server is already running
;; and calling this function will cause an error (port already occupied)
(ignore-errors
        (org-roam-server-mode))


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

;;(after! elpy (elpy-enable))

;; OLD CONFIG

;; Place your private configuration here
(setq doom-font (font-spec :family "Source Code Pro" :size 16))



(setq display-line-numbers-type 'relative)



;; Full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Setting up deft for note taking and a couple of helper functions
(use-package! deft
  :init
  ;;(setq deft-directory "~/notes")
  (setq deft-directory "~/notes")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title nil)
  (setq deft-extensions '("md" "org")))

  ;;(setq deft-use-filter-string-for-filename t)
  ;;(setq deft-extensions '("org"))
  ;;(setq deft-new-file-format "%Y-%m-%dT%H%M")
  ;;(setq deft-org-mode-title-prefix t))


(use-package! zetteldeft
  :init
  (setq zetteldeft-title-prefix "#+TITLE: ")
  (setq zetteldeft-title-suffix "\n#+TAGS: #unprocessed #unfinished \nSOURCE: \n\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let ((                                                                                              ;;
;;        '(;; Each group has an implicit boolean OR operator between its selectors.                    ;;
;;          (:name "Today"  ; Optionally specify section name                                           ;;
;;                 :time-grid t  ; Items that appear on the time grid                                   ;;
;;                 :todo "TODAY")  ; Items that have this TODO keyword                                  ;;
;;          (:name "Important"                                                                          ;;
;;                 ;; Single arguments given alone                                                      ;;
;;                 :tag "bills"                                                                         ;;
;;                 :priority "A")                                                                       ;;
;;          ;; Set order of multiple groups at once                                                     ;;
;;          (:order-multi (2 (:name "Shopping in town"                                                  ;;
;;                                  ;; Boolean AND group matches items that match all subgroups         ;;
;;                                  :and (:tag "shopping" :tag "@town"))                                ;;
;;                           (:name "Food-related"                                                      ;;
;;                                  ;; Multiple args given in list with implicit OR                     ;;
;;                                  :tag ("food" "dinner"))                                             ;;
;;                           (:name "Personal"                                                          ;;
;;                                  :habit t                                                            ;;
;;                                  :tag "personal")                                                    ;;
;;                           (:name "Space-related (non-moon-or-planet-related)"                        ;;
;;                                  ;; Regexps match case-insensitively on the entire entry             ;;
;;                                  :and (:regexp ("space" "NASA")                                      ;;
;;                                                ;; Boolean NOT also has implicit OR between selectors ;;
;;                                                :not (:regexp "moon" :tag "planet")))))               ;;
;;          ;; Groups supply their own section names when none are given                                ;;
;;          (:todo "WAITING" :order 8)  ; Set order of this section                                     ;;
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")                                  ;;
;;                 ;; Show this group at the end of the agenda (since it has the                        ;;
;;                 ;; highest number). If you specified this group last, items                          ;;
;;                 ;; with these todo keywords that e.g. have priority A would be                       ;;
;;                 ;; displayed in that group instead, because items are grouped                        ;;
;;                 ;; out in the order the groups are listed.                                           ;;
;;                 :order 9)                                                                            ;;
;;          (:priority<= "B"                                                                            ;;
;;                       ;; Show this section after "Today" and "Important", because                    ;;
;;                       ;; their order is unspecified, defaulting to 0. Sections                       ;;
;;                       ;; are displayed lowest-number-first.                                          ;;
;;                       :order 1)                                                                      ;;
;;          ;; After the last group, the agenda will display items that didn't                          ;;
;;          ;; match any of these groups, with the default order position of 99                         ;;
;;          )))                                                                                         ;;
;;   (org-agenda nil "a"))                                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! org-super-agenda                      ;;
;;   :after org-agenda                                 ;;
;;   :init                                             ;;
;;   (setq org-super-agenda-groups '((:auto-group t))) ;;
;;                                                     ;;
;;                                                     ;;
;;   :config                                           ;;
;;   (org-super-agenda-mode))                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Agenda files

(setq org-agenda-files '("~/notes/agenda"))

(use-package! winum)
(winum-mode)

(after! '(treemacs dired)
  (treemacs-icons-dired-mode))

(defun open-org-roam-graph (str &optional)
     "Open org-roam graph"
     (interactive "p")
     (org-link-open-from-string  "http://127.0.0.1:8080"))


(setq org-hide-emphasis-markers t)
(setq org-hide-block-startup t)

(defun org-toggle-markup ()
  (interactive "p")
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (setq org-hide-block-startup (not org-hide-block-startup)))


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

(map!   :map python-mode-map
        :mode python-mode
        :n "C-o" #'jedi:goto-definition-pop-marker
        :n "gd" #'jedi:goto-definition
        (:prefix "SPC"
        :n "be" #'python-shell-send-buffer))

(map!   :map emacs-lisp-mode-map
        :mode emacs-lisp-mode
        :prefix "SPC"
        :n "be" #'eval-buffer)




(map!   :map js2-mode-map
        :mode js2-mode
        (:prefix "SPC"
         :n "eb"  #'nodejs-repl-send-buffer
         :n "el"  #'nodejs-repl-send-line
         :v "er"  #'nodejs-repl-send-region))

(setq tide-tsserver-executable nil)

(setq org-log-done 'time)

;; transparency
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;; setting default transaprency to 85
(transparency 100)


(setq doom-theme 'doom-dracula)
(setq doom-themes-treemacs-theme "doom-colors")


(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4)))
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))

(global-undo-tree-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (after! org-agenda (setq org-capture-templates                                             ;;
;;         '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")                 ;;
;;       "** TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)                                  ;;
;;     ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")                 ;;
;;       "** MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)                      ;;
;;     ("d" "Diary" entry (file+headline+datetree org-default-notes-file "Diary")             ;;
;;       "** %?\n%U\n" :clock-in t :clock-resume t)                                           ;;
;;     ("i" "Islands" entry (file+headline "~/notes/islands-dev-notes.org"  "Captured notes") ;;
;;       "** %U \n%?" )                                                                       ;;
;;     ("x" "Xenfit" entry (file+headline "~/notes/2020-04-29t1020.org"  "Captured notes")    ;;
;;       "** %U \n%?" )                                                                       ;;
;;     ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")                  ;;
;;       "** NEXT %? \nDEADLINE: %t"))))                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;; Faces defines how things look
;; This is for org metaline, or all lines that start with #+ in org mode
;;(set-face-attribute 'org-meta-line nil :height 0.7 :slant 'normal)
;;(set-face-attribute 'org-block-begin-line nil :height 0.7)
;;(set-face-attribute 'org-block-begin-line nil :height 0.8 :foreground "#000000")
;;(set-face-attribute 'org-block-end-line nil  :height 0.7 )
(set-face-attribute 'org-level-1 nil  :height 1.7 )
(set-face-attribute 'org-level-2 nil  :height 1.3 )
(set-face-attribute 'org-level-3 nil  :height 1.2 )
(set-face-attribute 'org-level-4 nil  :height 1.1 )
(set-face-attribute 'org-level-5 nil  :height 1 )
(set-face-attribute 'org-level-6 nil  :height 0.9 )
(set-face-attribute 'org-level-7 nil  :height 0.8 )
(set-face-attribute 'org-level-8 nil  :height 0.7 )
