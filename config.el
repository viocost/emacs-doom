(setq user-full-name "Konstantin Y. Rybakov"
      user-mail-address "viocost@gmail.com")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq butter-jump-lines-distance 30)
(setq butter-jump-step-delay 0.0025)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq company-tooltip-limit 15)
(setq company-show-quick-access t)
(setq company-idle-delay 0)
(setq company-echo-delay 0)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!


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

(setq display-line-numbers-type 'relative)

;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-ephemeral)
;; (setq doom-theme 'doom-material)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'modus-vivendi)
;; (setq doom-theme 'doom-xcode)
(setq doom-theme 'doom-city-lights)

(setq doom-themes-treemacs-theme "doom-colors")
(setq +treemacs-git-mode 'extended)
(add-hook! 'treemacs-mode-hook
           (treemacs-follow-mode))

(setq doom-font (font-spec :family "Source Code Pro" :size 16))

(setq org-hide-emphasis-markers t)
(setq org-directory "~/org-roam/")
;; (setq org-agenda-files '("/home/kostia/org-roam"))


(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

(setq org-babel-default-header-args
       '((:session . "none")
        (:results . "replace output") ;; this is how to be in scripting mode when evaluating the code
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")))

(defun set-org-fonts()
(set-face-attribute 'org-level-1 nil  :height 1.7 )
    (set-face-attribute 'org-level-2 nil  :height 1.3 )
    (set-face-attribute 'org-level-3 nil  :height 1.2 )
    (set-face-attribute 'org-level-4 nil  :height 1.1 )
    (set-face-attribute 'org-level-5 nil  :height 1 )
    (set-face-attribute 'org-level-6 nil  :height 0.9 )
    (set-face-attribute 'org-level-7 nil  :height 0.8 )
    (set-face-attribute 'org-level-8 nil  :height 0.7 ))

(after! org
  (set-org-fonts))

(setq org-roam-directory (file-truename "~/org-roam"))
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)
(setq org-roam-database-connector 'sqlite3)
(setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-attach-id-dir "/home/kostia/org-roam")

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

(defun update-agenda-files()
  (interactive)

  (let* (
     (default-directory "/home/kostia/org-roam"))
        (setq org-agenda-files (split-string (shell-command-to-string "rg -l \"\\*+ (TODO|TICKET|BLOCKED|PROGRESS|REVIEW|CHORE|QA|DONE|CANCELLED|IDEA|PROJ).*\:(work|chore|study|idea|ticket|personal|hobby|finance)\:\"")))
    )

  (message "Updated agenda files")
)

(update-agenda-files)

(after! org

    (setq org-tag-alist '(
                        ("chore" . ?c)
                        ("study" . ?s)
                        ("business" . ?b)
                        ("hobby" . ?h)
                        ("finance" . ?f)
                        ("idea" . ?i)
                        ("work" . ?w)
                        ("personal" . ?p)
                        ("ticket" . ?t)
                        ))

    (setq org-todo-keywords
    '((sequence
        "TICKET(T)"
        "BLOCKED(b!)"
        "PROGRESS(p!)"
        "REVIEW(r!)"
        "QA(q!)"
        "|"
        "DONE(d!)"
        "CANCELLED(c!)")

    (sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c!)")
    (sequence "CHORE(c)" "PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(l!)")
    (sequence "IDEA(i)" "|" "DONE(d!)" "CANCELLED(c!)")
    (sequence "PROJ(p)" "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-todo-keyword-faces
    '(("PROJ" :foreground "purple" :weight bold)
      ("TODO" :foreground "orange" :weight bold )
      ("PROGRESS" :foreground "deep-sky-blue" :weight bold)
      ("REVIEW" :foreground "cyan" :weight bold)
      ("CHORE" :foreground "tan4" :weight bold)
      ("QA" :foreground "goldenrod" :weight bold)
      ("IDEA" :foreground "gold" :weight bold)
      ("CANCELLED" :foreground "dim gray" :weight bold)
      ("DONE" :foreground "green3" :weight bold)
      ("BLOCKED" :foreground "dark red" :weight bold)
    ;;     ("NEXT" :background "red1" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("BLOCKED" :background "yellow" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("DEFERRED" :background "gold" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("DELEGATED" :background "gold" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("MAYBE" :background "gray" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("APPT" :background "red1" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("DONE" :background "forest green" :weight bold :box (:line-width 2 :style released-button))
    ;;     ("CANCELLED" :background "lime green" :foreground "black" :weight bold :box (:line-width 2 :style released-button))))

    )))

(use-package! org-super-agenda
  ;;:custom-face
  ;;(org-super-agenda-header ((default (:inherit propositum-agenda-heading))))

  :init
  (require 'evil-org-agenda) ; to ensure keymaps are loaded

  :config
  (setq
   org-agenda-show-all-dates nil
   ))

(after! org-super-agenda
    (add-hook! org-roam-post-node-insert-hook #'update-agenda-files)
    (org-super-agenda-mode))

(defun my/style-org-agenda()
  (set-face-attribute 'org-agenda-date nil :height 1.1)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

(add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

(my/style-org-agenda)

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")

      org-agenda-prefix-format '((agenda . "%b% s")
                                 (todo . "  ┈┈┈┈ %i   %b")
                                 (tags . "")
                                 (search . "")))

(setq org-tags-match-list-sublevels nil)


(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))

(set-face-attribute 'org-super-agenda-header nil :height 1.6 :background "gray14")

(setq org-cycle-separator-lines 2)
(setq org-agenda-category-icon-alist
      `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
        ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
        ("Calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))
(setq org-agenda-custom-commands
      '(("z" "Kostia view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :scheduled today
                          :order 1)))))

          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(;; Each group has an implicit boolean OR operator between its selectors.
                          (:name "Today"
                           :deadline today
                           :scheduled today
                           :order 2
                           :face (:background "black"))

                          (:name "Projects"
                           :order 5
                           :and (:todo ("PROJ") :priority>= "B"))

                          (:name "Tickets"
                           :order 3
                           :and (:tag ("ticket") :not (:todo ("DONE"))))

                          (:name "Work, business"
                           :order 4
                           :and (
                                 :tag ("work" "business")
                                 :not (:todo ("DONE"))))

                          (:name "Study"
                           :and (
                                 :tag ("study")
                                 :not (:todo ("DONE"))))

                          (:name "Hobby, side projects, Linux ricing etc."
                           :and (
                                 :tag ("hobby" "personal")
                                 :not (:todo ("DONE"))))

                           ;;:face (:background "#7f1b19"))
                          (:name "Life, finances, errands"
                           :order 4
                           :tag ("chore" "finance")
                           :todo ("CHORE"))


                          (:name "On hold"
                           :todo "HOLD"
                           :order 10)))))))))
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
(defun open-my-agenda()
  (interactive)
  (org-agenda nil "z"))

;; (setq
;;  org-ql-views
;;  '(("stuck" lambda nil
;;     (interactive)
;;     (org-ql-search
;;       (org-agenda-files)
;;       '(and (tags "story")
;;             (not (tags "ignore"))
;;             (not (done)) ;; Finished stories should be excluded
;;             (not (descendants (todo "NEXT"))) ;; If there are already
;;             ;; something in progress
;;             ;; it will shown
;;             (and (not (descendants (done))) ;; There are not scheduled not finished items
;;                  (not (descendants (scheduled)))))
;;       :narrow nil :super-groups
;;       '((:name "Waiting"   :order 8 :todo "WAIT")
;;         (:name "Important" :order 1 :deadline t :priority>= "B")
;;         (:name "Work"      :order 2 :tag "work")
;;         (:name "Study"     :order 2 :tag "study")
;;         (:name "Stucked"   :order 3 :tag "story"))
;;       :title "stuck-projects"))
;;    ("reports" lambda nil
;;     (interactive)
;;     (org-ql-search
;;       (org-agenda-files)
;;       '(and (or (tags-local "weekly")
;;                 (tags-local "monthly"))
;;             (not (tags "ignore")))
;;       :narrow nil :super-groups
;;       '((:name "Weekly reports" :tag "weekly")
;;         (:name "Monthly reports" :tag "monthly"))
;;       :title "Introspection reports"))
;;    ("next" lambda nil
;;     (interactive)
;;     (org-ql-search
;;       (org-agenda-files)
;;       '(and (or (tags-local "refile")
;;                 (todo "PROG")
;;                 (todo "WAIT")
;;                 (todo "NEXT"))
;;             (not (tags "ignore"))
;;             (not (property "linked"))
;;             (not (done)))
;;       :sort '(date)
;;       :narrow nil
;;       :super-groups
;;       `((:name "In progress" :order 1 :todo "PROG")
;;         (:name "Daily"       :order 2 :regexp ,org-repeat-re)
;;         (:name "Waiting"     :order 3 :todo "WAIT")
;;         (:name "Refile"      :order 3 :tag "refile")
;;         (:name "Important"   :order 3 :priority>= "B")
;;         (:auto-tags t        :order 5))
;;       :title "Next actions"))
;;    ("calendar" lambda nil
;;     (interactive)
;;     (org-ql-search
;;       (org-agenda-files)
;;       `(and (ts-active)
;;             (regexp ,org-scheduled-time-hour-regexp)
;;             (not (done)))
;;       :sort '(date)
;;       :narrow nil
;;       :super-groups
;;       '((:auto-planning t))
;;       :title "Calendar"))
;;    ("dashboard" lambda nil
;;     (interactive)
;;     (org-ql-search
;;       (org-agenda-files)
;;       '(and (or (ts-active :to today)
;;                 (deadline auto)
;;                 (todo "PROG")
;;                 (and (tags "journal")
;;                      (not (tags "weekly"))
;;                      (not (tags "monthly"))
;;                      (not (tags "yearly"))
;;                      (todo)))
;;             (not (todo "WAIT"))
;;             (not (tags "ignore"))
;;             (not (property "linked"))
;;             (not (done)))
;;       :sort '(date)
;;       :narrow nil
;;       :super-groups
;;       `((:name "In progress" :order 1
;;          :tag "monthly" :tag "weekly" :todo "PROG")
;;         (:name "Agenda"      :order 2
;;          :deadline t :regexp ,org-scheduled-time-hour-regexp)
;;         (:name "Daily"       :order 2
;;          :and (:todo nil :regexp ,org-repeat-re))
;;         (:name "Today"       :order 3 :tag "journal")
;;         (:name "Important"   :order 3 :priority>= "B")
;;         (:auto-tags t        :order 5))
;;       :title "Dashboard"))))

;;(defvar done-todo-section-marker "done-todo-archive")

;; (print (org-map-entries (lambda ()
;;                   (string= (org-entry-get (point) "MARKER") "FOO_BAR"))
;;                 'FOO_BAR 'file))

(setq projectile-project-search-path '("~/projects" "~/cs"))
(projectile-discover-projects-in-search-path)

(setq magit-ediff-dwim-show-on-hunks t)
(setq vdiff-default-refinement-syntax-code "w")
(setq vdiff-auto-refine 1)

(custom-set-faces!
`(diff-removed
  :background "#550000"
  :weight semi-bold)
`(diff-changed
  :background "#004d00"
  :weight semi-bold)
`(diff-refine-changed
  :background "#660000"
  :weight semi-bold)
`(diff-added
  :background "#004d00"
  :weight semi-bold)
`(diff-refine-added
  :background "#004d00"
  :weight bold))

(map! :map magit-mode-map
      :mode magit-mode
      "e" #'vdiff-magit-dwim
      "E" #'vdiff-magit)

(map! (:map override
        :ni  "C-d" #'butter-jump-down
        :ni  "C-u" #'butter-jump-up
        :i  "C-f" #'right-char
        :i  "C-b" #'left-char
        :nv "C-j" #'evil-mc-make-cursor-move-next-line
        :nv "C-k" #'evil-mc-make-cursor-move-prev-line
        :nv "C-S-j" #'evil-mc-make-and-goto-next-match
        :nv "C-S-k" #'evil-mc-make-and-goto-prev-match
        :nv "C-/" #'comment-line
        ;;:nv "M-k" #'drag-stuff-up
        ;;:nv "M-j" #'drag-stuff-down
        :nv "M-h" #'drag-stuff-left
        :nv "M-l" #'drag-stuff-right

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
        :n "da" #'open-my-agenda
        :n "dd" #'deft
        :n "db" #'org-roam-buffer-toggle
        :n "dn" #'org-roam-node-insert
        :n "dc" #'org-roam-capture
        :n "dg" #'org-roam-ui-open
        :n "di" #'org-id-get-create
        :n "df" #'org-roam-node-find
        :n "dz" #'org-roam-ui-node-zoom
        :n "ds" #'org-roam-db-sync
        :n "dta" #'org-roam-tag-add
        :n "dtr" #'org-roam-tag-remove

        :n "e" #'treemacs

        ;;projectile
        :n "ps" #'projectile-save-project-buffers

        ;;vue piece of shit
        :n "v" #'vue-mode

        ;;lsp
        :n "lr" #'lsp-workspace-restart

        ;; insert commands
        :n "il" #'org-insert-link

        ;; chatgpt
        :nv "dq" #'chatgpt-query
        ))

(map! :map isearch-mode-map
      :mode isearch-mode
      :nv [?\t] #'isearch-repeat-forward
      :nv [S-?\t] #'isearch-repeat-backward      )

(map! :map treemacs-mode-map
      :mode treemacs-mode
      "C-=" #'text-scale-increase
      "C--" #'text-scale-decrease)

(defun org-todo-next-state()
  (interactive)
  (org-todo 'right))

(defun org-todo-previous-state()
  (interactive)
  (org-todo 'left))

(defun insert-ticket-todo (link desc)
  (interactive "sInsert JIRA link: \nsEnter description: " )
  (message link)
  (message desc)
  (let* ((ticket-name(car (last (split-string link "\\/")))))
    (insert (format "TICKET [[%s][%s]] %s :work:ticket:" link ticket-name desc))))

(map! :map org-mode-map
        :mode org-mode
        :n [C-right] #'org-todo-next-state
        :n [C-left] #'org-todo-previous-state
        :n [C-up] #'org-priority-up
        :n [C-down] #'org-priority-down
        :n [RET] #'org-todo-next-state
        :prefix "SPC"
        :n "lp" #'org-latex-preview-in-buffer
        :n "lu" #'org-latex-disable-preview-in-buffer
        :n "sY"  #'org-download-screenshot
        :n "sy"  #'org-download-yank
        :n "ts"  #'org-todo
        :n "td"  #'org-deadline
        :n "tt" #'org-set-tags-command

        ;; # Insert todo templates
        :n "tit" #'insert-ticket-todo
    )

(map!   :map deft-mode-map
        :mode deft-mode
        "C-=" #'text-scale-increase
        "C--" #'text-scale-decrease)

(map!   :map js2-mode-map
        :mode js2-mode
        (:prefix "SPC"
         :n "eb"  #'nodejs-repl-send-buffer
         :n "el"  #'nodejs-repl-send-line
         :v "er"  #'nodejs-repl-send-region))

(map!   :map emacs-lisp-mode-map
        :mode emacs-lisp-mode
        :prefix "SPC"
        :n "be" #'eval-buffer)

(add-hook! 'typescript-mode-hook 'prettier-js-mode)

(use-package! lsp-mode
  :custom
  (lsp-vetur-format-default-formatter-css "none")
  (lsp-vetur-format-default-formatter-html "none")
  (lsp-vetur-format-default-formatter-js "none")
  (lsp-vetur-validation-template nil))

(use-package! vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . prettier-js-mode)
  :config
  (add-hook! 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue")))

(defun setup-indent (n)
  (interactive)
  (setq tab-width n
        tab-width n
        sgml-basic-offset n
        c-basic-offset n
        coffee-tab-width n
        javascript-2-level n
        js-2-level n
        js2-basic-offset n
        web-mode-markup-2-offset n
        web-mode-css-2-offset n
        web-mode-code-2-offset n
        css-2-offset n
        standard-indent n
        evil-shift-width n
        rust-indent-offset n))

(after! '(evil typescript-mode org)
  (setup-indent 2))

(use-package! chatgpt
  :defer t
  :config
  (unless (boundp 'python-interpreter)
    (defvaralias 'python-interpreter 'python-shell-interpreter))
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'bottom :size .5 :ttl nil :quit t :modeline nil)
  :bind ("C-c q" . chatgpt-query))

(setq chatgpt-query-format-string-map '(
                                        ;; ChatGPT.el defaults
                                        ("doc" . "Please write the documentation for the following function.\n\n%s")
                                        ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
                                        ("understand" . "What does the following function do?\n\n%s")
                                        ("improve" . "Please improve the following code.\n\n%s")
                                        ;; your new prompt
                                        ("my-custom-type" . "My custom prompt.\n\n%s")))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (julia . t)
;;    (python . t)
;;    (jupyter . t)))
