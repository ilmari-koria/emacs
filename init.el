;; -------------------------------------------------- ;;
;; PACKAGES                                           ;;
;; -------------------------------------------------- ;;

;; sources
(require 'package)
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/")     t)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/")  t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)
(package-refresh-contents)

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; org extras
(use-package org
  :ensure org-contrib
  :demand t)

;; -------------------------------------------------- ;;
;; BASIC SETTINGS                                     ;;
;; -------------------------------------------------- ;;

;; misc
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq system-time-locale "C")
(setq scroll-bar-mode nil)
(setq warning-minimum-level :emergency)
(setq large-file-warning-threshold nil)
(setq word-wrap-by-category t)
(setq initial-buffer-choice "~/my-files/emacs/org/scratch.org")
(setq next-line-add-newlines 1)
(setq electric-pair-preserve-balance nil)

;; TODO check these
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; user
(setq user-full-name "Ilmari Koria")
(setq user-mail-address "ilmarikoria@posteo.net")

;; tramp and server
(setq auth-sources '("~/.authinfo.gpg"))
(setq tramp-verbose 1)
(setq server-client-instructions nil)

;; undo and deletion
(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)
(setq kill-ring-max 9999)
(setq delete-by-moving-to-trash t)

;; region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; dired
(setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
(put 'dired-find-alternate-file 'disabled nil)

;; scrolling
(setq scroll-conservatively 100)
(setq scroll-margin 20)

;; backups vanilla
(setq auto-save-interval 30)
(add-to-list 'backup-directory-alist
	     (cons "." "~/my-files/emacs/backups/vanilla/"))

;; backups tramp
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; backup each save
(use-package backup-each-save
  :ensure t
  :config
  (setq backup-each-save-mirror-location "~/my-files/emacs/backups/backup-each-save")
  (setq backup-each-save-remote-files nil)
  (add-hook 'after-save-hook 'backup-each-save))

;; modes
(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode t)
(tool-bar-mode -1)
(set-default 'truncate-lines t)
(global-auto-revert-mode)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode t)

;; load path
;; TODO check best practice for load path
(add-to-list 'load-path "~/my-files/emacs/init/my-elisp/")
(add-to-list 'load-path "~/my-files/emacs/init/")
(load "./helper-functions.el")


;; recentf
(recentf-mode t)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 50)
(setq recentf-exclude '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "bookmark"))
(setq recentf-filename-handlers '(abbreviate-file-name))


;; -------------------------------------------------- ;;
;; COMPLETION                                         ;;
;; -------------------------------------------------- ;;

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize nil)
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; -------------------------------------------------- ;;
;; PDF                                                ;;
;; -------------------------------------------------- ;;

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (pdf-loader-install)
  (setq pdf-view-use-scaling)
  (setq pdf-view-use-imagemagick nil)
  (setq revert-without-query '(".pdf"))
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-themed-minor-mode))))


;; -------------------------------------------------- ;;
;; OPEN WITH                                          ;;
;; -------------------------------------------------- ;;

(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.mp4\\'" "vlc" (file))
                                ("\\.wav\\'" "vlc" (file)))))

;; -------------------------------------------------- ;;
;; WRITING                                            ;;
;; -------------------------------------------------- ;;

;; abbrev
(use-package abbrev
  :config
  (setq abbrev-file-name "~/my-files/emacs/init/abbrev_defs")
  (setq save-abbrevs 'silently))

;; spelling
(use-package ispell
  :ensure t
  :config
  (setq ispell-personal-dictionary "~/my-files/emacs/init/ispell-personal-dictionary")
  (setq ispell-silently-savep t)
  (setq ispell-dictionary "en_GB")
  (setq ispell-highlight-face 'flyspell-incorrect))

;; palimpsest
(use-package palimpsest
  :ensure t)

;; move text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; word count
(use-package wc-mode
  :ensure t)

;; writegood
(use-package writegood-mode
  :ensure t
  :config
  (setq writegood-weasel-words
	'("many" "various" "very" "fairly"
	  "several" "extremely" "exceedingly" "quite"
	  "remarkably" "few" "surprisingly" "mostly"
	  "largely" "huge" "tiny" "are a number"
	  "is a number" "excellent" "interestingly" "significantly"
	  "substantially" "clearly" "vast" "relatively"
	  "completely" "literally" "not rocket science" "pretty"
	  "outside the box" "In this regard" "With this in mind"
	  "With the above in mind" "In this sense" "variety")))

;; key chord
(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define-global "jj" 'my-org-jump-nearest-heading)
  (key-chord-mode 1))


;; -------------------------------------------------- ;;
;; ELFEED                                             ;;
;; -------------------------------------------------- ;;

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-search-filter "@2-days-ago +unread")
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 100)
  (setq elfeed-use-curl t)
  (setq url-queue-timeout 30)
  (setq shr-inhibit-images t)
  (setq elfeed-sort-order 'descending)
  (setq flycheck-global-modes '(not . (elfeed-search-mode)))
  (add-hook 'elfeed-show-mode-hook 'visual-line-mode))

(use-package elfeed-org
  :ensure t
  :config
  (require 'elfeed-org)
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/my-files/emacs/org/rss/rss-feed.org")))


;; -------------------------------------------------- ;;
;; SEARCHING                                          ;;
;; -------------------------------------------------- ;;

;; deft
(use-package deft
  :ensure t
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory "~/my-files/emacs/org"))

;; engine
(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (defengine duckduckgo "https://duckduckgo.com/?q=%s"
	     :keybinding "d")
  (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
	     :keybinding "g")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
	     :keybinding "w")
  (defengine thesaurus "https://www.thesaurus.com/browse/%s"
	     :keybinding "t"))


;; -------------------------------------------------- ;;
;; ORG                                                ;;
;; -------------------------------------------------- ;;

;; agenda basic
;; agenda files are handles by emacs
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-include-diary t)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-use-tag-inheritance nil)
(setq org-tags-match-list-sublevels t)
(setq org-habit-following-days 1)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq diary-file "~/my-files/nextcloud/home-agenda/diary-google")

;; tags
(setq org-tag-alist '(("MEETING" . ?m) ("QA" . ?q) ("DEV" . ?d) ("MISC" . ?s) ("TRAINING" . ?t)))

;; -- org speed commands
(setq org-use-speed-commands t)

;; org priorities
(setq org-enable-priority-commands t)
(setq org-priority-start-cycle-with-default t)
(setq org-highest-priority 1)
(setq org-default-priority 3)
(setq org-lowest-priority 6)
(setq org-priority-faces '((?1 :foreground "#dc322f")
                           (?2 :foreground "#b58900")
                           (?3 :foreground "#6c71c4")
                           (?4 :foreground "#268bd2")
                           (?5 :foreground "#2aa198")
                           (?6 :foreground "#859900")))

;; fancy priorities
(use-package org-fancy-priorities
  :ensure t
  :config
  (add-hook 'org-agenda-mode-hook 'org-fancy-priorities-mode)
  (setq org-fancy-priorities-list '((?1 . "#1")
                                    (?2 . "#2")
                                    (?3 . "#3")
                                    (?4 . "#4")
                                    (?5 . "#5")
                                    (?6 . "#6"))))

;; linkmarks
(add-to-list 'load-path "~/my-files/emacs/init/my-elisp/linkmarks")
(require 'linkmarks)
(setq linkmarks-file "~/my-files/emacs/org/linkmarks/linkmarks.org")

;; agenda custom commands
(setq org-agenda-custom-commands
      '(("A" "AGENDA"
   	 (
	  (agenda ""
		  ((org-agenda-time-grid nil)
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-start-day "+1d")
		   (org-agenda-span 30)
		   (org-agenda-show-all-dates nil)
		   (org-deadline-warning-days 0)
		   (org-agenda-block-separator nil)
		   (org-agenda-entry-types '(:deadline))
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "\nUPCOMING DEADLINES (+14d)")))
          (todo "GOAL"
                ((org-agenda-overriding-header "\nGOALS")
                 (org-agenda-block-separator nil)
		 (org-tags-match-list-sublevels t)))
          (todo "REMINDER"
                ((org-agenda-overriding-header "\nREMINDERS")
                 (org-agenda-block-separator nil)
		 (org-tags-match-list-sublevels t)))
          (agenda ""
		  ((org-agenda-block-separator nil)
		   (org-agenda-span 1)
		   (org-deadline-warning-days 0)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("GOAL")))
		   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		   (org-agenda-overriding-header "\nTODAY'S TASKS")))
	  (agenda ""
		  ((org-agenda-start-on-weekday nil)
		   (org-agenda-start-day "+1d")
		   (org-agenda-span 7)
		   (org-deadline-warning-days 0)
		   (org-agenda-block-separator nil)
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "\nNEXT SEVEN DAYS")))
          (todo "WAITING"
		((org-agenda-overriding-header "PENDING TASKS")

		 (org-tags-match-list-sublevels t)))
	  (todo "*"
		((org-agenda-overriding-header "UNSCHEDULED TASKS")
		 (org-tags-match-list-sublevels t)
		 (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp))))))))) ;; org agenda custom commands ends here

;; org capture templates
(setq org-capture-templates '(
                              ("t" "TODO")
                              ("ta" "WAITING-unscheduled" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* WAITING %?\n")
                              ("te" "reminder" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* REMINDER %^{Description} :REMINDER:\nSCHEDULED: <%<%Y-%m-%d %a>>\n" :immediate-finish t)
                              ("tc" "scheduled-todo-full" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO %?\nSCHEDULED: %^t DEADLINE: %^t\n")
                              ("td" "scheduled-deadline-only" entry (file+headline "~~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO %?\nDEADLINE: %^t\n")
                              ("tl" "fleeting" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} :FLEETING:\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tw" "wiki" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} :WIKI:\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t) 
                              ("tf" "quick-clock-in-immediate" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("tk" "tinker" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO TINKER %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("tn" "quick-no-clock-in-immediate" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tq" "TODO-quick" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>")
                              ;; ("tr" "rss todo" entry (file+olp "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO %^{Description} %^g:RSS:\nSCHEDULED: %^t\n\n %a\n\n %i")
                              ("tt" "scheduled-todo" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO %?\nSCHEDULED: %^t\n")
                              ("tm" "messages" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO Email and messages :MESSAGES:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("ti" "misc" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO Admin and misc tasks :MISC:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("tr" "repeat" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "REPEAT-TASKS") "\n* REPEAT %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:REPEAT_TO_STATE: REPEAT\n:END:")

                              ("o" "TODO Pomodoro")
                              ("of" "quick-clock-in-immediate" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("oa" "anki" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO Anki :ANKI:\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("ok" "tinker" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO tinker %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("om" "messages" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO Email and messages :MESSAGES:\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("oi" "misc" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO Admin and misc tasks :MISC:\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("or" "reaper" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO reaper :DRONE:\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)
                              ("ow" "relax" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* TODO relax :MISC:\nSCHEDULED: <%<%Y-%m-%d %a>>" :pomodoro t :immediate-finish t)

                              ("a" "ad-hoc")
                              ("ac" "quick-task-or-notes" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE  %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("ae" "quick-meeting" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE %^{Description} :MEETING:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("ab" "comfort-break" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE Comfort Break :BREAK:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("ad" "get-drink" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE Get Drink :BREAK:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("am" "messages" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE Email and messages :MESSAGES:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("at" "tinker" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE Tinkering :DEV:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)
                              ("ai" "misc" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "\n* DONE Admin and misc tasks :MISC:\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-resume t)

                              ("n" "note-at-point" plain (file "") " - (%^{location}) Here it says that %?.")

                              ("k" "anki")
                              ("km" "anki-cloze-python" entry (file "~/my-files/anki/udemy/math-python.org") "\n* %<%Y%m%d%H%M%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: math-cloze\n:END:\n** expression\n%^{expression}\n*** Exercises\n")
                              )) ;; capture ends here

;; org export misc
(setq org-export-with-smart-quotes t)
(setq org-export-preserve-breaks t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-use-sub-superscripts nil)
(setq org-export-with-sub-superscripts nil)

;; org latex
(setq org-latex-tables-centered nil)
(setq org-latex-images-centered nil)
(setq org-latex-tables-centered nil)
(setq org-latex-toc-command "\\tableofcontents \\addtocontents{toc}{\\protect\\thispagestyle{empty}} \\newpage")
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; org html
(setq org-html-footnotes-section "<div id=\"footnotes\"><h2 class=\"footnotes\">%s </h2><div id=\"text-footnotes\">%s</div></div>")
(setq org-html-postamble-format '(("en" "<p class=\"postamble\" style=\"padding-top:5px;font-size:small;\">Author: %a (%e) | Last modified: %C.</p>")))
(setq org-html-head-include-default-style nil)
(setq org-html-postamble t)

;; org extras/contrib
(use-package org-contrib
  :ensure t
  :config
  (require 'ox-extra)
  (require 'ox-latex)
  (require 'ox-bibtex)
  (ox-extras-activate
   '(ignore-headlines)))

;; org misc
(setq org-directory "~/my-files/emacs/org")
(setq org-startup-folded t)
(setq org-log-into-drawer t)
(setq org-src-fontify-natively nil)
(setq org-clock-into-drawer "CLOCK")
(setq org-startup-truncated t)
(setq org-image-actual-width '(200))
(setq org-startup-indented t)
(setq org-habit-following-days 1)
(setq org-attach-auto-tag "attach")
(setq org-use-tag-inheritance nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; org archive
(setq org-archive-location "~/my-files/emacs/org/archive/org-archive.org::datetree/")
(setq org-archive-mark-done t)
(setq org-archive-subtree-save-file-p t)

;; org modules
(require 'org-habit)

;; org ref
(use-package org-ref
  :ensure t
  :config
  (setq org-ref-activate-cite-links t)
  (setq org-ref-cite-insert-version 2)
  (setq org-ref-show-broken-links nil)
  (setq bibtex-completion-bibliography '("/home/ilmari/my-files/zotero/bibliography.bib"))
  (setq bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath))))

;; orb
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :config
  (require 'org-ref)
  (add-hook 'after-init-hook 'org-roam-bibtex-mode))

;; org roam
(use-package org-roam
  :ensure t
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/my-files/emacs/org/roam"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template (concat "${type:15} | " (propertize "${tags:40}" 'face 'org-tag)" | ${title:*}"))
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))
          (not (member "attach" (org-get-tags)))
          (not (member "noexport" (org-get-tags)))
          (not (member "ignore" (org-get-tags)))
          (not (member "NOEXPORT" (org-get-tags)))))
  (setq org-roam-capture-templates '(("b" "blog-draft" plain "%?" :target (file+head "blog-drafts/%<%Y-%m-%d>-blog-draft-${slug}.org" "#+title: ${title}\n#+filetags: %^{TAGS}\n#+DESCRIPTION: %^{short description}\n#+date: <%<%Y-%m-%d %H:%M>>\n* Introduction\n* par2\n* par3\n* par4\n* par5\n* par6\n* par7\n* Conclusion\n* Timestamp :ignore:\n =This blog post was last updated on {{{time(%b %e\\, %Y)}}}.=\n* References :ignore:\n#+BIBLIOGRAPHY: bibliography.bib plain option:-a option:-noabstract option:-heveaurl limit:t\n* Footnotes :ignore:\n* Text-dump :noexport:") :unnarrowed t :jump-to-captured t)
                                     ("r" "reference" plain "%?" :target (file+head "reference/%<%Y-%m-%d>-reference-${citekey}.org" "#+title: ${citekey} - ${title}\n#+filetags: %^{TAGS}\n\n--\n + ") :jump-to-captured t :unnarrowed t)
                                     ("i" "index" plain "%?" :target (file+head "reference/index-${slug}.org" "#+title: ${title}") :jump-to-captured t :unnarrowed t)
                                     ("p" "permanent" plain "%?" :target (file+head "permanent/%<%Y-%m-%d>-permanent-${slug}.org" "#+title: ${title}\n#+filetags: %^{TAGS}\n\n - [ ] One subject, signified by the title.\n - [ ] Wording that is independent of any other topic.\n - [ ] Between 100-200 words.\n\n--\n + ") :jump-to-captured t :unnarrowed t)))
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.5)
                 (window-height . fit-window-to-buffer)))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory (directory-file-name
			         (file-name-directory
                                  (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (org-roam-db-autosync-mode)

(add-hook 'org-roam-mode-hook #'visual-line-mode)
(add-hook 'org-roam-mode-hook #'org-indent-mode)
) ;; org roam ends here

;; org hooks
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'writegood-mode)
(add-hook 'org-mode-hook 'wc-mode)
(add-hook 'org-mode-hook 'palimpsest-mode)
(add-hook 'org-mode-hook 'wrap-region-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
(add-hook 'org-mode-hook 'hl-line-mode)

;; org pomodoro
(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-long-break-frequency 5)
  (setq org-pomodoro-long-break-length 10)
  (setq org-pomodoro-keep-killed-pomodoro-time t))

;; org src
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
(setq org-src-strip-leading-and-trailing-blank-lines t)
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

;; org babel
(setq org-export-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (latex . t)))

;; babel kmacro add python src
(fset 'python-anki
      (kmacro-lambda-form [?\C-c ?0 ?s ?p ?y ?t ?h ?o ?n ?  ?- ?n ?  ?: ?r ?e ?s ?u ?l ?t ?s ?  ?p backspace ?o ?u ?p backspace ?t ?p ?u ?t ?  ?: ?e ?x ?p ?o ?r ?t ?  ?b ?o ?t ?h] 0 "%d"))

;; org roam vis
(use-package org-roam-ui
  :ensure t
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t))

;; org wc
(use-package org-wc
  :ensure t
  :config
  (setq org-wc-ignored-tags '("ARCHIVE")))

;; website
(use-package org-static-blog
  :ensure t
  :config
  (require 'org-static-blog)
  (setq org-static-blog-publish-title "Ilmari's Webpage")
  (setq org-static-blog-publish-url "https://ilmarikoria.xyz")
  (setq org-static-blog-archive-file "posts.html")
  (setq org-static-blog-publish-directory "~/my-files/websites/ilmarikoria/")
  (setq org-static-blog-posts-directory "~/my-files/emacs/org/roam/blog/")
  (setq org-static-blog-drafts-directory "~/my-files/emacs/org/roam/blog-drafts-dummy/") ;; because org-static-blog-publish will publish drafts folder
  (setq org-static-blog-preview-date-first-p t)
  (setq org-static-blog-enable-tags nil)
  (setq org-static-blog-preview-ellipsis "")
  (setq org-static-blog-use-preview t)
  (setq org-static-blog-preview-start "")
  (setq org-static-blog-preview-end "")
  (setq org-static-blog-index-front-matter "<div id=\"front-page-container\">
                                            <div id=\"welcome\">
                                            <h2>About</h2>
                                            <p>Hello! My name is Ilmari. This blog is simply for me to share my interests in software, sound and zen.</p>
                                            <p>Feel free to contact me via <a href=\"mailto:ilmarikoria@posteo.net\">ilmarikoria@posteo.net</a>.</p>
                                            <p id=\"about-disclaimer\">Apart from <a href=\"https://uk.linkedin.com/in/ilmari-koria-3151a5291\">LinkedIn</a> and <a href=\"https://www.youtube.com/channel/UCIwGuCqBXzXGozj0YeAcOTA\">YouTube</a>, I do not have (or use) any other social media accounts.</p>
                                            </div>
                                            <div id=\"profile-pic\"><figure><img src=\"./static/profile.png\" alt=\"Selfie of a Nordic caucasian man with short curly brown hair.\"><figcaption>‘Me / 2023.’</figcaption></figure></div>
                                            </div>
                                            <h2>Recent posts</h2>")
  (setq org-static-blog-page-header "<meta name=\"author\" content=\"Ilmari Koria, ilmarikoria@posteo.net\">
                                     <meta name=\"referrer\" content=\"no-referrer\">
                                     <meta name=\"viewport\" content=\"initial-scale=1.0,maximum-scale=1.0,user-scalable=no\" />
                                     <link href=\"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
                                     <link rel=\"icon\" href=\"data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🏞</text></svg>\">
                                     ")
  (setq org-static-blog-page-preamble
        "<h1>Ilmari's Webpage</h1>
             <ul>
                <li><a href=\"https://ilmarikoria.xyz\">Home</a></li>
                <li><a href=\"https://ilmarikoria.xyz/posts.html\">Posts</a></li>
                <li><a href=\"https://ilmarikoria.xyz/rss.xml\">RSS</a></li>
                <li><a href=\"https://git.ilmarikoria.xyz/\">Git</a></li>
                <li><a href=\"https://ilmarikoria.xyz/ilmari-koria-resume.pdf\">Résumé</a></li>
                <li><a href=\"https://nextcloud.ilmarikoria.xyz/\">Nextcloud</a></li>
                <li><a href=\"https://freesound.org/people/ilmari_freesound/\">Freesound</a></li>
                <li><a href=\"https://phonography.wiki/\">Phonography Wiki</a></li>
            </ul>")
  (setq org-static-blog-page-postamble
        (format "<p id=\"metadata-stamp\">This page was last modified on %s ❘ Created in GNU Emacs version %s and org-mode version %s (using <a href=\"https://github.com/bastibe/org-static-blog\">org-static-blog</a>) ❘ <a href=\"https://ilmarikoria.xyz/static/ilmari-pub.asc\">PGP Key</a> ❘ Support this site? <code id=\"bitcoin-donation\">Bitcoin BTC [<a href=\"https://ilmarikoria.xyz/static/bitcoin-qr.png\">QR</a>]: </code><code id=\"bitcoin-address\">bc1qjc0frqyyrgmcsugw7vmlj4e9vhxfvsrut3nnvs</code></p>
                 <p id=\"bottom-links\"><a href=\"https://creativecommons.org/licenses/by-nc/4.0/\">License</a> ❘ <a href=\"#top\">Top</p>"
                (format-time-string "%b %e, %Y")
                emacs-version
                (org-version)))) ;; -- org static blog ends here

;; org appear - TODO what is this??
(use-package org-appear
  :ensure t)

;; org structure templates
(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")
        ("t" . "translation")
        ("n" . "notes")
        ("y" . "commentary")
        ("g" . "enGB")
        ("z" . "zhTW")
        ("m" . "two_column"))) ;; -- org-structure-template-alist ends here

;; org journal
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/my-files/emacs/org/journal/")
  (setq org-journal-date-format "%Y-%m-%d")
  (setq org-journal-file-format "%Y-journal.org")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-file-type 'yearly)
  (setq org-journal-file-header "#+title: %Y Journal\n#+filetags: log todo diary"))


;; -------------------------------------------------- ;;
;; SCRIPTING/PROG                                     ;;
;; -------------------------------------------------- ;;

;; python
(setq python-indent-guess-indent-offset nil)
(setq python-indent-guess-indent-offset-verbose nil)

;; expand region
(use-package expand-region
  :ensure t)

;; rainbow colors
(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; wrap region
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrappers
   '(("/" "/")
     ("*" "*")
     ("=" "=")
     ("+" "+")
     ("_" "_"))))

;; format all
(use-package format-all
  :ensure t)

;; multiple cursors
(use-package multiple-cursors
  :ensure t)

;; latex
(use-package auctex
  :ensure t
  :config
  (load "auctex.el" nil t t))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'latex-mode-hook 'format-all-mode)
(add-hook 'latex-mode-hook 'rainbow-mode)
(add-hook 'latex-mode-hook 'rainbow-delimiters-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'hl-line-mode)
(add-hook 'latex-mode-hook 'multiple-cursors-mode)

;; control lock
(require 'control-lock)
(control-lock-keys)

;; ediff
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; magit
(use-package magit
  :ensure t)

;; golden ration
(use-package golden-ratio
  :ensure t
  :config
  (require 'golden-ratio)
  (golden-ratio-mode 1))

;; prog mode hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'wrap-region-mode)
(add-hook 'prog-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'multiple-cursors-mode)

;; html mode hook
(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'html-mode-hook 'electric-indent-mode)
(add-hook 'html-mode-hook 'wrap-region-mode)
(add-hook 'html-mode-hook 'abbrev-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-delimiters-mode)
(add-hook 'html-mode-hook 'multiple-cursors-mode)


;; -------------------------------------------------- ;;
;; STYLING                                            ;;
;; -------------------------------------------------- ;;
(setq-default line-spacing 0.3)


;; -------------------------------------------------- ;;
;; BINDINGS                                           ;;
;; -------------------------------------------------- ;;

;; free keys
(use-package free-keys
  :ensure t)

;; dangerous bindings
(global-set-key (kbd "C-! C-a") 'org-attach-dired-to-subtree)
(global-set-key (kbd "C-! C-i") 'org-id-get-create)
(global-set-key (kbd "C-! C-k") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-! C-l") 'org-toggle-link-display)
(global-set-key (kbd "C-! C-t") 'dired-toggle-read-only)

;; generic bindings
(global-set-key (kbd "<f5>" ) 'async-shell-command)
(global-set-key (kbd "<f6>" ) 'org-capture)

;; TODO sort these out
;; TODO remove org bindings and use speed org
(global-set-key (kbd "C-= .") 'recentf-open-files)
(global-set-key (kbd "C-= 1") 'elfeed)
(global-set-key (kbd "C-= =") 'dired-up-directory)
(global-set-key (kbd "C-= R") 'org-refile)
(global-set-key (kbd "C-c 0") 'org-insert-structure-template)
(global-set-key (kbd "C-c W") 'widen)
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

(global-set-key (kbd "C-= a") 'org-agenda)
(global-set-key (kbd "C-= b") 'format-all-buffer)
(global-set-key (kbd "C-= c") 'comment-region)
(global-set-key (kbd "C-= d") 'org-deadline)
(global-set-key (kbd "C-= e") 'org-sort)
(global-set-key (kbd "C-= f") 'whitespace-mode)
(global-set-key (kbd "C-= g") 'org-schedule)
(global-set-key (kbd "C-= h") 'hl-line-mode)
(global-set-key (kbd "C-= i") 'org-store-link)
(global-set-key (kbd "C-= j") 'org-wc-display)
(global-set-key (kbd "C-= k") 'clone-indirect-buffer)
(global-set-key (kbd "C-= l") 'flyspell-popup-correct)
(global-set-key (kbd "C-= m") 'overwrite-mode)
(global-set-key (kbd "C-= n") 'my-org-capture-at-point)
(global-set-key (kbd "C-= o") 'org-narrow-to-subtree)
(global-set-key (kbd "C-= p") 'org-pomodoro)
(global-set-key (kbd "C-= q") 'my-surround-region-with-actual-quotes)
(global-set-key (kbd "C-= r") 'replace-regexp)
(global-set-key (kbd "C-= s") 'count-words-region)
(global-set-key (kbd "C-= t") 'my-ispell-add-word)
(global-set-key (kbd "C-= u") 'uncomment-region)
(global-set-key (kbd "C-= v") 'visual-line-mode)
(global-set-key (kbd "C-= w") 'flyspell-buffer)
(global-set-key (kbd "C-= x") 'hl-tags-mode)
(global-set-key (kbd "C-= y") 'org-insert-heading-after-current)
(global-set-key (kbd "C-= z" ) 'my-sentence-counter)

;; roam bindings
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n j") 'org-journal-new-entry)
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n p") 'completion-at-point)
(global-set-key (kbd "C-c n r") 'org-journal-search-forever)
(global-set-key (kbd "C-c n s") 'deft)

;; multiple cursors
(global-set-key (kbd "C-M-j") 'mc/mark-all-dwim)
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-l") 'er/expand-region)
(global-set-key (kbd "C-M-/") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-s") 'just-one-space)
(global-set-key (kbd "C-M-y") 'mc/insert-numbers) ;; (C-u-1-0) -- also (rectangle-number-lines)
(global-set-key (kbd "C-'") 'mc/hide-unmatched-lines-mode)
(global-set-key (kbd "C-M-n") 'electric-newline-and-maybe-indent)

;; anki
(global-set-key (kbd "C-: c") 'my-anki-cloze)
(global-set-key (kbd "C-: r") 'my-reset-cloze-counter)
(global-set-key (kbd "C-: s") 'my-set-cloze-counter)
(global-set-key (kbd "C-: m") 'my-mark-and-run-my-anki-cloze)
(global-set-key (kbd "C-: p") 'python-anki)


;; -------------------------------------------------- ;;
;; ADDED BY EMACS                                     ;;
;; -------------------------------------------------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("2cc1b50120c0d608cc5064eb187bcc22c50390eb091fddfa920bf2639112adb6" "fc608d4c9f476ad1da7f07f7d19cc392ec0fb61f77f7236f2b6b42ae95801a62" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" "eb50f36ed5141c3f702f59baa1968494dc8e9bd22ed99d2aaa536c613c8782db" "4320a92406c5015e8cba1e581a88f058765f7400cf5d885a3aa9b7b9fc448fa7" default))
 '(org-agenda-files
   '("/home/ilmari/my-files/nextcloud/work-agenda/task-index-work/misc-index.org" "/home/ilmari/my-files/nextcloud/home-agenda/agenda/agenda.org" "/home/ilmari/my-files/emacs/org/journal/2023-journal.org"))
 '(package-selected-packages
   '(ox-mediawiki golden-ratio org-fancy-priorities auctex org-bullets lua-mode anki-editor openwith pdf-tools orderless vertico writegood-mode wrap-region wc-mode use-package tablist rainbow-mode rainbow-delimiters palimpsest org-wc org-roam-ui org-ref org-pomodoro org-make-toc org-journal org-contrib org-appear multiple-cursors move-text modus-themes magit key-chord free-keys format-all expand-region engine-mode elfeed-org deft backup-each-save)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Sans Mono" :foundry "GOOG" :slant normal :weight normal :height 143 :width normal))))
 '(hl-line ((t nil))))
