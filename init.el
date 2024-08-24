;; -------------------------------------------------- ;;
;; PACKAGES                                           ;;
;; -------------------------------------------------- ;;

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
(setq warning-minimum-level :emergency) ;; check docs for this
(setq large-file-warning-threshold nil)
(setq word-wrap-by-category t)
(setq initial-buffer-choice "~/my-files/org/scratch.org")
(setq next-line-add-newlines 1)
(setq electric-pair-preserve-balance nil)

;; TODO check these
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; user
(setq user-full-name "Ilmari Koria")
(setq user-mail-address "ilmarikoria@posteo.net")

;; gpg, tramp and server
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

;; dired narrow
(use-package dired-narrow
  :ensure t)

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
(load "helper-functions.el")

;; recentf
(recentf-mode t)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 50)
(setq recentf-exclude '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "bookmark"))
(setq recentf-filename-handlers '(abbreviate-file-name))

;; golden ration
(use-package golden-ratio
  :ensure t
  :config
  (require 'golden-ratio)
  (golden-ratio-mode 1))

;; -------------------------------------------------- ;;
;; SuperCollider, SCLang                              ;;
;; -------------------------------------------------- ;;

(add-to-list 'load-path "~/.local/share/SuperCollider/downloaded-quarks/scel/el")
(require 'sclang)
(require 'w3m)

;; TODO for some reason I am getting the error `SCLang: Error in
;; command handler` with base install. Installing extensions seems to
;; remover error(?)
(use-package sclang-extensions
  :ensure t
  :config
  (sclang-ac-mode -1))


;; -------------------------------------------------- ;;
;; REPEAT                                             ;;
;; -------------------------------------------------- ;;

;; (describe-repeat-maps)
;; see more maps "https://www.reddit.com/r/emacs/comments/1adwnse/repeatmode_is_awesome_share_you_useful_configs/"
(use-package repeat
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  (repeat-mode 1)

  (defvar buffer-navigation-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (define-key map (kbd "f") #'forward-word)
      (define-key map (kbd "b") #'backward-word)
      (define-key map (kbd "d") #'scroll-up-command)
      (define-key map (kbd "u") #'scroll-down-command)
      map))

  (dolist (cmd '(next-line previous-line forward-word backward-word scroll-up-command scroll-down-command))
    (put cmd 'repeat-map 'buffer-navigation-map)))


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

(use-package marginalia
  :ensure t)

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
  (key-chord-define-global "jj" 'my-mark-and-run-my-anki-cloze)
  (key-chord-define-global "oo" 'my-mark-and-run-my-anki-cloze)
  (key-chord-define-global "ii" 'my-mark-and-run-my-anki-cloze-but-dont-increase-counter)
  (key-chord-define-global "ss" 'my-set-cloze-counter)
  (key-chord-define-global "rr" 'my-reset-cloze-counter)
  (key-chord-mode -1))


;; -------------------------------------------------- ;;
;; PDF                                                ;;
;; -------------------------------------------------- ;;

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (pdf-loader-install)
  (setq pdf-view-use-scaling nil)
  (setq pdf-view-use-imagemagick nil))

(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-themed-minor-mode)))
(setq revert-without-query '(".pdf"))


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
  (setq rmh-elfeed-org-files (list "~/my-files/org/rss/rss-feed.org")))


;; -------------------------------------------------- ;;
;; SEARCHING                                          ;;
;; -------------------------------------------------- ;;

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
;; ENCRYPTION                                         ;;
;; -------------------------------------------------- ;;

(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg"))


;; -------------------------------------------------- ;;
;; ORG                                                ;;
;; -------------------------------------------------- ;;

;; agenda basic
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-include-diary nil)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-use-tag-inheritance nil)
(setq org-tags-match-list-sublevels t)
(setq org-habit-following-days 1)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq diary-file "~/my-files/todo/home-agenda/diary-google")

;; tags (max 6)
(setq org-tag-alist '(
                      ("MUSIC" . ?m)
                      ("ACADEMIC" . ?a)
                      ("DEV" . ?d)
                      ("HOME" . ?h)
                      ("FAMILY" . ?f)
                      ("WORK" . ?w)))

;; org priorities
(setq org-enable-priority-commands t)
(setq org-priority-start-cycle-with-default t)
(setq org-highest-priority 1)
(setq org-default-priority 3)
(setq org-lowest-priority 6)
(setq org-priority-faces '((?1 :foreground "#ff0000")
                           (?2 :foreground "#ff4500")
                           (?4 :foreground "#ffa500")
                           (?3 :foreground "#ffd700")
                           (?5 :foreground "#ffff00")
                           (?6 :foreground "#adff2f")))


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

;; timer
(setq org-timer-default-timer "25")

;; agenda custom commands
(setq org-agenda-custom-commands
      '(("a" "agenda"
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
          (agenda ""
		  ((org-agenda-block-separator nil)
		   (org-agenda-span 1)
		   (org-deadline-warning-days 0)
		   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		   (org-agenda-overriding-header "\ntoday's tasks --- max six todos --- be as specific as possible")))
	  (agenda ""
		  ((org-agenda-start-on-weekday nil)
		   (org-agenda-start-day "+1d")
		   (org-agenda-span 3)
		   (org-deadline-warning-days 0)
		   (org-agenda-block-separator nil)
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "\nNEXT THREE DAYS")))
	  (todo "*"
		((org-agenda-overriding-header "UNSCHEDULED TASKS")
		 (org-tags-match-list-sublevels t)
		 (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp))))))))) ;; org agenda custom commands ends here

;; org capture templates
(setq org-capture-templates '(
                              ("t" "todo")
                              ("ts" "scheduled" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* TODO [#3] %?\nSCHEDULED: %^t DEADLINE: %^t\n")
                              ("tw" "unscheduled-waiting" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* [#3] WAITING %?\n")
                              ("tr" "reminder" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* REMINDER [#3] %^{Description} :REMINDER:\nSCHEDULED: <%<%Y-%m-%d %a>>\n" :immediate-finish t)
                              ("tf" "fleeting" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* TODO [#3] %^{Description} :FLEETING:\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tq" "quick-clock-in" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("tn" "quick-no-clock-in" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tt" "quick-tomorrow" entry (file+headline "~/my-files/todo/todo/home/TODO.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))" :immediate-finish t)
                              ("tr" "repeat" entry (file+headline "~/my-files/todo/TODO.org" "REPEAT-TASKS") "* REPEAT [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:REPEAT_TO_STATE: REPEAT\n:END:")

                              ("n" "note-at-point" plain (file "") " - (%^{location}) Here it says that %?.")

                              ("a" "anki")
                              ("am" "rossModernMandarinChinese2023" entry (file "~/my-files/org/anki/rossModernMandarinChinese2023.org") "\n* %<%Y%m%d%H%M%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: rossModernMandarinChinese2023\n:END:\n** %^{Heading}\n%^{Text}\n" :immediate-finish t :jump-to-captured t)
                              ("ax" "xslt" entry (file "/home/ilmari/my-files/org/anki/xslt.org") "\n* %<%Y%m%d%H%M%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: xslt\n:END:\n** %^{Heading}\n%^{Text}\n" :immediate-finish t :jump-to-captured t)
                              )) ;; org capture ends here

;; anki reset cloze hook after capture
(add-hook 'org-capture-after-finalize-hook 'my-reset-cloze-counter)

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
(setq org-latex-toc-command "\\tableofcontents \\addtocontents{toc}{\\protect\\thispagestyle{empty}} \\newpage")
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; (setq org-latex-pdf-process '(
;; "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; "bibtex %b"
;; "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; ))

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
(setq org-habit-graph-column 100)
(setq org-attach-auto-tag "attach")
(setq org-use-tag-inheritance nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; org archive
(setq org-archive-location "~/my-files/todo/TODO-archive.org::datetree/")
(setq org-archive-mark-done t)
(setq org-archive-subtree-save-file-p t)

;; org modules
(require 'org-habit)

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

;; org wc
(use-package org-wc
  :ensure t
  :config
  (setq org-wc-ignored-tags '("ARCHIVE" "archive" "noexport")))

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
  (setq org-journal-dir "~/my-files/org/journal/")
  (setq org-journal-date-format "%Y-%m-%d")
  (setq org-journal-file-format "%<%Y%m%dT%H%M%S>--journal.org")
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


;; multiple cursors
(use-package multiple-cursors
  :ensure t)

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

;; nxml mode hook
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)
(add-hook 'nxml-mode-hook 'electric-indent-mode)
(add-hook 'nxml-mode-hook 'wrap-region-mode)
(add-hook 'nxml-mode-hook 'abbrev-mode)
(add-hook 'nxml-mode-hook 'rainbow-mode)
(add-hook 'nxml-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nxml-mode-hook 'multiple-cursors-mode)


;; -------------------------------------------------- ;;
;; LaTeX                                              ;;
;; -------------------------------------------------- ;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'latex-mode-hook 'rainbow-mode)
(add-hook 'latex-mode-hook 'rainbow-delimiters-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'hl-line-mode)
(add-hook 'latex-mode-hook 'multiple-cursors-mode)


;; -------------------------------------------------- ;;
;; ROAM + REF                                         ;;
;; -------------------------------------------------- ;;

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-activate-cite-links t)
  (setq org-ref-cite-insert-version 2)
  (setq org-ref-show-broken-links nil)
  (setq bibtex-completion-bibliography '("~/my-files/zotero/bibliography.bib"))
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
          (call-process "open" nil 0 nil fpath)))
  (require 'org-ref)
  (define-key org-mode-map (kbd "C-= ]") 'org-ref-insert-link))

;; org roam
(use-package org-roam
  :ensure t
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/my-files/roam/"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template (concat "${type:15} | " (propertize "${tags:40}" 'face 'org-tag)" | ${title:*}"))
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))
          (not (member "attach" (org-get-tags)))
          (not (member "noexport" (org-get-tags)))
          (not (member "ignore" (org-get-tags)))
          (not (member "NOEXPORT" (org-get-tags)))))


  (setq org-roam-capture-templates '(
                                     ("b" "blog-draft" plain "%?" :target (file+head "blog-drafts/%<%Y-%m-%d>-blog-draft-${slug}.org" "#+title: ${title}\n#+filetags: %^{TAGS}\n#+DESCRIPTION: %^{short description}\n#+date: <%<%Y-%m-%d %H:%M>>\n* Introduction\n* par2\n* par3\n* par4\n* par5\n* par6\n* par7\n* Conclusion\n* Timestamp :ignore:\n =This blog post was last updated on {{{time(%b %e\\, %Y)}}}.=\n* References :ignore:\n#+BIBLIOGRAPHY: bibliography.bib plain option:-a option:-noabstract option:-heveaurl limit:t\n* Footnotes :ignore:\n* Text-dump :noexport:") :unnarrowed t)
                                     ("p" "permanent" plain "%?" :target (file+head "permanent/%<%Y-%m-%d>-permanent-${slug}.org" "#+title: ${title}\n#+filetags: %^{TAGS}\n\n - [ ] One subject, signified by the title.\n - [ ] Wording that is independent of any other topic.\n - [ ] Between 100-200 words.\n\n--\n + ") :unnarrowed t)
                                     ("r" "reference" plain "%?" :target (file+head "reference/%<%Y-%m-%d>-reference-${citekey}.org" "#+title: ${citekey} - ${title}\n#+filetags: %^{TAGS}\n\n--\n + ") :unnarrowed t)
                                     ("i" "index" plain "%?" :target (file+head "index/index-${slug}.org" "#+title: ${title}\n#+filetags: index\n") :unnarrowed t)
                                     ))

(setq org-roam-dailies-directory "~/my-files/roam/fleeting"
      org-roam-dailies-capture-templates '(("f" "fleeting-notes" entry "\n* %<%Y-%m-%d %H:%M> - %?" :target (file "fleeting-notes.org"))))


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


  (org-roam-db-autosync-mode)) ;; org roam ends here

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :config
  (setq orb-insert-follow-link t)
  (add-hook 'after-init-hook 'org-roam-bibtex-mode))

;; deft
(use-package deft
  :ensure t
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory "~/my-files/roam"))


(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n j") 'org-journal-new-entry)
(global-set-key (kbd "C-c n r") 'org-journal-search-forever)
(global-set-key (kbd "C-c n p") 'completion-at-point)
(global-set-key (kbd "C-c n s") 'deft)


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
; (global-set-key (kbd "C-= b") 'format-all-buffer)
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
(global-set-key (kbd "C-= n") 'denote-silo-extras-create-note)
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
(global-set-key (kbd "C-: k") 'key-chord-mode)


;; -------------------------------------------------- ;;
;; SERVER                                             ;;
;; -------------------------------------------------- ;;
(server-start)

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
   '("b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" default))
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" csharpier)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("Erlang" efmt)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HCL" hclfmt)
     ("HTML" html-tidy)
     ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format)
     ("Hy" emacs-hy)
     ("Java" clang-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" /usr/bin/latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Meson" muon-fmt)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python" black)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("Zig" zig)
     ("_Angular" prettier)
     ("_Beancount" bean-format)
     ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
 '(org-agenda-files '("~/my-files/todo/todo/home/TODO.org"))
 '(org-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("letter" "\\documentclass[11pt]{letter}")))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-checklist))
 '(package-selected-packages
   '(sclang-extensions anki-editor org-roam-bibtex org-roam org-ml ebib citar-denote citar dired-narrow marginalia org-cite denote lua-mode modus-themes free-keys magit multiple-cursors format-all wrap-region rainbow-delimiters rainbow-mode expand-region org-journal org-static-blog org-wc org-pomodoro org-ref org-fancy-priorities engine-mode deft elfeed-org elfeed key-chord writegood-mode wc-mode move-text palimpsest openwith orderless vertico golden-ratio backup-each-save org-contrib use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
