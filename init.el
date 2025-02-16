;; vanilla emacs attempt (except for magit + modus themese)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq system-time-locale "C")
(setq scroll-bar-mode nil)
(setq warning-minimum-level :emergency)
(setq large-file-warning-threshold nil)
(setq word-wrap-by-category t)
(setq initial-buffer-choice "~/my-files/todo/TODO.org")
(setq next-line-add-newlines 1)
(setq electric-pair-preserve-balance nil)
(setq user-full-name "Ilmari Koria")
(setq user-mail-address "ilmarikoria@posteo.net")
(setq tramp-verbose 1)
(setq server-client-instructions nil)
(setq tramp-auto-save-directory "~/my-files/emacs/backups")
(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)
(setq kill-ring-max 9999)
(setq delete-by-moving-to-trash t)
(setq locale-coding-system 'utf-8)
(setq scroll-conservatively 100)
(setq scroll-margin 20)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq auto-save-interval 30)
(setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
(setq auth-sources '("~/.authinfo.gpg"))
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(set-default 'truncate-lines t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode t)
(tool-bar-mode -1)
(global-auto-revert-mode)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode t)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-include-diary nil)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-use-tag-inheritance nil)
(setq org-tags-match-list-sublevels t)
(setq org-habit-following-days 1)
(setq org-agenda-files '("~/my-files/todo/TODO.org"))
(setq org-enable-priority-commands t)
(setq org-priority-start-cycle-with-default t)
(setq org-highest-priority 1)
(setq org-default-priority 3)
(setq org-lowest-priority 6)
(setq org-timer-default-timer "25")
(setq org-export-with-smart-quotes t)
(setq org-export-preserve-breaks t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts t)
(setq org-latex-tables-centered nil)
(setq org-latex-images-centered nil)
(setq org-latex-toc-command "\\tableofcontents \\addtocontents{toc}{\\protect\\thispagestyle{empty}} \\newpage")
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-html-footnotes-section "<div id=\"footnotes\"><h2 class=\"footnotes\">%s </h2><div id=\"text-footnotes\">%s</div></div>")
(setq org-html-postamble-format '(("en" "<p class=\"postamble\" style=\"padding-top:5px;font-size:small;\">Author: %a (%e) | Last modified: %C.</p>")))
(setq org-html-head-include-default-style nil)
(setq org-html-postamble t)
(setq org-directory "~/my-files/todo/")
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
(setq alert-default-style 'libnotify)
(setq org-archive-location "~/my-files/todo/TODO-archive.org::datetree/")
(setq org-archive-mark-done t)
(setq org-archive-subtree-save-file-p t)
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
(setq org-src-strip-leading-and-trailing-blank-lines t)
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

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
		 (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))))))))


(setq org-capture-templates '(
("t" "todo")
("ts" "scheduled" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* TODO %?\nSCHEDULED: %^t DEADLINE: %^t\n")
("tw" "unscheduled-waiting" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* WAITING %?\n")
("tr" "reminder" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* REMINDER %^{Description} :REMINDER:\nSCHEDULED: <%<%Y-%m-%d %a>>\n" :immediate-finish t)
("tf" "fleeting" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* TODO %^{Description} :FLEETING:\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
("tq" "quick-clock-in" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
("tn" "quick-no-clock-in" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* TODO %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
("tt" "quick-tomorrow" entry (file+headline "~/my-files/todo/TODO.org" "TASK-INDEX") "* TODO %^{Description} %^g\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))" :immediate-finish t)
("tr" "repeat" entry (file+headline "~/my-files/todo/TODO.org" "REPEAT-TASKS") "* REPEAT %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:REPEAT_TO_STATE: REPEAT\n:END:")
("n" "note-at-point" plain (file "") " - (%^{location}) Here it says that %?.")
("b" "book" entry (file "~/my-files/website/org/reading-list.org")
"* TODO %^{Book Title}
:PROPERTIES:
:Img_url: %^{Image}
:Author: %^{Author}
:Pub_year: %^{Publication Year}
:ISBN: %^{ISBN}
:Publisher: %^{Publisher}
:Address: %^{Publisher Address}
:Date: %<%Y>
:END:")
))

(global-set-key (kbd "<f5>" ) 'org-agenda)
(global-set-key (kbd "<f6>" ) 'org-capture)


(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'backup-directory-alist
 	     (cons "." "~/my-files/emacs/backups/vanilla/"))


(require 'epa-file)
(epa-file-enable)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" default))
 '(package-selected-packages
   '(modus-themes s magit emacsql-sqlite3 emacsql-sqlite xquery-mode writegood-mode wrap-region wc-mode vertico use-package rainbow-mode rainbow-delimiters pdf-tools palimpsest org-wc org-roam org-ref org-pomodoro org-journal org-fancy-priorities org-contrib org-alert orderless openwith multiple-cursors move-text marginalia key-chord golden-ratio free-keys expand-region engine-mode elfeed-tube-mpv elfeed-org dired-narrow backup-each-save)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

