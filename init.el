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
(add-to-list 'load-path "~/my-files/emacs/init/")
(load "./helper-functions.el")

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
;; BIBTEX                                             ;;
;; -------------------------------------------------- ;;

(setq bibtex-autokey-edit-before-use nil)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-year-title-separator "")
(setq bibtex-autokey-titleword-length 12)

;; emacs bibtex doesn't provide a convenient way to order entries in bibkey creation
(eval-after-load "bibtex"
  '(defun bibtex-generate-autokey ()
     (let* ((names (bibtex-autokey-get-names))
            (title (bibtex-autokey-get-title))
            (year (bibtex-autokey-get-year))
            (autokey (concat
                      bibtex-autokey-prefix-string
                      names
                      (unless (or (equal names "")
                                  (equal title ""))
                        "")
                      title
                      (unless (or (and (equal names "")
                                       (equal title ""))
                                  (equal year ""))
                        bibtex-autokey-year-title-separator)
                      year)))
       (if bibtex-autokey-before-presentation-function
           (funcall bibtex-autokey-before-presentation-function autokey)
         autokey))))

(add-hook 'bibtex-mode-hook 'format-all-mode)

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
  (key-chord-mode 1))


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
;; EBIB                                              ;;
;; -------------------------------------------------- ;;
(use-package ebib
  :ensure t)


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
(setq diary-file "~/my-files/nextcloud/home-agenda/diary-google")

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
		   (org-agenda-span 7)
		   (org-deadline-warning-days 0)
		   (org-agenda-block-separator nil)
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "\nNEXT SEVEN DAYS")))
	  (todo "*"
		((org-agenda-overriding-header "UNSCHEDULED TASKS")
		 (org-tags-match-list-sublevels t)
		 (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp))))))))) ;; org agenda custom commands ends here

;; org capture templates
(setq org-capture-templates '(
                              ("t" "todo")
                              ("ts" "scheduled" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO [#3] %?\nSCHEDULED: %^t DEADLINE: %^t\n")
                              ("tw" "unscheduled-waiting" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* [#3] WAITING %?\n")
                              ("tr" "reminder" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* REMINDER [#3] %^{Description} :REMINDER:\nSCHEDULED: <%<%Y-%m-%d %a>>\n" :immediate-finish t)
                              ("tf" "fleeting" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO [#3] %^{Description} :FLEETING:\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tq" "quick-clock-in" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :clock-in t :clock-keep t :immediate-finish t)
                              ("tn" "quick-no-clock-in" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a>>" :immediate-finish t)
                              ("tt" "quick-tomorrow" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "TASK-INDEX") "* TODO [#3] %^{Description} %^g\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))" :immediate-finish t)
                              ("tr" "repeat" entry (file+headline "~/my-files/nextcloud/home-agenda/agenda/agenda.org" "REPEAT-TASKS") "* REPEAT [#3] %^{Description} %^g\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:REPEAT_TO_STATE: REPEAT\n:END:")

                              ("n" "note-at-point" plain (file "") " - (%^{location}) Here it says that %?.")

                              ("k" "anki")
                              ("km" "rossModernMandarinChinese2023" entry (file "~/my-files/org/anki/rossModernMandarinChinese2023.org") "\n* %<%Y%m%d%H%M%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: rossModernMandarinChinese2023\n:END:\n** %^{Heading}\n%^{Text}\n" :immediate-finish t :jump-to-captured t)
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
(setq org-latex-tables-centered nil)
(setq org-latex-toc-command "\\tableofcontents \\addtocontents{toc}{\\protect\\thispagestyle{empty}} \\newpage")
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; org html
(setq org-html-footnotes-section "<div id=\"footnotes\"><h2 class=\"footnotes\">%s </h2><div id=\"text-footnotes\">%s</div></div>")
(setq org-html-postamble-format '(("en" "<p class=\"postamble\" style=\"padding-top:5px;font-size:small;\">Author: %a (%e) | Last modified: %C.</p>")))
(setq org-html-head-include-default-style nil)
(setq org-html-postamble t)

;; org extras/contrib
(use-package org-contrib
  :ensure t
  :config
  (require 'oc-csl)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  (require 'oc-bibtex)
  (require 'ox-extra)
  (require 'ox-latex)
  ;; (require 'ox-bibtex)
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
(setq org-archive-location "~/my-files/org/archive/org-archive.org::datetree/")
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

;; website
(use-package org-static-blog
  :ensure t
  :config
  (require 'org-static-blog)
  (setq org-static-blog-publish-title "Ilmari's Webpage")
  (setq org-static-blog-publish-url "https://ilmarikoria.xyz")
  (setq org-static-blog-archive-file "posts.html")
  (setq org-static-blog-publish-directory "~/my-files/blog/ilmarikoria/")
  (setq org-static-blog-posts-directory "~/my-files/blog/posts/")
  (setq org-static-blog-drafts-directory "~/my-files/blog/post-dummy/") ;; because org-static-blog-publish will publish drafts folder
  (setq org-static-blog-preview-date-first-p t)
  (setq org-static-blog-enable-tags t)
  (setq org-static-blog-preview-ellipsis "")
  (setq org-static-blog-use-preview t)
  (setq org-static-blog-preview-start "")
  (setq org-static-blog-preview-end "")
  (setq org-static-blog-index-front-matter "
                                            <h2>About</h2>
                                            <p>Hello! My name is Ilmari. This blog is simply for me to share my interests in software, sound and zen.</p>
                                            <p>Feel free to contact me via <a href=\"mailto:ilmarikoria@posteo.net\">ilmarikoria@posteo.net</a>.</p>
                                            <p>I also have accounts on <a href=\"https://uk.linkedin.com/in/ilmari-koria-3151a5291\">LinkedIn</a> and <a href=\"https://www.youtube.com/channel/UCIwGuCqBXzXGozj0YeAcOTA\">YouTube</a>, but I am only semi-active on these.</p>
                                            <h2>Recent posts</h2>")
  (setq org-static-blog-page-header "<meta name=\"author\" content=\"Ilmari Koria, ilmarikoria@posteo.net\">
                                     <meta name=\"referrer\" content=\"no-referrer\">
                                     <meta name=\"viewport\" content=\"initial-scale=1.0,maximum-scale=1.0,user-scalable=no\" />
                                     <link href=\"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
                                     <link rel=\"icon\" href=\"data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🏞</text></svg>\">")
  (setq org-static-blog-page-preamble
        "<h1>Ilmari's Webpage</h1>
             <ul>
                <li><a href=\"https://ilmarikoria.xyz\">Home</a></li>
                <li><a href=\"https://ilmarikoria.xyz/posts.html\">Posts</a></li>
                <li><a href=\"https://ilmarikoria.xyz/tags.html\">All Tags</a></li>
                <li><a href=\"https://ilmarikoria.xyz/rss.xml\">RSS</a></li>
                <li><a href=\"https://ilmarikoria.xyz/static/gallery/index.html\">Gallery</a></li>
                 <li><a href=\"https://ilmarikoria.xyz/ilmari-koria-resume.pdf\">Résumé</a></li>
                 <li><a href=\"https://freesound.org/people/ilmari_freesound/\">Freesound</a></li>
            </ul>")
  (setq org-static-blog-page-postamble
        (format "<p>This page was last modified on %s ❘ Created in GNU Emacs version %s and org-mode version %s (using <a href=\"https://github.com/bastibe/org-static-blog\">org-static-blog</a>)</p>
                 <p><a href=\"https://creativecommons.org/licenses/by-nc/4.0/\">License</a> ❘ <a href=\"#top\">Top</p>"
                (format-time-string "%b %e, %Y")
                emacs-version
                (org-version)))) ;; -- org static blog ends here

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

;; format all
(use-package format-all
  :ensure t)

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

;; format all
(use-package format-all
  :ensure t
  :commands format-all-mode
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter))

;; prog mode hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'wrap-region-mode)
(add-hook 'prog-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'multiple-cursors-mode)
(add-hook 'prog-mode-hook 'format-all-mode)

;; html mode hook
(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'html-mode-hook 'electric-indent-mode)
(add-hook 'html-mode-hook 'wrap-region-mode)
(add-hook 'html-mode-hook 'abbrev-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-delimiters-mode)
(add-hook 'html-mode-hook 'multiple-cursors-mode)
(add-hook 'html-mode-hook 'format-all-mode)

;; nxml mode hook
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)
(add-hook 'nxml-mode-hook 'electric-indent-mode)
(add-hook 'nxml-mode-hook 'wrap-region-mode)
(add-hook 'nxml-mode-hook 'abbrev-mode)
(add-hook 'nxml-mode-hook 'rainbow-mode)
(add-hook 'nxml-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nxml-mode-hook 'multiple-cursors-mode)
(add-hook 'nxml-mode-hook 'format-all-mode)


;; -------------------------------------------------- ;;
;; LaTeX                                              ;;
;; -------------------------------------------------- ;;

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

;; -------------------------------------------------- ;;
;; DENOTE                                             ;;
;; -------------------------------------------------- ;;

;; TODO read:
;; - https://github.com/pprevos/citar-denote
;; - https://lucidmanager.org/productivity/taking-notes-with-emacs-denote/
;; - https://lucidmanager.org/productivity/using-emacs-image-dired/ ;; for the image file formatting
;; - https://protesilaos.com/emacs/denote
;; - https://lucidmanager.org/productivity/emacs-bibtex-mode/
;; - https://github.com/emacs-citar/citar

(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/my-files/business/notes")
  (denote-rename-buffer-mode t)
  (require 'denote-silo-extras)
  (setq denote-silo-extras-directories '("~/my-files/business/notes" "~/my-files/phd/notes/" "~/my-files/blog/notes"))
  (require 'denote-journal-extras)
  :hook
  (dired-mode . denote-dired-mode))

;; -------------------------------------------------- ;;
;; CITAR                                              ;;
;; -------------------------------------------------- ;;

(use-package citar
  :ensure t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography "~/my-files/bibliography/20240403T212049--bibliography__bib_bibtex_cite.bib"))

(use-package citar-denote
  :ensure t
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t))

;; use citeproc for csl bib stylesheets
(use-package citeproc
  :ensure t)

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
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" default))
 '(org-agenda-files
   '("~/my-files/nextcloud/cbeta-agenda/20240327T214353--cbeta-agenda__agenda_org_todo.org" "~/my-files/nextcloud/work-agenda/task-index-work/misc-index.org" "~/my-files/nextcloud/home-agenda/agenda/agenda.org"))
 '(package-selected-packages
   '(pdf-tools ebib citar-denote citar dired-narrow marginalia org-cite denote lua-mode modus-themes free-keys magit multiple-cursors format-all wrap-region rainbow-delimiters rainbow-mode expand-region org-journal org-static-blog org-wc org-pomodoro org-ref org-fancy-priorities engine-mode deft elfeed-org elfeed key-chord writegood-mode wc-mode move-text palimpsest openwith orderless vertico golden-ratio backup-each-save org-contrib use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
