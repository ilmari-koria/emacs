;; helper-functions.el

(defun my-ispell-add-word ()
  ;; helper function for adding words to personal dict
  (interactive)
  (let ((word (thing-at-point 'word)))
    (ispell-send-string (concat "*" word "\n"))
    (ispell-send-string "#\n")
    (message "Word added to personal dictionary: %s" word)))

(defun my-sentence-counter ()
  ;; helper function for counting words
  (interactive)
  (forward-char)
  (backward-sentence)
  (set-mark-command nil)
  (forward-sentence)
  (message "There are *%s* words in this sentence."
	   (count-words-region
	    (region-beginning)
	    (region-end))))

(defun my-surround-region-with-actual-quotes ()
  ;; helper function for adding quotes
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "’")
    (goto-char start)
    (insert "‘")))

(defun my-org-jump-nearest-heading ()
  "move cursor to nearest org tree heading"
  (interactive)
  (org-back-to-heading)
  (beginning-of-line))

;; helper functions for anki org
(defvar cloze-counter 1)

(defun my-anki-cloze ()
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (content (buffer-substring start end)))
    (goto-char start)
    (delete-region start end)
    (insert (format "{{c%d::%s}}" cloze-counter content))
    (setq cloze-counter (+ cloze-counter 1))))

(defun my-reset-cloze-counter ()
  (interactive)
  (setq cloze-counter 1)
  (message "c-counter reset to 1"))

(defun my-org-capture-reset-counter ()
  (when (org-capture-get :reset-counter)
    (my-reset-cloze-counter)))

(defun my-set-cloze-counter (value)
  (interactive "Set cloze-counter to: ")
  (setq cloze-counter value)
  (message "cloze-counter set to %d" value))

(defun my-mark-and-run-my-anki-cloze ()
  (interactive)
  (let ((beg (car (bounds-of-thing-at-point 'word)))
        (end (cdr (bounds-of-thing-at-point 'word))))
    (if (and beg end)
        (progn
          (set-mark beg)
          (goto-char end)
          (activate-mark)
          (my-anki-cloze))
      (message "No word at point"))))

(defun my-start-pomodoro ()
  ;; helper function start org pomodoro
  (when (org-capture-get :pomodoro)
    (org-pomodoro)))
(add-hook 'org-capture-mode-hook #'my-start-pomodoro)

(defun my-org-capture-at-point ()
  ;; helper function capture at point
  (interactive)
  (org-capture 0))

(defun my-org-align-tags ()
  ;; helper function align tags
  (interactive)
  (org-align-tags 100))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my-org-align-tags nil 'make-it-local)))

(defun my-version-info ()
  ;; helper function version printer
  (message "GNU Emacs version: %s and Org-mode version: %s"
           emacs-version
           (org-version)))

(defun my-update-blog ()
  ;; helper function update blog
  (interactive)
  (shell-command "bash check-and-delete-files")
  (find-file "~/my-files/emacs/org/roam/blog/")
  (org-static-blog-publish)
  (my-update-blog-pdf)
  (shell-command "bash update-website"))

(defun my-update-blog-pdf ()
  "export all *.org files in wd to org LaTeX pdf if they have the line '#+my_export: pdf'."
  (interactive)
  (setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "bibtex %b"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (find-file "~/my-files/emacs/org/roam/blog/")
  (let ((org-files (directory-files default-directory t "\\.org$")))
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (when (and (string-match-p "\\.org$" file)
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "#\\+my_export: pdf" nil t)))
          (org-latex-export-to-pdf)))))
    (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "bibtex %b"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))) ;; my-update-blog-pdf ends here

(defun my-org-journal-find-location ()
  ;; helper function find journal location
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defun my-sort-bindings ()
  ;; sort bindings
  (interactive)
  (sort-regexp-fields nil "^.*$" "\(kbd \"[^\"]+\"\)"
                      (region-beginning)
                      (region-end)))

(defun my-publish-mediawiki ()
  "Convert all .org files in the current directory to MediaWiki format using Pandoc, then move them to a predefined folder."
  (interactive)
  (let ((output-dir "/home/ilmari/my-files/websites/phonography/pages/mediawiki/"))
    (dolist (file (directory-files "." t "\\.org$"))
      (let ((output-file (concat output-dir (file-name-base file) ".wiki")))
        (shell-command (concat "pandoc -f org -t mediawiki -o " 
                               (shell-quote-argument output-file) " " 
                               (shell-quote-argument file)))
        (message "Converted and moved: %s" output-file)))))


