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
    (insert (format "{{c%d::%s}}" cloze-counter (format "%s" content))) ; Ensures content is treated as a string
    (setq cloze-counter (+ cloze-counter 1))))

(defun my-anki-cloze-but-dont-increase-counter ()
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (content (buffer-substring start end)))
    (goto-char start)
    (delete-region start end)
    (insert (format "{{c%d::%s}}" cloze-counter (format "%s" content)))))

(defun my-reset-cloze-counter ()
  (interactive)
  (setq cloze-counter 1)
  (message "c-counter reset to 1"))

(defun my-org-capture-reset-counter ()
  (when (org-capture-get :reset-counter)
    (my-reset-cloze-counter)))

(defun my-set-cloze-counter (value)
  (interactive "nSet cloze-counter to: ") ; Changed to "n" to ensure numeric input
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

(defun my-mark-and-run-my-anki-cloze-but-dont-increase-counter ()
  (interactive)
  (let ((beg (car (bounds-of-thing-at-point 'word)))
        (end (cdr (bounds-of-thing-at-point 'word))))
    (if (and beg end)
        (progn
          (set-mark beg)
          (goto-char end)
          (activate-mark)
          (my-anki-cloze-but-dont-increase-counter))
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
  (shell-command "bash ~/my-files/bin/check-and-delete-files")
  (find-file "~/my-files/blog/posts")
  (org-static-blog-publish)
  (shell-command "bash ~/my-files/bin/update-website"))

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
