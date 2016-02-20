;;; progutil-ruby.el --- My own Ruby utilities

;; Copyright (C) 2016 by Syohei YOSHIDA

;;; Code:

(require 'ruby-mode)

(defun progutil-ruby-insert-bar ()
  (interactive)
  (if (looking-back "\\(?:do\\s-+\\|{\\)" (line-beginning-position))
      (progn
        (insert "||")
        (backward-char 1))
    (insert "|")))

(defun progutil-ruby-beginning-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-backward (concat "^\\s-+\\(" ruby-block-beg-re "\\)\\_>")
                           nil 'move arg)
       (progn (back-to-indentation) t)))

(defun progutil-ruby-end-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-forward (concat "^\\s-+\\(" ruby-block-end-re "\\)\\($\\|\\b[^_]\\)")
                          nil 'move arg)
       (progn (beginning-of-line) t))
  (forward-line 1)
  (back-to-indentation))

;;;###autoload
(defun progutil-ruby-setup ()
  (define-key ruby-mode-map (kbd "|") #'progutil-ruby-insert-bar)
  (define-key ruby-mode-map (kbd "C-M-a") #'progutil-ruby-beginning-of-defun)
  (define-key ruby-mode-map (kbd "C-M-e") #'progutil-ruby-end-of-defun))

(provide 'progutil-ruby)

;;; progutil-ruby.el ends here
