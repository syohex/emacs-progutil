;;; progutil-go.el --- My own Ruby utilities

;; Copyright (C) 2016 by Syohei YOSHIDA

;;; Code:

(require 'go-mode)
(require 'go-eldoc)

(defun progutil-go-type-at-cursor ()
  (interactive)
  (save-excursion
    (unless (looking-at-p "\\>")
      (forward-word 1))
    (let ((cand (go-eldoc--invoke-autocomplete)))
      (when (and cand (string-match "\\`\\([^,]+\\),,\\(.+\\)$" cand))
        (let ((name (match-string-no-properties 1 cand))
              (type (match-string-no-properties 2 cand)))
          (when (string-match "\\`var\\(.+\\)" type)
            (setq type (match-string-no-properties 1 type)))
          (message "%s:%s" (propertize name 'face 'font-lock-type-face) type))))))

(defun progutil-go-gofmt ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (gofmt)
  (when (buffer-modified-p)
    (save-buffer)))

;;;###autoload
(defun progutil-go-setup ()
  (define-key go-mode-map (kbd "C-c C-s") 'progutil-go-gofmt)
  (define-key go-mode-map (kbd "C-c ?") 'progutil-go-type-at-cursor))

;;; progutil-go.el ends here
