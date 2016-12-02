;;; progutil-go.el --- My own Ruby utilities

;; Copyright (C) 2016 by Syohei YOSHIDA

;;; Code:

(require 'go-mode)
(require 'go-eldoc)
(require 'subr-x)

(defun progutil-go-type-at-cursor ()
  (interactive)
  (save-excursion
    (unless (looking-at-p "\\>")
      (forward-word 1))
    (when-let ((cand (go-eldoc--invoke-autocomplete)))
      (when (string-match "\\`\\([^,]+\\),,\\(.+\\)$" cand)
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

(defun progutil-go-gom-gopath ()
  "This is for `go-set-project` of go-mode"
  (when-let ((vendor (or (locate-dominating-file buffer-file-name "_vendor")
                         (locate-dominating-file buffer-file-name "vendor"))))
    (when (file-directory-p (concat vendor "src"))
      (list vendor))))

(defun progutil-go-gogetdoc ()
  (interactive)
  (let ((file-arg (format "%s:#%d" (buffer-file-name) (point)))
        (buf (get-buffer-create " *gogetdoc*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer))
    (let ((proc (start-file-process "gogetdoc" buf "gogetdoc" "-pos" file-arg)))
      (set-process-sentinel
       proc
       (lambda (p status)
         (let ((status (process-status p)))
           (when (eq status 'exit)
             (unless (zerop (process-exit-status p))
               (error "Failed: %s" (buffer-string)))
             (save-selected-window
               (with-current-buffer (process-buffer p)
                 (read-only-mode +1)
                 (goto-char (point-min))
                 (pop-to-buffer (current-buffer)))))))))))

;;;###autoload
(defun progutil-go-setup ()
  (define-key go-mode-map (kbd "C-c C-s") 'progutil-go-gofmt)
  (define-key go-mode-map (kbd "C-c ?") 'progutil-go-type-at-cursor)

  (add-to-list 'go-guess-gopath-functions #'progutil-go-gom-gopath))

(provide 'progutil-go)

;;; progutil-go.el ends here
