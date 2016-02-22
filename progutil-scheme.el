;;; progutil-scheme.el --- My own Scheme utilities

;; Copyright (C) 2016 by Syohei YOSHIDA

;;; Code:

(require 'scheme)
(require 'subr-x)
(require 'info)
(require 'helm)

(defvar progutil-scheme--info-files
  '((gauche . "gauche-refe")))

(defvar progutil-scheme--interpreter nil)
(defvar progutil-scheme--info-nodes nil)

(defun progutil-scheme--collect-info-nodes ()
  (save-window-excursion
    (info (assoc-default progutil-scheme--interpreter progutil-scheme--info-files))
    (let ((infobuf (current-buffer))
          Info-history
          nodes)
      (dolist (node (Info-index-nodes))
        (Info-goto-node node)
        (goto-char (point-min))
        (while (search-forward "\n* " nil t)
          (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
            (let* ((line (buffer-substring-no-properties
                          (+ (line-beginning-position) 2) (line-end-position))))
              (push line nodes)))))
      (nreverse nodes))))

(defun progutil-scheme--info-position (line)
  ;; This regexp is stolen from Info-apropos-matches
  (let ((info-file (assoc-default progutil-scheme--interpreter progutil-scheme--info-files)))
    (when (string-match
           "\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
           line)
      (cons (format "(%s)%s" info-file (match-string-no-properties 2 line))
            (string-to-number (or (match-string-no-properties 3 line) "1"))))))

(defun progutil-scheme--info-other-window (cand)
  (let ((info-pos (progutil-scheme--info-position cand)))
    (info-other-window (car info-pos))
    (goto-char (point-min))
    (forward-line (1- (cdr info-pos)))))

(defvar helm-progutil--gauche-source
  (helm-build-sync-source "Gauche Index"
    :candidates (lambda () progutil-scheme--info-nodes)
    :action #'progutil-scheme--info-other-window))

(defun helm-progutil-gauche-info ()
  (interactive)
  (let ((helm-execute-action-at-once-if-one t)
        (input (when-let ((input (thing-at-point 'symbol)))
                 (substring-no-properties input))))
    (helm :sources '(helm-progutil--gauche-source)
          :buffer "*gauche info*" :input input)))

(defun progutil-scheme--indent-setup ()
  (put 'and-let* 'scheme-indent-function 1)
  (put 'begin0 'scheme-indent-function 0)
  (put 'call-with-client-socket 'scheme-indent-function 1)
  (put 'call-with-input-conversion 'scheme-indent-function 1)
  (put 'call-with-input-file 'scheme-indent-function 1)
  (put 'call-with-input-process 'scheme-indent-function 1)
  (put 'call-with-input-string 'scheme-indent-function 1)
  (put 'call-with-iterator 'scheme-indent-function 1)
  (put 'call-with-output-conversion 'scheme-indent-function 1)
  (put 'call-with-output-file 'scheme-indent-function 1)
  (put 'call-with-output-string 'scheme-indent-function 0)
  (put 'call-with-temporary-file 'scheme-indent-function 1)
  (put 'call-with-values 'scheme-indent-function 1)
  (put 'dolist 'scheme-indent-function 1)
  (put 'dotimes 'scheme-indent-function 1)
  (put 'if-let1 'scheme-indent-function 2)
  (put 'if-match 'scheme-indent-function 2)
  (put 'let*-values 'scheme-indent-function 1)
  (put 'let-args 'scheme-indent-function 2)
  (put 'let-keywords* 'scheme-indent-function 2)
  (put 'let-match 'scheme-indent-function 2)
  (put 'let-optionals* 'scheme-indent-function 2)
  (put 'let-syntax 'scheme-indent-function 1)
  (put 'let-values 'scheme-indent-function 1)
  (put 'let/cc 'scheme-indent-function 1)
  (put 'let1 'scheme-indent-function 2)
  (put 'letrec-syntax 'scheme-indent-function 1)
  (put 'make 'scheme-indent-function 1)
  (put 'multiple-value-bind 'scheme-indent-function 2)
  (put 'match 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'parse-options 'scheme-indent-function 1)
  (put 'receive 'scheme-indent-function 2)
  (put 'rxmatch-case 'scheme-indent-function 1)
  (put 'rxmatch-cond 'scheme-indent-function 0)
  (put 'rxmatch-if 'scheme-indent-function 2)
  (put 'rxmatch-let 'scheme-indent-function 2)
  (put 'syntax-rules 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'until 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'while 'scheme-indent-function 1)
  (put 'with-builder 'scheme-indent-function 1)
  (put 'with-error-handler 'scheme-indent-function 0)
  (put 'with-error-to-port 'scheme-indent-function 1)
  (put 'with-input-conversion 'scheme-indent-function 1)
  (put 'with-input-from-port 'scheme-indent-function 1)
  (put 'with-input-from-process 'scheme-indent-function 1)
  (put 'with-input-from-string 'scheme-indent-function 1)
  (put 'with-iterator 'scheme-indent-function 1)
  (put 'with-module 'scheme-indent-function 1)
  (put 'with-output-conversion 'scheme-indent-function 1)
  (put 'with-output-to-port 'scheme-indent-function 1)
  (put 'with-output-to-process 'scheme-indent-function 1)
  (put 'with-output-to-string 'scheme-indent-function 1)
  (put 'with-port-locking 'scheme-indent-function 1)
  (put 'with-string-io 'scheme-indent-function 1)
  (put 'with-time-counter 'scheme-indent-function 1)
  (put 'with-signal-handlers 'scheme-indent-function 1)
  (put 'with-locking-mutex 'scheme-indent-function 1)
  (put 'guard 'scheme-indent-function 1))

;;;###autoload
(defun progutil-scheme-setup (prog)
  (let ((interpreters '((gauche . "gosh")
                        (guile . "guile"))))
    ;; interpreter
    (setq progutil-scheme--interpreter prog)
    (setq-default scheme-program-name (assoc-default prog interpreters))

    ;; documentation
    (setq progutil-scheme--info-nodes (progutil-scheme--collect-info-nodes))

    (progutil-scheme--indent-setup)

    (define-key scheme-mode-map (kbd "C-c C-d") #'helm-progutil-gauche-info)))

(provide 'progutil-scheme)

;;; progutil-scheme.el ends here
