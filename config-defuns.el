;;; config-defuns.el --- Functionality.

(require 'imenu)

;; Jump to a definition in the current buffer.
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
 Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))


;; Kill entire line with C-k and use C-S-backspace for killing from beginning
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))


;; Line movement --- http://www.emacswiki.org/emacs/MoveLine -- Joe Smith
;; (Paredit breaks line movement.  Use autopair.el)
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))


;; Line insertion
(defun insert-empty-line-below ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun insert-empty-line-above ()
  (interactive)
  (previous-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  )

(defun insert-empty-line-below-next-line ()
  (interactive)
  (next-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  )


;; Cleaning up indentation and whitespace.
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Tabify the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  "Properly indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun tidy-buffer ()
  "Indent, untabify, and clean up trailing whitespace from a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


;; Recent files + ido.
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Evaluation and compilation.
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun recompile-init ()
  "Byte-compile all emacs configuration modules."
  (interactive)
  (byte-recompile-directory config-dif 0)
  )

(defun my-revert-buffer()
  "revert buffer without asking for confirmation"
  (interactive "")
  (revert-buffer t t)
  )

(defvar my-latest-killed-buffer)
(defun my-kill-buffer()
  "
  Kill current buffer without confirmation.
  To undo latest kill call my-unkill-buffer
  "
  (interactive)
  (setq my-latest-killed-buffer (buffer-file-name) )
  (kill-buffer (buffer-name))
  )
(defun my-unkill-buffer()
  "Undo the latest buffer kill."
  (interactive)
  (find-file my-latest-killed-buffer )
  )


(provide 'config-defuns)
;;; config-defuns.el ends here
