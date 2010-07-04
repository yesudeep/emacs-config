
;; ***************************************************************************
;; Python mode
;; See http://github.com/EnigmaCurry/emacs/
;; ryan-python.el

(require 'python)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
	    interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
		    (set-variable 'py-indent-offset 4)
		    (set-variable 'py-smart-indentation nil)
		    (set-variable 'indent-tabs-mode nil)
		    ;;(highlight-beyond-fill-column)
                    (define-key python-mode-map "\C-m" 'newline-and-indent)
                                        ;(pabbrev-mode)
                                        ;(abbrev-mode)
                    )
         )
      )

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))
(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))


;;Autofill comments
;;TODO: make this work for docstrings too.
;;      but docstrings just use font-lock-string-face unfortunately
(add-hook 'python-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

;; Paredit mode
;; (autoload 'paredit-mode "paredit"
;;  "Minor mode for pseudo-structurally editing Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-mode-hook (lambda ()
;;                            (paredit-mode +1)))
;;(add-hook 'list-interaction-mode-hook (lambda ()
;;                                        (paredit-mode +1)))


(provide 'config-modes)
;;; config-modes.el ends here.
