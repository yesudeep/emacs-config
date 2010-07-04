;;; config-preferences.el --- Global preferences.

(require 'recentf)
(require 'saveplace)
(require 'uniquify)
(require 'autopair)
(require 'ido)
(require 'smex)


;; Set UTF-8 as default encoding for all buffers.
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-clipboard-coding-system 'utf-8)
;; MS Windows clipboard is UTF-16LE
(when (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le-dos)
    )

;; Make emacs use the clipboard.
(setq x-select-enable-clipboard t)

;; Enable autopair in all buffers.
(autopair-global-mode)
(setq autopair-blink nil
      autopair-autowrap t)

;; Remember recent files.
(recentf-mode 1)

;; Uniquify buffer names.
(setq uniquify-buffer-name-style 'forward)

;; Allows you to edit compressed files.  Compression/decompression is done on the fly.
(auto-compression-mode 1)

;; Save places in files between sessions.
(setq-default save-place t)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable the use of the mouse-wheel.
;; Breaks in the console.
;;(mouse-wheel-mode t)

;; Do not make any backup files.
(setq make-backup-files nil)

;; Disable indentation using tabs.
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)


;; Use 4 spaces for indentation.
(setq standard-indent 4)
(setq tab-width 4)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)


;; Automatically indent code when pasted.
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode
                                     lisp-mode
                                     clojure-mode
                                     scheme-mode
                                     haskell-mode
                                     ruby-mode
                                     rspec-mode
                                     python-mode
                                     c-mode
                                     c++-mode
                                     objc-mode
                                     latex-mode
                                     plain-tex-mode
                                     html-mode
                                     xml-mode
                                     yaml-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; Highlight the current line.
(when window-system
  (global-hl-line-mode 1)
  )

;; Show the line number in the minibuffer.
(line-number-mode 1)

;; Show the column number in the minibuffer.
(column-number-mode 1)

;; Fill column width is (right margin)
(setq-default fill-column 75)

;; Automatically fill (right margin) bleed.
;; Do not autofill.  This screws up HTML content.
;;(setq auto-fill-mode 1)

;; Inhibit startup message.
(setq inhibit-startup-message t
      initial-scratch-message nil
      require-final-newline 'visit-save
      )
;; Can have values: (t, 'visit-save, 'visit, nil)

;; Syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Make y or n suffice for a yes-or-no question.
(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; See the random-number generator.

;; Default to unified diffs.
(setq diff-switches "-u")

;; Turn on parentheses matching.
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Transient mark mode (show marks visually)
(transient-mark-mode t)

;; Highlight when doing a query replace.
(setq query-replace-highlight t)

;; Default major mode
(setq major-mode 'text-mode)

;; Font
;;(setq default-frame-alist '((font . "Monaco")))

;; Cua mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(setq cua-keep-region-after-copy t)
(setq cua-auto-tabify-rectangles nil)


;; ***************************************************************************
;; Key bindings
;;

;; Line movement.
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Automatically indent on return.
;; See http://www.emacswiki.org/emacs-en/AutoIndentation

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Cursor movement.

;;(global-set-key (kbd "<home>") 'beginning-of-line)
;;(global-set-key (kbd "<end") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-visual-line)
(global-set-key (kbd "<end>") 'end-of-visual-line)
(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)

;; Killing and yanking.
(define-key global-map (kbd "<delete>") 'delete-char)
(define-key global-map (kbd "M-<delete>") 'kill-word)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-S-<backspace>") 'kill-and-join-forward)

;; Line insertion
(global-set-key (kbd "C-S-<return>") 'insert-empty-line-above)
(global-set-key (kbd "S-<return>") 'insert-empty-line-below)
(global-set-key (kbd "s-<return>") 'insert-empty-line-below-next-line)


;;******************************************************************************
;; IDO (ordered last to avoid conflicts).
(ido-mode t)

;; (Get smex via ELPA)
;; Smex (ido completion for M-x commands)
(smex-initialize) ;; This should run *after* everything has loaded.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'config-preferences)
;;; config-preferences.el ends here
