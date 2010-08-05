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

;; Enable textmate mode when the system is Mac.
(when (eq system-type 'darwin)
  (require 'textmate)
  (textmate-mode)
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

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Delete trailing whitespace on save.
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

;; Associate modes with file extensions.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

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

;; Cosmetics for diff.
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

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

;; Don't use shift select mode.
(setq shift-select-mode nil)

;; Cua mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(setq cua-keep-region-after-copy t)
(setq cua-auto-tabify-rectangles nil)


;;******************************************************************************
;; IDO (ordered last to avoid conflicts).
(ido-mode t)

;; (Get smex via ELPA)
;; Smex (ido completion for M-x commands)
(smex-initialize) ;; This should run *after* everything has loaded.

(provide 'config-preferences)
;;; config-preferences.el ends here
