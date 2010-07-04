;;******************************************************************************
;; Anything
(require 'anything)
(require 'anything-config)
(require 'anything-extension)

;;******************************************************************************
;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-anything)
(require 'ac-dabbrev)


;; Configuration
(global-auto-complete-mode t)
(setq ac-auto-start 1)
(setq ac-dwim t)
(setq ac-ignore-case t)

;; Keybindings
(define-key ac-completing-map "\M-n" 'ac-next)
(define-key ac-completing-map "\M-p" 'ac-previous)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-anything)

;; The common sources for all modes.
(custom-set-variables
 '(ac-sources
   '(ac-source-abbrev
     ac-source-dabbrev
     ac-source-words-in-buffer
     ac-source-words-in-same-mode-buffers
     ac-source-files-in-current-dir
     ac-source-filename)))

;; Autocomplete will be enabled for all these modes.
(setq-default ac-modes
              '(ada-mode
                asm-mode
                c++-mode
                c-mode
                cc-mode
                cperl-mode
                css-mode
                css-mode
                ecmascript-mode
                emacs-lisp-mode
                emms-tag-editor-mode
                eshell-mode
                f90-mode
                fortran-mode
                haskell-mode
                html-mode
                java-mode
                javascript-mode
                js-mode
                lisp-interaction-mode
                lisp-mode
                literate-haskell-mode
                makefile-mode
                org-mode
                perl-mode
                php-mode
                python-mode
                ruby-mode
                sass-mode
                scheme-mode
                sgml-mode
                sh-mode
                xml-mode
                yaml-mode
                scala-mode))
;; (add-to-list 'ac-trigger-commands 'org-self-insert-command) ; if you want enable auto-complete at org-mode, uncomment this line


;; Mode hooks
(add-hook 'eshell-mode-hook
          (lambda () (add-to-list 'ac-sources
                                  '(ac-source-dabbrev
                                    ac-source-files-in-current-dir
                                    ac-source-words-in-buffer))))
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'ac-sources
                                  '(ac-source-symbols
                                    ac-source-filename
                                    ac-source-yasnippet
                                    ac-source-files-in-current-dir))))
(add-hook 'html-mode
          (lambda () (add-to-list 'ac-sources
                                  '(ac-source-symbols
                                    ac-source-filename
                                    ac-source-files-in-current-dir
                                    ac-source-words-in-buffer
                                    ac-source-abbrev
                                    ac-source-css-keywords))))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-to-list 'ac-sources
                                  'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook
          (lambda () (add-to-list 'ac-sources
                                  'ac-source-filename)))
(add-hook 'css-mode-hook
          (lambda() (add-to-list 'ac-sources
                                 '(ac-source-symbols
                                   ac-source-files-in-current-dir
                                   ac-source-filename
                                   ac-source-css-keywords))))
