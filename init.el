
;; Configuration root directory path.
(setq config-dir (file-name-directory
		       (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path config-dir)
(progn (cd (concat config-dir "/themes"))
       (normal-top-level-add-subdirs-to-load-path))
(progn (cd (concat config-dir "/extensions"))
       (normal-top-level-add-subdirs-to-load-path))
(progn (cd config-dir)
       (normal-top-level-add-subdirs-to-load-path))


;; Load required libraries.
(load-library "config-colors")

;; Now set preferences.
(load-library "config-preferences")


;; ***************************************************************************
;; Custom hooks for the window frame
;; ---------------------------------
;; Two new separate hooks for the creation of
;; 1. window-system
;; 2. tty (console) frames.
;;
;; This is used to run different code for console and window-system.
;; See colors.el for example.
;;

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions
          'run-after-make-frame-hooks)

(add-hook 'after-init-hook
          (lambda ()
            (run-after-make-frame-hooks (selected-frame))))


;; ***************************************************************************
;; Automatically recompile the emacs init file on buffer-save or exit
;; ------------------------------------------------------------------
;;

(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    ;; (message "%s compiled" user-init-file)
    ))

(defun my-emacs-lisp-mode-hook ()
  (when (equal buffer-file-name user-init-file)
    (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))

;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'kill-emacs-hook 'byte-compile-user-init-file t t)

;; ***************************************************************************
;; Default directory.
;;

(setq default-directory "~/")
