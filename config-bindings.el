;;; config-bindings.el --- Key bindings.

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

;; CLean up whitespace
(global-set-key (kbd "C-c n") 'tidy-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'config-bindings)
;;; config-bindings.el ends here
