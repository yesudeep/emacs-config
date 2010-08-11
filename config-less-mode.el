

(setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))
(autoload 'css-mode "css-mode" "CSS Editing mode." t)

(provide 'config-less-mode)
