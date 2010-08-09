

(load (concat config-dir "lib/haskell-mode/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Pick any ONE of the following three indentation functionality.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(provide 'config-haskell-mode)
