;;; config-clojure-mode.el --- Some configuration for clojure-mode.

(add-hook 'clojure-mode
          #'(lambda ()
              (push '(?[ . ?])
                    (getf autopair-extra-pairs :code))))


(provide 'config-clojure-mode)
;;; config-clojure-mode.el ends here
