;;; config-colors.el --- Setup colors-themes.

(require 'color-theme)
(color-theme-initialize)

;; Do not unnecessarily load all the libraries.
;; Load the required library here and then enable it below.

(require 'color-theme-tango)
(require 'zenburn)
(require 'color-theme-subdued)
(require 'color-theme-less)

(load-library "color-theme-github")
(load-library "color-theme-twilight")
(load-library "color-theme-blackboard")
(load-library "color-theme-railscasts")
(load-library "color-theme-ir-black")


;; ***************************************************************************
;; Pick a color theme for the GUI and the console
;; ----------------------------------------------
;; Themes made for GUI don't always look good in the console.
;;

(set-variable 'color-theme-is-global nil)
(add-hook 'after-make-window-system-frame-hooks
          ;; Light
          ;;'color-theme-emacs-nw
          ;;'color-theme-github

          ;; Dark
          ;;'color-theme-tango
          ;;'color-theme-zenburn
          ;;'color-theme-subdued
          ;;'color-theme-less
          ;;'color-theme-blackboard
          ;;'color-theme-twilight
          ;;'color-theme-railscasts
          'color-theme-ir-black
          )
;;(add-hook 'after-make-console-frame-hooks
;;          ;;'color-theme-tty-dark
;;          'color-theme-emacs-nw
;;          )

(provide 'config-colors)
;;; config-colors.el ends here
