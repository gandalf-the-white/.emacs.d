;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; Zenburn themes
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))


;; ;; DoomThemes
;; (use-package doom-themes
;;   :custom-face
;;   (cursor ((t (:background "BlanchedAlmond"))))
;;   :config
;;   ;; flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   (load-theme 'doom-one t)
;;   (defun switch-theme ()
;;     "An interactive funtion to switch themes."
;;     (interactive)
;;     (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes))))
;;     (call-interactively #'load-theme)))
;; ;; -DoomThemes

;; DoomModeline
(use-package doom-modeline
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  ;; (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))
;; -DoomModeline

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here