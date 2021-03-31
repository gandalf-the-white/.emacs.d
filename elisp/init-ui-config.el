;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; PreSym
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805)
          )))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)
;; -PreSym

;; TitleBar
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...\n")
;; -StartupScreen

;; DisLineNum
;; Hook line numbers to only when files are opened, also use linum-mode for emacs-version< 26
(if (version< emacs-version "26")
    (global-linum-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))
;; Display column numbers in modeline
(column-number-mode 1)
;; -DisLineNum

;; DisTimeBat
(display-time-mode 1)
(display-battery-mode 1)
;; -DisTimeBat

(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
