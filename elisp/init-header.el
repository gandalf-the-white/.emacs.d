;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Header2Pac
(use-package header2
  :load-path (lambda () (expand-file-name "site-elisp/header2" user-emacs-directory))
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :hook (emacs-lisp-mode . auto-make-header)
  :config
  (add-to-list 'write-file-functions 'auto-update-file-header)
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2"))
;; -Header2Pac

(provide 'init-header)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-header.el ends here
