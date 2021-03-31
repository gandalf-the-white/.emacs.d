;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode)
  :config
  (defun my/terraform-format-and-save ()
    (interactive)
    (terraform-format-buffer)
    (save-buffer))
  (my/define-major-mode-key 'terraform-mode "s" #'my/terraform-format-and-save))

(provide 'init-terraform)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-terraform.el ends here
