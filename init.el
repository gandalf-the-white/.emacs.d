(setq inhibit-startup-message t)

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "proxy.rd.francetelecom.fr:8080")
;;         ("https" . "proxy.rd.francetelecom.fr:8080")))

(menu-bar-mode -1)

(setq visible-bell t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			             ("org" . "https://orgmode.org/elpa/")
			             ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;##################################
;; LINE NUMBERS
;;##################################

(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()(display-line-numbers-mode 0))))

;;##################################
;; GRAPHIC
;;##################################

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;##################################
;; IVY
;;##################################

(use-package diminish)

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	     :map ivy-minibuffer-map
	     ("TAB" . ivy-alt-done)
	     ("C-l" . ivy-alt-done)
	     ("C-j" . ivy-next-line)
	     ("C-k" . ivy-previous-line)
	     :map ivy-switch-buffer-map
	     ("C-k" . ivy-previous-line)
	     ("C-l" . ivy-done)
	     ("C-d" . ivy-switch-buffer-kill)
	     :map ivy-reverse-i-search-map
	     ("C-k" . ivy-previous-line)
	     ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ;; Don't start searches with ^

;; Ctrl+h (f or v)
(use-package helpful
  :custom
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-describe-function-function #'helpful-callable)
  :config
  (defalias #'describe-command #'helpful-command)
  (defalias #'describe-function #'counsel-describe-function)
  (defalias #'describe-variable #'counsel-describe-variable)
  (defalias #'describe-key #'helpful-key))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)



;;##################################
;; YASNIPPET
;;##################################

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (add-to-list #'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode)
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
  :diminish yas-minor-mode)

;;##################################
;; Projectile
;;##################################

(use-package projectile
  :diminish pro
  jectile-mode
  :config (projectile-mode)
  :custom((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;;##################################
;; Magit
;;##################################

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

;;##################################
;; Org
;;##################################

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))


(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-agenda-files
        '("/Users/laurent/Documents/More/localsolver/Tasks.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)"))))

;;##################################
;; Lsp
;;##################################

(defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :hook ((c++-mode .lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  ;; :hook ((lsp-mode . efs/lsp-mode-setup)
  ;;        ((c++-mode cc-mode)  . lsp) 
  ;;        (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;##################################
;; Indent
;;##################################

(setq-default tab-width 4
	          indent-tabs-mode nil
	          c-basic-offset 4)


;;##################################
;; Ctrl+h or Ctrl+x
;;##################################

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))
