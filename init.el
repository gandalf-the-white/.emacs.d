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
  :ensure t
  :config (load-theme 'doom-one t))

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
