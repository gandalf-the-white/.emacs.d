(setq inhibit-startup-message t)

(setq url-proxy-services
     '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
         ("http" . "proxy.rd.francetelecom.fr:8080")
         ("https" . "proxy.rd.francetelecom.fr:8080")))

(menu-bar-mode -1)

(setq visible-bell t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;(load-theme 'tango-dark)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;##################################
;; GRAPHIC
;;##################################

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
