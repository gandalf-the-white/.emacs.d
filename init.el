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

;;##################################
;; IVY
;;##################################

(use-package diminish)

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
;;  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
