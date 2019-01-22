(use-package dash-functional :ensure t)
(use-package ov :ensure t)
(use-package frame-local :ensure t)

(add-to-list 'load-path "~/emacs/site-packages/")
(add-to-list 'load-path "~/.local/share/icons-in-terminal")
(add-to-list 'load-path "~/emacs/site-packages/sidebar.el")

(require 'sidebar)
(sidebar-open)
