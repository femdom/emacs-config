(use-package swiper :ensure t :pin "melpa-stable")
(use-package ivy :ensure t :pin "melpa-stable")
(use-package counsel :ensure t :pin "melpa-stable")
(use-package helm-ag :ensure t)
(use-package projectile :ensure t
  :init
  (projectile-mode)
  (setq projectile-project-search-path '("~/projects/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  )
