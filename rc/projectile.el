(use-package ivy :ensure t)
(use-package counsel :ensure t)
(use-package projectile :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/"))
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  )

(global-set-key (kbd "C-c s") 'counsel-ag)
