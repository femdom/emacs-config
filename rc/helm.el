(use-package helm
  :ensure t
  :init
  (setq helm-autoresize-mode t)


  )
(use-package helm-projectile :ensure t
  :init
  (helm-projectile-on))
(use-package helm-flycheck :ensure t
  )
