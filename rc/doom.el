(use-package all-the-icons :ensure t)
(use-package doom-modeline :ensure t
  :init
  (doom-modeline-mode t)
  (setq doom-modeline-height 1)
  )
(use-package doom-themes :ensure t)
(load-theme 'doom-one t nil)
