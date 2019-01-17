
(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  )
(use-package flycheck-rust :ensure t
  :init
  flycheck-rust-setup
  )
(use-package racer :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  )
