(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(use-package company :ensure t
  :init
  (push 'company-capf company-backends)
  )
(use-package rust-mode
  :ensure t
  :init
  (defun my-rust-mode-hook ()
    (lsp t)
    (company-mode t)
    (flycheck-mode t)
    (lsp-ui-mode t)
    )
  (setq lsp-prefer-flymake nil)
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'my-rust-mode-hook)
  (setq rust-format-on-save nil)
  )
;; (use-package flycheck-rust :ensure t
;;   :init
;;   (flycheck-rust-setup)
;;   )
;; (use-package racer :ensure t
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)
;;   )
