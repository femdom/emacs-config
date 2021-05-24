(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(use-package company :ensure t
  :init
  (push 'company-capf company-backends)
  )
(use-package rust-mode
  :ensure t
  :init
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-eldoc-render-all nil
        lsp-idle-delay 0.6
        lsp-prefer-flymake nil
        rust-format-on-save t
        lsp-rust-analyzer-server-display-inlay-hints t)

  (defun my-rust-mode-hook ()
    (lsp t)
    (company-mode t)
    (flycheck-mode t)
    (lsp-ui-mode t)
    (yas-minor-mode t)
    )

  (add-hook 'rust-mode-hook #'my-rust-mode-hook)
  )

(use-package flycheck-rust :ensure t
  :init
  (flycheck-rust-setup)
  )
;; (use-package racer :ensure t
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)
;;   )
