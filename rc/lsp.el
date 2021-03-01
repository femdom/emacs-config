
(use-package lsp-ui :ensure t)
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  :bind
  ("C-c C-x s" . 'helm-lsp-workspace-symbol)
  )

(setq lsp-headerline-breadcrumb-segments '(project file symbols))
(setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
