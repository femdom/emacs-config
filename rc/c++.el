(use-package cmake-mode :ensure t)
(use-package qml-mode :ensure t)
;; (use-package clang-format :ensure t)
(use-package cmake-mode :ensure t)

(add-hook 'c-mode-hook (lambda ()
                           (lsp)
                           (yas-minor-mode)
                           (local-set-key (kbd "C-c C-f") 'clang-format-buffer)
                           ))

(add-hook 'c++-mode-hook (lambda ()
                           (lsp)
                           (yas-minor-mode)
                           (local-set-key (kbd "C-c C-f") 'clang-format-buffer)
                           ))

(use-package company :ensure company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  )
