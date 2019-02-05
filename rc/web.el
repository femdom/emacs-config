(use-package emmet-mode :ensure t
             :init
             (add-hook 'web-mode-hook 'emmet-mode)
             )

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (emmet-mode t)
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'"))
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2) ;; html indent
              (auto-complete-mode t)
              )
            ))

(use-package rainbow-mode :ensure t
             :init
             (add-hook 'css-mode-hook 'rainbow-mode)
             )

(use-package haml-mode :ensure t)
