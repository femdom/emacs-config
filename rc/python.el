(use-package python-black :ensure t)


(use-package virtualenvwrapper :ensure t
  :init
  (setq venv-location "~/.virtualenvs")
  )

(use-package lsp-python-ms :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-headerline-breadcrumb-mode)
                          )))

(use-package py-isort :ensure py-isort
  :init
  (setq py-isort-options '("--multi-line=3"))
  )

(use-package lsp-ivy :ensure t :pin "melpa-stable")

(add-hook 'python-mode-hook
          (lambda()
            (hack-local-variables)
            (when (boundp 'project-venv-name)
              (venv-workon project-venv-name))
            (visual-line-mode t)
            (hs-minor-mode t)
            (adaptive-wrap-prefix-mode t)
            (local-set-key (kbd "\e\en") 'flycheck-next-error)
            (local-set-key (kbd "C-c C-f") 'py-isort-buffer)
            (local-set-key (kbd "C-c t") 'python-test-dwim)
            (yas-minor-mode)
            (flycheck-mode t)
            (flycheck-disable-checker 'python-pycompile)
            (setq outline-regexp "def\\|class ")
            (local-set-key (kbd "RET") 'newline-and-indent)
            (lsp)
            (flycheck-select-checker 'python-flake8)
            )
          )
