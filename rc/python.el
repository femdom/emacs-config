;; (use-package virtualenvwrapper :ensure t
;;   :init
;;   (setq venv-location "/Users/renat.galimov/.virtualenvs")
;;   )

(use-package jedi :ensure t)
(use-package py-isort :ensure py-isort)
(add-hook 'python-mode-hook
          (lambda()
            (hack-local-variables)
            (when (boundp 'project-venv-name)
              (venv-workon project-venv-name))

            (local-set-key "\C-s" 'swiper)
            (local-set-key (kbd "\e\en") 'flycheck-next-error)
            (local-set-key (kbd "C-c C-f") 'py-isort-buffer)
            (local-set-key (kbd "C-c t") 'nosetests-one)
            (setq jedi:complete-on-dot t)
            (setq jedi:setup-keys t)
            (setq jedi:tooltip-method nil)

            (jedi:setup)
            (flycheck-mode t)
            (flycheck-select-checker 'python-pylint)
            (flycheck-popup-tip-mode t)
            (setq outline-regexp "def\\|class ")
            (local-set-key (kbd "RET") 'newline-and-indent)
            )
          )
