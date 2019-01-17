(use-package virtualenvwrapper :ensure t
  :init
  (setq venv-location "/Users/renat.galimov/.virtualenvs")
  )

(use-package jedi :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  )
