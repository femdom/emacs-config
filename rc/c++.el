(require 'cedet)
(require 'semantic)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

(use-package stickyfunc-enhance :ensure stickyfunc-enhance)
(require 'stickyfunc-enhance)
(setq semantic-stickyfunc-sticky-classes '(type))
(semantic-mode 1)

(use-package irony :ensure irony
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  )

(use-package flycheck-irony :ensure flycheck-irony
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

(use-package company :ensure company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  )

(use-package company-irony
  :ensure company-irony
  :init
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode)
  )
