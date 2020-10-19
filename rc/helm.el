(use-package helm
  :ensure t
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  (helm-mode)
  )
(use-package helm-projectile :ensure t
  :init
  (helm-projectile-on))
(use-package helm-flycheck :ensure t
  )
