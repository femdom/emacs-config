(require 'exwm-systemtray)
(exwm-systemtray-enable)
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (shell-command "light -U 5; light")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda () (interactive) (shell-command "light -A 5; light")))
(exwm-input-set-key (kbd "s-l") (lambda () (interactive) (shell-command "xscreensaver-command -lock")))

(use-package helm-exwm
  :ensure t
  :config
  (setq exwm-layout-show-all-buffers t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))
