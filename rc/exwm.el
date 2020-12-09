(require 'exwm-systemtray)
(exwm-systemtray-enable)
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (shell-command "light -U 5; light")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda () (interactive) (shell-command "light -A 5; light")))
(exwm-input-set-key (kbd "s-l") (lambda () (interactive) (shell-command "xscreensaver-command -lock")))
(exwm-input-set-key (kbd "M-s-<left>") 'windmove-swap-states-left)
(exwm-input-set-key (kbd "M-s-<right>") 'windmove-swap-states-right)
(exwm-input-set-key (kbd "M-s-<up>") 'windmove-swap-states-up)
(exwm-input-set-key (kbd "M-s-<down>") 'windmove-swap-states-down)
(exwm-input-set-key (kbd "s-<left>") 'windmove-left)
(exwm-input-set-key (kbd "s-<right>") 'windmove-right)
(exwm-input-set-key (kbd "s-<up>") 'windmove-up)
(exwm-input-set-key (kbd "s-<down>") 'windmove-down)


(use-package helm-exwm
  :ensure t
  :config
  (setq exwm-layout-show-all-buffers t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))
(exwm-input--update-global-prefix-keys)
