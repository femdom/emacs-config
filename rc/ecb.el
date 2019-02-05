(require 'cl)

(use-package ecb :ensure t)

(require 'ecb)
;;(setq ecb-auto-activate 1)
;;(ecb-winman-winring-enable-support)
(setq ecb-examples-bufferinfo-buffer-name "*ECB - examples*")
(setq ecb-cedet-required-version-max '(2 1 0 0))
(setq ecb-compile-window-height 6)
(setq ecb-auto-expand-tag-tree-collapse-other (quote always))
(setq ecb-layout-name "left11")
(setq ecb-enlarged-compilation-window-max-height (quote best))
(setq ecb-compile-window-width (quote edit-window))
(setq ecb-tip-of-the-day nil)

(global-set-key (kbd "\e\el") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "\e\eea") 'ecb-activate)
(global-set-key (kbd "\e\eed") 'ecb-deactivate)
(ecb-restore-window-sizes)
(ecb-deactivate)
(defun display-buffer-at-bottom--display-buffer-at-bottom-around (orig-fun &rest args)
  "Bugfix for ECB: cannot use display-buffer-at-bottom', calldisplay-buffer-use-some-window' instead in ECB frame."
  (if (and ecb-minor-mode (equal (selected-frame) ecb-frame))
      (apply 'display-buffer-pop-up-window args)
    (apply orig-fun args)))

(advice-add 'display-buffer-at-bottom :around #'display-buffer-at-bottom--display-buffer-at-bottom-around)
;; emacs-rc-ecb.el ends here
