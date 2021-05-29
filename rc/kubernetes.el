(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :init
  (setq kubernetes-redraw-frequency nil)
  (setq kubernetes-poll-frequency nil)
  (setq kubernetes-default-overview-namespace "core")
  (setq kubernetes-commands-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer '(display-buffer-same-window))))
  (add-hook 'kubernetes-mode-hook (lambda () (local-set-key "M-w" 'kill-ring-save)))
  )
