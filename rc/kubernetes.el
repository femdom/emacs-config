(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :init
  (setq kubernetes-redraw-frequency nil)
  (setq kubernetes-poll-frequency nil)
  )
