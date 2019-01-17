(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (global-set-key "\M-e" 'yas-expand)
  (global-set-key (kbd "\e\ey i") 'yas-insert-snippet)
  (setq yas-snippet-dirs '("~/emacs/snippets"))
  (yas-reload-all)
)
