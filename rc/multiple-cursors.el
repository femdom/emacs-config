;; Multiple cursors
(use-package multiple-cursors
             :ensure t
             :init
             (global-set-key (kbd "C->") 'mc/mark-next-like-this)
             (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
             (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
