;; Multiple cursors
(use-package multiple-cursors
             :ensure t
             :init
             (global-set-key (kbd "M-n") 'mc/mark-next-like-this)
             (global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
             (global-set-key (kbd "C-c M-n") 'mc/mark-all-like-this))
