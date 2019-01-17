(invert-face 'default)

(require 'tls)
(push "/usr/local/etc/libressl/cert.pem" gnutls-trustfiles)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(package-refresh-contents)
(server-start)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-message t)
(windmove-default-keybindings 'meta)
(cua-selection-mode t)
(setq scroll-conservatively 50)
(setq scroll-margin 4)
(show-paren-mode t)
(set-default-font "Monaco 13")
(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'mule-utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save point and switch
(global-set-key (kbd "\e\e/") 'save-point-and-switch)
(global-set-key (kbd "\e\e?") 'save-point-only)

(load-file "~/emacs/rc/sudo.el")
(load-file "~/emacs/rc/web.el")
(load-file "~/emacs/rc/yasnippet.el")
(load-file "~/emacs/rc/magit.el")
(load-file "~/emacs/rc/c++.el")
(load-file "~/emacs/rc/coffee.el")
(load-file "~/emacs/rc/rust.el")
(load-file "~/emacs/rc/elm.el")
(load-file "~/emacs/rc/python.el")

;; Multiple cursors
(use-package multiple-cursors :ensure t
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  )
(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "PATH")
    )
  )

(use-package yaml-mode :ensure t)
