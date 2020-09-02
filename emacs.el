
(load-theme 'tango-dark)
(require 'tls)
(push "/usr/local/etc/libressl/cert.pem" gnutls-trustfiles)

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(package-refresh-contents)

(unless (condition-case nil (require 'use-package) (error nil))
  (package-install 'use-package)
  )

(setq ring-bell-function 'ignore)
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

;;(set-default-font "Monaco:pixelsize=16:foundry=unknown:weight=normal:slant=normal:width=normal:scalable=true")
(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'mule-utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(load-file "~/emacs/rc/autosave.el")
(load-file "~/emacs/rc/projectile.el")
(load-file "~/emacs/rc/sudo.el")
(load-file "~/emacs/rc/web.el")
(load-file "~/emacs/rc/yasnippet.el")
(load-file "~/emacs/rc/magit.el")
(load-file "~/emacs/rc/c++.el")
(load-file "~/emacs/rc/coffee.el")
(load-file "~/emacs/rc/rust.el")
(load-file "~/emacs/rc/elm.el")
(load-file "~/emacs/rc/nose.el")
(load-file "~/emacs/rc/python.el")
(load-file "~/emacs/rc/go.el")
(load-file "~/emacs/rc/company.el")
(load-file "~/emacs/rc/helm.el")

;; Save point and switch
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "ESC M-f") 'ffap)

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

(defun risky-local-variable-p (sym &optional _ignored) nil)
(add-to-list 'safe-local-variable-values
             '(nose-test-command . "./bin/test.sh"))

(use-package adaptive-wrap :ensure t)
(setq adaptive-wrap-extra-indent 8)

;; Saving points

;;Toggle between saved positions as in MIM editor
(defun save-point-and-switch ()
  "Save current point to register 0 and go to the previously saved position"
  (interactive)
  (let (temp)
    (setq temp (point-marker))
    (when (not (equal (get-register 'P) nil))
      (jump-to-register 'P))
    (set-register 'P temp)))

;;Save current position to register 0
(defun save-point-only ()
  "Save current point to register 0"
  (interactive)
  (set-register 'P (point-marker)))

(defun save-point-and-beginning-of-buffer ()
  "Save current point and go to the beginning of the buffer"
  (interactive)
  (let (temp)
    (setq temp (point-marker))
    (when (not (equal (get-register 'P) nil))
      (beginning-of-buffer))
    (set-register 'P temp)))

(defun save-point-and-end-of-buffer ()
  "Save current point and go to the end of the buffer"
  (interactive)
  (let (temp)
    (setq temp (point-marker))
    (when (not (equal (get-register 'P) nil))
      (end-of-buffer))
    (set-register 'P temp)))

(use-package feature-mode :ensure t)

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-h C-s") 'hs-toggle-hiding)
(global-set-key (kbd "C-h s") 'helm-semantic)
(global-set-key (kbd "ESC M-SPC") 'helm-mark-ring)

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(defun some-buffer (buffer)
  (with-current-buffer buffer
    (buffer-string)))


(defun eshell-git-prompt-robbyrussell ()
  "Eshell Git prompt with oh-my-zsh's robbyrussell theme.
It looks like:
➜ eshell-git-prompt git:(master) ✗ git status "
  ;; Prompt components
  (let (beg dir git-branch git-dirty venv-name tramp-str end)
    ;; Beg: start symbol
    (setq beg
          (with-face "➜" (if (eshell-git-prompt-exit-success-p)
                             'eshell-git-prompt-exit-success-face 'eshell-git-prompt-exit-fail-face)))

    ;; Dir: current working directory
    (setq dir (with-face (concat (eshell-git-prompt--shorten-directory-name) "/")
                         'eshell-git-prompt-directory-face))

    ;; Git: branch/detached head, dirty status
    (when (eshell-git-prompt--git-root-dir)
      (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
      (setq git-branch
            (concat
             (with-face "(" 'eshell-git-prompt-robyrussell-git-face)
             (with-face (eshell-git-prompt--readable-branch-name) 'eshell-git-prompt-robyrussell-branch-face)
             (with-face ")" 'eshell-git-prompt-robyrussell-git-face)))

      (setq git-dirty
            (when (eshell-git-prompt--collect-status)
              (with-face "✗" 'eshell-git-prompt-robyrussell-git-dirty-face))))

    (when (boundp 'venv-current-name)
      (setq venv-name
            venv-current-name
            )
      )

    (when (boundp 'tramp-current-host)
      (setq tramp-str
            (concat tramp-current-user "@" tramp-current-host)
            )
      )

    ;; End: To make it possible to let `eshell-prompt-regexp' to match the full prompt
    (setq end (propertize "$" 'invisible t))

    ;; Build prompt
    (concat (mapconcat #'identity (-non-nil (list beg venv-name git-branch tramp-str dir )) " ")
            end
            " ")))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun wsl-kill-ring-save (start end &optional region)
  (interactive "r")
  (with-suppressed-message
    (shell-command-on-region start end "clip.exe")
    (kill-ring-save start end region)))

(global-set-key
 (kbd "M-w")
 'wsl-kill-ring-save)

(defun wsl-paste ()
  (interactive)
  (let ((wslbuffername "wsl-temp-buffer"))
    (get-buffer-create wslbuffername)
    (with-current-buffer wslbuffername
      (insert (let ((coding-system-for-read 'dos))
                ;; TODO: put stderr somewhere else
                (shell-command "powershell.exe -command 'Get-Clipboard' 2> /dev/null" wslbuffername nil))))
    (insert-buffer wslbuffername)
    (kill-buffer wslbuffername)))
