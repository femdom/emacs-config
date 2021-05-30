(when (display-graphic-p)
  (load-theme 'tango-dark))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(fringe-mode '(1 . 1))
(delete-selection-mode t)
(require 'tls)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(push "/usr/local/etc/libressl/cert.pem" gnutls-trustfiles)
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(package-refresh-contents)

(unless (condition-case nil (require 'use-package) (error nil))
  (package-install 'use-package)
  )

(use-package request :ensure t :pin "melpa")
(use-package request-deferred :ensure t :pin "melpa")

(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(add-to-list 'load-path "~/emacs/site-packages")
(add-to-list 'load-path "~/emacs/site-packages/org-phabricator")

(setq ring-bell-function 'ignore)
(server-start)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-message t)
(setq scroll-conservatively 50)
(setq scroll-margin 4)
(setq compilation-scroll-output 'first-error)
(show-paren-mode t)
(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'mule-utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sentence-end-double-space nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq lsp-keymap-prefix "C-c C-l")

(load-file "~/emacs/rc/flycheck.el")
(load-file "~/emacs/rc/lsp.el")
(load-file "~/emacs/rc/docker.el")
(load-file "~/emacs/rc/autosave.el")
(load-file "~/emacs/rc/cucumber.el")
(load-file "~/emacs/rc/misc.el")
(load-file "~/emacs/rc/kubernetes.el")
(load-file "~/emacs/rc/magit.el")
(load-file "~/emacs/rc/helm.el")
(load-file "~/emacs/rc/multiple-cursors.el")
(load-file "~/emacs/rc/nose.el")
(load-file "~/emacs/rc/projectile.el")
(load-file "~/emacs/rc/python.el")
(load-file "~/emacs/rc/eshell.el")
(load-file "~/emacs/rc/c++.el")
(load-file "~/emacs/rc/yasnippet.el")
(load-file "~/emacs/rc/yaml.el")
(load-file "~/emacs/rc/org.el")
(load-file "~/emacs/site-packages/ob-plantuml.el")
(load-file "~/emacs/rc/ledger.el")
(load-file "~/emacs/rc/doom.el")
(load-file "~/emacs/rc/elisp.el")
(load-file "~/emacs/rc/mu4e.el")
(load-file "~/emacs/rc/rust.el")

(require 'helm)


;; (load-file "~/emacs/rc/sudo.el")
;; (load-file "~/emacs/rc/web.el")
;; (load-file "~/emacs/rc/coffee.el")
;; (load-file "~/emacs/rc/rust.el")
;; (load-file "~/emacs/rc/elm.el")
;;
;; (load-file "~/emacs/rc/go.el")
;; (load-file "~/emacs/rc/company.el")

(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "PATH")
    )
  )

(defun risky-local-variable-p (sym &optional _ignored) nil)
(add-to-list 'safe-local-variable-values
             '(nose-test-command . "./bin/test.sh"))

(defun eshell/some-buffer (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun gnome-screenshot-area ()
  (interactive)
  (start-process "gnome-screenshot" "*Gnome Screenshot*" "gnome-screenshot" "-a" "-c")
  )

(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(commandp 'gnome-screenshot-area)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmovppe-up)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-j") 'yafolding-toggle-all)
(global-set-key (kbd "C-c C-y") 'wsl-paste)
(global-set-key (kbd "C-c RET") 'yafolding-toggle-element)
(global-set-key (kbd "C-c s") 'helm-do-ag-project-root)
(global-set-key (kbd "C-c t") 'window-toggle-split-direction)
(global-set-key (kbd "C-h C-s") 'hs-toggle-hiding)
(global-set-key (kbd "C-h s") 'helm-semantic)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "ESC M-SPC") 'helm-mark-ring)
(global-set-key (kbd "ESC M-a") 'org-agenda)
(global-set-key (kbd "ESC M-f") 'ffap)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "\e\emk") 'kubernetes-overview)
(global-set-key (kbd "\e\ems") 'magit-status)
(global-set-key (kbd "C-c g c") 'org-clock-goto)
(global-set-key (kbd "C-<print>") #'gnome-screenshot-area)
(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x r b") #'ido-bookmark-jump)
(global-set-key (kbd "ESC M-h q") #'hlt-question)
(global-set-key (kbd "ESC M-h s") #'hlt-statement)
(global-set-key (kbd "ESC M-h u") #'hlt-unhighlight-region)
(global-set-key (kbd "ESC M-h h") #'hlt-general)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)

(projectile-mode)

(setq org-ph-fetch-api-url "https://ph.wireload.net/api")
(setq org-ph-fetch-user-phid "renat2017")

(use-package golden-ratio :ensure t
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale nil))
(use-package which-key :ensure t
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t)
  )


(require 'ffap)
(require 'projectile)
(require 'f)
;; View a



(defun r/dev-tests-display (path)
  "Displays a video file at PATH with a viewer."
  (interactive)
  (when (not (projectile-project-root)) (error "Not in a projectile project"))
  (let ((dev-tests-file (expand-file-name "docker/dev-tests" (projectile-project-root))))
    (if (not (file-exists-p dev-tests-file))
        (error "Composition file doesn't exist at %s" dev-tests-file))

    (let ((screencast-file (make-temp-file "screencast" nil ".mp4"))
          (stderr-file (make-temp-file "screencast-error" nil ".txt")))

      (condition-case ex
          (let ((exit-code (call-process dev-tests-file nil `((:file ,screencast-file) ,stderr-file) nil "exec" "-T" "tests" "cat" path)))
            (when (not (= exit-code 0))
              (error "Cannot download screencast file: %s" (f-read-text stderr-file)))
            (delete-file stderr-file)
            (start-process "dev-tests smplayer" nil "smplayer" screencast-file))
        (error (progn (delete-file screencast-file)
                      (delete-file stderr-file)
                      (error ex)))
        ))))

(call-process "bash" nil  nil "-c" "echo no >&2")

(defun r/view-screencast-at-point ()
  "Opens a media player with a screencast found at point."
  (interactive)

  (let ((url (ffap-string-at-point)))
    (cond ((string-blank-p url) (error "Nothing found at point"))
          ((not (file-name-absolute-p url)) (error "URL is not absolute path"))
          ((string-blank-p (projectile-project-root)) (error "Not in a projectile project")))
    (message "%s" (r/dev-tests-display url))))

(use-package highlight :ensure t
  :init

  (defface highlight-question
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Face for highlighting questions."
  :group 'basic-faces)

  (defface highlight-statement
  '((((class color) (min-colors 88) (background light))
     :background "#3c4c7a")
    (((class color) (min-colors 88) (background dark))
     :background "#3c4c7a")
    (((class color) (min-colors 16) (background light))
     :background "#3c4c7a")
    (((class color) (min-colors 16) (background dark))
     :background "#3c4c7a")
    (((class color) (min-colors 8))
     :background "blue" :foreground "black")
    (t :inverse-video t))
  "Face for highlighting statements."
  :group 'basic-faces)

  (defface highlight-general
    '((((class color) (min-colors 88) (background light))
       :background "#614b61")
      (((class color) (min-colors 88) (background dark))
       :background "#614b61")
      (((class color) (min-colors 16) (background light))
       :background "#614b61")
      (((class color) (min-colors 16) (background dark))
       :background "#614b61")
      (((class color) (min-colors 8))
       :background "red" :foreground "black")
      (t :inverse-video t))
    "Face for highlighting statements."
    :group 'basic-faces)


  (defun hlt-question()
    (interactive)
    (hlt-highlight-region (region-beginning) (region-end) 'highlight-question)
    )
  (defun hlt-statement()
    (interactive)
    (hlt-highlight-region (region-beginning) (region-end) 'highlight-statement)
    )
  (defun hlt-general()
    (interactive)
    (hlt-highlight-region (region-beginning) (region-end) 'highlight-general)
    )
  )
(setq mixed-pitch-set-height t)
