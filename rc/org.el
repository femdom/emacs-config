(use-package ein :ensure t)
(use-package ob-ipython :ensure t)
(use-package ob-async :ensure t)


(defvar org-babel-eval-verbose t
  "A non-nil value makes `org-babel-eval' display")

(setq org-babel-eval-verbose t)
(defun org-babel-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region
             (point-min) (point-max) cmd err-buff))
      (if (or (not (numberp exit-code)) (> exit-code 0)
              (and org-babel-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
          (progn
            (with-current-buffer err-buff
              (org-babel-eval-error-notify exit-code (buffer-string)))
            nil)
        (buffer-string)))))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package org
  :ensure t
  :init
  (require 'ox-md)
  (advice-add 'ob-ipython-auto-configure-kernels :around
              (lambda (orig-fun &rest args)
                "Configure the kernels when found jupyter."
                (when (executable-find ob-ipython-command)
                  (apply orig-fun args))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     (shell . t)
     (sql . t)
     ;; other languages..
     ))
  (require 'org-tempo)

  (setq org-default-notes-file "~/Dropbox/org/index.org")

  (defun org-ascii--box-string (s info)
    "Return string S with a partial box to its left.
INFO is a plist used as a communication channel."
    (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
      (format (if utf8p "  ┌────\n%s\n  └────" "  ,----\n%s\n  `----")
              (replace-regexp-in-string
               "^" (if utf8p "  │ " "  | ")
               ;; Remove last newline character.
               (replace-regexp-in-string "\n[ \t]*\\'" "" s)))))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refili
  (setq org-refile-use-outline-path 'file)

  (defun my-refile-targets ()
    (seq-filter
     (lambda (s)
       (and
        (not (string-prefix-p ".#" s))
        (string-suffix-p ".org" s)))
     (directory-files "~/Dropbox/org/")))

  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (my-refile-targets :maxlevel . 1)))
  )

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/Dropbox/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("NEGATIVE" . (:foreground "cadetblue"))
        ("POSITIVE" . (:foreground "darkseagreen"))))
