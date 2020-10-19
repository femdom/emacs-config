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
     ;; other languages..
     ))

  (setq org-default-notes-file "/mnt/c/Users/renat/Dropbox/org/index.org")

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
     (directory-files "/mnt/c/Users/renat/Dropbox/org/")))

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
          "/mnc/c/Users/renat/Dropbox/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)
