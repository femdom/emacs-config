;; [[file:../../projects/org-basb-code/README.org::*Emacs config][Emacs config:1]]
;; [[[[file:~/projects/org-basb-code/README.org::install-straight-el][install-straight-el]]][install-straight-el]]
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; install-straight-el ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-org-pandoc-import][install-org-pandoc-import]]][install-org-pandoc-import]]
(use-package org-pandoc-import
  :straight (:host github
                   :repo "tecosaur/org-pandoc-import"
                   :files ("*.el" "filters" "preprocessors"))

  :bind (("C-c n o" . org-pandoc-import-as-org)))
;; install-org-pandoc-import ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-org-web-tools][install-org-web-tools]]][install-org-web-tools]]
(use-package org-web-tools :ensure t
  :bind (("C-c n u" . org-web-tools-read-url-as-org)))
;; install-org-web-tools ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-helm-rg][install-helm-rg]]][install-helm-rg]]
(use-package helm-rg :ensure t
  :init
  (defun helm-rg-roam-directory (&optional query)
    "Search with rg in your roam directory, QUERY."
    (interactive)
    (let ((helm-rg-default-directory org-roam-directory))
      (helm-rg query nil)))
  :bind (("C-c n R" . helm-rg-roam-directory)))
;; install-helm-rg ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-org-rifle][install-org-rifle]]][install-org-rifle]]
(use-package helm-org-rifle :ensure t
  :init
  (defun org-rifle-roam-directory ()
    (interactive)
    (helm-org-rifle-directories org-roam-directory))
  :bind (("C-c n s" . org-rifle-roam-directory)))
;; install-org-rifle ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-deft][install-deft]]][install-deft]]
(use-package deft :ensure t
  :init (setq deft-directory org-roam-directory
              deft-recursive t)
  :bind (("C-c n d" . deft))
  )
;; (use-package helm-deft
;;   :ensure t
;;   :straight (:host github
;;                    :repo "dfeich/helm-deft"
;;                    :files ("*.el"))
;;   :init
;;   (setq helm-deft-dir-list `(,org-roam-directory)
;;         helm-deft-extension '("org"))
;;   :bind (("C-c n d" . helm-deft)))

;; install-deft ends here
;; [[[[file:~/projects/org-basb-code/README.org::install-org-ql][install-org-ql]]][install-org-ql]]
(use-package org-ql :ensure t
  :init
  (setq org-ql-search-directories-files-recursive t))
(use-package helm-org-ql :ensure t
  :init
  (setq helm-org-ql-recursive-paths t)
  :bind (("C-c n q" . helm-org-ql-org-directory)))
;; install-org-ql ends here
;; Emacs config:1 ends here
