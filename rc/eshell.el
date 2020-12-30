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


(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'shell-mode-hook 'with-editor-export-editor)
(add-hook 'shell-mode-hook (lambda() (local-set-key (kbd "C-r") 'counsel-shell-history)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*shell*")
               display-buffer-same-window
               (reusable-frames . visible)))
