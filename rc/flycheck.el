(use-package flycheck :ensure t :pin "melpa"
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'flycheck-error-list-mode-hook
            (lambda ()
              (setq tabulated-list-format
                    '[("File" 6)
                      ("Line" 5 flycheck-error-list-entry-< :right-align t)
                      ("Col" 3 nil :right-align t)
                      ("Level" 8 flycheck-error-list-entry-level-<)
                      ("ID" 12 t)
                      (#("Message (Checker)" 0 7
                         (face flycheck-error-list-error-message)
                         9 16
                         (face flycheck-error-list-checker-name))
                       0 t)]))))
