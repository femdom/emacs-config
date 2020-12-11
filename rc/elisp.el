(require 'header2)
(autoload 'auto-update-file-header "header2")
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (company-mode t)
   (auto-make-header)
   (flycheck-mode t)
   (add-hook 'write-file-hooks 'auto-update-file-header 0 t)
   ))
