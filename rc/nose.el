;;; nose.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Jason Pellerin, Augie Fackler
;; Copyright (C) 2013-2015 Sylvain Benner

;; Created: 04 Apr 2009
;; Version: 0.3
;; Keywords: nose python testing

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running nosetests on a
;; particular buffer or part of a buffer.

;;; Installation

;; In your Emacs config:
;;
;; (require 'nose)

;; This version is compatible with Windows.
;; It does not call directly the nosetests script.  Instead it calls
;; python with an inline script to call nose.
;; It can launch test suites (require to install the nose fixes via
;; `easy_install nose-fixes`).
;; It is also compatible with virtualenv.

;; By default, the root of a project is found by looking for any of the files
;; '.projectile',  'setup.cfg', '.hg' and '.git'.  You can add files to check for
;; to the file list:
;;
;;   (add-to-list 'nose-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;;   (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;;
;;   (defvar nose-use-verbose nil) ; default is t

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'nosetests-all)
;;             (local-set-key "\C-cm" 'nosetests-module)
;;             (local-set-key "\C-c." 'nosetests-one)
;;             (local-set-key "\C-cpa" 'nosetests-pdb-all)
;;             (local-set-key "\C-cpm" 'nosetests-pdb-module)
;;             (local-set-key "\C-cp." 'nosetests-pdb-one)))

(require 'cl) ;; for "reduce"
(require 'subr-x)

(defvar nose-project-root-files '("requirements.txt"
                                  "manage.py"
                                  ".projectile"
                                  "setup.cfg"
                                  ".hg"
                                  ".git"
                                  ))
(defvar nose-use-verbose t)
(defvar nose-test-command nil)

(defun run-nose (&optional tests suite debug failed)
  "run nosetests by calling python instead of nosetests script.
To be able to debug on Windows platform python output must be not buffered.
For more details: http://pswinkels.blogspot.ca/2010/04/debugging-python-code-from-within-emacs.html
"
  (let* ((nose (nosetests-nose-command))
         (where (nose-find-project-root))
         (args (concat (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")
                       " "
                       (if suite "--test-suite-func=load_tests" "")))
         (tnames (if tests tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S") nose-project-root-files)))
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (compilation-start command
                                     nil
                                     (lambda (mode) (concat "*nosetests*")))))
             (format
              (concat "%s%s "
                      (if nose-use-verbose "-v " "")
                      "%s \"%s\"")
              where nose args tnames)))
  )

(defun nosetests-nose-command ()
  (interactive)
  (if (null nose-test-command)
      "bin/run_unit_tests.sh --no-setup"
    nose-test-command
    ))

(defun nosetests-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-nose nil nil debug failed))

(defun nosetests-failed (&optional debug)
  (interactive)
  (nosetests-all debug t))

(defun nosetests-pdb-all ()
  (interactive)
  (nosetests-all t))

(defun nosetests-module (&optional debug)
  "run nosetests (via eggs/bin/test) on current buffer"
  (interactive)
  (run-nose (nose-module-path) nil debug))

(defun nosetests-pdb-module ()
  (interactive)
  (nosetests-module t))

(defun nosetests-suite (&optional debug)
  "run nosetests (via eggs/bin/test) on current suite buffer"
  (interactive)
  (run-nose buffer-file-name t debug))

(defun nosetests-pdb-suite ()
  (interactive)
  (nosetests-suite t))


(defun nose-module-path ()
  "get python module path from current file name"
  (interactive)
  (let ((relative-module-path (string-remove-prefix (nose-find-project-root) buffer-file-name)))
    (replace-regexp-in-string "/" "." (string-remove-suffix ".py" relative-module-path))
    )
  )

(defun nosetests-one (&optional debug)
  "run nosetests (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-nose (format "%s:%s" (nose-module-path) (nose-py-testable)) nil debug))

(defun python-test--local-django-prefix()
  (locate-file "manage.py" ","))

(defvar python-test-case-delimiter ".")
(defvar python-test-prefix 'python-test--local-django-prefix)
(defvar python-test-filter "")

(defun python-test-dwim ()
    "Run test under the cursor."
    (interactive)
    (let* ((project-root (nose-find-project-root))
           (default-directory project-root))
    (compile (python-test-dwim-cmd) t)))

(defun python-test-dwim-cmd ()
  (let* ((project-root (nose-find-project-root))
         (default-directory project-root)
         (manage-path
          (if (functionp python-test-prefix)
              (funcall python-test-prefix)
            python-test-prefix))
         (test-target (nose-module-path))
         (test-case-target (nose-py-testable)))
    (when test-case-target
      (setq test-target (concat test-target python-test-case-delimiter test-case-target)))
    (concat manage-path " " test-target python-test-filter)))

(defun nosetests-pdb-one ()
  (interactive)
  (nosetests-one t))

(defun nose-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (when (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
      (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))

(defun outer-testable ()
  (save-excursion
    (when (re-search-backward
           "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
      (let ((result
             (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

        (cons
         (buffer-substring-no-properties (match-beginning 1) (match-end 1))
         result)))))


(defun nose-find-project-root (&optional dirname)
  (let ((dirname
         (expand-file-name
          (if dirname
              dirname
            (file-name-directory buffer-file-name)))))
    (cond ((has-nose-project-files dirname) dirname)
          ((equal dirname "/") nil)
          (t (nose-find-project-root
              (file-name-directory (directory-file-name dirname)))))))

(defun has-nose-project-files (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  nose-project-root-files)))

(provide 'nose)

;;; nose.el ends here
