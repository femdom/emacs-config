;;; mu4e.el ---
;;
;; Filename: mu4e.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Пн фев 22 10:31:16 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Пт фев 26 15:13:45 2021 (+0300)
;;           By: Renat Galimov
;;     Update #: 22
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;; make sure mu4e is in your load-path
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
;; (setq mu4e-sent-folder   "/sent")
;; (setq mu4e-drafts-folder "/drafts")
;; (setq mu4e-trash-folder  "/trash")



(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                   (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
                   ))
      ,(make-mu4e-context
        :name "Screenly"
        :match-func (lambda (msg) (when msg
                                    (string-prefix-p "/Screenly" (mu4e-message-field msg :maildir))))
        :vars '(
                (mu4e-trash-folder . "/Screenly/[Gmail].Trash")
                (mu4e-refile-folder . "/Screenly/[Gmail].Archive")
       ))))

(setq shr-color-visible-luminance-min 60)
(add-hook 'mu4e-view-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;; smtp mail setting; these are the same that `gnus' uses.
(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server         "smtp.gmail.com"
   smtpmail-local-domain        "gmail.com")


(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "renat2017@gmail.com"
 smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
 smtpmail-auth-credentials (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program "/usr/bin/gnutls-cli"
 starttls-extra-arguments nil
 starttls-use-gnutls t
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mu4e.el ends here
