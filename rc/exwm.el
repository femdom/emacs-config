;;; exwm.el ---
;;
;; Filename: exwm.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Вт дек 22 17:42:17 2020 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Пт июн  4 06:10:01 2021 (+0300)
;;           By: Renat Galimov
;;     Update #: 59
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


(require 'exwm-systemtray)
(exwm-systemtray-enable)
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(exwm-config-example)
(set-face-attribute 'default nil :height 120)
(setq exwm-randr-workspace-output-plist '(1 "HDMI-A-0" 2 "eDP"))
;; (start-process-shell-command "xrandr" nil "xrandr --dpi 192 --output eDP --scale 2x2 --primary --mode 1920x1080 --output HDMI-A-0 --above eDP --scale 0.8x0.8 --mode 3840x2160")

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (shell-command "light -U 5; light")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda () (interactive) (shell-command "light -A 5; light")))
(exwm-input-set-key (kbd "s-l") (lambda () (interactive) (shell-command "xscreensaver-command -lock")))
(exwm-input-set-key (kbd "M-s-<left>") 'windmove-swap-states-left)
(exwm-input-set-key (kbd "M-s-<right>") 'windmove-swap-states-right)
(exwm-input-set-key (kbd "M-s-<up>") 'windmove-swap-states-up)
(exwm-input-set-key (kbd "M-s-<down>") 'windmove-swap-states-down)
(exwm-input-set-key (kbd "s-<left>") 'windmove-left)
(exwm-input-set-key (kbd "s-<right>") 'windmove-right)
(exwm-input-set-key (kbd "s-<up>") 'windmove-up)
(exwm-input-set-key (kbd "s-<down>") 'windmove-down)
(exwm-input-set-key (kbd "s-<down>") 'windmove-down)
(exwm-input-set-key (kbd "C-<print>") #'gnome-screenshot-area)
(exwm-input-set-key (kbd "ESC M-a") 'org-agenda)


(setq exwm-floating-setup-hook (lambda() (exwm-floating-toggle-floating)))

(setq exwm-input-simulation-keys
	  '(([?\C-b] . [left])
	    ([?\C-f] . [right])
	    ([?\C-p] . [up])
	    ([?\C-n] . [down])
	    ([?\C-a] . [home])
	    ([?\C-e] . [end])
	    ([?\M-v] . [prior])
	    ([?\C-v] . [next])
	    ([?\C-d] . [delete])
	    ([?\C-k] . [S-end delete])
	    ([?\C-m] . [return])
        ([?\M-w] . [C-c])
        ([?\C-y] . [C-v])))

(use-package helm-exwm
  :ensure t
  :config
  (setq exwm-layout-show-all-buffers t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))
(exwm-input--update-global-prefix-keys)
(exwm-randr-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exwm.el ends here
