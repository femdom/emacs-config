(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(use-package adaptive-wrap :ensure t :init
  (setq adaptive-wrap-extra-indent 8))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun wsl-kill-ring-save (start end &optional region)
  (interactive "r")
  (with-suppressed-message
    (shell-command-on-region start end "clip.exe")
    (kill-ring-save start end region)))

(defun wsl-paste ()
  (interactive)
  (let ((wslbuffername "wsl-temp-buffer"))
    (get-buffer-create wslbuffername)
    (with-current-buffer wslbuffername
      (insert (let ((coding-system-for-read 'dos))
                ;; TODO: put stderr somewhere else
                (shell-command "powershell.exe -command 'Get-Clipboard' 2> /dev/null" wslbuffername nil))))
    (insert-buffer wslbuffername)
    (kill-buffer wslbuffername)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
