;; Use =org-capture f= to put a link to the text you selected.into an
;; org entry with the current timer enabled.

(add-to-list
 'org-capture-templates
 '("f" "Curently watched" item (clock)
   "%(with-current-buffer (org-capture-get :original-buffer) (replace-regexp-in-string \"\n\" \" \" (buffer-substring (region-beginning) (region-end)))) [[%F::%(with-current-buffer (org-capture-get :original-buffer) (replace-regexp-in-string \"\n\" \" \" (buffer-substring (region-beginning) (region-end))))][↗]]%?" :unnarrowed t))

;; If you get errors saying somethign about facemenu, try
;; uncommenting this.
;; (setq facemenu-menu nil)

;; Intalls the package
(use-package highlight :ensure t)

(defface highlight-question
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Face for highlighting questions."
  :group 'basic-faces)

(defface highlight-statement
  '((((class color) (min-colors 88) (background light))
     :background "#3c4c7a")
    (((class color) (min-colors 88) (background dark))
     :background "#3c4c7a")
    (((class color) (min-colors 16) (background light))
     :background "#3c4c7a")
    (((class color) (min-colors 16) (background dark))
     :background "#3c4c7a")
    (((class color) (min-colors 8))
     :background "blue" :foreground "black")
    (t :inverse-video t))
  "Face for highlighting statements."
  :group 'basic-faces)

(defface highlight-general
  '((((class color) (min-colors 88) (background light))
     :background "#614b61")
    (((class color) (min-colors 88) (background dark))
     :background "#614b61")
    (((class color) (min-colors 16) (background light))
     :background "#614b61")
    (((class color) (min-colors 16) (background dark))
     :background "#614b61")
    (((class color) (min-colors 8))
     :background "red" :foreground "black")
    (t :inverse-video t))
  "Face for highlighting."
  :group 'basic-faces)

(defun hlt-question()
  (interactive)
  (hlt-highlight-region (region-beginning) (region-end) 'highlight-question))

(defun hlt-statement()
  (interactive)
  (hlt-highlight-region (region-beginning) (region-end) 'highlight-statement))

(defun hlt-general()
  (interactive)
  (message "Im in hlt-general"
  (hlt-highlight-region (region-beginning) (region-end) 'highlight-general)))

(global-set-key (kbd "ESC M-h q") #'hlt-question)
(global-set-key (kbd "ESC M-h s") #'hlt-statement)
(global-set-key (kbd "ESC M-h h") #'hlt-general)
(global-set-key (kbd "ESC M-h u") #'hlt-unhighlight-region)

(defun r/do-highlight-on-capture ()
  "Highlight selected region of the buffer you were in at capture."
  (save-excursion
    (with-current-buffer (plist-get org-capture-plist :original-buffer)
      (hlt-general))))

(defun r/highlight-on-capture ()
  (message "Running highlight on capture hook")
  (when (equal (plist-get org-capture-plist :key) "f")
    (r/do-highlight-on-capture)))

(add-hook 'org-capture-after-finalize-hook #'r/highlight-on-capture)
