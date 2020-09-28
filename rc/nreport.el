;; My implementation of the gunicorn notify performance

(defun r/get-notify-files (container-name)
  "Read gunicorn notify files from the appserver"
  (interactive)
  (let ((directory-path (format "/docker:%s:/tmp/gunicorn" container-name)))
    (mapcar (lambda (path) (expand-file-name path directory-path)) (directory-files directory-path nil "notify.*"))
    )
  )

(defun r/read-notify-file (path)
  "Read the notify file contents into a string"
  (interactive)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)
    )
  )

(defun r/parse-notify-file (path)
  "Parse gunicorn notify file into a list of items"
  (interactive)
  (mapcar 'string-to-number (split-string (r/read-notify-file path) "\n"))
  )

(defun r/only-greater-than (number items)
  (seq-filter (apply-partially '< number) items)
  )

(defun r/only-less-than (number items)
  (seq-filter (apply-partially '>= number) items)
  )

(defun println (object)
  (princ object)
  (princ "\n")
  )

(defun r/report-notify-files (container-name)
  "Report the metrics about the notify function performace"
  (interactive)
  (let*
      ((header
        (format "Notify statistics for %s" container-name))
       (items
        (mapcan 'r/parse-notify-file (r/get-notify-files "appserver")))
       (errorneous-items
        (sort (r/only-greater-than 3 items) '>))
       (good-items
        (r/only-less-than 3 items))
       )
    (with-output-to-temp-buffer (format "*%s*" header)
      (println header)
      (println (make-string (length header) ?=))
      (println "")

      (println (format "Total notify calls: %s" (length items)))
      (println (format "Total normal calls: %s" (length good-items)))
      (println (format "Total error calls: %s" (length errorneous-items)))

      (println "")
      (println "Error calls")
      (println (make-string (length "Error calls") ?=))
      (println (mapcar (apply-partially 'format "%.3s") errorneous-items))
      )
    ))

  (r/report-notify-files "appserver")
