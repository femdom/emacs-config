(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun yas-rust-convert-args ()
  "Convert arguments entered to the format string to arg: {} form"
  (if (> (length yas-text) 0)
      (mapconcat (lambda(value) (format "%s: {}"
                                        (replace-regexp-in-string "^self."
                                                                  ""
                                                                  value)))
                 (mapcar 'chomp (split-string yas-text ","))
                 ", ")
    ""))
