(module-load "/home/phil/git/emacs-fix-message-parser/emacs-fix-parser.so")

(defun parse-and-display-fix-message ()
  "Parse the FIX message in the current buffer and display results in a table."
  (interactive)
  (let ((fix-message (buffer-string))
        (output-buffer-name "*FIX Message Output*"))
    (with-current-buffer (get-buffer-create output-buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      (org-mode)  ; Enable org-mode to use org-table features.
      ;; Insert the table header.
      (insert "|-\n")
      (insert "| Tag | Name | Value |\n")
      ;; Insert the separator line for the header.
      (insert "|-")
      ;; Insert the parsed FIX message as table rows.
      (let ((parsed (parse-fix-message fix-message)))
        (dolist (item parsed)
          (let ((tag (car item))
                (tag-data (cdr item)))
            (insert (format "| %s | %s | %s |\n" tag (car tag-data) (cdr tag-data))))))
      (insert "|-\n")
      ;; Finalize the table to apply proper formatting.
      (org-table-align)
      ;; Apply header formatting.
      (goto-char (point-min))
      (let ((header-end (search-forward "|-" nil t)))
        (put-text-property (point-min) header-end 'face '(:background "yellow" :weight bold)))
      (read-only-mode 1))  ; Make the buffer read-only.
    (display-buffer output-buffer-name)))


(defun setup-fix-message-parsing-buffer ()
  "Set up a buffer for entering and parsing FIX messages."
  (interactive)
  (switch-to-buffer (get-buffer-create "*FIX Message Input*"))
  (text-mode)
  (visual-line-mode 1) ;; Enable line wrapping
  (use-local-map (copy-keymap text-mode-map))
  (local-set-key (kbd "C-c C-c") 'parse-and-display-fix-message)
  (message "Paste FIX message here and press C-c C-c to parse."))

;; Execute this function to start
;; (setup-fix-message-parsing-buffer)
