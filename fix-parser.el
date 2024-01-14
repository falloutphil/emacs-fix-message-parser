(module-load "/home/phil/git/emacs-fix-message-parser/emacs-fix-parser.so")

(defun parse-and-display-fix-message ()
  "Parse the FIX message in the current buffer and display results in a table."
  (interactive)
  (let ((fix-message (buffer-string))
        (output-buffer-name "*FIX Message Output*"))
    ;; Clear the output buffer
    (with-current-buffer (get-buffer-create output-buffer-name)
      (read-only-mode 0) ; Turn off readonly
      (erase-buffer)
      (org-mode)  ; Enable org-mode to use org-table features.
      ;; Insert the table header with formatting.
      (insert "| Tag | Name | Value |\n")
      (insert "|-----+------+-------|\n")  ; Separator for header.
      (let ((parsed (parse-fix-message fix-message)))
        (dolist (item parsed)
          (let ((tag (car item))
                (tag-data (cdr item)))
            (insert (format "| %s | %s | %s |\n" tag (car tag-data) (cdr tag-data))))))

      ;; Apply styling to the header.
      (goto-char (point-min))
      (while (re-search-forward "^|-" nil t) (org-table-next-row))
      (org-table-insert-hline t)
      (add-text-properties (point-min) (point-at-eol)
                           '(face (:background "yellow" :weight bold))))
    (read-only-mode 1) ; Make the output buffer read-only.

    ;; Display the output buffer
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
