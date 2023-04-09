;;; -*- lexical-binding: t -*-
(require 'json)
(require 'shr)

;;;###autoload
(defun hoon-assist-definition (term)
  "Display information about the Hoon term under point.

With a prefix arg, or if there is no \"thing\" at point, prompt
for the term to show information about."
  (interactive (list (hoon-assist--thing-at-point)))
  (let ((definition (hoon-assist--definition term)))
    (if (null definition)
        (message "Cannot assist with %s, sorry!" term)

      (or (fboundp 'libxml-parse-html-region)
          (error "This function requires Emacs to be compiled with libxml2"))

      (pop-to-buffer (generate-new-buffer "*hoon-assist*"))

      (hoon-assist-popup-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (shr-insert-document
         (with-temp-buffer
           (insert definition)
           (libxml-parse-html-region (point-min) (point-max)))))
      (goto-char (point-min)))))

(let ((dictionary nil))
  (defun hoon-assist--ensure-dictionary ()
    (unless dictionary
      (let* ((this-file (symbol-file 'hoon-assist--ensure-dictionary))
             (dictionary-file (concat (file-name-directory this-file) "hoon-dictionary.json"))
             (definitions (let ((json-object-type 'alist)
                                (json-array-type 'list))
                            (json-read-file dictionary-file)))
             (table (make-hash-table :test 'equal)))
        (dolist (definition definitions)
          (dolist (key (cdr (assq 'keys definition)))
            (puthash key (cdr (assq 'doc definition)) table)))

        (setq dictionary table)))
    dictionary))

(defun hoon-assist--definition (key)
  (gethash key (hoon-assist--ensure-dictionary)))

(define-derived-mode hoon-assist-popup-mode special-mode "Hoon Assist Popup"
  "Mode for Hoon Assist popup help buffers."
  (setq buffer-read-only t))

(defun hoon-assist--thing-at-point ()
  (let* ((at-point (thing-at-point 'symbol)))
    (if (or current-prefix-arg (null at-point))
        (completing-read "Term: "
                         (hash-table-keys (hoon-assist--ensure-dictionary))
                         nil t nil nil at-point)
      at-point)))

(provide 'hoon-assist)
