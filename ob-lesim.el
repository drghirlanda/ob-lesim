;;; ob-lesim.el --- org-babel functions for lesim-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano ghirlanda
;; Keywords: literate programming, reproducible research
;; PackageRequires: ((emacs "28.1") ("lesim-mode" 0.1))
;; URL: https://github.com/drghirlanda/ob-lesim
;; Version: 0.1

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add support for Learning Simulator scripts in org-mode. Requires
;; lesim-mode. 

;;; Requirements:

;; Learning Simulator, https://learningsimulator.org
;; lesim-mode, https://github.com/drghirlanda/lesim-mode

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'lesim-mode)

;; define .les as the Learning Simulator file extension
(add-to-list 'org-babel-tangle-lang-exts '("lesim" . "les"))

;; default header arguments
(defvar org-babel-default-header-args:lesim
  '((:results . "value")
    (:noweb . "yes")
    (:exports . "none")
    (:eval "no-export")))

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:lesim' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:lesim (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-lesim nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s = %S"
                (car pair) (org-babel-lesim-var-to-lesim (cdr pair))))
      vars "\n")
     "\n" body "\n")))

(defun org-babel-execute:lesim (body params)
  "Execute a block of Lesim code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))
         ;; expand the body with `org-babel-expand-body:lesim'
         (full-body (org-babel-expand-body:lesim body
						 params
						 processed-params))
	 (ob-lesim-file (make-temp-file "lesim")))
    (with-temp-file ob-lesim-file
      (insert full-body))
    ;; we return a message because org-babel outputs our return value
    ;; to the minibuffer:
    (let ((error-message (lesim-error (lesim-run ob-lesim-file))))
      (delete-file ob-lesim-file)
      (if error-message
	  error-message
	"No error") 
      )))

(defun ob-lesim-run ()
  "Save buffer to temporary file and run."
  (interactive)
  (let ((ob-lesim-file (make-temp-file "lesim"))
	(code (buffer-substring (point-min) (point-max))))
    (write-region nil nil ob-lesim-file)
    (lesim-error (lesim-run ob-lesim-file))
    (delete-file ob-lesim-file)))

(defun ob-lesim--expand-noweb-ref (ref)
  ""
  (save-excursion
    (save-match-data
      (org-src-do-at-code-block
       (goto-char (point-min))
       (let ((reg  (concat "#\\+name:\\s-+"
			   ref
			   "\\s-*\n#\\+begin_src lesim\\s-*\n")))
	 (if (re-search-forward reg (point-max) t)
	     (let ((org-beg (match-end 0)))
	       (re-search-forward "#\\+end_src" (point-max))
	       ;; -no-properties needed to make read-only later:
	       (buffer-substring-no-properties org-beg
					       (1- (match-beginning 0))))
	   (user-error "Cannot find code block %s" ref)))))))

(defun ob-lesim--expand-noweb ()
  ""
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<<\\(.+?\\)>>" (point-max) t)
      (let* ((ref  (match-string-no-properties 1))
	     (beg  (match-beginning 0))
	     (code (ob-lesim--expand-noweb-ref ref)))
	(replace-match code)
	(put-text-property beg (point) 'ob-lesim-noweb ref)
	(put-text-property beg (point)
			   'read-only
			   (format "Edit block %s instead" ref))))))

(defun ob-lesim--collapse-noweb ()
  ""
  (save-excursion
    (goto-char (point-min))
    (while (let ((prop (text-property-search-forward 'ob-lesim-noweb)))
	     (when prop
	       (let ((beg (prop-match-beginning prop))
		     (end (prop-match-end prop))
		     (inhibit-read-only t))
		 (delete-region beg end)
		 (insert "<<" (prop-match-value prop) ">>")))))))

(defun ob-lesim-edit-src-exit ()
  ""
  (interactive)
  (ob-lesim--collapse-noweb)
  (org-edit-src-exit))

(defun ob-lesim-edit-src-save ()
  ""
  (interactive)
  (ob-lesim--collapse-noweb)
  (org-edit-src-save)
  (ob-lesim--expand-noweb))

(setq ob-lesim--org-src-mode-map nil)
(setq ob-lesim--lesim-mode-map nil)

(defun ob-lesim-restore-keymaps ()
  ""
  (interactive)
  (when ob-lesim--org-src-mode-map
    (setq org-src-mode-map ob-lesim--org-src-mode-map))
  (when ob-lesim--lesim-mode-map
    (setq org-src-mode-map ob-lesim--lesim-mode-map)))
  
(defun ob-lesim-hook ()
  "Expand noweb references and redefine keys."
  (cond
   ((equal major-mode 'lesim-mode)
    ;; save keymaps we redefine:
    (setq ob-lesim--org-src-mode-map org-src-mode-map)
    (setq ob-lesim--lesim-mode-map lesim-mode-map)
    ;; run scripts as if this buffer were a file:
    (define-key lesim-mode-map lesim-run-key #'ob-lesim-run)
    ;; handle noweb references:
    (define-key org-src-mode-map (kbd "C-c '") #'ob-lesim-edit-src-exit)
    (define-key org-src-mode-map (kbd "C-c C-s") #'ob-lesim-edit-src-save)
    (define-key org-src-mode-map (kbd "C-x C-s") #'ob-lesim-edit-src-save)
    ;; expand noweb references:
    (ob-lesim--expand-noweb))
   ;; restore saved keymaps:
   (t
    (ob-lesim-restore-keymaps))))

(add-hook 'org-src-mode-hook #'ob-lesim-hook)

(provide 'ob-lesim)
;;; ob-lesim.el ends here
