;;; ob-lesim.el --- Org-babel functions for lesim-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano Ghirlanda <drghirlanda@gmail.com>
;; Package-Requires: ((emacs "28.1") (org "9.3") (lesim-mode "0.1"))
;; URL: https://github.com/drghirlanda/ob-lesim
;; Keywords: languages, tools
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

;; Add support for Learning Simulator scripts in org-mode.

;;; Requirements:

;; Learning Simulator, https://learningsimulator.org
;; lesim-mode, https://github.com/drghirlanda/lesim-mode

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'lesim-mode)

;; Let org-mode know about the .les file extension.
(add-to-list 'org-babel-tangle-lang-exts '("lesim" . "les"))

;; Set default header arguments.
(defvar org-babel-default-header-args:lesim
  '((:results . "value")
    (:noweb . "yes")
    (:exports . "none")
    (:eval "no-export"))
  "Default header arguments for lesim code blocks.")

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:lesim' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:lesim (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
Optional argument PROCESSED-PARAMS contains already processed parameters."
  (require 'inf-lesim nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s = %S"
                (car pair) (org-babel-lesim-var-to-lesim (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is just a pass-through, at least for now.
(defun org-babel-lesim-var-to-lesim (var)
  "Convert a VAR header variable to Learning Simulator syntax.
This is just a pass-through for now; no conversions are made."
  var)

;; Execute a lesim code block. Create a temporary script file from the
;; code block, then use lesim-mode functions to run the script and
;; report errors.
(defun org-babel-execute:lesim (body params)
  "Execute a block of Lesim code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY is the code.
Argument PARAMS is any parameters to be expanded."
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
    ;; We return a message because org-babel outputs our return value
    ;; to the minibuffer, but we also need to delete the temporary file.
    (let ((error-message (lesim-error (lesim-run ob-lesim-file))))
      (delete-file ob-lesim-file)
      (if error-message
	  error-message
	"No error."))))

(defun ob-lesim-run ()
  "Save buffer to temporary file and run.
\\[lesim-run-key] in `org-mode' source edit buffers."
  (interactive)
  (let ((ob-lesim-file (make-temp-file "lesim")))
    (write-region nil nil ob-lesim-file)
    (lesim-error (lesim-run ob-lesim-file))
    (delete-file ob-lesim-file)))

(defun ob-lesim--expand-noweb-ref (ref)
  "Expand REF by looking for a definition in the `org-mode' file."
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
  "Expand noweb references in lesim `org-mode' source edit buffers."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<<\\(.+?\\)>>" (point-max) t)
      (let* ((ref  (match-string-no-properties 1))
	     (beg  (match-beginning 0))
	     (code (ob-lesim--expand-noweb-ref ref)))
	(replace-match code)
	;; the expanded code is marked with 'ob-lesim-noweb and
	;; 'read-only properties so that it can be recognized exiting
	;; from the edit buffer.
	(put-text-property beg (point) 'ob-lesim-noweb ref)
	(put-text-property beg (point)
			   'read-only
			   (format "Edit block %s instead" ref))))))

(defun ob-lesim--collapse-noweb ()
  "Replace expanded noweb reference with their #+name:."
  (save-excursion
    (goto-char (point-min))
    ;; noweb references are recognized by the 'ob-lesim-noweb
    ;; property, whose value is their #+name:.
    (while (let ((prop (text-property-search-forward 'ob-lesim-noweb)))
	     (when prop
	       (let ((beg (prop-match-beginning prop))
		     (end (prop-match-end prop))
		     (inhibit-read-only t))
		 (delete-region beg end)
		 (insert "<<" (prop-match-value prop) ">>")))))))

(defun ob-lesim-edit-src-exit ()
  "Handle noweb references when calling `org-edit-src-exit'."
  (interactive)
  (ob-lesim--collapse-noweb)
  (org-edit-src-exit))

(defun ob-lesim-edit-src-save ()
  "Handle noweb references when calling `org-edit-src-save'."
  (interactive)
  (ob-lesim--collapse-noweb)
  (org-edit-src-save)
  (ob-lesim--expand-noweb))

;; Dear MELPA reviewer, to support noweb references and running
;; Learning Simulator scripts in code blocks and source edit buffers,
;; we need to modify the org-src keymap (and the lesim-mode keymap,
;; but I'm the author and I'm fine with that). This is not the best
;; and I try to be as invisible as possible. The original keymap is
;; restored ASAP, and the modified keymap is (hopefully) in effect
;; only in lesim source edit buffers. (I cannot use a local map before
;; it looks like the org-src-mode keymap is set *after* the org-src
;; hooks run.)

;; Stored keymaps as we found them.
(setq ob-lesim--org-src-mode-map nil)
(setq ob-lesim--lesim-mode-map nil)

(defun ob-lesim-restore-keymaps ()
  "Restore the original `org-src-mode' and `lesim-mode' keymaps."
  (interactive)
  (when ob-lesim--org-src-mode-map
    (setq org-src-mode-map ob-lesim--org-src-mode-map))
  (when ob-lesim--lesim-mode-map
    (setq org-src-mode-map ob-lesim--lesim-mode-map)))

;; This hook runs
(defun ob-lesim-hook ()
  "Expand noweb refs and redefine keys in `lesim-mode' edit buffers."
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
