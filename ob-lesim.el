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
      (or error-message "No error."))))

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
	       (buffer-substring org-beg
				 (1- (match-beginning 0))))
	   (user-error "Cannot find code block %s" ref)))))))

(defun ob-lesim--expand-noweb ()
  "Expand noweb references in source edit buffers."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<<\\(.+?\\)>>" (point-max) t)
      (let* ((ref  (match-string 1))
	     (beg  (match-beginning 0))
	     (code (ob-lesim--expand-noweb-ref ref)))
	(replace-match code)
	(put-text-property beg (point) 'ob-lesim-noweb-ref ref)
	(put-text-property beg (point) 'ob-lesim-noweb-len (- (point) beg))))))

(defun ob-lesim--collapse-noweb ()
  "Replace expanded noweb reference with their #+name:."
  (save-excursion
    (goto-char (point-min))
    ;; noweb references are recognized by the 'ob-lesim-noweb
    ;; property, whose value is their #+name:.
    (while (let ((prop (text-property-search-forward 'ob-lesim-noweb-ref)))
	     (when prop
	       (let* ((beg (prop-match-beginning prop))
		      ;;		     (end (prop-match-end prop)))
		      (end (+ beg (get-text-property beg 'ob-lesim-noweb-len))))
		 (delete-region beg end)
		 (insert "<<" (prop-match-value prop) ">>")))))))

(defun ob-lesim-run ()
  "Save buffer to temporary file and run.
\\[lesim-run-key] in `org-mode' source edit buffers."
  (interactive)
  (ob-lesim--expand-noweb)
  (let ((ob-lesim-file (make-temp-file "lesim")))
    (write-region nil nil ob-lesim-file)
    (lesim-error (lesim-run ob-lesim-file))
    (delete-file ob-lesim-file))
  (ob-lesim--collapse-noweb))

(defun ob-lesim-forward-word ()
  "Expand noweb, move forward, then collapse noweb."
  (interactive)
  (ob-lesim--expand-noweb)
  (lesim-forward-word)
  (ob-lesim--collapse-noweb))

(defun ob-lesim-backward-word ()
  "Expand noweb, move backward, then collapse noweb."
  (interactive)
  (ob-lesim--expand-noweb)
  (lesim-backward-word)
  (ob-lesim--collapse-noweb))

(defun ob-lesim--unhook ()
  "Revert keymaps to their original state."
  (define-key lesim-mode-map [remap lesim-run-and-error] nil)
  (define-key lesim-mode-map [remap lesim-backward-word] nil)
  (define-key org-src-mode-map [remap org-edit-src-exit] nil)
  (define-key org-src-mode-map [remap org-edit-src-abort] nil))

(defun ob-lesim-edit-src-exit ()
  "Exit a `lesim-mode' edit buffer."
  (interactive)
  (ob-lesim--unhook)
  (org-edit-src-exit))

(defun ob-lesim-edit-src-abort ()
  "Abort editing a `lesim-mode' edit buffer."
  (interactive)
  (ob-lesim--unhook)
  (org-edit-src-abort))

;; This hook runs when entering an org-mode source edit buffer.  It
;; redefines key bindings to make org-src minor mode compatible with
;; lesim-mode, and expands noweb references
(defun ob-lesim-hook ()
  "Redefine keys in `lesim-mode' edit buffers."
  (define-key org-src-mode-map [remap org-src-exit] #'ob-lesim-edit-src-exit)
  (define-key org-src-mode-map [remap org-src-abort] #'ob-lesim-edit-src-abort)
  (define-key lesim-mode-map [remap lesim-run-and-error] #'ob-lesim-run)
  (define-key lesim-mode-map [remap lesim-backward-word] #'ob-lesim-backward-word)
  (setq-local indent-line-function #'ob-lesim-forward-word))

(add-hook 'org-src-mode-hook #'ob-lesim-hook)

(provide 'ob-lesim)
;;; ob-lesim.el ends here
