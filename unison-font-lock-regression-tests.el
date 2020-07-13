;;; unison-font-lock-regression-tests.el --- Test suite for unison font-lock.

;; Copyright © 2020, Dario Oddenino

;; Author: Dario Oddenino
;; Version: 0.2.0
;; Created: 24 Apr 2020
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A test suite for unison mode font-lock

;;; Code:

(require 'faceup)

(defvar font-lock-regression-suite-languages
  '(("unison" unison-mode))
  "List of directories and corresponding modes.
Each entry in the list has the following format:
    (DIR MODE-OR-MODES ...)
Where DIR is a directory in the source tree and MODE-OR-MODES is
a mode or a list of modes that should be used.")

(defvar font-lock-regression-suite-dir (faceup-this-file-directory))


(defvar font-lock-regression-suite-reference-version
  (if (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\.[0-9]+"
                    emacs-version)
      (concat (match-string 1 emacs-version) ".x")
    emacs-version)
  "The version of the revision files to use, defaults to Emacs version.
For released Emacs versions, the same as `emacs-versions', else simplified.
When building an Emacs from source, the fourth version number is
increased for every build. This normalized this by replacing the
fourth version number with an `x'.")


;; -------------------------------------------------------------------
;; Support functions.
;;

(defun font-lock-regression-suite-each-src-ref-file--internal
    (src-dir ref-dir ref-start path func &rest args)
  "Helper function for `font-lock-regression-suite-each-src-ref-file'.
Traverse SRC-DIR recursively, relative to PATH and call FUNC with
name, source file, reference files (rooted in REF-DIR), and
ARGS."
  (dolist (f (directory-files (if path
                                  (concat src-dir path)
                                src-dir)))
    (unless (string-match "^\\." f)     ; ".", "..", ".nosearch" etc.
      (let ((new-path (if path
                          (concat path "/" f)
                        f)))
        (if (file-directory-p (concat src-dir new-path))
            (apply 'font-lock-regression-suite-each-src-ref-file--internal
                   src-dir ref-dir ref-start new-path func args)
          (apply func
                 ;; Note: In theory, the same name could be generated
                 ;; twice. For example, if both "A-B" and "A/B"
                 ;; exists, they will both be mapped to "A-B". One way
                 ;; to work-around this would be to map "-" into "--".
                 (font-lock-regression-suite-dashify
                  (concat ref-start "/" new-path))
                 (concat src-dir new-path)
                 (concat ref-dir ref-start "/" new-path ".faceup")
                 args))))))


(defun font-lock-regression-suite-each-src-ref-file (func &rest args)
  "Call FUNC with each name, source file, reference file, mode, and ARGS.
The name is a unique identifier representing the file.  The
reference file may not exist.
Modes is a function to call or a list of function to call.  You
can use `font-lock-regression-suite-apply-modes' to enable the
modes.
`font-lock-regression-suite-dir' contains the root of the source
files and `font-lock-regression-suite-languages' contains a list
of subdirectories and corresponding modes.
When non-nil `font-lock-regression-suite-reference-version', the
reference files of that version of Emacs is used.  When nil, the
reference files of the current Emacs version is used.
Example:
    (font-lock-regression-suite-each-src-ref-file
     (lambda (name src-file ref-file modes)
       (message \"%s: %s %s in %s\" name src-file ref-file mode)))"
  (let ((seen-ids '()))
    (dolist (entry font-lock-regression-suite-languages)
      (dolist (mode-or-modes (cdr entry))
        (let* ((ref-dir (font-lock-regression-suite-reference-directory))
               (src-start (nth 0 entry))
               (ref-start-base (if (stringp (car-safe mode-or-modes))
                                   (pop mode-or-modes)
                                 (nth 0 entry)))
               (ref-start ref-start-base)
               (count 2))
          ;; Make the reference directory unique.
          (while (member ref-start seen-ids)
            (setq ref-start (format "%s-%d" ref-start-base count))
            (setq count (+ count 1)))
          (push ref-start seen-ids)
          (apply
           #'font-lock-regression-suite-each-src-ref-file--internal
           (concat font-lock-regression-suite-dir
                   "src/"
                   src-start "/")
           ref-dir
           ref-start
           nil
           func
           mode-or-modes
           args))))))


(defun font-lock-regression-suite-dashify (path)
  "Convert PATH to something suitable to be part of an elisp identifier."
  (setq path (file-name-sans-extension path))
  (while (string-match "/" path)
    (setq path (replace-match "-" nil nil path)))
  path)


(defun font-lock-regression-suite-reference-directory ()
  "The root of the reference directory, with a trailing slash."
  (concat font-lock-regression-suite-dir
          "ref/"
          font-lock-regression-suite-reference-version "/"))


(defun font-lock-regression-suite-apply-modes (modes)
  "Apply all modes in MODES.
Modes can be a function to call or a list of functions.
Return nil if any of the function isn't defined, non-nil otherwise."
  ;; Note: Both are needed to recognize lambda expressions and symbols
  ;; referring to undefined functions.
  (when (or (symbolp modes)
            (functionp modes))
    (setq modes (list modes)))
  (let ((res t))
    (while (and res
                modes)
      (let ((m (pop modes)))
        (if (functionp m)
            (funcall m)
          (setq res nil))))
    res))


;; ----------------------------------------------------------------------
;; Add ERT test cases.
;;
;; This generates one ERT test case for each source file. This allows
;; you to use the ERT selection mechanism to test a subset of the files.

(defun font-lock-regression-suite-add-testcases ()
  (interactive)
  (font-lock-regression-suite-each-src-ref-file
   (lambda (name
            src-file
            ref-file
            mode)
     (eval `(ert-deftest
                ,(intern (concat "font-lock-regression-suite--" name))
                ()
              (if (file-exists-p ,ref-file)
                  (should (faceup-test-font-lock-file
                           (quote ,mode)
                           ,src-file
                           ,ref-file))
                (error "The reference file `%s' does not exist."
                       ,ref-file)))))))


;; ----------------------------------------------------------------------
;; Regenerate
;;

(defun font-lock-regression-suite-regenerate (&optional force)
  "Regenerate all reference files.
When C-u prefix, or when FORCE is non-nil, only regenerate missing files."
  (interactive (list current-prefix-arg))
  (font-lock-regression-suite-each-src-ref-file
   (lambda (name
            src-file
            ref-file
            modes)
     (when (or force
               (not (file-exists-p ref-file)))
       (with-temp-buffer
         (insert-file-contents src-file)
         (when (font-lock-regression-suite-apply-modes modes)
           ;; Don't generate a reference file when the font-lock
           ;; keywords have triggered an error. (For example,
           ;; prolog-mode on Emacs 23.3 throws a "No match 3 in
           ;; highlight" error.)
           (when (condition-case nil
                     (progn
                       (font-lock-fontify-region (point-min) (point-max))
                       t)
                   (error nil))
             (make-directory (file-name-directory ref-file) t)
             (faceup-write-file ref-file))))))))


;; ----------------------------------------------------------------------
;; Testing
;;

(defun font-lock-regression-suite-list ()
  "Echo all source files in the regression suite."
  (interactive)
  (with-output-to-temp-buffer "*FontLockRegressionSuite*"
    (font-lock-regression-suite-each-src-ref-file
     (lambda (name src-file ref-file mode)
       (princ (format "%s:\n  %s\n  %s\n  %s\n" name src-file ref-file mode))))
    (display-buffer (current-buffer))))


;; ----------------------------------------------------------------------
;; The End
;;


;;; unison-font-lock-regression-tests.el ends here
