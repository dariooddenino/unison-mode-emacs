;;; unison-mode.el --- simple major mode for editing Unison. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020, Dario Oddenino

;; Author: Dario Oddenino
;; Version: 0.0.1
;; Created: 24 Apr 2020
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; A simple major mode to edit Unison files (.u, .uu)

;;; Code:

(defconst unison-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; -- Are comments
    (modify-syntax-entry ?- ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    ;; [: :] for docs
    (modify-syntax-entry ?\[ ". 1" table)
    (modify-syntax-entry ?: ". 23b" table)
    (modify-syntax-entry ?\] ". 4" table)
    table))

(setq unison-font-lock-keywords
      (let* (
             ;; Regex for identifiers
             (i-re "[a-z_][A-Za-z_!'0-9]*")
             (t-re "[A-Z_][A-Za-z_!'0-9]*")
             (n-re "[A-Za-z_][A-Za-z_!'0-9]*")
             (o-re "[!$%^&*-=\\+<>.~\\/|:]+")

             (x-fold-regexp "---\\(\n\\|.\\)*")

             ;; define several categories of keywords
             (x-keywords '("type" "namespace" "use" "if" "else" "unique" "ability" "where" "match" "cases" "let" "with" "handle" "forall" "infix" "infixl" "infixr" "module"))

             ;; generate regex strings for each category
             (x-keywords-regexp (regexp-opt x-keywords 'words))

             (x-type-def-regexp (concat "type\s\\(" t-re "\\)\s.+"))
             (x-ability-def-regexp (concat "ability\s\\(" t-re "\\)\s.+"))
             (x-namespace-def-regexp (concat "namespace\s\\(" n-re "\\)\s+where"))

             (x-arrow-regexp "->")
             (x-colon-regexp ":")
             (x-apex-regexp "'")
             (x-esc-regexp "!")

             (x-ability-regexp (concat "{\s*\\(" t-re "\\)\s"))
             (x-func-sig-regexp (concat "\\([" n-re ".+\s+\\):\s+.*$"))
             (x-type-regexp (concat "[^a-z]\\(" t-re "\\)"))

             (x-type-dot (concat t-re "\.+")))


        `(
          (,x-fold-regexp . (0 font-lock-comment-face t))
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-func-sig-regexp . (1 font-lock-function-name-face))
          (,x-namespace-def-regexp . (1 font-lock-constant-face))
          (,x-ability-def-regexp . (1 font-lock-constant-face))
          (,x-type-def-regexp . (1 font-lock-type-face))
          (,x-ability-regexp . (1 font-lock-constant-face))
          (,x-type-dot . font-lock-defaults)
          (,x-type-regexp . (1 font-lock-type-face))
          (,x-arrow-regexp . font-lock-keyword-face)
          (,x-colon-regexp . font-lock-keyword-face)
          (,x-apex-regexp . font-lock-negation-char-face)
          (,x-esc-regexp . font-lock-negation-char-face))))

(defun unison-mode-add-fold ()
  "Add a fold above the current line."
  (interactive)
  (newline)
  (newline)
  (newline)
  (save-excursion
    (forward-line -2)
    (insert "---")))

(defun unison-mode-remove-fold ()
  "Remove the fold directly above the current line."
  (interactive)
  (defun delete-line ()
    "Delete the current line if empty."
    (let (start end content)
      (setq start (line-beginning-position))
      (setq end (line-end-position))
      (setq content (buffer-substring start end))
      (if (eq start end)
        (delete-region start (+ 1 end))
        (if (string-equal content "---")
          (delete-region start (+ 1 end))
          (forward-line 1)))))
  (progn
     (goto-char (search-backward "---"))
     (forward-line -1)
     (delete-line)
     (delete-line)
     (delete-line)))

(defvar unison-mode-map nil "Keymap for `unison-mode'.")
(progn
  (setq unison-mode-map (make-sparse-keymap))
  (define-key unison-mode-map (kbd "C-c C-f") 'unison-mode-add-fold)
  (define-key unison-mode-map (kbd "C-c C-d") 'unison-mode-remove-fold))

(defun unison-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

;;;###autoload
(define-derived-mode unison-mode prog-mode "unison-mode"
  "Major mode for editing Unison"

  :syntax-table unison-mode-syntax-table
  ;; Apply the custom syntax table
  ;; (setq syntax-propertize-function 'apply-custom-syntax-table)


  (setq font-lock-defaults '(unison-font-lock-keywords))
  (setq font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions 'unison-font-lock-extend-region)
  (font-lock-ensure)

  (setq-local comment-start "--  ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.u\\'" . unison-mode))

;; add the mode
(provide 'unison-mode)

;;; unison-mode.el ends here
