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

;; (setq unison-font-lock-keywords
;;       (let* (
;;              (x-keywords '("namespace", "type"))

;;              ;; generate regex string for each category of keywords
;;              (x-keywords-regexp (regexp-opt x-keywords 'words)))

;;         `(
;;           (,x-keywords-regexp . font-lock-keyword-face)
;;        )))

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

             ;; define several categories of keywords
             (x-keywords '("type" "namespace" "use" "if" "else" "unique" "ability" "where" "match" "cases" "let" "with" "handle" "forall" "infix" "infixl" "infixr" "module"))

             ;; generate regex strings for each category
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             ;; (x-types-regexp (regexp-opt x-types 'words))

             (x-type-def-regexp (concat "type\s\\(" t-re "\\)\s.+"))
             (x-ability-def-regexp (concat "ability\s\\(" t-re "\\)\s.+"))
             (x-namespace-def-regexp (concat "namespace\s\\(" n-re "\\)\s+where"))

             (x-namespaced-regexp "[^A-Za-z]\\([A-Za-z\\.]+\\.\\)")
             ;; TODO fix this

             (x-arrow-regexp "->")
             (x-colon-regexp ":")
             (x-apex-regexp "'")
             (x-esc-regexp "!")

             (x-func-sig-regexp (concat "\\([" n-re ".+\s+\\):\s+.*$"))

             ;; (x-func-type-regexp ".+\s:\s.+\\([A-Z]\w+\\)")
             ;; this one needs lookbehind and lookahead
             (x-func-type-regexp (concat ".+\s+:\s.*\\(" t-re "\\)"))

             (x-type-constructor-regexp "[^A-Za-z]\\([A-Z][A-Za-z]+\\)\s*")

             )
             `(
               (,x-keywords-regexp . font-lock-keyword-face)
               (,x-func-sig-regexp . (1 font-lock-function-name-face))
               (,x-namespace-def-regexp . (1 font-lock-constant-face))
               (,x-func-type-regexp . (1 font-lock-constant-face))
               ;; (,x-namespaced-regexp . (1 font-lock-constant-face))
               (,x-type-def-regexp . (1 font-lock-constant-face))
               (,x-ability-def-regexp . (1 font-lock-constant-face))
               ;; (,x-type-constructor-regexp . (1 font-lock-type-face))

               (,x-arrow-regexp . font-lock-keyword-face)
               (,x-colon-regexp . font-lock-keyword-face)
               (,x-apex-regexp . font-lock-negation-char-face)
               (,x-esc-regexp . font-lock-negation-char-face)
             )))

;;;###autoload
(define-derived-mode unison-mode prog-mode "unison-mode"
  "Major mode for editing Unison"

  :syntax-table unison-mode-syntax-table
  (setq font-lock-defaults '(unison-font-lock-keywords))
  (font-lock-fontify-buffer)

  ;; code for syntax highlighting
  ;; (setq font-lock-defaults '((unison-font-lock-keywords)))
  )

;; add the mode for the 'features' list
(provide 'unison-mode)

;;; unison-mode.el ends here
