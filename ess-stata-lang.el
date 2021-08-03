;;; ess-stata-lang.el --- Stata customization  -*- lexical-binding: t; -*-

;; Copyright (C) 1999--2000, Thomas Lumley, A. J. Rossini, Brendan Halpin.
;; Copyright (C) 1997--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;     Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Thomas Lumley <thomas@biostat.washington.edu>,
;;     Brendan Halpin <brendan@essex.ac.uk>
;; Created: 2 Nov 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; This is based upon Version 0.4 of Stata mode.

;; Stata modes.  Emacs modes for using the Stata statistical package
;; Modified from S-mode, comint-mode
;;
;; (c) Thomas Lumley 1997
;;
;;  version 0.4  20/7/97
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; (for the GNU GPL see above)


;;; Code:

(declare-function ess-help-mode "ess-help")

(require 'ado-font-lock) ;; all the font-lock definitions
;; (require 'make-regexp)  ; it's now local to the directory.
;;(load-library "make-regexp") ;; this is necessary for
;; ado-set-font-lock-keywords
;; only needed in Emacs >= 22.x
(require 'comint)
(require 'ess-trns)

(defconst ess-help-STA-sec-keys-alist
  '((?d . "Description")
    (?e . "Examples")
    (?o . "Options")
    (?s . "Also see")
    (?S . "Syntax")
    (?r . "Remarks")
    (?t . "Title"))
  "Help section keys.
`key' indicates the keystroke to use to search for the section heading
`string' in an Stata help file. `string' is used as part of a
regexp-search, and so specials should be quoted.
")

(defconst ess-help-STA-sec-regex "^[A-Z a-z]+:?\n-+\\|http:"
  "Reg(ular) Ex(pression) of section headers in help file.")

;; (defun ado-set-font-lock-keywords () )
;; use ado-mode's ado-font.el and ado-font-lock.el instead
;; so that font-lock would work properly

(defvar ess-STA-mode-font-lock-defaults (ado-set-font-lock-keywords)
  "Set the Stata mode font-lock keywords to Bill Rising's ado-mode keywords.")

(defvar STA-editing-alist
  '((paragraph-start              . (concat "[ \t\f]*$\\|" page-delimiter))
    (paragraph-separate           . (concat  "[ \t\f]*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (comment-column               . 40)
    ;;(comment-indent-function      . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-calculate-indent         . 'ess-calculate-indent)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (font-lock-defaults           . '(ess-STA-mode-font-lock-defaults
                                      nil nil ((?\. . "w")))))
  "General options for editing Stata do and ado source files.")

;; YOU USED TO HAVE TO (with Thomas's version):
;;;;; Add the following to your .emacs file
;;
;;(autoload 'stata "~/ess-sta-l.el" "inferior stata mode" t )
;;(autoload 'stata-help "stata" "stata help mode" t)
;;(autoload 'stata-mode "~/ess-sta-l.el" "stata mode" t)
;;
;;(if (assoc "\\.do$" auto-mode-alist) nil
;;  (setq auto-mode-alist
;;      (append
;;       '(("\\.do$" . stata-mode)
;;         ("\\.ado$" . stata-mode))
;;       auto-mode-alist)))
;;


;; QUESTIONS TO ASK THOMAS:
;; 1 - are 'help' and 'lookup' the same?
;; 2 - what is the point of the review buffer?
;; 3 - how to quit?

;;
;; NOTE: all of Thomas's functions have been left here, to be removed
;; or merged into real locations as we work on this.
;;


;;;;;;;;; Things to change

(defvar stata-switches "-q"
  "Switches to apply to stata invocation.")

(defvar stata-profile "~/.stataprofile"
  "File to read on startup (nil for no file).")

(define-obsolete-function-alias 'stata-help 'ess-display-help-on-object "ESS[15.09]")
(define-obsolete-function-alias 'stata-lookup 'ess-display-help-apropos "ESS[16.03]")

;; fixme? Hook this on C-c C-s /ess-execute-search does an R search() right now/
(defun stata-variables ()
  "Stata variable list in other buffer."
  (interactive)
  (let* ((stata-process (get-process "stata"))
         (stata-buffer (if stata-process
                           (process-buffer stata-process)
                         (error "Stata is not running.")))
         oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
      (save-excursion
        (set-process-buffer stata-process
                            (get-buffer-create "*stata variables*"))
        (set-process-filter stata-process 'inferior-ess-ordinary-filter)
        (set-buffer "*stata variables*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (process-send-string stata-process "desc \n ")
        (stata-prompt-wait stata-process)
        (setq buffer-read-only t)
        (set-buffer stata-buffer)
        (set-process-buffer stata-process oldpb)
        (set-marker (process-mark stata-process) oldpm)
        (set-process-filter stata-process oldpf)))
    (display-buffer "*stata variables*")
    (goto-char (point-max))))

(defun stata-prompt-wait (proc &optional start-of-output)
  "Wait for a prompt to appear at BOL of current buffer.
PROC is the stata process. Does not change point."
  (if start-of-output nil (setq start-of-output (point-min)))
  (save-excursion
    (while (progn
             ;; get output if there is some ready
             (accept-process-output proc 0 50)
             (goto-char (marker-position (process-mark proc)))
             (beginning-of-line)
             (if (< (point) start-of-output) (goto-char start-of-output))
             (not (looking-at "^. "))))))

(cl-defmethod ess--help-major-mode (&context (ess-dialect "stata"))
  (ess-stata-help-mode))

(define-derived-mode ess-stata-help-mode ess-help-mode "Stata help"
  "Major mode for displaying Stata help in a read-only buffer.
Active commands are Help (\\[stata-help]) and hyperlink
(\\[stata-rehelp] or mouse-2)."
  :group 'ess-Stata)

;;; Suggested function from Brendan Halpin:
(defvar ess-STA-delimit-do-file "delimit-do.do")

(defun ess-STA-delimit-do ()
  (save-excursion
    (let ((commands (buffer-substring-no-properties (region-beginning)
                                                    (region-end))))
      (set-buffer (get-buffer-create ess-STA-delimit-do-file))
      (delete-region (point-min) (point-max))
      (insert "#delimit ;\n"
              commands
              "\n#delimit cr\n")
      (write-file ess-STA-delimit-do-file nil)
      (comint-send-string "Stata"
                          (format "do %s \n" ess-STA-delimit-do-file)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ess-stata-lang)

;;; ess-stata-lang.el ends here
