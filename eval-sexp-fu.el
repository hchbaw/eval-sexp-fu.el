;;; eval-sexp-fu.el --- Small functionality enhancements for evaluating sexps.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: lisp, highlight, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Small functionality enhancements for evaluating sexps.
;; This package provides:
;; - Flashing the sexps during the evaluation.
;; - `eval-last-sexp' variants (innser-list/inner-sexp).

;;; Installation:
;;
;; Put the highlight.el to your load-path.
;; Then require this package.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `eval-sexp-fu-eval-sexp-inner-list'
;;    Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.
;;  `eval-sexp-fu-eval-sexp-inner-sexp'
;;    Evaluate the sexp _currently_ pointed; print value in minibuffer.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `eval-sexp-fu-flash-face'
;;    *Face to use for showing the sexps' overlay during the evaluation.
;;    default = (quote region)
;;  `eval-sexp-fu-flash-duration'
;;    *For time duration, highlight the evaluating sexps.
;;    default = 0.25
;;  `eval-sexp-fu-flash-doit-function'
;;    *Function to use for flashing the sexps.
;;    default = (quote eval-sexp-fu-flash-doit-simple)

;;; Note:
;;
;; For SLIME user, this package registers the setup clauses which set up the
;; flashers and the several interactive commands at `eval-after-load' the
;; 'slime phase. The interactive commands bellow will be defined,
;; `eval-sexp-fu-slime-eval-expression-inner-list',
;; `eval-sexp-fu-slime-eval-expression-inner-sexp'
;; and the pprint variants respectively.

;;; Code:

(eval-when-compile (require 'cl))
(require 'highlight)

(defgroup eval-sexp-fu nil
  "Small functionality enhancements for evaluating sexps."
  :prefix "eval-sexp-fu-"
  :group 'eval-sexp-fu)

;;; Flashing the sexps during the evaluation for just an eye candy.
(defcustom eval-sexp-fu-flash-face 'region
  "*Face to use for showing the sexps' overlay during the evaluation."
  :type 'face
  :group 'eval-sexp-fu)
(defcustom eval-sexp-fu-flash-duration 0.25
  "*For time duration, highlight the evaluating sexps."
  :type 'number
  :group 'eval-sexp-fu)

(defun esf-hl-highlight-bounds (bounds face buf)
  (with-current-buffer buf
    (hlt-highlight-region (car bounds) (cdr bounds) face)))
(defun esf-hl-unhighlight-bounds (bounds buf)
  (with-current-buffer buf
    (hlt-unhighlight-region (car bounds) (cdr bounds))))
(defun* eval-sexp-fu-flash (bounds &optional (face eval-sexp-fu-flash-face))
  "BOUNS is either the cell or the function returns, such that (BEGIN . END).
Reurn the 3 values of bounds, highlighting and un-highlighting procedure.
This function is convenient to use with `define-eval-sexp-fu-flash-command'."
  (flet ((bounds () (if (functionp bounds) (funcall bounds) bounds)))
    (lexical-let ((b (bounds)) (face face) (buf (current-buffer)))
      (when b
        (values b
                (apply-partially 'esf-hl-highlight-bounds b face buf)
                (apply-partially 'esf-hl-unhighlight-bounds b buf))))))

(defcustom eval-sexp-fu-flash-doit-function 'eval-sexp-fu-flash-doit-simple
  "*Function to use for flashing the sexps.

Please see the actual implementations:
- `eval-sexp-fu-flash-doit-simple'
- `eval-sexp-fu-flash-doit-hold-on-error'"
  :type 'function
  :group 'eval-sexp-fu)
(defun eval-sexp-fu-flash-doit (do-it-thunk hi unhi)
  (funcall eval-sexp-fu-flash-doit-function do-it-thunk hi unhi))
(defun eval-sexp-fu-flash-doit-simple (do-it-thunk hi unhi)
  (funcall hi)
  (run-at-time eval-sexp-fu-flash-duration nil unhi)
  (funcall do-it-thunk))
(defun eval-sexp-fu-flash-doit-hold-on-error (do-it-thunk hi unhi)
  (funcall hi)
  (unwind-protect
       (funcall do-it-thunk)
    (run-at-time eval-sexp-fu-flash-duration nil unhi)))

(defmacro esf-konstantly (v)
  `(lambda (&rest _it) ,v))

;; Entry point.
(defmacro define-eval-sexp-fu-flash-command (command form)
  "Install the flasher implemented as the COMMAND's around advice.

FORM is expected to return 3 values;
- A bounds (BEGIN . END) to be highlighted or nil.
- An actual highlighting procedure takes 0 arguments.
- An actual un-highliting procedure takes 0 arguments.
See also `eval-sexp-fu-flash'."
  (declare (indent 1))
  `(defadvice ,command (around eval-sexp-fu-flash-region activate)
     (multiple-value-bind (bounds hi unhi) ,form
       (if bounds
           (eval-sexp-fu-flash-doit (esf-konstantly ad-do-it) hi unhi)
         ad-do-it))))

;;; eval-inner- stuff.
(defun esf-funcall-and-eval-last-sexp (before eval-last-sexp)
  "Call 0 arg procedure BEFORE then call interactive command EVAL-LAST-SEXP."
  (save-excursion
    (funcall before)
    (call-interactively eval-last-sexp)))
(defun esf-forward-sexp ()
  (if (eq (char-after) ? ) (ignore) (forward-sexp)))
(defun esf-end-of-backward-up-list (steps)
  (unless steps (setq steps 1))
  (dotimes (_ steps) (backward-up-list))
  (forward-sexp))

(defun eval-sexp-fu-eval-sexp-inner-list (&optional arg)
  "Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.

Interactivelly with numeric prefix argument, call to `backward-up-list' happens several times. This function is an \"Evaluate this N lists, please.\" thing."
  (interactive "P")
  (esf-funcall-and-eval-last-sexp (apply-partially
                                   'esf-end-of-backward-up-list arg)
                                  'eval-last-sexp))
(defun eval-sexp-fu-eval-sexp-inner-sexp ()
  "Evaluate the sexp _currently_ pointed; print value in minibuffer."
  (interactive)
  (esf-funcall-and-eval-last-sexp 'esf-forward-sexp 'eval-last-sexp))

;; Piece of code which defines the above inner-{sexp,list} functions.
(defmacro define-esf-eval-last-sexp-1 (command-name eval-last-sexp)
  "Define an interactive command COMMAND-NAME kind of EVAL-LAST-SEXP
such that ignores any prefix arguments."
  `(defun ,command-name ()
     (interactive)
     (let (current-prefix-arg)
       (call-interactively ',eval-last-sexp))))
(defmacro define-esf-eval-sexp* (eval-last-sexp inner-sexp inner-list)
  "Based on EVAL-LAST-SEXP, define INNER-SEXP and INNER-LIST interactive commands."
  (declare (indent 1))
  `(progn
     (defun ,inner-sexp ()
       (interactive)
       (esf-funcall-and-eval-last-sexp 'esf-forward-sexp ',eval-last-sexp))
     (defun ,inner-list (&optional arg)
       (interactive "P")
       (esf-funcall-and-eval-last-sexp (apply-partially
                                        'esf-end-of-backward-up-list arg)
                                       ',eval-last-sexp))))
(defmacro define-eval-sexp-fu-eval-sexp (command-name-prefix eval-last-sexp)
  "Define -inner-sexp and -inner-list interactive commands prefixed by COMMAND-NAME-PREFIX based on EVAL-LAST-SEXP. Actual work is done by `define-esf-eval-sexp*'."
  (let ((esf-eval-last-sexp-1
         (intern (format "esf-%s-1" (symbol-name eval-last-sexp)))))
    `(progn
       (define-esf-eval-last-sexp-1 ,esf-eval-last-sexp-1 ,eval-last-sexp)
       (define-esf-eval-sexp* ,esf-eval-last-sexp-1
         ,@(mapcar (lambda (post)
                     (intern (concat (symbol-name command-name-prefix) post)))
                   '("-inner-sexp" "-inner-list"))))))

(eval-when (load eval)
  ;; Install the flashers implemented as the command's advice.
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (save-excursion
                          (backward-char)
                          (bounds-of-thing-at-point 'sexp))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (save-excursion
                          (beginning-of-defun)
                          (bounds-of-thing-at-point 'sexp))))
  ;; An example usage of `define-eval-sexp-fu-flash-command'.
  (when (fboundp 'eek-eval-last-sexp)
    ;; `eek-eval-last-sexp' is defined in eev.el.
    (define-eval-sexp-fu-flash-command eek-eval-last-sexp
      (eval-sexp-fu-flash (cons (save-excursion (eek-backward-sexp)) (point))))
    )
  )

(provide 'eval-sexp-fu)
;;; eval-sexp-fu.el ends here

;;
(eval-when (load eval)
  ;; TODO: Extract it out into the documents.
  (setq eval-sexp-fu-flash-face paren-match-face)
  (setq eval-sexp-fu-flash-duration 0.15)
  (setq eval-sexp-fu-flash-doit-function
        'eval-sexp-fu-flash-doit-hold-on-error)

  (define-key emacs-lisp-mode-map (kbd "s-c s-e")
    'eval-sexp-fu-eval-sexp-inner-list)
  (define-key emacs-lisp-mode-map (kbd "s-c s-c s-e")
    'eval-sexp-fu-eval-sexp-inner-sexp)
  (define-key lisp-interaction-mode-map (kbd "s-c s-e")
    'eval-sexp-fu-eval-sexp-inner-list)
  (define-key lisp-interaction-mode-map (kbd "s-c s-c s-e")
    'eval-sexp-fu-eval-sexp-inner-sexp)
  )
