;;; friendly-tramp-path.el --- Human-friendly TRAMP path contruction

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((emacs "24.1")(dash "2.16.0")(s "1.11.0"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;  -----------
;;
;; More friendly (permissive) TRAMP path constructions for us humans.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/friendly-tramp-path/blob/master/README.md


;;; Code:



;; REQUIRES

(require 'tramp)
(require 'tramp-sh)

(require 'dash)
(require 's)



;; CONSTANTS: PERMISSIVE TRAMP PATH DISSECT

(defconst friendly-tramp-path--rx-method
  `(line-start
    ,tramp-prefix-format
    (group (one-or-more (any "a-z" "A-Z" "0-9")))
    ;; (group ,tramp-method-regexp)
    ,tramp-postfix-method-format)
  "Pattern for extracting the method part at the begining of a TRAMP path.")

(defconst friendly-tramp-path--rx-user-no-postfix
  `((group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    )
  "Pattern for extracting the user part in a TRAMP path, without end delimiter.")

(defconst friendly-tramp-path--rx-user
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    ,tramp-postfix-user-format)
  "Pattern for extracting the user part in a TRAMP path.")

(defconst friendly-tramp-path--rx-domain
  `(,tramp-prefix-domain-regexp
    (group ,tramp-domain-regexp))
  "Pattern for extracting the domain part in a TRAMP path.")

(defconst friendly-tramp-path--rx-host
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" "." "-")))
    ;; (group ,tramp-host-regexp)
    (zero-or-more ,tramp-postfix-host-format))
  "Pattern for extracting the host part in a TRAMP path.")

(defconst friendly-tramp-path--rx-port
  `(,tramp-prefix-port-format
    (group ,tramp-port-regexp))
  "Pattern for extracting the port part in a TRAMP path.")

(defconst friendly-tramp-path--rx-localname
  `(
    (group (zero-or-more anything))
    ;; (group ,tramp-localname-regexp)
    )
  "Pattern for extracting the localname part in a TRAMP path.")



;; UTILS: rx WRAPPER

;; REVIEW: Doing this might induces a performance penalty but this makes things reusable.

(defun friendly-tramp-path--rx (args)
  "Call `rx' with list ARGS.
This allows calling `rx' with patterns built programatically."
  ;; NB: We're using `eval' instead of `apply' as `rx' is a macro.
  (eval (cons #'rx args)))

(defun friendly-tramp-path--match-rx-p (str rx-args)
  "Return t if STR match RX-ARGS."
  (string-match (friendly-tramp-path--rx rx-args)
                str))

(defun friendly-tramp-path--match-rx (str rx-args pos)
  "Extract group at group POS from string STR matching RX-ARGS."
  (when
      (friendly-tramp-path--match-rx-p str rx-args)
    (match-string pos str)))



;; UTILS: PERMISSIVE TRAMP PATH DISSECT

(defun friendly-tramp-path-method (path)
  "Extract method part from friendly PATH."
  ;; /METHOD:*
  (friendly-tramp-path--match-rx path friendly-tramp-path--rx-method 1))

(defun friendly-tramp-path-user (path)
  "Extract user part from friendly PATH."
  (cond
   ;; /METHOD:USER%DOMAIN@*
   ((friendly-tramp-path--match-rx-p path
                                     (-concat friendly-tramp-path--rx-method
                                              friendly-tramp-path--rx-user-no-postfix
                                              friendly-tramp-path--rx-domain
                                              `(,tramp-postfix-user-format)))
    (match-string 2 path))
   ;; /METHOD:USER@*
   ((friendly-tramp-path--match-rx-p path
                                     (-concat friendly-tramp-path--rx-method
                                              friendly-tramp-path--rx-user))
    (match-string 2 path))
   ;; USER@*
   ((friendly-tramp-path--match-rx-p path
                                     (-concat '(line-start)
                                              friendly-tramp-path--rx-user))
    (match-string 1 path))))

(defun friendly-tramp-path-host (path)
  "Extract host part from friendly PATH."
  ;; *@*
  (if (s-contains? tramp-postfix-user-format path)
      ;; *USER@HOST[:*]
      (friendly-tramp-path--match-rx path
                                     (-concat friendly-tramp-path--rx-user
                                              friendly-tramp-path--rx-host)
                                     2)
    (cond
     ;; /METHOD:HOST[:*]
     ((friendly-tramp-path--match-rx-p path
                                       (-concat friendly-tramp-path--rx-method
                                                friendly-tramp-path--rx-host))
      (match-string 2 path))
     ;; HOST:*
     ((friendly-tramp-path--match-rx-p path
                                       (-concat '(line-start)
                                                friendly-tramp-path--rx-host
                                                friendly-tramp-path--rx-localname))
      (match-string 1 path))
     ;; HOST[:]
     ((friendly-tramp-path--match-rx-p path
                                       (-concat '(line-start)
                                                friendly-tramp-path--rx-host
                                                '(eol)))
      (match-string 1 path)))))

(defun friendly-tramp-path-localname (path)
  "Extract localname part from friendly PATH."
  (let (localname)
    (setq localname
          (cond
           ;; /METHOD:USER@HOST:LOCALNAME (i.e. 2 ":" in the path)
           ((friendly-tramp-path--match-rx-p path
                                             (-concat friendly-tramp-path--rx-method
                                                      friendly-tramp-path--rx-user
                                                      friendly-tramp-path--rx-host
                                                      friendly-tramp-path--rx-localname))
            (match-string 4 path))
           ;; [USER@]HOST:LOCALNAME
           ((friendly-tramp-path--match-rx-p path
                                             (-concat `((one-or-more anything))
                                                      `(,tramp-postfix-host-format)
                                                      friendly-tramp-path--rx-localname))
            (match-string 1 path))))
    (if (and (stringp localname)
             (eq (length localname) 0))
        nil
      localname)))

(defun friendly-tramp-path-disect (path)
  "Convert PATH into a TRAMP VEC.
More permissive version of `tramp-dissect-file-name'.
Accepts input from `prf/tramp/remote-shell'."
  (if (file-remote-p path)
      (tramp-dissect-file-name path)
    (let ((method (friendly-tramp-path-method path))
          (user (friendly-tramp-path-user path))
          (host (friendly-tramp-path-host path))
          (localname (friendly-tramp-path-localname path)))
      (if (eq (length method) 0)
          (setq method tramp-default-method))
      (if (eq (length user) 0)
          (setq user tramp-default-user))
      (if (eq (length localname) 0)
          (setq localname "/"))
      `(tramp-file-name ,method ,user nil ,host nil ,localname))))




(provide 'friendly-tramp-path)

;;; friendly-tramp-path.el ends here
