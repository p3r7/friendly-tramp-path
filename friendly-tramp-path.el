;;; friendly-tramp-path.el --- Human-friendly TRAMP path contruction

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp, rx
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((dash "2.16.0")(s "1.11.0"))
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
    ,tramp-postfix-method-format))

(defconst friendly-tramp-path--rx-user-no-postfix
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    ))

(defconst friendly-tramp-path--rx-user
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    ,tramp-postfix-user-format))

(defconst friendly-tramp-path--rx-domain
  `(,tramp-prefix-domain-regexp
    (group ,tramp-domain-regexp)))

(defconst friendly-tramp-path--rx-host
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" "." "-")))
    ;; (group ,tramp-host-regexp)
    (zero-or-more ,tramp-postfix-host-format)))

(defconst friendly-tramp-path--rx-port
  `(,tramp-prefix-port-format
    (group ,tramp-port-regexp)))

(defconst friendly-tramp-path--rx-localname
  `(
    (group (zero-or-more anything))
    (group ,tramp-localname-regexp)
    ))



;; UTILS: rx WRAPPER

;; REVIEW: Doing this might induces a performance penalty but this makes things reusable.

(defun friendly-tramp-path--rx (args)
  "Call `rx' with list ARGS."
  ;; NB: We're using `eval' instead of `apply' as `rx' is a macro.
  (eval (cons #'rx args)))



;; UTILS: PERMISSIVE TRAMP PATH DISSECT

(defun friendly-tramp-path-method (path)
  ;; /METHOD:*
  (when (string-match (friendly-tramp-path--rx friendly-tramp-path--rx-method)
                      path)
    (match-string 1 path)))

(defun friendly-tramp-path-user (path)
  ;; /METHOD:USER%DOMAIN@*
  (if (string-match (friendly-tramp-path--rx (-concat friendly-tramp-path--rx-method
                                                      friendly-tramp-path--rx-user-no-postfix
                                                      friendly-tramp-path--rx-domain
                                                      `(,tramp-postfix-user-format)))
                    path)
      (match-string 2 path)
    ;; /METHOD:USER@*
    (if (string-match (friendly-tramp-path--rx (-concat friendly-tramp-path--rx-method
                                                        friendly-tramp-path--rx-user))
                      path)
        (match-string 2 path)
      ;; USER@*
      (when (string-match (friendly-tramp-path--rx (-concat '(line-start)
                                                            friendly-tramp-path--rx-user))
                          path)
        (match-string 1 path)))))

(defun friendly-tramp-path-host (path)
  ;; *@*
  (if (s-contains? tramp-postfix-user-format path)
      ;; *USER@HOST[:*]
      (when (string-match (friendly-tramp-path--rx (-concat friendly-tramp-path--rx-user
                                                            friendly-tramp-path--rx-host))
                          path)
        (match-string 2 path))
    (cond
     ;; /METHOD:HOST[:*]
     ((string-match (friendly-tramp-path--rx (-concat friendly-tramp-path--rx-method
                                                      friendly-tramp-path--rx-host))
                    path)
      (match-string 2 path))
     ;; HOST
     ((string-match (friendly-tramp-path--rx (-concat '(line-start)
                                                      friendly-tramp-path--rx-host
                                                      '(eol)))
                    path)
      (match-string 1 path))
     ;; TODO: HOST:* ?
     )))

(defun friendly-tramp-path-localname (path)
  ;; /METHOD:USER@HOST:LOCALNAME, i.e. 2 : in the path
  (if (string-match (friendly-tramp-path--rx (-concat friendly-tramp-path--rx-method
                                                      friendly-tramp-path--rx-user
                                                      friendly-tramp-path--rx-host
                                                      friendly-tramp-path--rx-localname))
                    path)
      (match-string 4 path)
    (when (string-match (friendly-tramp-path--rx (-concat '((one-or-more anything))
                                                          `(,tramp-postfix-host-format)
                                                          friendly-tramp-path--rx-localname))
                        path)
      (match-string 1 path))))

(defun friendly-tramp-path-disect (path)
  "More permissive version of `tramp-dissect-file-name'.
Return a VEC.
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

;;; friendly-tramp-path.el ends here.
