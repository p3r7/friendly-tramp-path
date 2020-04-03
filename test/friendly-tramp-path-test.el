

(require 'ert)
(require 'friendly-tramp-path)



;; DATA SET

(defconst friendly-tramp-path--test-paths
  '(("/method:user@host:/" . (:method "method" :user "user" :host "host" :localname "/"))
    ("/method:user@host:" . (:method "method" :user "user" :host "host" :localname nil))
    ("/method:user@host" . (:method "method" :user "user" :host "host" :localname nil))
    ("user@host:/" . (:method nil :user "user" :host "host" :localname "/"))
    ("user@host:" . (:method nil :user "user" :host "host" :localname nil))
    ("user@host" . (:method nil :user "user" :host "host" :localname nil))
    ("host:/" . (:method nil :user nil :host "host" :localname "/"))
    ("host:" . (:method nil :user nil :host "host" :localname nil))
    ("host" . (:method nil :user nil :host "host" :localname nil)))
  "Alist of paths VS parsed values used for tests.")



;; TESTS

(ert-deftest prf-tramp-get-method-from-path-test ()
  "Ensure TRAMP method gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (plist-get (cdr item) :method)))
            (eval `(should (string= (friendly-tramp-path-method ,path) ,expected)))))
        friendly-tramp-path--test-paths))

(ert-deftest prf-tramp-get-user-from-path-test ()
  "Ensure user gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (plist-get (cdr item) :user)))
            (eval `(should
                    (string= (friendly-tramp-path-user ,path) ,expected)))))
        friendly-tramp-path--test-paths))

(ert-deftest prf-tramp-get-host-from-path-test ()
  "Ensure host gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (plist-get (cdr item) :host)))
            (eval `(should (string= (friendly-tramp-path-host ,path) ,expected)))))
        friendly-tramp-path--test-paths))

;; (ert-deftest prf-tramp-get-port-from-path-test ()
;;   "Ensure host gets correctly extracted from pseudo-tramp path."
;;   (mapc (lambda (item)
;;           (let ((path (car item))
;;                 (expected (plist-get (cdr item) :port)))
;;             (eval `(should (string= (prf/tramp/get-host-port-path ,path) ,expected)))))
;;         friendly-tramp-path--test-paths))

(ert-deftest prf-tramp-get-localname-from-path-test ()
  "Ensure localname gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (plist-get (cdr item) :localname)))
            (eval `(should (string= (friendly-tramp-path-localname ,path) ,expected)))))
        friendly-tramp-path--test-paths))




;;; friendly-tramp-path-test.el ends here
