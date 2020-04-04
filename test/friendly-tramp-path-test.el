

(require 'ert)
(require 'friendly-tramp-path)



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
