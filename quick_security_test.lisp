;; Quick Security Test for SBCL
(format t "=== SBCL Quick Security Test ===~%")

;; Test 1: Basic symbol creation
(format t "Test 1: Symbol creation~%")
(loop for i from 1 to 100 do
  (intern (format nil "TEST-SYMBOL-~A" i)))
(format t "  Created 100 symbols successfully~%")

;; Test 2: Format string safety
(format t "Test 2: Format string test~%")
(handler-case
    (format t "~A~%" "This is safe")
  (error (e)
    (format t "  Format error: ~A~%" e)))

;; Test 3: Eval injection attempt
(format t "Test 3: Eval injection test~%")
(let ((secret-data "PROTECTED"))
  (format t "  Before: secret-data = ~A~%" secret-data)
  (handler-case
      (progn
        (eval '(setf secret-data "EVAL_INJECTED"))
        (format t "  After eval: secret-data = ~A~%" secret-data))
    (error (e)
      (format t "  Eval failed: ~A~%" e))))

;; Test 4: Reader security
(format t "Test 4: Reader security test~%")
(handler-case
    (read-from-string "(format t \"Reader test passed~%\")")
  (error (e)
    (format t "  Reader error: ~A~%" e)))

(format t "=== Tests completed ===~%")
