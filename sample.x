(defun fibonacci (N)
    (if (or (zerop N) (= N 1))
        1
        (+ fibonacci (- N 1 F2) fibonacci (- N 2))))


(print (fibonacci 5))