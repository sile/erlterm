(in-package :erlterm)

(defun sym-trunc-last (sym)
  (let ((name (symbol-name sym)))
    (intern (subseq name 0 (1- (length name))))))

(defun symb (&rest args)
  (intern
   (with-output-to-string (out)
     (dolist (a args) (princ a out)))))


