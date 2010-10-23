(in-package :erlterm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-decoder-helper (operands &optional vars readers bind)
    (if (null operands)
        (values vars (nreverse readers))
      (destructuring-bind (op . rest) operands
        (if (symbolp op)
            (generate-decoder-helper rest (cons (sym-trunc-last op) vars) readers t)
          (destructuring-bind (fn . args) op
            (if (null bind)
                (generate-decoder-helper rest vars
                                         `((,(symb 'read- fn) in ,@args) . ,readers)
                                         nil)
              (generate-decoder-helper rest vars
                                       `((setf ,(car vars) (,(symb 'read- fn) in ,@args)) . ,readers)
                                       nil)))))))
  
  (defun generate-decoder (definition)
    (if (eq (car definition) 'error)
        definition
      (destructuring-bind (constructor . operands) definition
        (multiple-value-bind (vars readers) (generate-decoder-helper operands)
          `(let ,vars
             (,constructor ,@readers)))))))

(defmacro erlang-tag-case (tag &rest case-clauses) 
  `(case ,tag
     ,@(loop FOR (key-sym decode-spec) ON case-clauses BY #'cddr
             FOR key = (symbol-value key-sym)
             COLLECT `(,key ,(generate-decoder decode-spec)))
     (otherwise (error "[TAG ~A]: Undefined tag ID" ,tag))))

(defun read-uint (in byte-width)
  (declare #.*fastest* 
           #.*muffle-compiler-note*
           ((integer 1 16) byte-width))
  (loop FOR offset FROM (* (1- byte-width) 8) DOWNTO 0 BY 8
        SUM (ash (the octet (read-byte in)) offset)))

(defun read-int (in byte-width)
  (declare #.*fastest*
           #.*muffle-compiler-note*
           ((integer 1 16) byte-width))
  (let ((n (read-uint in byte-width)))
    (if (< n (expt #x80 byte-width))
        n
      (- n (expt #x100 byte-width)))))

(defun read-atom (in)
  (let ((term (read-term in)))
    (check-type term erl-atom)
    term))

(defun read-string (in length)
  (let ((str (make-string length)))
    (dotimes (i length str)
      (setf (aref str i) (code-char (the octet (read-byte in)))))))

(defun read-octets (in size)
  (let ((octets (make-array size :element-type 'octet)))
    (dotimes (i size octets)
      (setf (aref octets i) (the octet (read-byte in))))))

(defun read-list (in length)
  (loop REPEAT length COLLECT (read-term in)))

(defun read-bignum (in byte-width)
  (loop FOR offset FROM 0 BELOW (* byte-width 8) BY 8
        SUM (ash (the octet (read-byte in)) offset)))

(defmacro read-array (in reader count)
  `(let ((#1=#:ary (make-array ,count)))
     (dotimes (#2=#:i ,count #1#)
       (setf (aref #1# #2#) (,(symb 'read- (car reader)) ,in . ,(cdr reader))))))

(defun read-bit-binary (in len bits)
  (let ((bit-string (make-array (+ (* (1- len) 8) bits) :element-type 'bit)))
    (loop FOR i FROM 0 BELOW (length bit-string) BY 8
          FOR octet = (the octet (read-byte in))
      DO
      (loop FOR j FROM (1- (min (length bit-string) (+ i 8))) DOWNTO i
            FOR k FROM 7 DOWNTO 0
            WHILE (< j (length bit-string))
        DO
        (setf (aref bit-string j) (ldb (byte 1 k) octet))))
    bit-string))

(defun read-new-ref-id (in length)
  (let ((id (make-array length :element-type '(unsigned-byte 32))))
    (dotimes (i length id)
      (setf (aref id i) (read-uint in 4)))))

(defun read-term (in)
  (let ((tag (read-byte in)))
    (erlang-tag-case tag
      +SMALL_INTEGER+ (int (byte))
      +INTEGER+       (int (int 4))
      +SMALL_BIG+     (erl-bignum width=(byte)   (byte) (bignum width))
      +LARGE_BIG+     (erl-bignum width=(uint 4) (byte) (bignum width))
      +FLOAT+         (erl-float (string 31))
      +NEW_FLOAT+     (erl-new-float (uint 8))
      +ATOM+          (erl-atom len=(uint 2) (string len))
      +SMALL_ATOM+    (erl-atom len=(byte)   (string len))
      +STRING+        (erl-string len=(uint 2) (octets len))
      +LIST+          (erl-list len=(uint 4) (list len) (term))
      +NIL+           (list)
      +SMALL_TUPLE+   (tuple arity=(byte)   (array (term) arity))
      +LARGE_TUPLE+   (tuple arity=(uint 4) (array (term) arity))
      +BINARY+        (octets  len=(uint 4) (octets len))
      +BIT_BINARY+    (bit-binary len=(uint 4) bits=(byte) (bit-binary len bits))
      +REFERENCE+     (reference (atom) (uint 4) (byte))
      +PORT+          (port      (atom) (uint 4) (byte))
      +PID+           (pid       (atom) (uint 4) (uint 4) (byte))
      +NEW_REFERENCE+ (new-reference len=(uint 2) (atom) (byte) (new-ref-id len))
      +FUN+           (fun num-free=(uint 4) 
                           (term) (atom) (term) (term) (array (term) num-free))
      +NEW_FUN+       (new-fun (uint 4) (byte) (uint 16) (uint 4) num-free=(uint 4)
                               (atom) (term) (term) (term) (array (term) num-free))
      +EXPORT+        (erl-export (atom) (atom) (term))

      +COMPRESSED+     (error "[TAG ~A(COMPRESSED)]: not implemented" tag)
      +ATOM_CACHE_REF+ (error "[TAG ~A(ATOM_CACHE_REF)]: not implemented" tag)
      +DIST_HEADER+    (error "[TAG ~A(DISTRIBUTION_HEADER)]: not implemented" tag))))

(defun decode-term (binary-input-stream &key packet)
  (declare ((member nil 1 2 4) packet))
  (when packet
    (loop REPEAT packet DO (read-byte binary-input-stream))) ; TODO: message length check
  (let ((version (read-byte binary-input-stream)))
    (assert (= version +VERSION+))
    (read-term binary-input-stream)))
