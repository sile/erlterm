(in-package :erlterm)

(declaim (inline int octets erl-list erl-bignum tuple bit-binary 
                 erl-atom erl-float erl-new-float
                 reference port pid new-reference fun new-fun erl-export))

(defun int (int)
  int)

(defun octets (size octets)
  (declare (ignore size))
  octets)

(defun erl-atom (len atom-name)
  (assert (<= 0 len 255))
  (intern (string-inverse-case atom-name) :keyword))

(defun erl-list (len list tail)
  (declare (ignore len))
  (nconc list tail))

(defun erl-bignum (byte-width sign num)
  (declare (ignore byte-width))
  (ecase sign
    (0 num)
    (1 (- num))))

(defun tuple (arity array)
  (declare (ignore arity))
  array)

(defun bit-binary (len bits bit-string)
  (declare (ignore len bits))
  bit-string)

(defstruct reference
  (node   nil :type node               :read-only t)
  (id       0 :type (unsigned-byte 32) :read-only t)
  (creation 0 :type (unsigned-byte 8)  :read-only t))
(defun reference (node id creation)
  (assert (<= id #b111111111111111111))
  (assert (<= creation #b11))
  (make-reference :node node :id id :creation creation))

(defstruct port 
  (node     nil :type node               :read-only t)
  (id         0 :type (unsigned-byte 32) :read-only t)
  (creation   0 :type (unsigned-byte 8)  :read-only t))
(defun port (node id creation)
  (make-port :node node :id id :creation creation))

(defstruct pid
  (node     nil :type node               :read-only t)
  (id         0 :type (unsigned-byte 32) :read-only t)
  (serial     0 :type (unsigned-byte 32) :read-only t)
  (creation   0 :type (unsigned-byte 8)  :read-only t))
(defun pid (node id serial creation)
  (assert (<= id #b111111111111111))
  (make-pid :node node :id id :serial serial :creation creation))

(defstruct new-reference
  (node   nil :type node                              :read-only t)
  (creation 0 :type (unsigned-byte 8)                 :read-only t)
  (id     #() :type (simple-array (unsigned-byte 32)) :read-only t))
(defun new-reference (len node creation id)
  (declare (ignore len))
  (assert (plusp (length id)))
  (assert (<= (aref id 0) #b111111111111111111))
  (assert (<= creation #b11))
  (make-new-reference :node node :creation creation :id id))

(defstruct fun
  (pid       nil :type pid                :read-only t)
  (module    nil :type atom               :read-only t)
  (index       0 :type (unsigned-byte 32) :read-only t)
  (uniq        0 :type (unsigned-byte 32) :read-only t)
  (free-vars #() :type simple-vector      :read-only t))
(defun fun (num-free pid module index uniq free-vars)
  (declare (ignore num-free))
  (check-type index (unsigned-byte 32))
  (check-type uniq  (unsigned-byte 32))
  (make-fun :pid pid :module module :index index :uniq uniq :free-vars free-vars))

(defstruct new-fun
  (size        0 :type (unsigned-byte 32) :read-only t)
  (arity       0 :type (unsigned-byte 8)  :read-only t)
  (uniq        0 :type (unsigned-byte 128):read-only t)
  (index       0 :type (unsigned-byte 32) :read-only t)
  (module    nil :type atom               :read-only t)
  (old-index   0 :type (unsigned-byte 32) :read-only t)
  (old-uniq    0 :type (unsigned-byte 32) :read-only t)
  (pid       nil :type pid                :read-only t)
  (free-vars #() :type simple-vector      :read-only t))
(defun new-fun (size arity uniq index num-free module old-index old-uniq pid free-vars)
  (declare (ignore num-free))
  (check-type old-index (unsigned-byte 32))
  (check-type old-uniq  (unsigned-byte 32))
  (make-new-fun :size size :arity arity :uniq uniq :index index :module module
                :old-index old-index :old-uniq old-uniq :pid pid :free-vars free-vars))

(defstruct erl-export
  (module   nil :type atom              :read-only t)
  (function nil :type atom              :read-only t)
  (arity      0 :type (unsigned-byte 8) :read-only t))
(defun erl-export (module function arity)
  (check-type arity (unsigned-byte 8))
  (make-erl-export :module module :function function :arity arity))

(defun erl-float (float-str &aux (*read-default-float-format* 'double-float))
  (let ((f (read-from-string 
            (string-right-trim '(#\Null) float-str))))
    (check-type f float)
    f))
  
(defun erl-new-float (ieee-double)
  (let ((sign     (ldb (byte  1 63) ieee-double))
        (exponent (ldb (byte 11 52) ieee-double))
        (fraction (ldb (byte 52  0) ieee-double)))
    (assert (not (and (= exponent #b11111111111) (= fraction 0)))) ; infinity
    (assert (not (and (= exponent #b11111111111) (/= fraction 0)))); NaN
    (let ((n (scale-float (+ 1.0d0 (* (float fraction 0.0d0) (expt 2 -52)))
                          (- exponent 1023) )))
      (if (zerop sign)
          n
        (- n)))))
