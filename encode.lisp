(in-package :erlterm)

(declaim (inline int-to-be-bytes int-to-le-bytes float-to-str
                 int-to-bin float-to-bin real-to-bin symbol-to-bin
                 nil-to-bin string-to-bin list-to-bin octets-to-bin 
                 bit-vector-to-bin vector-to-bin 
                 reference-to-bin new-reference-to-bin
                 port-to-bin pid-to-bin fun-to-bin new-fun-to-bin
                 export-to-bin))

(defun int-to-be-bytes (int byte-width)
  (loop FOR offset FROM (* (1- byte-width) 8) DOWNTO 0 BY 8
        COLLECT (ldb (byte 8 offset) int)))

(defun int-to-le-bytes (int byte-width)
  (loop FOR offset FROM 0 BELOW (* byte-width 8) BY 8
        COLLECT (ldb (byte 8 offset) int)))

;; int
(defun int-to-bin (int)
  (typecase int
    ((unsigned-byte 8) `(,+SMALL_INTEGER+ ,int))
    ((signed-byte 32)  `(,+INTEGER+ ,@(int-to-be-bytes int 4)))
    (t 
     (let* ((byte-width (ceiling (integer-length int) 8))
            (sign (if (minusp int) 1 0))
            (bytes (int-to-le-bytes (abs int) byte-width)))
       (if (< byte-width #x100)
           `(,+SMALL_BIG+ ,byte-width ,sign ,@bytes)
         `(,+LARGE_BIG+ ,@(int-to-be-bytes byte-width 4) ,sign ,@bytes))))))

;; float
(defun float-to-str (float &aux (*read-default-float-format* 'double-float))
  (format nil #.(mkstr "~31,,,'"#\Null"@<~,20e~>") float))

(defun float-to-bin (float &aux (float (float float 1.0d0)))
  (if (= *minor-version* 0)
      `(,+FLOAT+ ,@(map 'list #'char-code (float-to-str float)))
    (multiple-value-bind (fraction exponent sign) 
                         (integer-decode-float float)
      (let ((ieee-double 0))
        (setf (ldb (byte  1 63) ieee-double) (if (plusp sign) 0 1)
              (ldb (byte 11 52) ieee-double) (+ exponent 52 1023)
              (ldb (byte 52  0) ieee-double) fraction)
        `(,+NEW_FLOAT+ ,@(int-to-be-bytes ieee-double 8))))))

(defun real-to-bin (num)
  (float-to-bin num))

(defun symbol-to-bin (sym)
  (let ((name (string-inverse-case (symbol-name sym))))
    (assert (< (length name) #x100))
    (assert (every (lambda (c) (< (char-code c) #x80)) name))
    `(,+ATOM+ ,@(int-to-be-bytes (length name) 2) ,@(map 'list #'char-code name))))

(defun nil-to-bin (_)
  (declare (ignore _))
  `(,+NIL+))

(defun list-to-bin (list)
  (labels ((recur (list len acc)
             (if (atom list)
                 `(,+LIST+ ,@(int-to-be-bytes len 4)
                           ,@(apply #'nconc (nreverse acc))
                           ,@(term-to-bin list))
               (destructuring-bind (term . rest) list
                 (recur rest (1+ len) (cons (term-to-bin term) acc))))))
    (recur list 0 '())))

(defun string-to-bin (str)
  (if (and (< (length str) #x10000)
           (every (lambda (c) (< (char-code c) #x100)) str))
      `(,+STRING+ ,@(int-to-be-bytes (length str) 2) ,@(map 'list #'char-code str))
    (list-to-bin (map 'list #'char-code str))))

(defun octets-to-bin (octets)
  `(,+BINARY+ ,@(int-to-be-bytes (length octets) 4) ,@(coerce octets 'list)))

(defun bit-vector-to-bin (bits)
  (flet ((bits-to-bytes (bits)
           (loop FOR i FROM 0 BELOW (length bits) BY 8
             COLLECT
             (loop FOR j FROM 0 BELOW 8
                   WHILE (< (+ i j) (length bits))
                   SUM (ash (bit bits (+ i j)) (- 7 j))))))
    (let ((len (ceiling (length bits) 8))
          (last-width (mod (length bits) 8)))
      `(,+BIT_BINARY+ ,@(int-to-be-bytes len 4) ,last-width ,@(bits-to-bytes bits)))))

(defun vector-to-bin (tuple &aux (len (length tuple)))
  (if (< len #x100)
      `(,+SMALL_TUPLE+ ,len ,@(apply #'nconc (map 'list #'term-to-bin tuple)))
    `(,+LARGE_TUPLE+ ,@(int-to-be-bytes len 4) ,@(apply #'nconc (map 'list #'term-to-bin tuple)))))

(defun reference-to-bin (ref)
  (with-slots (node id creation) ref
    `(,+REFERENCE+ ,@(term-to-bin node) ,@(int-to-be-bytes id 4) ,creation)))

(defun new-reference-to-bin (ref)
  (with-slots (node creation id) ref
    `(,+NEW_REFERENCE+ ,@(int-to-be-bytes (length id) 2)
                       ,@(term-to-bin node)
                       ,creation
                       ,@(apply #'nconc (map 'list (lambda (n) (int-to-be-bytes n 4)) id)))))

(defun port-to-bin (port)
  (with-slots (node id creation) port
    `(,+PORT+ ,@(term-to-bin node)
              ,@(int-to-be-bytes id 4)
              ,creation)))

(defun pid-to-bin (pid)
  (with-slots (node id serial creation) pid
    `(,+PID+ ,@(term-to-bin node)
             ,@(int-to-be-bytes id 4)
             ,@(int-to-be-bytes serial 4)
             ,creation)))

(defun fun-to-bin (fun)
  (with-slots (pid module index uniq free-vars) fun
    `(,+FUN+ ,@(int-to-be-bytes (length free-vars) 4)
             ,@(term-to-bin pid)
             ,@(term-to-bin module)
             ,@(term-to-bin index)
             ,@(term-to-bin uniq)
             ,@(apply #'nconc (map 'list #'term-to-bin free-vars)))))

(defun new-fun-to-bin (fun)
  (with-slots (arity uniq index module old-index old-uniq pid free-vars) fun
    (let ((tmp `(,arity
                 ,@(int-to-be-bytes uniq 16)
                 ,@(int-to-be-bytes index 4)
                 ,@(int-to-be-bytes (length free-vars) 4)
                 ,@(term-to-bin module)
                 ,@(term-to-bin old-index)
                 ,@(term-to-bin old-uniq)
                 ,@(term-to-bin pid)
                 ,@(apply #'nconc (map 'list #'term-to-bin free-vars)))))
      `(,+NEW_FUN+ ,@(int-to-be-bytes (+ (length tmp) 4) 4)
                   ,@tmp))))

(defun export-to-bin (export)
  (with-slots (module function arity) export
    `(,+EXPORT+ ,@(term-to-bin module)
                ,@(term-to-bin function)
                ,@(term-to-bin arity))))

(defun term-to-bin (term)
  (etypecase term
    (integer        (int-to-bin term))
    (float          (float-to-bin term))
    (real           (real-to-bin term))
    (null           (nil-to-bin term))
    (symbol         (symbol-to-bin term))
    (string         (string-to-bin term))
    ((vector octet) (octets-to-bin term))
    (cons           (list-to-bin term))
    (bit-vector     (bit-vector-to-bin term))
    (vector         (vector-to-bin term))
    (reference      (reference-to-bin term))
    (new-reference  (new-reference-to-bin term))
    (port           (port-to-bin term))
    (pid            (pid-to-bin term))
    (fun            (fun-to-bin term))
    (new-fun        (new-fun-to-bin term))
    (erl-export     (export-to-bin term))))

(defun encode-term (term output-binary-stream &key (minor-version 0) packet)
  (declare ((member nil 1 2 4) packet)
           ((member 0 1) minor-version))
  (let ((*minor-version* minor-version))
    (let ((bytes (coerce (term-to-bin term) 'octets)))
      (when packet
        (let ((size (1+ (length bytes))))
          (assert (< size (expt #x100 packet)))
          (loop FOR offset FROM (* (1- packet) 8) DOWNTO 0 BY 8
                DO (write-byte (ldb (byte 8 offset) size) output-binary-stream))))
      (write-byte +VERSION+ output-binary-stream)
      (write-sequence bytes output-binary-stream)))
  t)
