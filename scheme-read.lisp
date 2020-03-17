(cl:in-package #:airship-scheme)

;; TODO: Stub
(defun tokenize (stream)
  (loop :for match := (read-case (stream x)
                        ((:range #\0 #\9) (format t "~S~%" x) (cons :digit x))
                        ((:range #\A #\Z) (format t "~S~%" x) (cons :letter x))
                        ((:range #\a #\z) (format t "~S~%" x) (cons :letter x))
                        ((:or #\Space #\Newline #\Tab) (format t "~S~%" x) (cons :whitespace x))
                        ((:or #\( #\)) (format t "~S~%" x) (cons :parens x)))
        :while match
        :collect match))

(defun parse (tokens &optional literal?)
  (loop :with tokens* := tokens
        :for token := (car tokens*)
        :with literal? := literal?
        ;; TODO: balance parens
        :until (or (endp tokens*) (eql token #\)))
        :collect (if (eql token #\()
                     (multiple-value-bind (result rest)
                         (parse (cdr tokens*) literal?)
                       (prog1 result
                         (setf tokens* rest)))
                     (prog1
                         (etypecase token
                           (character token)
                           (string (let ((scheme-symbol (intern (invert-case token) 'scheme)))
                                     (if literal?
                                         scheme-symbol
                                         (cons :variable scheme-symbol)))))
                       (setf tokens* (cdr tokens*))))
          :into parse-tree
        :finally
           ;; TODO: check for balanced parens
           (if t
               (return parse-tree)
               (error "Unmatched parentheses"))))

;;; TODO: stub
(defun simplify (parse-tree)
  parse-tree)

;; TODO: Stub
(defun scheme-read (stream)
  (let* ((tokens (tokenize stream))
         (parse-tree (parse tokens)))
    (simplify parse-tree)))
