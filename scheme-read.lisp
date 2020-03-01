(in-package #:airship-scheme)

;; TODO: Stub
(defun scheme-read (stream)
  ;; Tokenizer
  ;;
  ;; TODO: Support combining :repeat with :range
  (loop :for match := (read-case (stream c)
                        ;; Note: digit is technically a "subtype" of
                        ;; alphanumeric but at the moment the
                        ;; classification is simple conses.
                        ((:range #\0 #\9) (format t "~S~%" c) (cons :digit c))
                        ((:range #\A #\Z) (format t "~S~%" c) (cons :alphanumeric c))
                        ((:range #\a #\z) (format t "~S~%" c) (cons :alphanumeric c))
                        ((:or #\Space #\Newline #\Tab) (format t "~S~%" c) (cons :whitespace c)))
        :while match
        :collect match))
