#lang racket
;;Note that this has the midi-readwrite dependency, this is a seperate dependency than rsound
(require midi-readwrite)
(define (both a b) b)
(define (three a b c) c)
(define soundfiles (list "mhall.mid"))
(define sequencehash (make-hash))
(define (getkey list)
  (make-key (first list) (first (rest list))))
(define (make-key n1 n2)
  (+ (* 1000 (note-pitch n1)) (note-pitch n2)))
(define (add-notes-to-hash list-of-notes)
  (cond
    [(empty? list-of-notes) 0]
    [(empty? (rest list-of-notes)) 0]
    [else
     (both
      (cond
        [(hash-has-key? sequencehash (getkey list-of-notes)) (hash-set! sequencehash (getkey list-of-notes) (+ 1 (hash-ref sequencehash (getkey list-of-notes))))]
        [else (hash-set! sequencehash (getkey list-of-notes) 1)]
        )
      (add-notes-to-hash (rest list-of-notes)))]
    )
  )

(define (parse-sound-file filenames)
  (cond
    [(empty? filenames) "Finished Parsing Sound Files"]
    [else (both
   (add-notes-to-hash (MIDIFile->notelist (midi-file-parse (first filenames))))
   (parse-sound-file (rest filenames))
   )]))
(parse-sound-file soundfiles)

(define-struct node [midi connections])
(define node-list (build-list 120 (lambda (y) (build-list 120 (lambda (x) (* x 0))))))

(define (change-node-list first second value)
  (set! node-list (list-set node-list first (list-set (list-ref node-list first) second value))))

(define (looperino keys)
  (cond
    [(empty? keys) ""]
    [else
     (both
      (change-node-list (floor (/ (first keys) 1000)) (modulo (first keys) 1000) (hash-ref sequencehash (first keys)))
      (looperino (rest keys)))]))
(looperino (hash-keys sequencehash))
(define (getsum list)
  (cond [(empty? list) 0]
        [else (+ (first list) (getsum (rest list)))]))