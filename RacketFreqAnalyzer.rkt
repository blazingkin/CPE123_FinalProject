#lang racket
;;Note that this has the midi-readwrite dependency, this is a seperate dependency than rsound
(require midi-readwrite)
(define (both a b) b)
(define (three a b c) c)

;;Just as a note, I can't really do signatures for functions as many of the functions are not λ-calc style
;;Many of my functions are really just looping through something using list-folding recursion

;;This is the list of file names to parse
(define soundfiles (list "mhall.mid"))

;;This is the hashmap that will keep track of the frequency of one note following another
(define sequencehash (make-hash))

;;This makes the encoded key out of the list
(define (getkey list)
  (make-key (first list) (first (rest list))))
(define (make-key n1 n2)
  (+ (* 1000 (note-pitch n1)) (note-pitch n2)))
;;The way to encode to a key is 1000 * leading note + following note



;;This loops through the list of notes and adds them to the hash
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


;;This generates the list of notes by using the midi library
(define (parse-sound-file filenames)
  (cond
    [(empty? filenames) "Finished Parsing Sound Files"]
    [else (both
   (add-notes-to-hash (MIDIFile->notelist (midi-file-parse (first filenames))))
   (parse-sound-file (rest filenames))
   )]))
(parse-sound-file soundfiles)

;;This is a list of list, the parent list and each child list is 120 long and the default value for all elements of the child list is 0
(define node-list (build-list 120 (lambda (y) (build-list 120 (lambda (x) (* x 0))))))

;;Sets node-list[first][second] = value
(define (change-node-list first second value)
  (set! node-list (list-set node-list first (list-set (list-ref node-list first) second value))))

;;Loops through the keys in the hashmap and adds them to the node-list
(define (looperino keys)
  (cond
    [(empty? keys) ""]
    [else
     (both
      (change-node-list (floor (/ (first keys) 1000)) (modulo (first keys) 1000) (hash-ref sequencehash (first keys)))
      (looperino (rest keys)))]))
(looperino (hash-keys sequencehash))

;;Loops through a list of numbers and give the arithmetic sum
(define (getsum lst)
  (cond [(empty? lst) 0]
        [else (+ (first lst) (getsum (rest lst)))]))

;;This is the second layer of the normalizing, it loops through all of the embedded lists
(define (normalizeind l sum)
  (cond
    [(= sum 0) l]
    [(empty? l) '()]
    [else (cons (/ (first l) sum) (normalizeind (rest l) sum))]))
;;This is the first layer of the normalizing, it loops through the outer list and calls normalizeind on each element list
(define (normalize nl)
  (cond
    [(empty? nl) '()]
    [else (cons (normalizeind (first nl) (getsum (first nl))) (normalize (rest nl)))]))

;;Checks to see if a node has any connections
(define (check-empty l)
  (not (= (getsum l) 0)))

;;These two lists are used to keep track of which nodes we are purging from the list
(define emptylists (list))
(define usedlist (list))


;;This function populates emptylists and usedlist
(define (flag-empties nl i)
  (cond
    [(empty? nl) "Finished Flagging Empties"]
    [else (both
           (cond
             [(not (check-empty (first nl))) (set! emptylists (append emptylists (list i)))]
             [else (set! usedlist (append usedlist (list i)))])
           (flag-empties (rest nl) (add1 i)))]))
(flag-empties node-list 0)

;;This gets rid of all of the nodes with no connections
(set! node-list (filter check-empty node-list))

;;This is the 2nd part of the rebuild, it loops through an inner list and gets rid of all of the connections to nodes that no longer exist
(define (rebuild-list l i)
  (cond
    [(empty? l) '()]
    [(check-duplicates (cons i emptylists)) (rebuild-list (rest l) (add1 i))]
    [else (cons (first l) (rebuild-list (rest l) (add1 i)))]))

;;This is the 1st part of the rebuild, it loops through all of the nodes and calls rebuild-list on them
(define (loop-and-rebuild l)
  (cond
    [(empty? l) '()]
    [else (cons (rebuild-list (first l) 0) (loop-and-rebuild (rest l)))]))
(set! node-list (loop-and-rebuild node-list))

;;Converts a list of numbers into a list of string of the elements of that list space deliniated
(define (l->s l)
  (cond
    [(empty? l) ""]
    [else (string-append (number->string (first l)) " " (l->s (rest l)))]))


;;This is the primary output logic
(display "(make-markov-chain (list\n")

;;Loops through the nodes and prints them out one at a time
(define (printloop l i)
  (cond
    [(empty? l) (display ") 0)")]
    [else (both
   (display (string-append "(make-markov-node " (number->string (list-ref usedlist i)) " (list " (l->s (first l)) "))\n"))
   (printloop (rest l) (add1 i)))]))
(set! node-list (normalize node-list))
(printloop node-list 0)