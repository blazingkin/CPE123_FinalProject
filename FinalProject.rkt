;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname FinalProject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require rsound)
(require rsound/single-cycle)
(require 2htdp/universe)
(require 2htdp/image)

;; More dirty hacks
(define (both a b) b)

;; A markov-node has
;; - midi: a integer value that corresponds to a MIDI note number
;; - connections: a list of connection strengths, where the strengths are numbers between 0.0 and 1.0
(define-struct markov-node [midi connections])

;; A markov-chain has
;; - nodes: a list of markov-nodes in the markov-chain
;; - current-node: a integer value corresponding to the current index of the markov-chain list
(define-struct markov-chain [nodes current-node])

;; the audio track the notes will be queued into
(define p (make-pstream))

;; the markov-map is the graphic interface where all the markov-nodes can be viewed and adjusted
;; note: for now, the markov-map is the red rectangular outline and the numbers representing the note
(define markov-map (rectangle 1200 600 "outline" "red"))

;; Add a markov-node to a markov-chain
;; node: the markov-node to be added
;; chain: the markov-chain to add the markov-node to
;; markov-node markov-chain -> markov-chain
;; --------------- ADD CHECK-EXPECTS ---------------
(define (add-node-to-chain node chain)
  (make-markov-chain (append (alert-all-nodes-add (markov-chain-nodes chain)) (list node)) (markov-chain-current-node chain))
  )

;; Alert all nodes of a new node
;; list: the nodes of a markov-chain
;; markov-chain-nodes -> markov-chain-nodes (with updated nodes)
;; --------------- ADD CHECK-EXPECTS ---------------
(define (alert-all-nodes-add list)
  (cond
    [(empty? list) '()]
    [else (cons (node-alert-chain-add (first list)) (alert-all-nodes-add (rest list)))]))


;; Add a connection to each of the markov-nodes in the markov-chain
;; Adds a new connection to the markov-node, then normalizes all of that markov-node's connections
;; node: the markov-node whose connections are being adjusted
;; markov-node -> markov-node
;; --------------- ADD CHECK-EXPECTS --------------- (note: how do you check the code when it calls for a random value?)
(define (node-alert-chain-add node)
  (make-markov-node (markov-node-midi node)
                    (normalize-node (append (markov-node-connections node) (list (random)))))
  )

;; Takes a list of connection strengths and returns a normalized list of connection strengths
;; connections: a list of numbers between 0.0 and 1.0 that denote the connection strengths of a markov-node
;; List-of-Numbers -> List-of-Numbers (whose sum is 1)
(check-expect (normalize-node (cons 1.0 '())) (cons 1.0 '()))
(check-expect (normalize-node (cons 0.2 (cons 0.3 '()))) (cons 0.4 (cons 0.6 '())))
(check-expect (normalize-node (cons 0.4 (cons 0.6 (cons 0.4 (cons 0.6 '()))))) (cons 0.2 (cons 0.3 (cons 0.2 (cons 0.3 '())))))
(define (normalize-node connections)
  (get-normalized connections (sum-of-list connections)))

;; normalizes a list of connections by dividing each value by the sum of the list
;; essentially converts each value to the percentage of the whole that it represents
;; connections: a list of numbers between 0.0 and 1.0 that denote the connection strengths of a markov-node
;; sum: the sum of the values in connections
;; list-of-numbers decimal-number -> list-of-numbers (whose sum is 1)
(check-expect (get-normalized (cons 1.0 '()) 1.0) (cons 1.0 '()))
(check-expect (get-normalized (cons 0.2 (cons 0.3 '())) 0.5) (cons 0.4 (cons 0.6 '())))
(check-expect (get-normalized (cons 0.4 (cons 0.6 (cons 0.4 (cons 0.6 '())))) 2.0) (cons 0.2 (cons 0.3 (cons 0.2 (cons 0.3 '())))))
(define (get-normalized connections sum)
  (cond
    [(empty? connections) '()]
    [else (cons (/ (first connections) sum) (get-normalized (rest connections) sum))]))

;; computes the sum of all elements in a list
;; list: a list of numbers
;; List-of-Numbers -> Number
(define (sum-of-list list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum-of-list (rest list)))]
    ))

;; Takes a chain and returns the same chain with current-node changed to be the new one
;; Markov Chain -> Number
(define (get-next-node chain)
  (pick-node-based-on-weights (markov-node-connections (list-ref (markov-chain-nodes chain) (markov-chain-current-node chain))) 0 (random))
  )

;; Call this with the node's weights, 0 and a random floating point number in 0 < rand < 1
;; Number List, 0, 0 < rand < 1 -> Number
(define (pick-node-based-on-weights weights inc rand)
  (cond
    [(empty? weights) 0]
    [(< rand (first weights)) inc]
    [else (pick-node-based-on-weights (rest weights) (+ 1 inc) (- rand (first weights)))])
  )

;; draw the background, queue next note to be played at the end of the pstream, and write the note's corresponding number onto the markov-map
;; the duration of each note is half the FRAME-RATE
(define (draw-handler ws)
  (overlay/xy markov-map
              200
              200
              (write-note-number ws)))

;; writes the number of the note currently playing
;; in the future, will be updated to change the color of the node currently playing
;; ------------------------ FINISH ------------------------
(define (write-note-number ws)
  (text (number->string (markov-chain-current-node ws)) 24 "blue"))

;; on each clock-tick, create a markov-chain of the nodes already played and pick the next node to be played
(define (tick-handler ws)
  (both (pstream-queue p (synth-note "main" 35 (markov-node-midi (list-ref (markov-chain-nodes ws) (markov-chain-current-node ws))) (/ FRAME-RATE 5)) (pstream-current-frame p))
        (make-markov-chain (markov-chain-nodes ws) (get-next-node ws))))

;; the initial nodes in the markov-chain
(define initial-chain
  (add-node-to-chain (make-markov-node 72 '(.25 .25 .25 .25))
                     (add-node-to-chain (make-markov-node 67 '(.2 .25 .5))
                                        (add-node-to-chain (make-markov-node 64 '(.5 .5))
                                                           (add-node-to-chain (make-markov-node 60 '(1)) (make-markov-chain '() 0))))))

;; The world state of this big-bang is a markov-chain
(big-bang initial-chain
          [to-draw draw-handler]
          [on-tick tick-handler 1/5])
