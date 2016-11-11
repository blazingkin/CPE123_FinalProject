;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname FinalProject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

;;More dirty hacks
(define (both a b) b)

;;A markov chain has
;;A List of nodes in the chain AND a integer value corresponding to the index of the list where the markov chain is currently at
(define-struct markov-chain [nodes current-node])

;;A Markov Node has
;;A value of the node that corresponds to a MIDI note number AND a list of connection strenghts
(define-struct markov-node [value connections])

;;This is used to add a node to a chain
(define (add-node-to-chain node chain)
  (make-markov-chain (append (alert-all-nodes-add (markov-chain-nodes chain)) (list node)) (markov-chain-current-node chain))
  )

;;Chain -> Chain with updated nodes
;;Alert all nodes of a new node
(define (alert-all-nodes-add chain)
  (cond
    [(empty? chain) '()]
    [else (cons (node-alert-chain-add (first chain)) (alert-all-nodes-add (rest chain)))]))


;;This is called on all of the nodes in the chain when another node is added
;; Markov Node -> Markov Node
(define (node-alert-chain-add node)
  (make-markov-node (markov-node-value node)
                    (normalize-node (append (markov-node-connections node) (list (random)))))
  )

;;Takes a list of connection strengths and returns a normalized list of connection strengths
;; List of Numbers -> List of Numbers (whose sum is 1)
(define (normalize-node connections)
  (get-normalized connections (sum-of-list connections)))

(define (get-normalized connections sum)
  (cond
    [(empty? connections) '()]
    [else (cons (/ (first connections) sum) (get-normalized (rest connections) sum))]))

;; Gets the sum of all elements in a list
;; List of Numbers -> Number
(define (sum-of-list list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum-of-list (rest list)))]
    ))

;;Takes a chain and returns the same chain with current-node changed to be the new one
;;Markov Chain -> Number
(define (get-next-node chain)
  (pick-node-based-on-weights (markov-node-connections (list-ref (markov-chain-nodes chain) (markov-chain-current-node chain))) 0 (random))
  )

;;Call this with the node's weights, 0 and a random floating point number in 0 < rand < 1
;;Number List, 0, 0 < rand < 1 -> Number
(define (pick-node-based-on-weights weights inc rand)
  (cond
    [(empty? weights) 0]
    [(< rand (first weights)) inc]
    [else (pick-node-based-on-weights (rest weights) (+ 1 inc) (- rand (first weights)))])
  )


(define (draw-handler ws)
  (overlay/xy (rectangle 1200 600 "outline" "red")
  200
  200
  (text (number->string (markov-chain-current-node ws)) 24 "blue")))

(define (tick-handler ws)
  (make-markov-chain (markov-chain-nodes ws) (get-next-node ws)))

(define initial-chain
  (add-node-to-chain (make-markov-node 3 '(.25 .25 .25 .25))
  (add-node-to-chain (make-markov-node 2 '(.2 .25 .5))
  (add-node-to-chain (make-markov-node 1 '(.5 .5))
  (add-node-to-chain (make-markov-node 0 '(1)) (make-markov-chain '() 0))))))

;;The world state of this big bang is a Markov Chain
(big-bang initial-chain
          [to-draw draw-handler]
          [on-tick tick-handler 1/5])