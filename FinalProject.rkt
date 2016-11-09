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
(define (node-alert-chain-add node)
  (make-markov-node (markov-node-value node) (append (markov-node-connections node) (list 0)))
  )

;;Takes a list of connection strengths and returns a normalized list of connection strengths
(define (normalize-node connections)
  0)

(define (sum-of-list list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum-of-list (rest list)))]
    ))



(define (draw-handler ws)
  (rectangle 1200 600 "outline" "red"))


(define initial-chain
  (make-markov-chain '() 0))
(big-bang initial-chain
          [to-draw draw-handler])