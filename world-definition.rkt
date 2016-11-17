;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname world-definition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Final Project: Markov Audio Generation
;; Team "I feel like a tater tot"
;; Kellie Banzon, Alex Gravenor, Jeffery Ho, Steven Pineda

;; World Definitions

;; The world state for big-bang is a markov-chain

(define-struct markov-node [midi connections])
;; A markov-node is a structure:
;;  (make-markov-node Number list-of-numbers)
;; interpretation: A markov-node has
;; - midi: a integer value that corresponds to a MIDI note number
;; - connections: a list of connection strengths, where the strengths are numbers between 0.0 and 1.0

(define-struct markov-chain [nodes current-node])
;; A markov-chain is a structure:
;;  (make-markov-chain list-of-markov-nodes Number)
;; interpretation: A markov-chain has
;; - nodes: a list of markov-nodes in the markov-chain
;; - current-node: a integer value corresponding to the current index of the markov-chain list