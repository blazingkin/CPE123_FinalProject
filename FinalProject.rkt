;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname FinalProject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Final Project: Markov Audio Generation
;; Team "I feel like a tater tot"
;; Kellie Banzon, Alex Gravenor, Jeffery Ho, Steven Pineda

(require rsound)
(require rsound/single-cycle)
(require 2htdp/universe)
(require 2htdp/image)

;; More dirty hacks
;; Any Any -> Any
;; Takes in 2 values and returns the second
(define (both a b) b)
;; Any Any -> Any
;; Takes in 2 values and returns the first
(define (invboth a b) a)

;; World Definitions

;;A gui-state is a structure:
;; (make-gui-state Number)
;; interpretation: A Gui state has:
;; - Node-selected: an integer corresponding to the index of the selected node
(define-struct gui-state [node-selected])
(define starting-gui-state (make-gui-state 0))

(define-struct markov-node [midi connections])
;; A markov-node is a structure:
;;  (make-markov-node Number list-of-numbers)
;; interpretation: A markov-node has
;; - midi: a integer value that corresponds to a MIDI note number
;; - connections: a list of connection strengths, where the strengths are numbers between 0.0 and 1.0

(define-struct markov-chain [nodes current-node gui])
;; A markov-chain is a structure:
;;  (make-markov-chain list-of-markov-nodes Number)
;; interpretation: A markov-chain has
;; - nodes: a list of markov-nodes in the markov-chain
;; - current-node: a integer value corresponding to the current index of the markov-chain list

;; Variables

;; the audio track the notes will be queued into
(define p (make-pstream))

;; the width and height of the markov-map background
(define map-width 1200)
(define map-height 600)

;; the markov-map is the graphic interface where all the markov-nodes can be viewed and adjusted
;; For now, the markov-map is the red rectangular outline and the numbers representing the note
(define markov-map (rectangle map-width map-height "outline" "white"))

;; the width and height of the connections-box
(define connections-width 300)
(define connections-height 600)
;; the connections-box background 
(define connections-box (rectangle connections-width connections-height "solid" "green"))

;; the nodes are represented as circles on the markov-map, spaced in a larger circle around some center point
;; map-center-x and map-center-y define the x- and y-position of this center point
(define map-center-x (/ map-width 2))
(define map-center-y (/ map-height 2))
;; the distance each node sits from the center point
(define map-radius 200)
;; the radius of the circle that represents a node
(define circle-radius 75)

;;The list of midi note names in order
(define midi-names '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

;;The font size of the text inside each markov node
(define node-text-size 24)

;; Add a markov-node to the nodes field of the markov-chain
;; node: the markov-node to be added
;; chain: the markov-chain to add the markov-node to
;; markov-node markov-chain -> markov-chain
(check-expect (add-node-to-chain (make-markov-node 60 '(1)) (make-markov-chain '() 0 starting-gui-state))
              (make-markov-chain (list (make-markov-node 60 '(1))) 0 starting-gui-state))
;; --------------- ADD CHECK-EXPECTS --------------- * I wasnt sure how to add a second case to check for the random new connections when adding a node *
(define (add-node-to-chain node chain)
  (make-markov-chain (append (alert-all-nodes-add (markov-chain-nodes chain)) (list node)) (markov-chain-current-node chain) (markov-chain-gui chain))
  )

;; Alert all nodes in list-of-markov-nodes of a new node
;; list: the list-of-markov-nodes of a markov-chain
;; list-of-markov-nodes -> list-of-markov-nodes (updates connection field)
;; --------------- ADD CHECK-EXPECTS ---------------
(define (alert-all-nodes-add list)
  (cond
    [(empty? list) '()]
    [else (cons (node-alert-chain-add (first list)) (alert-all-nodes-add (rest list)))]))


;; Add a connection to each of the markov-nodes in the markov-chain
;; Adds a new, random connection to the markov-node, then normalizes all of that markov-node's connections
;; node: the markov-node whose connections are being adjusted
;; markov-node -> markov-node 
;; --------------- ADD CHECK-EXPECTS --------------- (note: how do you check the code when it calls for a random value?)
(define (node-alert-chain-add node)
  (make-markov-node (markov-node-midi node)
                    (normalize-node (append (markov-node-connections node) (list (random)))))
  )

;; Takes a list of connection strengths and returns a normalized list of connection strengths
;; connections: a list of numbers between 0.0 and 1.0 that denote the connection strengths of a markov-node to itself and each other markov-node in the list
;; List-of-Numbers (whose sum is above 1) -> List-of-Numbers (whose sum is 1)
(check-expect (normalize-node (cons 1.0 '())) (cons 1.0 '()))
(check-expect (normalize-node (cons 1.0 (cons 0.5 '()))) (cons 2/3 (cons 1/3 '())))
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

;; updates the current-node (index number) of markov-chain to reflect the next node and changes the "active" node (and thus changes which node is lit up on the markov-map)
;; markov-chain -> number
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

;; Draws the connection strengths and nodes with corresponding name onto the markov-map
(define (draw-handler ws)
  (place-image
   (draw-connections ws 0)
   150
   300
   (draw-circles ws 0))
  )

;; Draws the connections of one particular node to itself and all the other nodes
;; Should always be called with index as 0 (loops through the list-of-markov-nodes starting at index: 0)
;; Index: number representing a certain element of list-of-markov-nodes
;; Markov-Chain -> Image 
(define (draw-connections chain index)
  (cond
    [(= index (length (markov-chain-nodes chain)))
     (place-image (text (string-append "Connections for: " (get-node-name (list-ref (markov-chain-nodes chain) (gui-state-node-selected (markov-chain-gui chain))))) 25 "black")
                  (/ connections-width 2)
                  15
                  connections-box)]
    [else (place-image
           (draw-connection-line (list-ref (markov-chain-nodes chain) (gui-state-node-selected (markov-chain-gui chain))) (list-ref (markov-chain-nodes chain) index) index)
           (/ connections-width 2)
           (+ 50 (* index (/ (- connections-height 50) (length (markov-chain-nodes chain)))))
           (draw-connections chain (add1 index)))]))

;; Draws a single connection line (includes name of the node and name of the other node with a percentage representing the connection strength) in the connections box
;; Node1: The node that the user clicks on
;; Node2: A node in the Markov-Chain at a specific index number
;; Index: number represinting a certain element of the list-of-markov-nodes
(define (draw-connection-line node1 node2 index)
  (text (string-append (get-node-name node1) " <--> "
                       (get-node-name node2) " : "
                       (number->string (round (* 100 (list-ref (markov-node-connections node1) index)))) " %") 20 "black"))

;; Returns the x-coordinate where the node should be drawn (depends on how many markov-nodes are in the list-of-markov-nodes)
;; Index: number representing a certain element of the list-of-markov-nodes
;; Markov-Chain Number -> Number
(define (calc-circle-x chain index)
  (+ map-center-x (* map-radius (cos (- (/ (* 2 pi index) (length (markov-chain-nodes chain))) (/ pi 2) ))))
  )
(check-within (calc-circle-x (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 starting-gui-state) 1)
              (+ map-center-x (* map-radius (cos (- (/ (* 2 pi 1) 2) (/ pi 2))))) 1e-8)
;; Returns the y-coordinate where the node should be drawn
;; Index: number representing a certain element of the list-of-markov-nodes
;; Markov-Chain Number -> Number
(define (calc-circle-y chain index)
  (+ map-center-y (* map-radius (sin (- (/ (* 2 pi index) (length (markov-chain-nodes chain))) (/ pi 2) ))))
  )
(check-within (calc-circle-y (make-markov-chain (list (make-markov-node 40 '(0.5 0.5)) (make-markov-node 70 '(0.7 0.3))) 0 starting-gui-state) 0)
              (+ map-center-y (* map-radius (sin (- (/ (* 2 pi 0) 2) (/ pi 2))))) 1e-8)

;; Draws circles for all markov-nodes in list-of-markov-nodes of markov-chain in a circular pattern
;; Should always be called with index as 0 (loops through the list-of-markov-nodes starting at index: 0)
;; Index: number representing a certain element of list-of-markov-nodes
;; Markov-Chiain Number -> Image
(define (draw-circles chain index)
  (cond
    [(= index (length (markov-chain-nodes chain))) markov-map]
    [else (place-image
           (draw-node (= index (markov-chain-current-node chain)) (list-ref (markov-chain-nodes chain) index))
           (calc-circle-x chain index)
           (calc-circle-y chain index)
           (draw-circles chain (+ 1 index)))]))

;; Generates the name of a node
;; Markov-Node -> String
(define (get-node-name node)
  (string-append (list-ref midi-names (modulo (markov-node-midi node) 12)) (number->string (floor (/ (markov-node-midi node) 12)))))

;; Generates the text to be placed in a circle (indicates note name and octave)
;; Markov-Node -> Image
(define (get-node-text node)
  (text (string-append (list-ref midi-names (modulo (markov-node-midi node) 12)) (number->string (floor (/ (markov-node-midi node) 12)))) node-text-size "white")
  )
(check-expect (get-node-text (make-markov-node 60 '(1)))
                             (text (string-append (list-ref midi-names 0) (number->string 5)) node-text-size "white"))
(check-expect (get-node-text (make-markov-node 79 '(1)))
                             (text (string-append (list-ref midi-names 7) (number->string 6)) node-text-size "white"))

;; Draws a single circle with labeled note name and octave to represent a markov-node and creates a green circle for the "active" node
;; Active: boolean value that is #true when index equals the markov-chain's current node
;; Node: an element of list-of-markov-nodes that will be made into an image
;; Boolean Markov-Node -> Image
(define (draw-node active node)
  (place-image
   (get-node-text node)
   circle-radius
   circle-radius
   (circle circle-radius "solid" (cond
                                   [active "green"]
                                   [else "blue"]))
   )  )
(check-expect (draw-node #false (make-markov-node 55 '(.25 .75)))
              (place-image
               (get-node-text (make-markov-node 55 '(.25 .75)))
               circle-radius
               circle-radius
               (circle circle-radius "solid" "blue")))
(check-expect (draw-node #true (make-markov-node 64 '(.25 .75)))
              (place-image
               (get-node-text (make-markov-node 64 '(.25 .75)))
               circle-radius
               circle-radius
               (circle circle-radius "solid" "green")))
              

;; On each clock tick:
;; Create and return a markov-chain with the same list-of-markov-nodes but a new index
;; Queue the note corresponding to the current node (based on the index)
;; Markov-Chain -> Markov-Chain
(define (tick-handler ws)
  (invboth 
   (make-markov-chain (markov-chain-nodes ws) (get-next-node ws) (markov-chain-gui ws))
   (pstream-queue p (synth-note "main" 35 (markov-node-midi (list-ref (markov-chain-nodes ws) (markov-chain-current-node ws))) (/ FRAME-RATE 5)) (pstream-current-frame p))))


;; Makes a list of random numbers 0 < rand < 1 of a given length
;; Number -> List-Of-Numbers
(define (make-random-list length)
  (cond
    [(= length 0) '()]
    [else (cons (random) (make-random-list (- length 1)))]))

;; Key Handler
;; Adds a random markov-node to the list-of-markov-nodes in the markov-chain when "." is pressed
;; Markov-Chain -> Markov-Chain
(define (key-handler ws ke)
  (cond
    [(key=? ke ".") (add-node-to-chain (make-markov-node (+ 40 (random 40)) (normalize-node (make-random-list (+ 1 (length (markov-chain-nodes ws)))))  ) ws)]
    [else ws])
  )

(define (get-distance x_1 y_1 x_2 y_2)
  (sqrt (+ (sqr (- x_2 x_1)) (sqr (- y_2 y_1)))))
(check-expect (get-distance 0 0 3 4) 5)
(check-expect (get-distance 0 0 0 0) 0)
(check-expect (get-distance 0 1 0 0) 1)

;;This takes - A x and y position, 0, the list of nodes, 0
;;X pos, Y pos, 0, markov-chain, 0 -> The index of the highest node
(define (find-closest x y index chain prevlowest)
  (cond
    [(= index (length (markov-chain-nodes chain))) prevlowest]
    [else
     (cond
       [(< (get-distance x y (calc-circle-x chain index) (calc-circle-y chain index))
           (get-distance x y (calc-circle-x chain prevlowest) (calc-circle-y chain prevlowest))) (find-closest x y (add1 index) chain index)]
       [else (find-closest x y (add1 index) chain prevlowest)])
     ]))

(define (click-handler ws x y me)
  (cond
    [(string=? me "button-down") (cond
                                  [(< (get-distance x y (calc-circle-x ws (find-closest x y 0 ws 0)) (calc-circle-y ws (find-closest x y 0 ws 0))) circle-radius)
                                   (make-markov-chain (markov-chain-nodes ws) (markov-chain-current-node ws) (make-gui-state (find-closest x y 0 ws 0)))]
                                  [else ws])]
    [else ws]))

;;An example of an initial-chain (a world state)
#|(define initial-chain
  (add-node-to-chain (make-markov-node 79 '(.1 .1 .2 .2 .2 .2))
  (add-node-to-chain (make-markov-node 76 '(.2 .2 .2 .2 .2))
  (add-node-to-chain (make-markov-node 72 '(.25 .25 .25 .25))
  (add-node-to-chain (make-markov-node 67 '(.2 .25 .5))
  (add-node-to-chain (make-markov-node 64 '(.5 .5))
  (add-node-to-chain (make-markov-node 60 '(1)) (make-markov-chain '() 0 starting-gui-state))))))))
|#

;; The initial state of the world is a markov-chain starting with this list-of-markov-nodes and these connections (This sample initial-chain is from mary had a little lamb)
(define initial-chain
  (make-markov-chain (list 
(make-markov-node 52 (list 0 0 1.0 0 0 0 ))
(make-markov-node 55 (list 0 0 0 0.42857142857142855 0.5 0.07142857142857142 ))
(make-markov-node 60 (list 0.3333333333333333 0 0.3333333333333333 0.3333333333333333 0 0 ))
(make-markov-node 62 (list 0 0.3 0.15 0.35 0.2 0 ))
(make-markov-node 64 (list 0 0.3181818181818182 0 0.22727272727272727 0.4090909090909091 0.045454545454545456 ))
(make-markov-node 67 (list 0 0.25 0 0 0.25 0.5 ))
) 0 starting-gui-state)
)

;; The world state of this big-bang is a markov-chain
(big-bang initial-chain
          [to-draw draw-handler]
          [on-tick tick-handler 1/5]
          [on-key key-handler]
          [on-mouse click-handler])
