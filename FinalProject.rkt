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

;; END OF DEPENDENCIES







;; More dirty hacks
;; Any Any -> Any
;; Takes in 2 values and returns the second
(define (both a b) b)
;; Any Any -> Any
;; Takes in 2 values and returns the first
(define (invboth a b) a)

;; World Definitions


(define-struct gui-state [node-selected connection-selected tick-rate tick-count])
;;A gui-state is a structure:
;; (make-gui-state Number)
;; interpretation: A Gui state has:
;; - Node-selected: an integer corresponding to the index of the selected node

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

;;END OF WORLD DEFINITIONS







;; Variables

;; the audio track the notes will be queued into
(define p (make-pstream))

;; the width and height of the markov-map background
(define map-width 1200)
(define map-height 600)

;;The gui-state that the program starts in and will be used for some check-expect type things
(define starting-gui-state (make-gui-state 0 0 2 0))

(define max-tick 10)

;; the markov-map is the graphic interface where all the markov-nodes can be viewed and adjusted
;; For now, the markov-map is the red rectangular outline and the numbers representing the note
(define markov-map (rectangle map-width map-height "outline" "white"))

;; the width and height of the connections-box
(define connections-width 300)
(define connections-height 600)
;; the connections-box background 
(define connections-box (rectangle connections-width connections-height "solid" "blue"))

;; the nodes are represented as circles on the markov-map, spaced in a larger circle around some center point
;; map-center-x and map-center-y define the x- and y-position of this center point
(define map-center-x (/ map-width 2))
(define map-center-y (/ map-height 2))
;; the distance each node sits from the center point
(define map-radius 200)
;; the radius of the circle that represents a node
(define circle-radius 60)

;;The list of midi note names in order
(define midi-names '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

;;The font size of the text inside each markov node
(define node-text-size 24)

;;END OF VARIABLES







;; Makes a list of random numbers 0 < rand < 1 of a given length
;; Number -> List-Of-Numbers
(define (make-random-list length)
  (cond
    [(= length 0) '()]
    [else (cons (random) (make-random-list (- length 1)))]))

;;Sets a particular index of a list to value
;;List, Number, Value -> List
;;If the index is out of range of the list, the original list will be returned
(define (list-set list index value)
  (cond
    [(empty? list) '()]
    [(= index 0) (cons value (list-set (rest list) -1 value))]
    [else (cons (first list) (list-set (rest list) (sub1 index) value))]))
(check-expect (list-set (list 1 2 3) 0 0) (list 0 2 3))
(check-expect (list-set '() 0 0) '())
(check-expect (list-set (list 3 4 5) 1 1) (list 3 1 5))

;; computes the sum of all elements in a list
;; list: a list of numbers
;; List-of-Numbers -> Number
(define (sum-of-list list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum-of-list (rest list)))]
    ))
(check-expect (sum-of-list (list 1 2 3)) 6)
(check-expect (sum-of-list (list 0 1 0 1 0 1)) 3)
(check-expect (sum-of-list '()) 0)

;;Finds the Euclidean distance between two points
(define (get-distance x_1 y_1 x_2 y_2)
  (sqrt (+ (sqr (- x_2 x_1)) (sqr (- y_2 y_1)))))
(check-expect (get-distance 0 0 3 4) 5)
(check-expect (get-distance 0 0 0 0) 0)
(check-expect (get-distance 0 1 0 0) 1)

;;Update GUI connection
(define (update-gui-connection gui newcon)
  (make-gui-state (gui-state-node-selected gui) newcon (gui-state-tick-rate gui) (gui-state-tick-count gui)))

;;Update GUI selection
(define (update-gui-node gui newnode)
  (make-gui-state newnode (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui)))

;;Update GUI Tick Rate
(define (update-gui-rate gui tickrate)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) tickrate (gui-state-tick-count gui)))

;;Update GUI Tick Count
(define (update-gui-tickcount gui tickcount)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) tickcount))

;;Update GUI
(define (update-gui chain newgui)
  (make-markov-chain (markov-chain-nodes chain) (markov-chain-current-node chain) newgui))


;;END OF UTILITY FUNCTIONS








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


;;Updates a single connection in a node and then normalizes the rest of the node
;;Markov-Chain, Int, Int, Num -> Markov-Chain
;;Chain: The chain to be updated
;;Node: The index of the node to be changed
;;Connection: The index of the connection to be changed
;;Delta: The amount to change it
;;Note - This function will never let a connection go below 0
(define (change-connection chain node connection delta)
  (make-markov-chain (list-set (markov-chain-nodes chain)
                               node
                               (make-markov-node (markov-node-midi (list-ref (markov-chain-nodes chain) node)) (normalize-node (list-set
                                                (markov-node-connections (list-ref (markov-chain-nodes chain) node))
                                                connection
                                                (max 0 (+ delta (list-ref (markov-node-connections (list-ref (markov-chain-nodes chain) node)) connection)))))
                               ))
                     (markov-chain-current-node chain) (markov-chain-gui chain)))


;;END OF MARKOV CHAIN FUNCTIONS








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
     (place-image (text (string-append "Connections for: " (get-node-name (list-ref (markov-chain-nodes chain) (gui-state-node-selected (markov-chain-gui chain))))) 25 "white")
                  (/ connections-width 2)
                  15
                  connections-box)]
    [else (place-image
           (draw-connection-line (list-ref (markov-chain-nodes chain) (gui-state-node-selected (markov-chain-gui chain))) (list-ref (markov-chain-nodes chain) index) index (gui-state-connection-selected (markov-chain-gui chain)))
           (/ connections-width 2)
           (+ 50 (* index (/ (- connections-height 50) (length (markov-chain-nodes chain)))))
           (draw-connections chain (add1 index)))]))

;; Draws a single connection line (includes name of the node and name of the other node with a percentage representing the connection strength) in the connections box
;; Node1: The node that the user clicks on
;; Node2: A node in the Markov-Chain at a specific index number
;; Index: number represinting a certain element of the list-of-markov-nodes
;; Selected: The index of the selected connection
(define (draw-connection-line node1 node2 index selected)
  (text (string-append  (get-node-name node1) " → "
                       (get-node-name node2) " : "
                       (number->string (round (* 100 (list-ref (markov-node-connections node1) index)))) " %") 20 (cond
                         [(= index selected) "black"]
                         [else "white"])))

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
;; Markov-Chain Number -> Image
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

;;END OF DRAW LOGIC







;; On each clock tick:
;; Create and return a markov-chain with the same list-of-markov-nodes but a new index
;; Queue the note corresponding to the current node (based on the index)
;; Markov-Chain -> Markov-Chain
(define (tick-handler ws)
  (cond
    
  [(= (gui-state-tick-rate (markov-chain-gui ws)) (gui-state-tick-count (markov-chain-gui ws)))
   (invboth 
   (make-markov-chain (markov-chain-nodes ws) (get-next-node ws) (update-gui-rate (update-gui-tickcount (markov-chain-gui ws) 0) (max 1 (min max-tick (+ (gui-state-tick-rate (markov-chain-gui ws)) (- (random 1 5) 3))))))
   (pstream-queue p (synth-note "main" 35 (markov-node-midi (list-ref (markov-chain-nodes ws) (markov-chain-current-node ws))) (* (/ FRAME-RATE 5) (gui-state-tick-rate (markov-chain-gui ws)))) (pstream-current-frame p)))]
  [else
   (update-gui ws (update-gui-tickcount (markov-chain-gui ws) (add1 (gui-state-tick-count (markov-chain-gui ws)))))
   ]))

;;END TICK HANDLER










;; Key Handler
;; Adds a random markov-node to the list-of-markov-nodes in the markov-chain when "." is pressed
;; Markov-Chain -> Markov-Chain
(define (key-handler ws ke)
  (cond
    ;;For the input "." add new random nodes to the chain
    [(key=? ke ".") (add-node-to-chain (make-markov-node (+ 40 (random 40)) (normalize-node (make-random-list (+ 1 (length (markov-chain-nodes ws)))))  ) ws)]
    ;;For the left and right arrow keys, adjust the value of the selected connection's weight
    [(key=? ke "left") (change-connection ws (gui-state-node-selected (markov-chain-gui ws)) (gui-state-connection-selected (markov-chain-gui ws)) -.1)]
    [(key=? ke "right") (change-connection ws (gui-state-node-selected (markov-chain-gui ws)) (gui-state-connection-selected (markov-chain-gui ws)) .1)]
    ;;For the up and down arrow keys, change which node is selected
    [(key=? ke "up") (make-markov-chain (markov-chain-nodes ws) (markov-chain-current-node ws)
                                        (update-gui-connection (markov-chain-gui ws) (max 0 (- (gui-state-connection-selected (markov-chain-gui ws)) 1))))]
    [(key=? ke "down") (make-markov-chain (markov-chain-nodes ws) (markov-chain-current-node ws)
                                        (update-gui-connection (markov-chain-gui ws) (min (sub1 (length (markov-chain-nodes ws))) (+ (gui-state-connection-selected (markov-chain-gui ws)) 1))))]
    [(key=? ke "a") frosty]
    [(key=? ke "s") jingle-bells]
    [(key=? ke "d") misirlou]
    [(key=? ke "f") ghostbusters]
    [else ws])
  )

;;END OF KEY HANDLER







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
(check-expect (find-closest 0 0 0 initial-chain 0) 5)
(check-expect (find-closest 400 400 0 initial-chain 0) 4)

(define (click-handler ws x y me)
  (cond
    [(string=? me "button-down") (cond
                                  [(< (get-distance x y (calc-circle-x ws (find-closest x y 0 ws 0)) (calc-circle-y ws (find-closest x y 0 ws 0))) circle-radius)
                                   (update-gui ws (update-gui-node (markov-chain-gui ws) (find-closest x y 0 ws 0)))]
                                  [else ws])]
    [else ws]))

;;END OF CLICK HANDLER







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


(define frosty
  (make-markov-chain (list
(make-markov-node 35 (list 0 0 11/201 26/201 10/67 9/67 16/201 14/67 26/201 11/201 4/67 ))
(make-markov-node 38 (list 1/149 6/149 39/149 7/149 29/149 7/149 12/149 13/149 19/149 12/149 4/149 ))
(make-markov-node 42 (list 131/273 145/546 31/546 19/546 5/273 5/273 2/273 1/26 1/26 17/546 1/91 ))
(make-markov-node 52 (list 0 5/99 23/99 1/99 38/99 4/99 0 7/33 1/99 1/99 5/99 ))
(make-markov-node 55 (list 0 1/50 3/40 49/200 9/200 1/50 3/40 81/200 11/200 3/200 9/200 ))
(make-markov-node 57 (list 0 1/116 9/116 3/58 3/116 2/29 0 35/58 3/58 11/116 1/58 ))
(make-markov-node 59 (list 0 1/20 1/60 1/10 17/60 0 11/60 0 2/15 3/20 1/12 ))
(make-markov-node 60 (list 0 1/173 13/346 27/346 73/346 29/173 1/346 11/173 41/173 49/346 19/346 ))
(make-markov-node 64 (list 0 2/221 15/221 20/221 42/221 8/221 7/221 83/221 6/221 1/221 37/221 ))
(make-markov-node 65 (list 0 1/98 13/98 3/98 3/49 13/98 13/98 22/49 0 2/49 1/98 ))
(make-markov-node 67 (list 0 1/136 1/68 1/68 27/136 3/136 11/136 13/136 8/17 3/34 1/136 ))
) 0 starting-gui-state))


(define jingle-bells
  (make-markov-chain (list
(make-markov-node 28 (list 0 0 0 0 0 0 0 0 0 ))
(make-markov-node 29 (list 25/171 1/38 5/171 25/38 2/171 1/38 13/342 1/171 10/171 ))
(make-markov-node 30 (list 0 2/149 0 0 80/149 28/149 36/149 3/149 0 ))
(make-markov-node 44 (list 0 0 1 0 0 0 0 0 0 ))
(make-markov-node 60 (list 3/152 4/57 0 0 5/57 29/114 1/4 13/114 31/152 ))
(make-markov-node 65 (list 5/254 6/127 0 0 239/508 61/508 49/254 9/254 29/254 ))
(make-markov-node 69 (list 13/537 7/179 0 0 107/537 235/537 110/537 7/179 10/179 ))
(make-markov-node 72 (list 8/207 1/23 0 0 35/207 25/207 88/207 8/207 34/207 ))
(make-markov-node 83 (list 61/204 101/204 0 1/136 11/204 19/408 19/408 7/408 7/204 ))
) 0 starting-gui-state))

(define misirlou
  (make-markov-chain (list
(make-markov-node 38 (list 11/207 17/207 25/207 29/207 8/69 5/69 20/207 10/69 11/207 25/207 ))
(make-markov-node 40 (list 1/242 59/121 3/22 13/242 13/242 2/121 29/242 6/121 1/242 9/121 ))
(make-markov-node 42 (list 27/53 20/371 12/371 4/53 12/371 16/371 10/371 26/371 16/371 6/53 ))
(make-markov-node 47 (list 1/118 8/177 11/59 259/354 2/177 0 2/177 1/354 1/354 0 ))
(make-markov-node 52 (list 1/372 1/62 3/31 1/372 59/186 11/62 14/93 35/186 0 3/62 ))
(make-markov-node 59 (list 0 8/281 16/281 3/281 70/281 37/281 87/281 47/281 0 13/281 ))
(make-markov-node 64 (list 0 7/388 37/388 1/388 35/194 75/388 37/194 24/97 5/194 9/194 ))
(make-markov-node 68 (list 1/150 1/75 8/75 2/225 11/90 77/450 43/225 149/450 1/90 17/450 ))
(make-markov-node 69 (list 0 1/73 29/146 0 1/73 1/73 5/146 5/73 87/146 9/146 ))
(make-markov-node 71 (list 0 2/121 79/363 1/363 14/363 1/33 13/363 19/363 2/121 214/363 ))
) 0 starting-gui-state))

(define ghostbusters
  (make-markov-chain (list
(make-markov-node 35 (list 0 41/151 0 0 101/151 0 9/151 0 0 0 0 ))
(make-markov-node 36 (list 3/172 1/344 3/86 3/86 41/172 17/344 17/172 5/172 7/86 109/344 33/344 ))
(make-markov-node 38 (list 1/99 1/22 1/198 53/198 71/198 0 13/198 0 0 2/9 5/198 ))
(make-markov-node 40 (list 0 1/44 1/22 1/88 69/88 0 0 1/44 1/88 3/88 3/44 ))
(make-markov-node 42 (list 1/580 197/580 7/29 1/145 1/5 1/145 3/290 27/580 3/58 53/580 1/290 ))
(make-markov-node 45 (list 0 29/80 0 0 93/160 0 0 0 3/160 3/160 3/160 ))
(make-markov-node 54 (list 19/87 25/87 14/87 0 5/87 1/87 14/87 0 5/87 2/87 2/87 ))
(make-markov-node 64 (list 0 1/61 0 10/61 0 19/61 0 0 26/61 5/61 0 ))
(make-markov-node 69 (list 0 0 0 3/100 0 29/50 1/20 1/5 1/25 1/25 3/50 ))
(make-markov-node 71 (list 35/366 1/366 17/183 11/122 12/61 23/366 4/183 5/366 0 58/183 13/122 ))
(make-markov-node 74 (list 5/37 0 7/111 4/111 0 2/37 7/111 0 0 29/111 43/111 ))
) 0 starting-gui-state))

;;END OF CHAIN DEFINITIONS






;; The world state of this big-bang is a markov-chain
(big-bang initial-chain
          [to-draw draw-handler]
          [on-tick tick-handler 1/10]
          [on-key key-handler]
          [on-mouse click-handler])
