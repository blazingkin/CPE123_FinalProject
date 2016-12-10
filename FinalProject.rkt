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
(require slideshow/text)

;; END OF DEPENDENCIES







;; More dirty hacks
;; Any Any -> Any
;; Takes in 2 values and returns the second
(define (both a b) b)
;; Any Any -> Any
;; Takes in 2 values and returns the first
(define (invboth a b) a)

;; World Definitions


(define-struct gui-state [node-selected connection-selected tick-rate tick-count volume in-help paused beat-count])
;;A gui-state is a structure:
;; (make-gui-state Number Number Number Number Number Boolean Boolean Number)
;; interpretation: A Gui state has:
;; - node-selected: an integer corresponding to the index of the selected node
;; - connection-selected: the index of the currently selected connection
;; - tick-rate: the length of the current note in ticks
;; - tick-count: the current "tick-index" of the note being played
;;               the tick-count starts at 0 and increments every time the clock ticks
;;               when tick-count = tick-rate, the current note has finished playing, and tick-count resets to 0
;; - volume: a number between 0.0 and 1.0 that reflects the volume of the audio
;; - in-help: a boolean that represents whether the help menu should be shown or not
;; - paused: a boolean that represents if the program is paused or not
;; - beat-count: a number that represents when the background track should be queued

(define-struct markov-node [midi connections])
;; A markov-node is a structure:
;;  (make-markov-node Number list-of-numbers)
;; interpretation: A markov-node has
;; - midi: a integer value that corresponds to a MIDI note number
;; - connections: a list of connection strengths, where the strengths are numbers between 0.0 and 1.0

(define-struct markov-chain [nodes current-node gui])
;; A markov-chain is a structure:
;;  (make-markov-chain list-of-markov-nodes Number gui-state)
;; interpretation: A markov-chain has
;; - nodes: a list of markov-nodes in the markov-chain
;; - current-node: a integer value corresponding to the current index of the markov-chain list
;; - gui: a gui-state

;;END OF WORLD DEFINITIONS







;; Variables

;; the audio track the notes will be queued into
(define p (make-pstream))

;; the width and height of the markov-map background
(define map-width 1200)
(define map-height 600)

;;The gui-state that the program starts in and will be used for some check-expect type things
(define starting-gui-state (make-gui-state 0 0 2 0 0.5 #t #f 0))


;; the maximum possible length of a note (in ticks) note-length
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

;; the width and height of the volume rectangle
(define vol-bg-height 30)
(define vol-bg-width 190)
;; the volume background
(define vol-bg (rectangle vol-bg-width vol-bg-height "solid" "gray"))
;; the width and height of the volume slider
(define vol-slider-height 25)
(define vol-slider-width 30)
;; the volume slider box
(define vol-slider (rectangle vol-slider-width vol-slider-height "solid" "black"))

;;The list of midi note names in order
(define midi-names '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

;;The font size of the text inside each markov node
(define node-text-size 24)

;; gray mute button for when the volume is on
(define mute-gray (place-image (text "Mute" 12 "black") 25 15 (rectangle 50 30 "solid" "gray")))
;; red mute button for when the volume is muted
(define mute-red (place-image (text "Mute" 12 "black") 25 15 (rectangle 50 30 "solid" "red")))

;; reset button
(define reset-button (place-image (text "Reset" 12 "black") 25 15 (rectangle 50 30 "solid" "gray")))

;; the buttons for the pre-loaded audio tracks
(define frosty-false (place-image (text "Frosty the Snowman" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define frosty-true (place-image (text "Frosty the Snowman" 12 "white") 65 15 (rectangle 130 30 "solid" "CornflowerBlue")))
(define jingle-bells-false (place-image (text "Jingle Bells" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define jingle-bells-true (place-image (text "Jingle Bells" 12 "white") 65 15 (rectangle 130 30 "solid" "ForestGreen")))
(define misirlou-false (place-image (text "Misirlou" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define misirlou-true (place-image (text "Misirlou" 12 "white") 65 15 (rectangle 130 30 "solid" "DarkOrange")))
(define ghostbusters-false (place-image (text "Ghostbusters" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define ghostbusters-true (place-image (text "Ghostbusters" 12 "white") 65 15 (rectangle 130 30 "solid" "olive")))
(define ode-false (place-image (text "Ode to Joy" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define ode-true (place-image (text "Ode to Joy" 12 "white") 65 15 (rectangle 130 30 "solid" "violet")))
(define adele-false (place-image (text "Someone Like You" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define adele-true (place-image (text "Someone Like You" 12 "white") 65 15 (rectangle 130 30 "solid" "aquamarine")))
(define hbday-false (place-image (text "Happy Birthday" 12 "black") 65 15 (rectangle 130 30 "solid" "gray")))
(define hbday-true (place-image (text "Happy Birthday" 12 "white") 65 15 (rectangle 130 30 "solid" "pink")))


;;The Paused Indicator
(define paused (place-image
  (text "Paused" 20 "black")
  100
  100
  (rectangle 200 200 "solid" "gray")))

;;The background for the help screen
(define help-background (rectangle map-width map-height "solid" "white"))
;; the help button
(define help-button (place-image (bold (text "?" 12 "black")) 15 15 (rectangle 30 30 "solid" "gray")))

;;The text that shows on startup in reverse order of this list
(define intro-text (list "Click the ? in the bottom right or press H to toggle this help menu."
                         "Click the reset button or press R to reset the program to its original launch state."
                         "Click the mute button in the top right or press M to Mute"
                         "The volume slider is in the top right."
                         "Some preset songs are available by clicking the buttons on the right or pressing A, S, D or F."
                         "You can press . to add a new random node."
                         "The selected connection will be red, the connection going the other way will be purple."
                         "Use the ↑ and ↓ keys to scroll and the ← and → to change the node's connection strength."
                         "Click any node to select it."
                         "CONTROLS:"
                         " "
                         " "
                         "and load pre-programmed probabilities to hear how the audio changes."
                         "then generates audio based on those probabilities. You can also change those probabilities, add more notes,"
                         "Our program analyzes known music to find the probabilities of one musical note occurring after another,"
                         "If you click anywhere... You'll be brought to the program!!!"
                         "WELCOME TO THE WORLD OF TOMORROW!!!"))

;;The widest the connection line can be
(define max-line-width 10)
;;The color of the connection lines
(define connection-line-color "blue")
;;The color of the selected connection
(define selected-connection-line-color "red")
;;The color of the selected connection that goes the other way
(define inverse-selected-connection-line-color "purple")

;;END OF VARIABLES







;; Makes a list of random numbers 0 < rand < 1 of a designated length
;; Number -> List-Of-Numbers
(check-expect (make-random-list 0) '())
; Given: (make-random-list 2) Expect: list of 2 random numbers between 0 and 1
(define (make-random-list length)
  (cond
    [(= length 0) '()]
    [else (cons (random) (make-random-list (- length 1)))]))

;;Sets a particular index of a list to specified value
;;If the index is out of range of the list, the original list will be returned
;; list: a list of values
;; index: the index of the value to be changed
;; value: the new value to replace the original
;; List Number Value -> List
(define (list-set list index value)
  (cond
    [(empty? list) '()]
    [(= index 0) (cons value (list-set (rest list) -1 value))]
    [else (cons (first list) (list-set (rest list) (sub1 index) value))]))
(check-expect (list-set (list 1 2 3) 0 0) (list 0 2 3))
(check-expect (list-set '() 0 0) '())
(check-expect (list-set (list 3 4 5) 1 1) (list 3 1 5))
(check-expect (list-set (list 0 1 2 3 4) 1 4) (list 0 4 2 3 4))

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
;; x_1: the x-coordinate of the first point
;; y_1: the y-coordinate of the first point
;; x_2: the x-coordinate of the second point
;; y_2: the y-coordinate of the second point
;; number number number number -> number
(define (get-distance x_1 y_1 x_2 y_2)
  (sqrt (+ (sqr (- x_2 x_1)) (sqr (- y_2 y_1)))))
(check-expect (get-distance 0 0 3 4) 5)
(check-expect (get-distance 0 0 0 0) 0)
(check-expect (get-distance 0 1 0 0) 1)

;;Update GUI selection
;; gui-state number -> gui-state
(define (update-gui-node gui newnode)
  (make-gui-state newnode (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui) (gui-state-volume gui) (gui-state-in-help gui) (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-node (make-gui-state 0 0 2 0 0.5 #t #t 0) 2) (make-gui-state 2 0 2 0 0.5 #t #t 0))

;;Update GUI connection
;; gui-state number -> gui-state
(define (update-gui-connection gui newcon)
  (make-gui-state (gui-state-node-selected gui) newcon (gui-state-tick-rate gui) (gui-state-tick-count gui) (gui-state-volume gui) (gui-state-in-help gui) (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-connection (make-gui-state 0 0 2 0 0.5 #t #f 0) 3) (make-gui-state 0 3 2 0 0.5 #t #f 0))

;;Update GUI Tick Rate
;; gui-state number -> gui-state
(define (update-gui-rate gui tickrate)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) tickrate (gui-state-tick-count gui) (gui-state-volume gui) (gui-state-in-help gui) (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-rate (make-gui-state 0 0 2 0 0.5 #t #f 0) 4) (make-gui-state 0 0 4 0 0.5 #t #f 0))

;;Update GUI Tick Count
;; gui-state number -> gui-state
(define (update-gui-tickcount gui tickcount)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) tickcount (gui-state-volume gui) (gui-state-in-help gui) (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-tickcount (make-gui-state 0 0 2 0 0.5 #t #f 0) 1) (make-gui-state 0 0 2 1 0.5 #t #f 0))

;;Update GUI Volume
;; gui-state number -> gui-state
(define (update-gui-volume gui volume)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui) volume (gui-state-in-help gui) (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-volume (make-gui-state 0 0 2 0 0.5 #t #f 0) 0.8) (make-gui-state 0 0 2 0 0.8 #t #f 0))

;;Update GUI Help Status
;; gui-state boolean -> gui-state
(define (update-gui-in-help gui in-help)
  (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui) (gui-state-volume gui) in-help (gui-state-paused gui) (gui-state-beat-count gui)))
(check-expect (update-gui-in-help (make-gui-state 0 0 2 0 0.5 #f #f 0) #t) (make-gui-state 0 0 2 0 0.5 #t #f 0))

;;Update GUI Paused Status
;; gui-state, boolean -> gui-state
(define (update-gui-paused gui paused)
    (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui) (gui-state-volume gui) (gui-state-in-help gui) paused (gui-state-beat-count gui)))
(check-expect (update-gui-paused (make-gui-state 0 0 2 0 0.5 #f #f 0) #t) (make-gui-state 0 0 2 0 0.5 #f #t 0))

;;Update GUI Beat Count Status
;; gui-state, number -> gui-state
(define (update-gui-beat-count gui beat)
    (make-gui-state (gui-state-node-selected gui) (gui-state-connection-selected gui) (gui-state-tick-rate gui) (gui-state-tick-count gui) (gui-state-volume gui) (gui-state-in-help gui) (gui-state-paused gui) beat))
(check-expect (update-gui-beat-count (make-gui-state 0 0 2 0 0.5 #f #f 0) 1) (make-gui-state 0 0 2 0 0.5 #f #f 1))


;;Update GUI
;; markov-chain gui-state -> markov-chain
(define (update-gui chain newgui)
  (make-markov-chain (markov-chain-nodes chain) (markov-chain-current-node chain) newgui))
(check-expect (update-gui (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 starting-gui-state) (make-gui-state 0 3 2 0 0.9 #t #f 0))
              (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 3 2 0 0.9 #t #f 0)))

;; mute or unmute the volume
;; if the volume is already 0.0: change it to 0.5
;; otherwise: change the volume to 0.0
;; markov-chain -> markov-chain
(define (mute ws)
  (cond
    [(= (gui-state-volume (markov-chain-gui ws)) 0.0) (update-gui ws (update-gui-volume (markov-chain-gui ws) 0.5))]
    [else (update-gui ws (update-gui-volume (markov-chain-gui ws) 0.0))]))
(check-expect (mute (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 3 2 0 0.9 #t #f 0)))
                    (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 3 2 0 0.0 #t #f 0)))
(check-expect (mute (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 3 2 0 0.0 #t #f 0)))
                    (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 3 2 0 0.5 #t #f 0)))

;; reset the markov-chain to its initial state upon program launch
;; markov-chain -> markov-chain
(define (reset ws)
  (update-gui initial-chain (update-gui-in-help starting-gui-state #f)))
(check-expect (reset frosty) (update-gui initial-chain (update-gui-in-help starting-gui-state #f)))
(check-expect (reset (make-markov-chain (list (make-markov-node 60 '(1))) 0 starting-gui-state)) (update-gui initial-chain (update-gui-in-help starting-gui-state #f)))

;; show the help menu
;; markov-chain -> markov-chain
(define (show-help ws)
  (update-gui ws (update-gui-in-help (markov-chain-gui ws) (not (gui-state-in-help (markov-chain-gui ws))))))
(check-expect (show-help initial-chain) (update-gui initial-chain (update-gui-in-help (markov-chain-gui initial-chain) #f)))
(check-expect (show-help frosty) (update-gui frosty (update-gui-in-help (markov-chain-gui frosty) #t)))

;;END OF UTILITY FUNCTIONS








;; Add a markov-node to the nodes field of the markov-chain
;; node: the markov-node to be added
;; chain: the markov-chain to add the markov-node to
;; markov-node markov-chain -> markov-chain
(check-expect (add-node-to-chain (make-markov-node 60 '(1)) (make-markov-chain '() 0 starting-gui-state))
              (make-markov-chain (list (make-markov-node 60 '(1))) 0 starting-gui-state))
;; Given: (add-node-to-chain (make-markov-node 40 '(0.5 0.5)) (make-markov-chain (list (make-markov-node 60 '(1))) 0 starting-gui-state))
;; Expect: (make-markov-chain (list (make-markov-node 60 (list 1/(1+random) (random)/(1+random))) (make-markov-node 40 (list 0.5 0.5))) 0 (make-gui-state 0 0 2 0 0.5)) --FIX THIS--
;; where random represents the random number between 0 and 1 that is being added as a connection
(define (add-node-to-chain node chain)
  (make-markov-chain (append (alert-all-nodes-add (markov-chain-nodes chain)) (list node)) (markov-chain-current-node chain) (markov-chain-gui chain))
  )

;; Alert all nodes in list-of-markov-nodes of a new node
;; list: the list-of-markov-nodes of a markov-chain
;; list-of-markov-nodes -> list-of-markov-nodes (updates connection field)
;; Given: (alert-all-nodes-add (list (make-markov-node 50 '(1)))) Expect: (list (make-markov-node 50 (list 1/(1 + random) (random)/(1 + random))) Numbers in list add up to 1
(define (alert-all-nodes-add list)
  (cond
    [(empty? list) '()]
    [else (cons (node-alert-chain-add (first list)) (alert-all-nodes-add (rest list)))]))


;; Add a connection to each of the markov-nodes in the markov-chain
;; Adds a new, random connection to the markov-node, then normalizes all of that markov-node's connections
;; node: the markov-node whose connections are being adjusted
;; markov-node -> markov-node 
;; Given: (node-alert-chain-add (make-markov-node 30 '(1))) Expect: (make-markov-node 30 (list 1/(1 + random) (random)/(1 + random))) Numbers in list add up to 1
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
;; Given: (get-next-node (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.4 0.6))) 1 starting-gui-state))
;; Expect: 0 if the random number generated is < 0.4, 1 if the random number generated is >= 0.4
(define (get-next-node chain)
  (pick-node-based-on-weights (markov-node-connections (list-ref (markov-chain-nodes chain) (markov-chain-current-node chain))) 0 (random))
  )

;; Call this with the node's weights, 0 and a random floating point number in 0 < rand < 1
;; Given a list of connections for a specific node, generates a random number between 0 and 1 and determines which range of numbers this random number falls into.
;; Returns the index number with the correct range.
;; List-of-numbers, 0, 0 < rand < 1 -> Number
(check-expect (pick-node-based-on-weights '() 0 0.33) 0)
(check-expect (pick-node-based-on-weights (list 0.1 0.3 0.6) 0 0.09) 0)
(check-expect (pick-node-based-on-weights (list 0.1 0.3 0.6) 0 0.1) 1)
(check-expect (pick-node-based-on-weights (list 0.1 0.3 0.6) 0 0.23) 1)
(check-expect (pick-node-based-on-weights (list 0.1 0.3 0.6) 0 0.85) 2)
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
(check-expect (change-connection (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.4 0.6))) 0 starting-gui-state) 0 1 0.4)
              (make-markov-chain (list (make-markov-node 50 (list 0.5 0.5)) (make-markov-node 30 (list 0.4 0.6))) 0 starting-gui-state))
(check-expect (change-connection (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state) 1 1 0.2)
              (make-markov-chain (list (make-markov-node 50 (list 0.7 0.3)) (make-markov-node 30 (list 0.25 0.75))) 0 starting-gui-state))
(define (change-connection chain node connection delta)
  (make-markov-chain (list-set (markov-chain-nodes chain)
                               node
                               (make-markov-node (markov-node-midi (list-ref (markov-chain-nodes chain) node)) (normalize-node (list-set
                                                (markov-node-connections (list-ref (markov-chain-nodes chain) node))
                                                connection
                                                (max 0 (+ delta (list-ref (markov-node-connections (list-ref (markov-chain-nodes chain) node)) connection)))))
                               ))
                     (markov-chain-current-node chain) (markov-chain-gui chain)))

;; checks if two markov-chains are exactly equal
;; markov-chain markov-chain -> boolean
(define (markov-chain-exact=? ws wt)
  (equal? ws wt))
(check-expect (markov-chain-exact=? (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state)
                                    (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state))
              #t)
(check-expect (markov-chain-exact=? (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state)
                                    (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state))
              #f)

;; checks if two markov-chains are equal
;; only compares markov-nodes of markov-chain, ignores current-node and gui-state
;; markov-chain markov-chain -> boolean
(define (markov-chain=? ws wt)
  (equal? (markov-chain-nodes ws) (markov-chain-nodes wt)))
(check-expect (markov-chain=? (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state)
                              (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state))
              #t)
(check-expect (markov-chain=? (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 (make-gui-state 0 0 2 0 0.5 #t #f 0))
                              (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 3 (make-gui-state 2 4 2 0 0.5 #t #f 0)))
              #t)
(check-expect (markov-chain=? (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state)
                              (make-markov-chain (list (make-markov-node 50 '(0.7 0.3)) (make-markov-node 30 '(0.3 0.7))) 0 starting-gui-state))
              #f)

;;END OF MARKOV CHAIN FUNCTIONS








;; Draws the connection strengths, nodes, and volume slider onto the markov-map
(define (draw-handler ws)
  (cond
    [(gui-state-in-help (markov-chain-gui ws)) (draw-help ws)]
    [else (place-images
    (list 
          (draw-connections ws 0)
          (text "Volume: " 18 "black")
          (draw-vol-slider ws)
          (draw-mute-button ws)
          reset-button
          (draw-frosty-button ws)
          (draw-jingle-bells-button ws)
          (draw-misirlou-button ws)
          (draw-ghostbusters-button ws)
          (draw-ode-button ws)
          (draw-adele-button ws)
          (draw-hbday-button ws)
          help-button
          (draw-paused ws)
          (draw-circles ws 0)
          (loop-through-lines ws 0)
          )
    (list
          (make-posn 150 300)   ;; draw-connections
          (make-posn 965 20)    ;; volume text
          (make-posn 1100 20)   ;; vol-slider
          (make-posn 1030 60)   ;; mute-button
          (make-posn 1090 60)   ;; reset-button
          (make-posn 1070 100)  ;; frosty-button
          (make-posn 1070 140)  ;; jingle-bells-button
          (make-posn 1070 180)  ;; misirlou-button
          (make-posn 1070 220)  ;; ghostbusters-button
          (make-posn 1070 260)  ;; ode-button
          (make-posn 1070 300)  ;; adele-button
          (make-posn 1070 340)  ;; hbday-button
          (make-posn (- map-width 25) (- map-height 25)) ;; help button
          (make-posn map-center-x map-center-y) ;; Paused indicator
          (make-posn map-center-x map-center-y)  ;; draw-circles
          (make-posn map-center-x map-center-y)  ;; loop-through-lines
          )
    markov-map)]))

;;Maps from a list of strings to a list of texts
;;List-of-Strings -> List-of-Text
(define (l->lot list)
  (cond
    [(empty? list) '()]
    [else (cons (text (first list) 24 "black") (l->lot (rest list)))]))
(check-expect (l->lot '()) '())
(check-expect (l->lot (list "Hello"))
              (list (text "Hello" 24 "black")))
;;Draw either the paused indictator or nothing
;; Markov-chain -> Picture
(define (draw-paused ws)
  (cond
    [(gui-state-paused (markov-chain-gui ws)) paused]
    [else (rectangle 0 0 "outline" "white")]))
(check-expect (draw-paused initial-chain) (rectangle 0 0 "outline" "white"))

;;Gets a list of posns of any lengths
;;Length->List-of-Posns
(define (get-posns length)
  (cond
    [(= 0 length) '()]
    [else (cons (make-posn (/ map-width 2) (* 30 length)) (get-posns (- length 1)))]))
(check-expect (get-posns 0) '())
(check-expect (get-posns 1) (cons (make-posn (/ map-width 2) 30) '()))

;; Draws Help menu
;; Markov-chain -> Image
(define (draw-help chain)
  (place-images
   (l->lot intro-text)
   (get-posns (length intro-text))
   help-background))

;; Draws the connections of one particular node to itself and all the other nodes
;; Should always be called with index as 0 (loops through the list-of-markov-nodes starting at index: 0)
;; chain: a markov-chain
;; Index: number representing a certain element of list-of-markov-nodes
;; Markov-Chain number -> Image 
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
;; markov-node markov-node number number -> image
(define (draw-connection-line node1 node2 index selected)
  (text (string-append  (get-node-name node1) " → "
                       (get-node-name node2) " : "
                       (number->string (round (* 100 (list-ref (markov-node-connections node1) index)))) " %") 20 (cond
                         [(= index selected) "black"]
                         [else "white"])))

;; Returns the x-coordinate where the node should be drawn (depends on how many markov-nodes are in the list-of-markov-nodes)
;; chain: a markov-chain
;; Index: number representing a certain element of the list-of-markov-nodes
;; Markov-Chain Number -> Number
(define (calc-circle-x chain index)
  (+ map-center-x (* map-radius (cos (- (/ (* 2 pi index) (length (markov-chain-nodes chain))) (/ pi 2) ))))
  )
(check-within (calc-circle-x (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 starting-gui-state) 1)
              (+ map-center-x (* map-radius (cos (- (/ (* 2 pi 1) 2) (/ pi 2))))) 1e-8)
;; Returns the y-coordinate where the node should be drawn
;; chain: a markov-chain
;; Index: number representing a certain element of the list-of-markov-nodes
;; Markov-Chain Number -> Number
(define (calc-circle-y chain index)
  (+ map-center-y (* map-radius (sin (- (/ (* 2 pi index) (length (markov-chain-nodes chain))) (/ pi 2) ))))
  )
(check-within (calc-circle-y (make-markov-chain (list (make-markov-node 40 '(0.5 0.5)) (make-markov-node 70 '(0.7 0.3))) 0 starting-gui-state) 0)
              (+ map-center-y (* map-radius (sin (- (/ (* 2 pi 0) 2) (/ pi 2))))) 1e-8)

;;Get a pen so that our node connection is drawn with the correct thickness
;;This multiplies a max line width by the weight of the node to get the thickness
;; Markov-chain Number Number -> Pen Object 
(define (get-pen chain node-on index)
  (make-pen (cond
              [(and (= node-on (gui-state-node-selected (markov-chain-gui chain))) (= index (gui-state-connection-selected (markov-chain-gui chain)))) selected-connection-line-color]
              [(and (= index (gui-state-node-selected (markov-chain-gui chain))) (= node-on (gui-state-connection-selected (markov-chain-gui chain)))) inverse-selected-connection-line-color]
              [else connection-line-color]) (floor (* max-line-width (list-ref (markov-node-connections (list-ref (markov-chain-nodes chain) node-on)) index))) "solid" "round" "bevel"))
(check-expect (get-pen initial-chain 0 0) (make-pen selected-connection-line-color 0 "solid" "round" "bevel"))

;;This function loops through all of the nodes to draw the connections
;; Markov-chain, 0 -> Image
(define (loop-through-lines chain index)
  (cond
    [(= (length (markov-chain-nodes chain)) index) markov-map]
    [else (add-lines chain index 0 (loop-through-lines chain (add1 index)))]))

;;Draws all the lines for one node's connections
;; Markov-Chain, Number, Number, Image -> Image
(define (add-lines chain node-on index image)
  (cond
    [(= index (length (markov-chain-nodes chain))) image]
    [(= index node-on) (add-lines chain node-on (add1 index) (place-image
                                                              (circle (/ circle-radius 2) "outline" (get-pen chain node-on index))
                                                              (- (calc-circle-x chain node-on) (* circle-radius (cos (+ (/ pi 2) (/ (* 2 pi node-on) (length (markov-chain-nodes chain)))))))
                                                              (- (calc-circle-y chain node-on) (* circle-radius (sin (+ (/ pi 2) (/ (* 2 pi node-on) (length (markov-chain-nodes chain)))))))
                                                              image))]
    [else (add-lines chain node-on (add1 index) (add-line image (calc-circle-x chain node-on) (calc-circle-y chain node-on) (calc-circle-x chain index) (calc-circle-y chain index) (get-pen chain node-on index)))])
  )

;; Draws circles for all markov-nodes in list-of-markov-nodes of markov-chain in a circular pattern
;; Should always be called with index as 0 (loops through the list-of-markov-nodes starting at index: 0)
;; chain: a markov-chain
;; Index: number representing a certain element of list-of-markov-nodes
;; Markov-Chain Number -> Image
(define (draw-circles chain index)
  (cond
    [(= index (length (markov-chain-nodes chain))) markov-map]
    [else 
           (place-image
           (draw-node (= index (markov-chain-current-node chain)) (= index (gui-state-node-selected (markov-chain-gui chain))) (list-ref (markov-chain-nodes chain) index))
           (calc-circle-x chain index)
           (calc-circle-y chain index)
           (draw-circles chain (+ 1 index)))]))

;; Generates the name of a node
;; Markov-Node -> String
(define (get-node-name node)
  (string-append (list-ref midi-names (modulo (markov-node-midi node) 12)) (number->string (floor (/ (markov-node-midi node) 12)))))
(check-expect (get-node-name (make-markov-node 60 '(1))) "C5")
(check-expect (get-node-name (make-markov-node 75 '(1))) "D#6")

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
(define (draw-node active selected node)
  (place-image
   (get-node-text node)
   circle-radius
   circle-radius
   (circle circle-radius "solid" (cond
                                   [active "green"]
                                   [selected "red"]
                                   [else "blue"]))
   )  )
(check-expect (draw-node #false #f (make-markov-node 55 '(.25 .75)))
              (place-image
               (get-node-text (make-markov-node 55 '(.25 .75)))
               circle-radius
               circle-radius
               (circle circle-radius "solid" "blue")))
(check-expect (draw-node #true #f (make-markov-node 64 '(.25 .75)))
              (place-image
               (get-node-text (make-markov-node 64 '(.25 .75)))
               circle-radius
               circle-radius
               (circle circle-radius "solid" "green")))
(check-expect (draw-node #f #t (make-markov-node 55 '(.25 .75)))
              (place-image
               (get-node-text (make-markov-node 55 '(.25 .75)))
               circle-radius
               circle-radius
               (circle circle-radius "solid" "red")))


;; draw the volume slider
;; markov-chain -> image
(define (draw-vol-slider ws)
  (place-image vol-slider (* (gui-state-volume (markov-chain-gui ws)) vol-bg-width) (/ vol-bg-height 2) vol-bg))
(check-expect (draw-vol-slider (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 starting-gui-state))
              (place-image (rectangle vol-slider-width vol-slider-height "solid" "black") 95 15 vol-bg))

;; draw the mute-button
;; mute-button is:
;; - mute-gray, if volume is anything but 0.0
;; - mute-red, if volume is 0.0
;; markov-chain -> image
(define (draw-mute-button ws)
  (cond
    [(= (gui-state-volume (markov-chain-gui ws)) 0.0) mute-red]
    [else mute-gray]))
(check-expect (draw-mute-button (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 0 2 0 0.5 #t #f 0)))
              mute-gray)
(check-expect (draw-mute-button (make-markov-chain (list (make-markov-node 50 '(0.5 0.5)) (make-markov-node 50 '(0.7 0.3))) 0 (make-gui-state 0 0 2 0 0.0 #t #f 0)))
              mute-red)

;; draw the song buttons
;; chain: a markov-chain
;; song: the song to compare to
;; true-button: the button to display if song is currently playing
;; false-button: the button to display if song is not currently playing
;; markov-chain markov-chain image image -> image
(define (draw-song-button chain song true-button false-button)
  (cond
    [(markov-chain=? chain song) true-button]
    [else false-button]))
(check-expect (draw-song-button frosty frosty frosty-true frosty-false)
              frosty-true)
(check-expect (draw-song-button frosty misirlou frosty-true frosty-false)
              frosty-false)

;; draw the frosty-button
;; if frosty is the song playing: show the frosty-true (blue) button
;; otherwise: show the frosty-false (gray) button
;; markov-chain -> image
(define (draw-frosty-button ws)
  (draw-song-button ws frosty frosty-true frosty-false))
(check-expect (draw-frosty-button frosty)
              frosty-true)
(check-expect (draw-frosty-button misirlou)
              frosty-false)

;; draw the jingle-bells-button
;; if jingle-bells is the song playing: show the jingle-bells-true (green) button
;; otherwise: show the jingle-bells-false (gray) button
;; markov-chain -> image
(define (draw-jingle-bells-button ws)
  (draw-song-button ws jingle-bells jingle-bells-true jingle-bells-false))
(check-expect (draw-jingle-bells-button jingle-bells)
              jingle-bells-true)
(check-expect (draw-jingle-bells-button misirlou)
              jingle-bells-false)

;; draw the misirlou-button
;; if misirlou is the song playing: show the misirlou-true (orange) button
;; otherwise: show the misirlou-false (gray) button
;; markov-chain -> image
(define (draw-misirlou-button ws)
  (draw-song-button ws misirlou misirlou-true misirlou-false))
(check-expect (draw-misirlou-button misirlou)
              misirlou-true)
(check-expect (draw-misirlou-button frosty)
              misirlou-false)

;; draw the ghostbusters-button
;; if ghostbusters is the song playing: show the ghostbusters-true (olive) button
;; otherwise: show the ghostbusters-false (gray) button
;; markov-chain -> image
(define (draw-ghostbusters-button ws)
  (draw-song-button ws ghostbusters ghostbusters-true ghostbusters-false))
(check-expect (draw-ghostbusters-button ghostbusters)
              ghostbusters-true)
(check-expect (draw-ghostbusters-button misirlou)
              ghostbusters-false)

;; draw the ode-button
;; if ode is the song playing: show the ode-true (violet) button
;; otherwise: show the ode-false (gray) button
;; markov-chain -> image
(define (draw-ode-button ws)
  (draw-song-button ws ode ode-true ode-false))
(check-expect (draw-ode-button ode)
              ode-true)
(check-expect (draw-ode-button misirlou)
              ode-false)

;; draw the adele-button
;; if adele is the song playing: show the adele-true (aquamarine) button
;; otherwise: show the adele-false (gray) button
;; markov-chain -> image
(define (draw-adele-button ws)
  (draw-song-button ws adele adele-true adele-false))
(check-expect (draw-adele-button adele)
              adele-true)
(check-expect (draw-adele-button misirlou)
              adele-false)

;; draw the hbday-button
;; if hbday is the song playing: show the hbday-true (pink) button
;; otherwise: show the hbday-false (gray) button
;; markov-chain -> image
(define (draw-hbday-button ws)
  (draw-song-button ws hbday hbday-true hbday-false))
(check-expect (draw-hbday-button hbday)
              hbday-true)
(check-expect (draw-hbday-button misirlou)
              hbday-false)

;;END OF DRAW LOGIC







;; On each clock tick:
;; - Create and return a markov-chain with the same list-of-markov-nodes but a new index
;; - Queue the note corresponding to the current node (based on the index)
;; Markov-Chain -> Markov-Chain
(define background
  (rs-append*
   (list kick (silence (- (/ FRAME-RATE 4) (rs-frames kick)))
         c-hi-hat-1 (silence (- (/ FRAME-RATE 4) (rs-frames c-hi-hat-1)))
         snare 
         c-hi-hat-1 (silence (- (/ FRAME-RATE 4) (rs-frames c-hi-hat-1))))))

(define (tick-handler ws)
  (cond
  [(gui-state-paused (markov-chain-gui ws)) ws]
  [else
   (both
    (cond
      [(= (modulo (gui-state-beat-count (markov-chain-gui ws)) 4) 0) (pstream-queue p (rs-scale 0.1 background) (pstream-current-frame p))]
      [else (pstream-queue p (silence 1) (pstream-current-frame p))])
   (invboth 
   (make-markov-chain (markov-chain-nodes ws) (get-next-node ws) (update-gui-beat-count (markov-chain-gui ws) (add1 (gui-state-beat-count (markov-chain-gui ws)))))
   (pstream-queue p
                  (rs-scale (gui-state-volume (markov-chain-gui ws))
                            (synth-note "main"
                                        35
                                        (markov-node-midi (list-ref (markov-chain-nodes ws) (markov-chain-current-node ws)))
                                        (* (/ FRAME-RATE 5) (gui-state-tick-rate (markov-chain-gui ws)))))
                  (pstream-current-frame p))))]))


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
    [(key=? ke " ") (update-gui ws (update-gui-paused (markov-chain-gui ws) (not (gui-state-paused (markov-chain-gui ws)))))]
    [(key=? ke "h") (show-help ws)]
    [(key=? ke "m") (mute ws)]
    [(key=? ke "r") (reset ws)]
    [(key=? ke "a") frosty]
    [(key=? ke "s") jingle-bells]
    [(key=? ke "d") misirlou]
    [(key=? ke "f") ghostbusters]
    [(key=? ke "g") ode]
    [(key=? ke "j") adele]
    [(key=? ke "k") hbday]
    [else ws])
  )

;;END OF KEY HANDLER







;; finds which node is closest to a given x- and y-position
;; x: the x-position
;; y: the y-position
;; index: a number indicating the index of the node (always starts at 0)
;; chain: a markov-chain
;; prevlowest: the index of the last node checked (always starts at 0)
;; number number number markov-chain number -> number
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


;; when the mouse clicks:
;; - if it is within the bounds of the volume slider: updates the volume
;; - if it is within the bounds of one of the nodes: updates the current node selected
;; - if it is within the bounds of one of the songs: plays the song selected
;; - if it is within the bounds of the help button: show the help menu
;; - otherwise: does nothing
;; when the mouse drags:
;; - if it is within the bounds of the volume slider: updates the volume
;; - otherwise: does nothing
;; markov-chain number number string -> ws
(define (click-handler ws x y me)
  (cond
    [(string=? me "button-down") (cond
                                   [(gui-state-in-help (markov-chain-gui ws)) (update-gui ws (update-gui-in-help (markov-chain-gui ws) #f))]
                                   [else
                                    (cond
                                      [(and (and (>= x 1005) (<= x 1195)) (and (>= y 5) (<= y 35))) (update-gui ws (update-gui-volume (markov-chain-gui ws) (/ (- x 1005) vol-bg-width)))]
                                      [(and (and (>= x 1005) (<= x 1055)) (and (>= y 45) (<= y 75))) (mute ws)]
                                      [(and (and (>= x 1065) (<= x 1115)) (and (>= y 45) (<= y 75))) (reset ws)]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 85) (<= y 115))) frosty]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 125) (<= y 155))) jingle-bells]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 165) (<= y 195))) misirlou]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 205) (<= y 235))) ghostbusters]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 205) (<= y 275))) ode]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 205) (<= y 315))) adele]
                                      [(and (and (>= x 1005) (<= x 1135)) (and (>= y 205) (<= y 355))) hbday]
                                      [(and (and (>= x 1160) (<= x 1190)) (and (>= y 550) (<= y 590))) (show-help ws)]
                                      [(< (get-distance x y (calc-circle-x ws (find-closest x y 0 ws 0)) (calc-circle-y ws (find-closest x y 0 ws 0))) circle-radius)
                                       (update-gui ws (update-gui-node (markov-chain-gui ws) (find-closest x y 0 ws 0)))]
                                      [else ws])])]
    [(string=? me "drag") (cond
                            [(and (and (>= x 1005) (<= x 1195)) (and (>= y 5) (<= y 35))) (update-gui ws (update-gui-volume (markov-chain-gui ws) (/ (- x 1005) vol-bg-width)))]
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

;;The frost initial markov-chain
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
) 0 (update-gui-in-help starting-gui-state #f)))


;;The jingle bells initial markov-chain
(define jingle-bells
  (make-markov-chain (list
(make-markov-node 28 (list 0 1 0 0 0 0 0 0 ))
(make-markov-node 29 (list 25/171 1/38 5/171 25/38 2/171 1/38 13/342 11/171 ))
(make-markov-node 30 (list 0 2/149 0 0 80/149 28/149 36/149 3/149 ))
(make-markov-node 44 (list 0 0 1 0 0 0 0 0 ))
(make-markov-node 60 (list 34/152 4/57 0 0 5/57 29/114 1/4 13/114 ))
(make-markov-node 65 (list 5/254 6/127 0 0 239/508 61/508 49/254 38/254 ))
(make-markov-node 69 (list 13/537 17/179 0 0 107/537 235/537 110/537 7/179 ))
(make-markov-node 72 (list 8/207 1/23 0 0 35/207 25/207 88/207 42/207 ))
) 0 (update-gui-in-help starting-gui-state #f)))

;;The misirlou initial markov-chain
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
) 0 (update-gui-in-help starting-gui-state #f)))

;;The ghostbusters initial markov-chain
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
) 0 (update-gui-in-help starting-gui-state #f)))

;;The odetojoy initial markov-chain
(define ode
  (make-markov-chain (list
(make-markov-node 48 (list 0 1/2 1/3 1/6 0 0 0 0 ))
(make-markov-node 52 (list 0 0 3/5 2/5 0 0 0 0 ))
(make-markov-node 55 (list 0 0 0 1/3 5/18 5/18 1/9 0 ))
(make-markov-node 60 (list 4/15 1/15 1/15 0 1/3 4/15 0 0 ))
(make-markov-node 62 (list 0 1/15 7/15 4/15 0 1/5 0 0 ))
(make-markov-node 64 (list 1/8 0 1/4 1/8 1/8 1/16 5/16 0 ))
(make-markov-node 65 (list 0 0 0 0 0 5/7 0 2/7 ))
(make-markov-node 67 (list 0 0 2/5 0 0 0 0 3/5 ))
) 0 (update-gui-in-help starting-gui-state #f)))

;;The adele initial markov-chain
(define adele
  (make-markov-chain (list
(make-markov-node 54 (list 0 1/5 9/25 0 21/50 0 0 1/50 0 ))
(make-markov-node 56 (list 13/68 0 1/68 5/17 21/68 0 13/68 0 0 ))
(make-markov-node 57 (list 1/6 1/156 1/156 0 19/52 9/26 3/52 2/39 0 ))
(make-markov-node 59 (list 1/48 5/12 1/3 0 0 1/48 1/6 0 1/24 ))
(make-markov-node 61 (list 29/307 43/307 79/307 0 0 0 58/307 62/307 36/307 ))
(make-markov-node 62 (list 1/76 1/152 37/76 1/76 0 1/152 0 63/152 9/152 ))
(make-markov-node 64 (list 0 6/47 5/47 5/47 31/47 0 0 0 0 ))
(make-markov-node 66 (list 2/147 0 8/147 0 64/147 65/147 1/147 0 1/21 ))
(make-markov-node 69 (list 0 0 1/54 0 5/9 1/6 2/27 5/27 0 ))
) 0 (update-gui-in-help starting-gui-state #f)))

;;The Happy Birthday initial markov-chain
(define hbday
  (make-markov-chain (list
(make-markov-node 60 (list 3/8 1/4 0 1/8 1/8 0 0 1/8 ))
(make-markov-node 62 (list 2/3 0 0 0 0 0 1/3 0 ))
(make-markov-node 64 (list 1/2 1/2 0 0 0 0 0 0 ))
(make-markov-node 65 (list 1/4 0 1/2 0 1/4 0 0 0 ))
(make-markov-node 67 (list 0 0 0 1 0 0 0 0 ))
(make-markov-node 69 (list 0 0 0 1 0 0 0 0 ))
(make-markov-node 70 (list 0 0 0 0 0 1/2 1/2 0 ))
(make-markov-node 72 (list 0 0 0 0 0 1 0 0 ))
) 0 (update-gui-in-help starting-gui-state #f)))
;;END OF CHAIN DEFINITIONS






;; The world state of this big-bang is a markov-chain
(big-bang initial-chain
          [to-draw draw-handler]
          [on-tick tick-handler 1/4]
          [on-key key-handler]
          [on-mouse click-handler])
