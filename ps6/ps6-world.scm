;;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;;	   Department of Electrical Engineering and Computer Science
;;;	   6.001---Structure and Interpretation of Computer Programs
;;;			      Fall Semester, 1991
;;;				 Problem Set 6
;;;
;;;			    Code file PS6-WORLD.SCM


;;;============================================================================
;;; You can extend this file to make more stuff part of your world.
;;;============================================================================

;;;============================================================================
;;; *CAVEAT* To keep your world consistent, whenever you change a procedure or 
;;;          redefine a person/place/etc you should reload this entire file    
;;;          into Scheme. This prevents you from having old-moldy folks running
;;;          around who have not evolved to adhere to your modifications. To   
;;;          make this work out well, you should create little scripts at the  
;;;          end of this file to make the game evolve as you work through it.  
;;;          [See the bottom of this file for an example.]                     
;;;============================================================================


(initialize-clock-list)

;; Here we define the places in our world...
;;------------------------------------------

(define EGG-Atrium   (make-place 'EGG-Atrium))
(define dungeon      (make-place 'dungeon))
(define Building-36  (make-place 'Building-36))
(define computer-lab (make-place 'computer-lab))
(define Tech-Square  (make-place 'Tech-Square))
(define eric-office  (make-place 'eric-office))
(define  hal-office  (make-place  'hal-office))
(define dormitory    (make-place 'dormitory))

;; The following isolated place is defined in PS6-ADV.SCM too but redefined
;; here so you can just "zap" altered definitions there then re-evaluate this
;; file w/o worrying about forgetting to update any places.
;;
;; Consequently, to be consistent, if you find it appropriate to define any new
;; places in PS6-ADV.SCM, you should likewise duplicate their definitions here.

(define heaven       (make-place 'heaven))	; The point of no return

;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(can-go Building-36  'up    computer-lab)
(can-go computer-lab 'down  Building-36)
(can-go Building-36  'north Tech-Square)
(can-go Tech-Square  'south Building-36)
(can-go Building-36  'west  EGG-Atrium)
(can-go EGG-Atrium   'east  Building-36)
(can-go Tech-Square  'up    hal-office)
(can-go  hal-office  'down  Tech-Square)
(can-go  hal-office  'up    eric-office)
(can-go eric-office  'down   hal-office)
(can-go dormitory    'west  Building-36)
(can-go Building-36  'east  dormitory)
(can-go dungeon      'up    EGG-Atrium)

;; The important critters in our world...
;;---------------------------------------

(define hal     (make&install-person 'hal      hal-office 2))
(define eric    (make&install-person 'eric    eric-office 1))

(define grendel (make&install-troll  'grendel dungeon     4))


;; The beginning of an ever-expanding game scrypt
;;------------------------------------------------

(define (play-game)
  (ask hal 'go 'up)				; Hal meets Eric
  )

;; ...now whenever you re-zap this world, you can bring things up to
;; date by invoking PLAY-GAME.
