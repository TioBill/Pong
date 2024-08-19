(setq 
  PLAYER_1_SPEED    .5
  PLAYER_2_SPEED    .5
    
  ;; Randomize which direction the ball is going will occur later.
  DIRECTION_BALL_X  1 ;; VARIATES BETWEEN (-1,1) INCLUDED
  DIRECTION_BALL_Y  1 ;; VARIATES BETWEEN (-1,1) INCLUDED

  BALL_INCL_X 1
  BALL_INCL_Y 1  
  
  ;; BALL CENTER ORIGIN
  ORIGINAL_BALL.CENTER '(50 5 0)
  ORIGINAL_BALL_SPEED .08
)

(defun rand ( minimum maximum)
  (setq current (getvar 'cdate))
  
  (setq 
    modulus 4294967296.0
    multiplier 1664525.0
    increment 1013904223.0
    current (rem (+ (* multiplier current) increment) modulos)
  )

  (+ minimum (rem current (+ 1 (- maximum minimum))))
)

(defun c:Will:pong ( / *error* acad aDoc modelSpace )
  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )
  
  (setvar 'cmdecho 0)
  
  (vla-startundomark aDoc)
  
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
    
    (if ball
      (vla-delete ball)
    )
    
    (if playerOne
      (vla-delete playerOne)
    )
    
    (if playerTwo
      (vla-delete playerTwo)
    )
    
    (if areaLimit
      (vla-delte areaLimit)
    )
    
    (setvar 'cmdecho 1)
    (vla-endundomark aDoc)
  )
  
  (pong.start)
  
  (gameLoop)
)

(defun pong.start ()
  (ball.start)
  (playerOne.start)
  (playerTwo.start)
  (areaLimit.start)
  (areaLimit.render)
  
  (command "_zoom" "_o" (entlast) "")
)

(defun gameLoop ()
  (while t
    (command "_delay" 1)
    (tick)
    (render)
  )
)

(defun tick ()
  (ball.tick)
  (playerOne.tick)
  (playerTwo.tick)
)

(defun render ()
  (ball.render)
  (playerOne.render)
  (playerTwo.render)
)

(defun ball.tick ()
  ;; CHECKS IF THE BALL IS NEITHER AT THE LEFT-LIMIT NOR RIGHT-LIMIT
  (if
    (not
        (or
          (<= (car ball.center) (car left-topLimit))
          (>= (car ball.center) (car right-toplimit))
        )
    )

    ;; INSIDE THE LIMITS
    (progn
      ;; CHECKS IF THE BALL IS EITHER AT THE TOP-LIMIT OR BOTTOM-LIMIT
      (if
        (or
          (>= (cadr ball.center) (cadr left-topLimit))
          (<= (cadr ball.center) (cadr left-bottomLimit))
        )
        ;; YES
        (progn
          (setq DIRECTION_BALL_Y (* -1 DIRECTION_BALL_Y))
          (setq BALL_INCL_Y (ball.GEN_INC DIRECTION_BALL_Y))
          (setq BALL_INCL_X (ball.GEN_INC DIRECTION_BALL_X))
        )
      )
      
      
      ;; CHECK IF THE BALL IS COLIDING WITH EITHER THE PLAYER_ONE OR PLAYER_TWO
      (if
        ;; COLLIDING WITH THE PLAYER_ONE
        (or
          (vlax-invoke playerOne 'intersectwith ball acExtendNone)
          (vlax-invoke playerTwo 'intersectwith ball acExtendNone)
          ;(inters playerOne.startPoint playerOne.endPoint ball.center ball.center)        
          ;(inters playerTwo.startPoint playerTwo.endPoint ball.center ball.center)
        )

          (PROGN
            (setq 
              DIRECTION_BALL_X (* -1 DIRECTION_BALL_X)
              BALL_SPEED (+ BALL_SPEED .01)
            )
            
            (setq BALL_INCL_X (ball.GEN_INC DIRECTION_BALL_X))
            (setq BALL_INCL_Y (ball.GEN_INC DIRECTION_BALL_Y))
          )
      )
      
      ;; New Ball poisition
      (setq ball.center
        (list
          (+ (* (+ DIRECTION_BALL_X BALL_INCL_X) BALL_SPEED) (car ball.center))
          (+ (* (+ DIRECTION_BALL_Y BALL_INCL_Y) BALL_SPEED) (cadr ball.center))
          0
        )
      )
    )
    
    ;; OUTSIDE THE LEFT-RIGHT LIMITS
    (progn
      (setq ball.center ORIGINAL_BALL.CENTER)   
      (ball.GEN_DIRECTION)     
      (setq BALL_SPEED ORIGINAL_BALL_SPEED)
    )
  )
)

(defun ball.GEN_INC ( maxi / value )
  (setq value (rand 0 1))

  (if (< maxi 1)
    (setq value (* -1 value))
    value
  )
)

(defun ball.GEN_DIRECTION ()
  (setq DIRECTION_BALL_X (if (>= (rand 1 2) 1) -1 1))
  (setq DIRECTION_BALL_Y (if (>= (rand 1 2) 1) -1 1))
)

(defun playerOne.tick ( / playerOne.center)
  (setq playerOne.center
    (list
      (/ (+ (car playerOne.startPoint) (car playerOne.endPoint)) 2)
      (/ (+ (cadr playerOne.startPoint) (cadr PlayerOne.endPoint)) 2)
      0
    )
  )
  
  ;; Chance of 50% (?)
  (if (>= (rand 1 10) (rand 1 10))
    (if
      (and
        (< (cadr playerOne.endPoint) (cadr left-topLimit))
        (>= (cadr ball.center) (cadr playerOne.center))
      )
      
      ;; Case Yes
      (progn
        (setq 
          playerOne.startPoint
            (list
              (car playerOne.startPoint)
              (+ (cadr playerOne.startPoint) PLAYER_1_SPEED)
              0
            )  
        )
        
        (setq
          playerOne.endPoint
          (list
            (car playerOne.endPoint)
            (+ (cadr playerOne.endPoint) PLAYER_1_SPEED)
            0
          )
        )
      )
      
      ;; Case not
      (if
        (and
          (> (cadr playerOne.startPoint) (cadr left-bottomLimit))
          (<= (cadr ball.center) (cadr PlayerOne.center))
        )
      
        ;; Case Yes
        (progn
          (setq 
            playerOne.startPoint
              (list
                (car playerOne.startPoint)
                (- (cadr playerOne.startPoint) PLAYER_1_SPEED)
                0
              )
            playerOne.endPoint
              (list
                (car playerOne.endPoint)
                (- (cadr playerOne.endPoint) PLAYER_1_SPEED)
                0
              )
          )
        )
      )
    )
  )
)

(defun playerTwo.tick ( / playerTwo.center )
  (setq playerTwo.center
    (list
      (/ (+ (car playerTwo.startPoint) (car playerTwo.endPoint)) 2)
      (/ (+ (cadr playerTwo.startPoint) (cadr PlayerTwo.endPoint)) 2)
      0
    )
  )
  
  ;; Chance of 50% (?)
  (if (>= (rand 1 10) (rand 1 10))
    (if
      (and
        (< (cadr playerTwo.endPoint) (cadr left-topLimit))
        (>= (cadr ball.center) (cadr playerTwo.center))
      )
      
      ;; Case Yes
      (progn
        (setq 
          playerTwo.startPoint
            (list
              (car playerTwo.startPoint)
              (+ (cadr playerTwo.startPoint) PLAYER_2_SPEED)
              0
            )  
        )
        
        (setq
          playerTwo.endPoint
          (list
            (car playerTwo.endPoint)
            (+ (cadr playerTwo.endPoint) PLAYER_2_SPEED)
            0
          )
        )
      )
      
      ;; Case not
      (if
        (and
          (> (cadr playerTwo.startPoint) (cadr left-bottomLimit))
          (<= (cadr ball.center) (cadr PlayerTwo.center))
        )
      
        ;; Case Yes
        (progn
          (setq 
            playerTwo.startPoint
              (list
                (car playerTwo.startPoint)
                (- (cadr playerTwo.startPoint) PLAYER_2_SPEED)
                0
              )
            playerTwo.endPoint
              (list
                (car playerTwo.endPoint)
                (- (cadr playerTwo.endPoint) PLAYER_2_SPEED)
                0
              )
          )
        )
      )
    )
  )
  
  
)

(defun ball.render ()
  (if ball
    (vla-delete ball)
  )
  
  (setq ball (vla-addcircle modelSpace (vlax-3d-point ball.center) ball.radius))
)

(defun playerOne.render ()
  (if playerOne
    (vla-delete playerOne)
  )
  
  (setq playerOne (vla-addline modelSpace (vlax-3d-point playerOne.startPoint) (vlax-3d-point playerOne.endPoint)))
  
  (vla-put-color playerOne 3)
)

(defun playerTwo.render ()
  (if playerTwo
    (vla-delete playerTwo)
  )

  (setq playerTwo (vla-addline modelSpace (vlax-3d-point playerTwo.startPoint) (vlax-3d-point playerTwo.endPoint)))
  
  (vla-put-color playerTwo 1)
)

(defun areaLimit.render ()
  
  (if areaLimit
    (vla-delete areaLimit)
  )
  
  (command "_pline" left-topLimit right-topLimit right-bottomLimit left-bottomLimit left-topLimit "")
  
  (setq areaLimit (vlax-ename->vla-object (entlast)))
)

(defun ball.start ( / radius)
  (setq ball.center ORIGINAL_BALL.CENTER)
  (setq ball.radius .5)

  (setq ball (vla-addcircle modelSpace (vlax-3d-point ball.center) ball.radius))

  (setq 
    DIRECTION_BALL_X (rand 1 2)
    DIRECTION_BALL_Y (rand 1 2)
  )
  
  (setq
    BALL_INCL_X (ball.GEN_INC DIRECTION_BALL_X)
    BALL_INCL_Y (ball.GEN_INC DIRECTION_BALL_Y)
    BALL_SPEED ORIGINAL_BALL_SPEED
  )
  
)

(defun playerOne.start ()
  (setq 
    playerOne.startPoint '(0 0 0)
    playerOne.endPoint '(0 10 0)
    playerOne (vla-addline modelSpace (vlax-3d-point playerOne.startPoint) (vlax-3d-point playerOne.endPoint))
  )
)

(defun playerTwo.start ()
  (setq 
    playerTwo.startPoint '(100 0 0)
    playerTwo.endPoint '(100 10 0)
    playerTwo (vla-addline modelSpace (vlax-3d-point playerTwo.startPoint) (vlax-3d-point playerTwo.endPoint))
  )
)

(defun areaLimit.start ()
  (setq areaLimit nil)
  
  (setq left-topLimit '(-14.8241 30 0))
  (setq right-topLimit '(110.1759 30 0))
  
  (setq right-bottomLimit '(110.1759 -20 0))
  (setq left-bottomLimit '(-14.8241 -20 0))
)

(alert "Warning! This program is intended to only be used in a fresh Drawing.\n\nType: \"Will:pong\" to start.")