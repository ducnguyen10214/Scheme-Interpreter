;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Twists And Turns
;;;
;;; Description:
;;;   <The missing pieces, intertwine
;;;             eventually.
;;;    The end of our journeys.>

(define (helper-2 i sides)
  (cond ((= i 500))
        (else
              (fd (+ (/ (* i 2) sides) i))
              (lt (+ (/ 360 sides) 0.35))
              (speed 30)
              (helper-2 (+ i 1) sides)
            ))
)


(define (draw)
  (bgcolor "black")
  (color "white")
  (hideturtle)
  (helper-2 0 3)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)