(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond
       ((eq? m 'get-signal) signal-value)
       ((eq? m 'set-signal!) set-my-signal!)
       ((eq? m 'add-action!) accept-action-procedure!)
       (else (error "Unknown Operation -- WIRE" m))))

    dispatch))

(define (call-each procedures)
  (if (null? procedures) 'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))
      ))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action)
  ((wire 'add-action!) action))


;;  LOGICAL GATES

(define (logical-not s)
  (cond
   ((= s 0) 1)
   ((= s 1) 0)
   (else (error "Invalid signal -- LOGICAL NOT" s))))

(define (logical-and s1 s2)
  (cond
   ((and (= s1 1) (= s2 1)) 1)
   ((and (= s1 0) (= s2 1)) 0)
   ((and (= s1 1) (= s2 0)) 0)
   (else (error "Invalid signal -- LOGICAL AND" s))))

(define (logical-or s1 s2)
  (cond
   ((or (= s1 1) (= s2 1)) 1)
   ((and ( = s1 0) (= s2 0)) 0)
   (else (error "Invalid signal -- LOGICAL OR" s))))


;; Implementing the agenda
(load "queue.scm")

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))
