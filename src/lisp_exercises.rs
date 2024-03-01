#[cfg(test)]
pub mod tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn square() {
        Evaluator::new()
            .eval("(define (square x) (* x x))")
            .eval_to("(square 3)", "9");
    }

    #[test]
    fn sum() {
        Evaluator::new()
            .eval("(define (sum x y) (+ x y))")
            .eval_to("(sum 3 4)", "7");
    }

    #[test]
    fn gcd() {
        Evaluator::new()
            .eval("
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))")
            .eval_to("(gcd 4 0)", "4");
    }

    #[test]
    fn rational_ex_2_1() {
        Evaluator::new()
            .eval("
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))")
            .eval_to("(gcd 4 4)", "4")
            .eval("
    (define (add-rat x y)
          (make-rat (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))")
            .eval("
    (define (make-rat n d)
      (let [
         (g (gcd (abs n) (abs d)))
         (neg-n (< n 0))
         (neg-d (< d 0))
         (both-neg (and neg-n neg-d))
         (both-pos (and (not neg-n) (not neg-d)))
        ]
        [if (or both-neg both-pos)
             [cons (/ (abs n) g) (/ (abs d) g)]
             [cons (/ (- (abs n)) g) (/ (abs d) g)]
         ]
      )
    )")
            .eval_all(vec![
                "(define (numer x) (car x))",
                "(define (denom x) (cdr x))"])
            .eval("
    (define (print-rat x)
       (display (numer x))
       (display /)
       (display (denom x))
       (new-line))")
            .eval_all(vec![
                "(print-rat (make-rat 1 2))",
                "(define one-half (make-rat 1 2))"])
            .eval_to("(add-rat one-half one-half)", "(cons 1 1)")
            .eval_to("(make-rat (- 1) 2)", "(cons -1 2)")
            .eval_to("(make-rat 1 (- 2))", "(cons -1 2)")
            .eval_to("(make-rat (- 1) (- 2))", "(cons 1 2)");
    }

    #[test]
    fn line_segment_ex_2_2_etc() {
        Evaluator::new()
            .eval_all(vec![
                "(define (make-point x y) (cons x y))",
                "(define (x-point p) (car p))",
                "(define (y-point p) (cdr p))",
                "(define (make-segment p1 p2) (cons p1 p2))",
                "(define (start-segment s) (car s))",
                "(define (end-segment s) (cdr s))",
                "(define (midpoint-segment s)
                         [let (
                                 (x1 (x-point (start-segment s)))
                                 (y1 (y-point (start-segment s)))
                                 (x2 (x-point (end-segment s)))
                                 (y2 (y-point (end-segment s)))
                                 (x (/ (+ x1 x2) 2.0))
                                 (y (/ (+ y1 y2) 2.0))
                              )
                              (make-point x y)
                          ])",
                "(define (make-rectangle p1 p2) (cons p1 p2))",
                "(define (top-left r) (car r))",
                "(define (bottom-right r) (cdr r))",
                "(define (area r)
                         [let (
                                 (x1 (x-point (top-left r)))
                                 (y1 (y-point (top-left r)))
                                 (x2 (x-point (bottom-right r)))
                                 (y2 (y-point (bottom-right r)))
                                 (x-delta (abs (- x1 x2)))
                                 (y-delta (abs (- y1 y2)))
                              )
                              (* x-delta y-delta)
                          ])",
            ])
            .eval("(define seg (make-segment (make-point 3 4) (make-point 6 8)))")
            .eval_to("(start-segment seg)", "(cons 3 4)")
            .eval_to("(end-segment seg)", "(cons 6 8)")
            .eval_to("(x-point (start-segment seg))", "3")
            .eval("(define p (make-point 3 4))")
            .eval_to("(y-point p)", "4")
            .eval_to("(midpoint-segment seg)", "(cons 4.5 6)")
            .eval("(define rect (make-rectangle (make-point 3 4) (make-point 6 8)))")
            .eval_to("(area rect)", "12");
    }

    #[test]
    fn alt_cons() {
        Evaluator::new()
            .eval_all(vec![
                "(define (cons x y) (lambda (m) (m x y)))",
                "(define (car z) (z (lambda (p q) p)))",
                "(define (cdr z) (z (lambda (p q) q)))",
            ])
            .eval_to("(car (cons 1 2))", "1")
            .eval_to("(cdr (cons 1 2))", "2");
    }

    #[test]
    fn church_ex_2_6() {
        Evaluator::new()
            .eval_all(vec![
                "(define zero (lambda (f) (lambda (x) x)))",
                "(define (add-1 n)
                         (lambda (f) (lambda (x) (f ((n f) x)))))",
                "(define (ff x) (cons x ignore))",
            ])
            .eval_to("((zero ff) 99)", "99")
            .eval_to("(((add-1 zero) ff) 99)", "(cons 99 ignore)");
    }

    #[test]
    fn intervals_ex_2_7() {
        Evaluator::new()
            .eval_all(vec![
                "(define (make-interval x y) (cons x y))",
                "(define (lower-bound x) (car x))",
                "(define (upper-bound x) (cdr x))",
                "(define (make-percent-offset centre offset)
                   (make-interval
                         (- centre offset)
                         (+ centre offset)
                   ))",
                "(define (make-percent-interval centre percent)
                   (make-percent-offset
                        centre (* centre (/ percent 100.0))
                   ))",
                "(define (centre-interval interval)
                    (/ (+ (upper-bound interval) (lower-bound interval)) 2.0)
                )",
            ])
            .eval("
    (define (add-interval x y)
      (make-interval (+ (lower-bound x) (lower-bound y))
                     (+ (upper-bound x) (upper-bound y))))")
            .eval("
    (define (subtract-interval x y)
      (make-interval (- (lower-bound x) (lower-bound y))
                     (- (upper-bound x) (upper-bound y))))")
            .eval("
    (define (mul-interval x y)
      (let ((p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))")
            .eval_to("(add-interval (make-interval 1 2) (make-interval 3 4))",
                     "(cons 4 6)")
            .eval_to("(subtract-interval (make-interval 10 20) (make-interval 3 4))",
                     "(cons 7 16)")
            .eval_to("(mul-interval (make-interval 1 2) (make-interval 3 4))",
                     "(cons 3 8)")
            .eval_to("(make-percent-offset 100 10)", "(cons 90 110)")
            .eval_to("(make-percent-interval 100 10)", "(cons 90 110)")
            .eval_to("(centre-interval (make-percent-interval 100 10))", "100");
    }

    #[test]
    fn list_ref() {
        Evaluator::new()
            .eval("
   (define (list-ref items n)
       (if (= n 0)
           (car items)
           (list-ref (cdr items) (- n 1))))")
            .eval_to("(list-ref (list 1 4 9 16 25) 3)", "16");
    }

    #[test]
    fn length() {
        Evaluator::new()
            .eval("
   (define (length items)
      (if (null? items)
         0
         (+ 1 (length (cdr items)))))")
            .eval_to("(length (list 1 4 9 16 25))", "5");
    }

    #[test]
    fn append() {
        Evaluator::new()
            .eval("
   (define (append list1 list2)
       (if (null? list1)
           list2
           (cons (car list1) (append (cdr list1) list2))))")
            .eval_to("(append (list 4 9) (list 16 25))", "(list 4 9 16 25)");
    }

    #[test]
    fn last_pair_ex_2_17() {
        Evaluator::new()
            .eval("
   (define (last-pair list)
       (if (null? (cdr list))
           list
           (last-pair (cdr list))))")
            .eval_to("(last-pair (list 16 25))", "(list 25)");
    }

    #[test]
    fn reverse() {
        Evaluator::new()
            .eval("
   (define (reversing list reversed)
       (if (null? list)
           reversed
           (reversing (cdr list) (cons (car list) reversed))
        ))")
            .eval("
   (define (reverse list1)
       (reversing list1 (list)))")
            .eval_to("(reverse (list 16 25))", "(list 25 16))");
    }

    #[test]
    fn for_each_ex_2_23() {
        Evaluator::new()
            .eval("
       (define (for-each f list)
           (cond ((null? list) true)
                 (else (f (car list)) (for-each f (cdr list)))
          ))")
            .eval_to("(for-each (lambda (x) (new-line) (display x)) (list 1 2 3 44))", "true");
    }

    #[test]
    fn accumulate_ex_2_33() {
        Evaluator::new()
            .eval("
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))")
            .eval("
       (define (map2 p sequence)
          (accumulate (lambda (x y) (cons (p x) y))
                      nil sequence))")
            .eval("
        (define (append2 seq1 seq2)
                (accumulate cons seq2 seq1)
                )")
            .eval("
        (define (length2 sequence)
             (accumulate (lambda (x y) (+ y 1)) 0 sequence)
             )")
            .eval_to("(map2 (lambda (x) (+ x 1)) (list 1 2 3 4 5))", "(list 2 3 4 5 6)")
            .eval_to("(append2 (list 2 3) (list 4 5 6))", "(list 2 3 4 5 6)")
            .eval_to("(length2 (list 1 2 3 4 5 6))", "6");
    }

    #[test]
    fn matrix_ex_2_37() {
        Evaluator::new()
            .eval("
     (define (dot-product v w)
        (accumulate + 0 (map * v w)))");
    }

    #[test]
    fn equal_lists_ex_2_54() {
        Evaluator::new()
            .eval("
     (define (equals? x y)
        (cond ([and (symbol? x) (symbol? y) (eq? x y)] true)
              ([and (pair? x) (pair? y)]
                 (and [equals? (car x) (car y)]
                      [equals? (car x) (car y)]))
              (else false)
      ))")
            .eval_to("(equals? nil nil)", "true")
            .eval_to("(equals? 'a 'a)", "true")
            .eval_to("(equals? (list 'a) (list 'a))", "true")
            .eval_to("(equals? (list (list 'a)) (list (list 'a)))", "true")
            .eval_to("(equals? nil 'a)", "false")
            .eval_to("(equals? 'a 'b)", "false")
            .eval_to("(equals? (list 'b) (list 'a))", "false")
            .eval_to("(equals? (list 'a) (list (list 'a)))", "false");
    }

    #[test]
    fn differentiation_ex_2_55() {
        Evaluator::new()
            .eval("(define (variable? x) (symbol? x))")
            .eval("(define (same-variable? v1 v2)
      (and (variable? v1) (variable? v2) (eq? v1 v2)))")
            .eval("(define (make-sum a1 a2) (list '+ a1 a2))")
            .eval("(define (make-product m1 m2) (list '* m1 m2))")
            .eval("(define (sum? x) (and (pair? x) (eq? (car x) '+)))")
            .eval("(define (addend s) (cadr s))")
            .eval("(define (augend s) (caddr s))")
            .eval("(define (product? x) (and (pair? x) (eq? (car x) '*)))")
            .eval("(define (multiplier p) (cadr p))")
            .eval("(define (multiplicand p) (caddr p))")
            .eval("
  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
          ((product? exp)
             (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))
          (else (error \"unknown expression type\"))))
")
            .eval_to("(deriv 'a 'x)", "0")
            .eval_to("(deriv 'x 'x)", "1")
            .eval_to("(deriv '(+ x 3) 'x)", "(cons 1 0)");
    }
}