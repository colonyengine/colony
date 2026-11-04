(in-package #:cl-user)

(defpackage #:vorigin.constants
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:shadow
   #:pi)
  (:export
   #:+deg+
   #:+deg/double+
   #:+rad+
   #:+rad/double+

   ;; pi constants.
   #:pi
   #:2pi/1
   #:pi/2
   #:2pi/2
   #:3pi/2
   #:4pi/2
   #:pi/3
   #:2pi/3
   #:3pi/3
   #:4pi/3
   #:5pi/3
   #:6pi/3
   #:pi/4
   #:2pi/4
   #:3pi/4
   #:4pi/4
   #:5pi/4
   #:6pi/4
   #:7pi/4
   #:8pi/4
   #:pi/5
   #:2pi/5
   #:3pi/5
   #:4pi/5
   #:5pi/5
   #:6pi/5
   #:7pi/5
   #:8pi/5
   #:9pi/5
   #:10pi/5
   #:pi/6
   #:2pi/6
   #:3pi/6
   #:4pi/6
   #:5pi/6
   #:6pi/6
   #:7pi/6
   #:8pi/6
   #:9pi/6
   #:10pi/6
   #:11pi/6
   #:12pi/6
   #:pi/7
   #:2pi/7
   #:3pi/7
   #:4pi/7
   #:5pi/7
   #:6pi/7
   #:7pi/7
   #:8pi/7
   #:9pi/7
   #:10pi/7
   #:11pi/7
   #:12pi/7
   #:13pi/7
   #:14pi/7
   #:pi/8
   #:2pi/8
   #:3pi/8
   #:4pi/8
   #:5pi/8
   #:6pi/8
   #:7pi/8
   #:8pi/8
   #:9pi/8
   #:10pi/8
   #:11pi/8
   #:12pi/8
   #:13pi/8
   #:14pi/8
   #:15pi/8
   #:16pi/8
   #:pi/9
   #:2pi/9
   #:3pi/9
   #:4pi/9
   #:5pi/9
   #:6pi/9
   #:7pi/9
   #:8pi/9
   #:9pi/9
   #:10pi/9
   #:11pi/9
   #:12pi/9
   #:13pi/9
   #:14pi/9
   #:15pi/9
   #:16pi/9
   #:17pi/9
   #:18pi/9
   #:pi/10
   #:2pi/10
   #:3pi/10
   #:4pi/10
   #:5pi/10
   #:6pi/10
   #:7pi/10
   #:8pi/10
   #:9pi/10
   #:10pi/10
   #:11pi/10
   #:12pi/10
   #:13pi/10
   #:14pi/10
   #:15pi/10
   #:16pi/10
   #:17pi/10
   #:18pi/10
   #:19pi/10
   #:20pi/10
   #:pi/11
   #:2pi/11
   #:3pi/11
   #:4pi/11
   #:5pi/11
   #:6pi/11
   #:7pi/11
   #:8pi/11
   #:9pi/11
   #:10pi/11
   #:11pi/11
   #:12pi/11
   #:13pi/11
   #:14pi/11
   #:15pi/11
   #:16pi/11
   #:17pi/11
   #:18pi/11
   #:19pi/11
   #:20pi/11
   #:21pi/11
   #:22pi/11
   #:pi/12
   #:2pi/12
   #:3pi/12
   #:4pi/12
   #:5pi/12
   #:6pi/12
   #:7pi/12
   #:8pi/12
   #:9pi/12
   #:10pi/12
   #:11pi/12
   #:12pi/12
   #:13pi/12
   #:14pi/12
   #:15pi/12
   #:16pi/12
   #:17pi/12
   #:18pi/12
   #:19pi/12
   #:20pi/12
   #:21pi/12
   #:22pi/12
   #:23pi/12
   #:24pi/12
   #:pi/13
   #:2pi/13
   #:3pi/13
   #:4pi/13
   #:5pi/13
   #:6pi/13
   #:7pi/13
   #:8pi/13
   #:9pi/13
   #:10pi/13
   #:11pi/13
   #:12pi/13
   #:13pi/13
   #:14pi/13
   #:15pi/13
   #:16pi/13
   #:17pi/13
   #:18pi/13
   #:19pi/13
   #:20pi/13
   #:21pi/13
   #:22pi/13
   #:23pi/13
   #:24pi/13
   #:25pi/13
   #:26pi/13
   #:pi/14
   #:2pi/14
   #:3pi/14
   #:4pi/14
   #:5pi/14
   #:6pi/14
   #:7pi/14
   #:8pi/14
   #:9pi/14
   #:10pi/14
   #:11pi/14
   #:12pi/14
   #:13pi/14
   #:14pi/14
   #:15pi/14
   #:16pi/14
   #:17pi/14
   #:18pi/14
   #:19pi/14
   #:20pi/14
   #:21pi/14
   #:22pi/14
   #:23pi/14
   #:24pi/14
   #:25pi/14
   #:26pi/14
   #:27pi/14
   #:28pi/14
   #:pi/15
   #:2pi/15
   #:3pi/15
   #:4pi/15
   #:5pi/15
   #:6pi/15
   #:7pi/15
   #:8pi/15
   #:9pi/15
   #:10pi/15
   #:11pi/15
   #:12pi/15
   #:13pi/15
   #:14pi/15
   #:15pi/15
   #:16pi/15
   #:17pi/15
   #:18pi/15
   #:19pi/15
   #:20pi/15
   #:21pi/15
   #:22pi/15
   #:23pi/15
   #:24pi/15
   #:25pi/15
   #:26pi/15
   #:27pi/15
   #:28pi/15
   #:29pi/15
   #:30pi/15
   #:pi/16
   #:2pi/16
   #:3pi/16
   #:4pi/16
   #:5pi/16
   #:6pi/16
   #:7pi/16
   #:8pi/16
   #:9pi/16
   #:10pi/16
   #:11pi/16
   #:12pi/16
   #:13pi/16
   #:14pi/16
   #:15pi/16
   #:16pi/16
   #:17pi/16
   #:18pi/16
   #:19pi/16
   #:20pi/16
   #:21pi/16
   #:22pi/16
   #:23pi/16
   #:24pi/16
   #:25pi/16
   #:26pi/16
   #:27pi/16
   #:28pi/16
   #:29pi/16
   #:30pi/16
   #:31pi/16
   #:32pi/16

   ;; tau constants
   #:tau
   #:tau/2
   #:2tau/2
   #:tau/3
   #:2tau/3
   #:3tau/3
   #:tau/4
   #:2tau/4
   #:3tau/4
   #:4tau/4
   #:tau/5
   #:2tau/5
   #:3tau/5
   #:4tau/5
   #:5tau/5
   #:tau/6
   #:2tau/6
   #:3tau/6
   #:4tau/6
   #:5tau/6
   #:6tau/6
   #:tau/7
   #:2tau/7
   #:3tau/7
   #:4tau/7
   #:5tau/7
   #:6tau/7
   #:7tau/7
   #:tau/8
   #:2tau/8
   #:3tau/8
   #:4tau/8
   #:5tau/8
   #:6tau/8
   #:7tau/8
   #:8tau/8
   #:tau/9
   #:2tau/9
   #:3tau/9
   #:4tau/9
   #:5tau/9
   #:6tau/9
   #:7tau/9
   #:8tau/9
   #:9tau/9
   #:tau/10
   #:2tau/10
   #:3tau/10
   #:4tau/10
   #:5tau/10
   #:6tau/10
   #:7tau/10
   #:8tau/10
   #:9tau/10
   #:10tau/10
   #:tau/11
   #:2tau/11
   #:3tau/11
   #:4tau/11
   #:5tau/11
   #:6tau/11
   #:7tau/11
   #:8tau/11
   #:9tau/11
   #:10tau/11
   #:11tau/11
   #:tau/12
   #:2tau/12
   #:3tau/12
   #:4tau/12
   #:5tau/12
   #:6tau/12
   #:7tau/12
   #:8tau/12
   #:9tau/12
   #:10tau/12
   #:11tau/12
   #:12tau/12
   #:tau/13
   #:2tau/13
   #:3tau/13
   #:4tau/13
   #:5tau/13
   #:6tau/13
   #:7tau/13
   #:8tau/13
   #:9tau/13
   #:10tau/13
   #:11tau/13
   #:12tau/13
   #:13tau/13
   #:tau/14
   #:2tau/14
   #:3tau/14
   #:4tau/14
   #:5tau/14
   #:6tau/14
   #:7tau/14
   #:8tau/14
   #:9tau/14
   #:10tau/14
   #:11tau/14
   #:12tau/14
   #:13tau/14
   #:14tau/14
   #:tau/15
   #:2tau/15
   #:3tau/15
   #:4tau/15
   #:5tau/15
   #:6tau/15
   #:7tau/15
   #:8tau/15
   #:9tau/15
   #:10tau/15
   #:11tau/15
   #:12tau/15
   #:13tau/15
   #:14tau/15
   #:15tau/15
   #:tau/16
   #:2tau/16
   #:3tau/16
   #:4tau/16
   #:5tau/16
   #:6tau/16
   #:7tau/16
   #:8tau/16
   #:9tau/16
   #:10tau/16
   #:11tau/16
   #:12tau/16
   #:13tau/16
   #:14tau/16
   #:15tau/16
   #:16tau/16
   #:tau/17
   #:2tau/17
   #:3tau/17
   #:4tau/17
   #:5tau/17
   #:6tau/17
   #:7tau/17
   #:8tau/17
   #:9tau/17
   #:10tau/17
   #:11tau/17
   #:12tau/17
   #:13tau/17
   #:14tau/17
   #:15tau/17
   #:16tau/17
   #:17tau/17
   #:tau/18
   #:2tau/18
   #:3tau/18
   #:4tau/18
   #:5tau/18
   #:6tau/18
   #:7tau/18
   #:8tau/18
   #:9tau/18
   #:10tau/18
   #:11tau/18
   #:12tau/18
   #:13tau/18
   #:14tau/18
   #:15tau/18
   #:16tau/18
   #:17tau/18
   #:18tau/18
   #:tau/19
   #:2tau/19
   #:3tau/19
   #:4tau/19
   #:5tau/19
   #:6tau/19
   #:7tau/19
   #:8tau/19
   #:9tau/19
   #:10tau/19
   #:11tau/19
   #:12tau/19
   #:13tau/19
   #:14tau/19
   #:15tau/19
   #:16tau/19
   #:17tau/19
   #:18tau/19
   #:19tau/19
   #:tau/20
   #:2tau/20
   #:3tau/20
   #:4tau/20
   #:5tau/20
   #:6tau/20
   #:7tau/20
   #:8tau/20
   #:9tau/20
   #:10tau/20
   #:11tau/20
   #:12tau/20
   #:13tau/20
   #:14tau/20
   #:15tau/20
   #:16tau/20
   #:17tau/20
   #:18tau/20
   #:19tau/20
   #:20tau/20
   #:tau/21
   #:2tau/21
   #:3tau/21
   #:4tau/21
   #:5tau/21
   #:6tau/21
   #:7tau/21
   #:8tau/21
   #:9tau/21
   #:10tau/21
   #:11tau/21
   #:12tau/21
   #:13tau/21
   #:14tau/21
   #:15tau/21
   #:16tau/21
   #:17tau/21
   #:18tau/21
   #:19tau/21
   #:20tau/21
   #:21tau/21
   #:tau/22
   #:2tau/22
   #:3tau/22
   #:4tau/22
   #:5tau/22
   #:6tau/22
   #:7tau/22
   #:8tau/22
   #:9tau/22
   #:10tau/22
   #:11tau/22
   #:12tau/22
   #:13tau/22
   #:14tau/22
   #:15tau/22
   #:16tau/22
   #:17tau/22
   #:18tau/22
   #:19tau/22
   #:20tau/22
   #:21tau/22
   #:22tau/22
   #:tau/23
   #:2tau/23
   #:3tau/23
   #:4tau/23
   #:5tau/23
   #:6tau/23
   #:7tau/23
   #:8tau/23
   #:9tau/23
   #:10tau/23
   #:11tau/23
   #:12tau/23
   #:13tau/23
   #:14tau/23
   #:15tau/23
   #:16tau/23
   #:17tau/23
   #:18tau/23
   #:19tau/23
   #:20tau/23
   #:21tau/23
   #:22tau/23
   #:23tau/23
   #:tau/24
   #:2tau/24
   #:3tau/24
   #:4tau/24
   #:5tau/24
   #:6tau/24
   #:7tau/24
   #:8tau/24
   #:9tau/24
   #:10tau/24
   #:11tau/24
   #:12tau/24
   #:13tau/24
   #:14tau/24
   #:15tau/24
   #:16tau/24
   #:17tau/24
   #:18tau/24
   #:19tau/24
   #:20tau/24
   #:21tau/24
   #:22tau/24
   #:23tau/24
   #:24tau/24
   #:tau/25
   #:2tau/25
   #:3tau/25
   #:4tau/25
   #:5tau/25
   #:6tau/25
   #:7tau/25
   #:8tau/25
   #:9tau/25
   #:10tau/25
   #:11tau/25
   #:12tau/25
   #:13tau/25
   #:14tau/25
   #:15tau/25
   #:16tau/25
   #:17tau/25
   #:18tau/25
   #:19tau/25
   #:20tau/25
   #:21tau/25
   #:22tau/25
   #:23tau/25
   #:24tau/25
   #:25tau/25
   #:tau/26
   #:2tau/26
   #:3tau/26
   #:4tau/26
   #:5tau/26
   #:6tau/26
   #:7tau/26
   #:8tau/26
   #:9tau/26
   #:10tau/26
   #:11tau/26
   #:12tau/26
   #:13tau/26
   #:14tau/26
   #:15tau/26
   #:16tau/26
   #:17tau/26
   #:18tau/26
   #:19tau/26
   #:20tau/26
   #:21tau/26
   #:22tau/26
   #:23tau/26
   #:24tau/26
   #:25tau/26
   #:26tau/26
   #:tau/27
   #:2tau/27
   #:3tau/27
   #:4tau/27
   #:5tau/27
   #:6tau/27
   #:7tau/27
   #:8tau/27
   #:9tau/27
   #:10tau/27
   #:11tau/27
   #:12tau/27
   #:13tau/27
   #:14tau/27
   #:15tau/27
   #:16tau/27
   #:17tau/27
   #:18tau/27
   #:19tau/27
   #:20tau/27
   #:21tau/27
   #:22tau/27
   #:23tau/27
   #:24tau/27
   #:25tau/27
   #:26tau/27
   #:27tau/27
   #:tau/28
   #:2tau/28
   #:3tau/28
   #:4tau/28
   #:5tau/28
   #:6tau/28
   #:7tau/28
   #:8tau/28
   #:9tau/28
   #:10tau/28
   #:11tau/28
   #:12tau/28
   #:13tau/28
   #:14tau/28
   #:15tau/28
   #:16tau/28
   #:17tau/28
   #:18tau/28
   #:19tau/28
   #:20tau/28
   #:21tau/28
   #:22tau/28
   #:23tau/28
   #:24tau/28
   #:25tau/28
   #:26tau/28
   #:27tau/28
   #:28tau/28
   #:tau/29
   #:2tau/29
   #:3tau/29
   #:4tau/29
   #:5tau/29
   #:6tau/29
   #:7tau/29
   #:8tau/29
   #:9tau/29
   #:10tau/29
   #:11tau/29
   #:12tau/29
   #:13tau/29
   #:14tau/29
   #:15tau/29
   #:16tau/29
   #:17tau/29
   #:18tau/29
   #:19tau/29
   #:20tau/29
   #:21tau/29
   #:22tau/29
   #:23tau/29
   #:24tau/29
   #:25tau/29
   #:26tau/29
   #:27tau/29
   #:28tau/29
   #:29tau/29
   #:tau/30
   #:2tau/30
   #:3tau/30
   #:4tau/30
   #:5tau/30
   #:6tau/30
   #:7tau/30
   #:8tau/30
   #:9tau/30
   #:10tau/30
   #:11tau/30
   #:12tau/30
   #:13tau/30
   #:14tau/30
   #:15tau/30
   #:16tau/30
   #:17tau/30
   #:18tau/30
   #:19tau/30
   #:20tau/30
   #:21tau/30
   #:22tau/30
   #:23tau/30
   #:24tau/30
   #:25tau/30
   #:26tau/30
   #:27tau/30
   #:28tau/30
   #:29tau/30
   #:30tau/30
   #:tau/31
   #:2tau/31
   #:3tau/31
   #:4tau/31
   #:5tau/31
   #:6tau/31
   #:7tau/31
   #:8tau/31
   #:9tau/31
   #:10tau/31
   #:11tau/31
   #:12tau/31
   #:13tau/31
   #:14tau/31
   #:15tau/31
   #:16tau/31
   #:17tau/31
   #:18tau/31
   #:19tau/31
   #:20tau/31
   #:21tau/31
   #:22tau/31
   #:23tau/31
   #:24tau/31
   #:25tau/31
   #:26tau/31
   #:27tau/31
   #:28tau/31
   #:29tau/31
   #:30tau/31
   #:31tau/31
   #:tau/32
   #:2tau/32
   #:3tau/32
   #:4tau/32
   #:5tau/32
   #:6tau/32
   #:7tau/32
   #:8tau/32
   #:9tau/32
   #:10tau/32
   #:11tau/32
   #:12tau/32
   #:13tau/32
   #:14tau/32
   #:15tau/32
   #:16tau/32
   #:17tau/32
   #:18tau/32
   #:19tau/32
   #:20tau/32
   #:21tau/32
   #:22tau/32
   #:23tau/32
   #:24tau/32
   #:25tau/32
   #:26tau/32
   #:27tau/32
   #:28tau/32
   #:29tau/32
   #:30tau/32
   #:31tau/32
   #:32tau/32

   ))

(in-package #:vorigin.constants)

(u:define-constant +rad+ (float (/ 180 cl:pi) 1f0))
(u:define-constant +rad/double+ (/ 180 cl:pi))
(u:define-constant +deg+ (float (/ cl:pi 180) 1f0))
(u:define-constant +deg/double+ (/ cl:pi 180))

;; NOTE: These pi/tau constants all could have been generated with a
;; defmacro and probably should have been. But I wanted M-. to find the
;; exact constant form and not just some aggregate macro form. Hence
;; they are written out in longhand up to 16 divisions of the half
;; circle and 32 divisions for the full circle.

(eval-when (:compile-toplevel :load-toplevel)
  (u:fn-> pi (&optional u:f32 u:f32) u:f32)
  (declaim (inline pi))
  (defun pi (&optional (multiplier 1f0) (divisor 1f0))
    (declare (type u:f32 multiplier divisor))
    "A function which expects MULTIPLIER, which defaults to 1f0, and
DIVISOR, which defaults to 1f0, to be single-float values and computes
and returns: (float (/ (* cl:pi MULTIPLIER) DIVISOR) 1f0)"
    (float (/ (* cl:pi multiplier) divisor) 1f0))

  (u:fn-> tau (&optional u:f32 u:f32) u:f32)
  (declaim (inline tau))
  (defun tau (&optional (multiplier 1f0) (divisor 1f0))
    (declare (type u:f32 multiplier divisor))
    "A function which expects MULTIPLIER, which defaults to 1f0, and DIVISOR,
which defaults to 1f0, to be single-float values and computes and
returns: (float (/ (* cl:pi 2f0 MULTIPLIER) DIVISOR) 1f0)"
    (float (/ (* cl:pi 2f0 multiplier) divisor) 1f0)))

;; Circle as a single whole
(u:define-constant tau (tau))

;; Circle with 1 piece in each half.
(u:define-constant pi (pi))
(u:define-constant 2pi (pi 2f0))

;; Circle with 2 pieces in each half
(u:define-constant pi/2 (pi 1f0 2f0))
(u:define-constant 2pi/2 pi)
(u:define-constant 3pi/2 (pi 3f0 2f0))
(u:define-constant 4pi/2 2pi)

;; Circle with 3 pieces in each half.
(u:define-constant pi/3 (pi 1f0 3f0))
(u:define-constant 2pi/3 (pi 2f0 3f0))
(u:define-constant 3pi/3 pi)
(u:define-constant 4pi/3 (pi 3f0 3f0))
(u:define-constant 5pi/3 (pi 5f0 3f0))
(u:define-constant 6pi/3 2pi)

;; Circle with 4 pieces in each half.
(u:define-constant pi/4 (pi 1f0 4f0))
(u:define-constant 2pi/4 pi/2)
(u:define-constant 3pi/4 (pi 3f0 4f0))
(u:define-constant 4pi/4 pi)
(u:define-constant 5pi/4 (pi 5f0 4f0))
(u:define-constant 6pi/4 3pi/2)
(u:define-constant 7pi/4 (pi 7f0 4f0))
(u:define-constant 8pi/4 2pi)

;; Circle with 5 pieces in each half
(u:define-constant pi/5 (pi 1f0 5f0))
(u:define-constant 2pi/5 (pi 2f0 5f0))
(u:define-constant 3pi/5 (pi 3f0 5f0))
(u:define-constant 4pi/5 (pi 4f0 5f0))
(u:define-constant 5pi/5 pi)
(u:define-constant 6pi/5 (pi 6f0 5f0))
(u:define-constant 7pi/5 (pi 7f0 5f0))
(u:define-constant 8pi/5 (pi 8f0 5f0))
(u:define-constant 9pi/5 (pi 9f0 5f0))
(u:define-constant 10pi/5 2pi)

;; Circle with 6 pieces in each half
(u:define-constant pi/6 (pi 1f0 6f0))
(u:define-constant 2pi/6 pi/3)
(u:define-constant 3pi/6 pi/2)
(u:define-constant 4pi/6 2pi/3)
(u:define-constant 5pi/6 (pi 5f0 6f0))
(u:define-constant 6pi/6 pi)
(u:define-constant 7pi/6 (pi 7f0 6f0))
(u:define-constant 8pi/6 4pi/3)
(u:define-constant 9pi/6 3pi/2)
(u:define-constant 10pi/6 5pi/3)
(u:define-constant 11pi/6 (pi 11f0 6f0))
(u:define-constant 12pi/6 2pi)

;; Circle with 7 pieces in each half
(u:define-constant pi/7 (pi 1f0 7f0))
(u:define-constant 2pi/7 (pi 2f0 7f0))
(u:define-constant 3pi/7 (pi 3f0 7f0))
(u:define-constant 4pi/7 (pi 4f0 7f0))
(u:define-constant 5pi/7 (pi 5f0 7f0))
(u:define-constant 6pi/7 (pi 6f0 7f0))
(u:define-constant 7pi/7 pi)
(u:define-constant 8pi/7 (pi 8f0 7f0))
(u:define-constant 9pi/7 (pi 9f0 7f0))
(u:define-constant 10pi/7 (pi 10f0 7f0))
(u:define-constant 11pi/7 (pi 11f0 7f0))
(u:define-constant 12pi/7 (pi 12f0 7f0))
(u:define-constant 13pi/7 (pi 13f0 7f0))
(u:define-constant 14pi/7 2pi)

;; Circle with 8 pieces in each half
(u:define-constant pi/8 (pi 1f0 8f0))
(u:define-constant 2pi/8 pi/4)
(u:define-constant 3pi/8 (pi 3f0 8f0))
(u:define-constant 4pi/8 pi/2)
(u:define-constant 5pi/8 (pi 5f0 8f0))
(u:define-constant 6pi/8 3pi/4)
(u:define-constant 7pi/8 (pi 7f0 8f0))
(u:define-constant 8pi/8 pi)
(u:define-constant 9pi/8 (pi 9f0 8f0))
(u:define-constant 10pi/8 5pi/4)
(u:define-constant 11pi/8 (pi 11f0 8f0))
(u:define-constant 12pi/8 3pi/2)
(u:define-constant 13pi/8 (pi 13f0 8f0))
(u:define-constant 14pi/8 7pi/4)
(u:define-constant 15pi/8 (pi 15f0 8f0))
(u:define-constant 16pi/8 2pi)

;; Circle with 9 pieces in each half
(u:define-constant pi/9 (pi 1f0 9f0))
(u:define-constant 2pi/9 (pi 2f0 9f0))
(u:define-constant 3pi/9 pi/3)
(u:define-constant 4pi/9 (pi 4f0 9f0))
(u:define-constant 5pi/9 (pi 5f0 9f0))
(u:define-constant 6pi/9 2pi/3)
(u:define-constant 7pi/9 (pi 7f0 9f0))
(u:define-constant 8pi/9 (pi 8f0 9f0))
(u:define-constant 9pi/9 pi)
(u:define-constant 10pi/9 (pi 10f0 9f0))
(u:define-constant 11pi/9 (pi 11f0 9f0))
(u:define-constant 12pi/9 4pi/3)
(u:define-constant 13pi/9 (pi 13f0 9f0))
(u:define-constant 14pi/9 (pi 14f0 9f0))
(u:define-constant 15pi/9 5pi/3)
(u:define-constant 16pi/9 (pi 16f0 9f0))
(u:define-constant 17pi/9 (pi 17f0 9f0))
(u:define-constant 18pi/9 2pi)

;; Circle with 10 pieces in each half
(u:define-constant pi/10 (pi 1f0 10f0))
(u:define-constant 2pi/10 pi/5)
(u:define-constant 3pi/10 (pi 3f0 10f0))
(u:define-constant 4pi/10 2pi/5)
(u:define-constant 5pi/10 pi/2)
(u:define-constant 6pi/10 3pi/5)
(u:define-constant 7pi/10 (pi 7f0 10f0))
(u:define-constant 8pi/10 4pi/5)
(u:define-constant 9pi/10 (pi 9f0 10f0))
(u:define-constant 10pi/10 pi)
(u:define-constant 11pi/10 (pi 11f0 10f0))
(u:define-constant 12pi/10 6pi/5)
(u:define-constant 13pi/10 (pi 13f0 10f0))
(u:define-constant 14pi/10 7pi/5)
(u:define-constant 15pi/10 3pi/2)
(u:define-constant 16pi/10 8pi/5)
(u:define-constant 17pi/10 (pi 17f0 10f0))
(u:define-constant 18pi/10 9pi/5)
(u:define-constant 19pi/10 (pi 19f0 10f0))
(u:define-constant 20pi/10 2pi)

;; Circle with 11 pieces in each half
(u:define-constant pi/11 (pi 1f0 11f0))
(u:define-constant 2pi/11 (pi 2f0 11f0))
(u:define-constant 3pi/11 (pi 3f0 11f0))
(u:define-constant 4pi/11 (pi 4f0 11f0))
(u:define-constant 5pi/11 (pi 5f0 11f0))
(u:define-constant 6pi/11 (pi 6f0 11f0))
(u:define-constant 7pi/11 (pi 7f0 11f0))
(u:define-constant 8pi/11 (pi 8f0 11f0))
(u:define-constant 9pi/11 (pi 9f0 11f0))
(u:define-constant 10pi/11 (pi 10f0 11f0))
(u:define-constant 11pi/11 pi)
(u:define-constant 12pi/11 (pi 12f0 11f0))
(u:define-constant 13pi/11 (pi 13f0 11f0))
(u:define-constant 14pi/11 (pi 14f0 11f0))
(u:define-constant 15pi/11 (pi 15f0 11f0))
(u:define-constant 16pi/11 (pi 16f0 11f0))
(u:define-constant 17pi/11 (pi 17f0 11f0))
(u:define-constant 18pi/11 (pi 18f0 11f0))
(u:define-constant 19pi/11 (pi 19f0 11f0))
(u:define-constant 20pi/11 (pi 20f0 11f0))
(u:define-constant 21pi/11 (pi 21f0 11f0))
(u:define-constant 22pi/11 2pi)

;; Circle with 12 pieces in a half.
(u:define-constant pi/12 (pi 1f0 12f0))
(u:define-constant 2pi/12 pi/6)
(u:define-constant 3pi/12 pi/4)
(u:define-constant 4pi/12 pi/3)
(u:define-constant 5pi/12 (pi 5f0 12f0))
(u:define-constant 6pi/12 pi/2)
(u:define-constant 7pi/12 (pi 7f0 12f0))
(u:define-constant 8pi/12 2pi/3)
(u:define-constant 9pi/12 3pi/4)
(u:define-constant 10pi/12 5pi/6)
(u:define-constant 11pi/12 (pi 11f0 12f0))
(u:define-constant 12pi/12 pi)
(u:define-constant 13pi/12 (pi 13f0 12f0))
(u:define-constant 14pi/12 7pi/6)
(u:define-constant 15pi/12 5pi/4)
(u:define-constant 16pi/12 4pi/3)
(u:define-constant 17pi/12 (pi 17f0 12f0))
(u:define-constant 18pi/12 3pi/2)
(u:define-constant 19pi/12 (pi 19f0 12f0))
(u:define-constant 20pi/12 5pi/3)
(u:define-constant 21pi/12 7pi/4)
(u:define-constant 22pi/12 11pi/6)
(u:define-constant 23pi/12 (pi 23f0 12f0))
(u:define-constant 24pi/12 2pi)

;; Circle with 13 pieces in a half.
(u:define-constant pi/13 (pi 1f0 13f0))
(u:define-constant 2pi/13 (pi 2f0 13f0))
(u:define-constant 3pi/13 (pi 2f0 13f0))
(u:define-constant 4pi/13 (pi 4f0 13f0))
(u:define-constant 5pi/13 (pi 5f0 13f0))
(u:define-constant 6pi/13 (pi 6f0 13f0))
(u:define-constant 7pi/13 (pi 7f0 13f0))
(u:define-constant 8pi/13 (pi 8f0 13f0))
(u:define-constant 9pi/13 (pi 9f0 13f0))
(u:define-constant 10pi/13 (pi 10f0 13f0))
(u:define-constant 11pi/13 (pi 11f0 13f0))
(u:define-constant 12pi/13 (pi 12f0 13f0))
(u:define-constant 13pi/13 pi)
(u:define-constant 14pi/13 (pi 14f0 13f0))
(u:define-constant 15pi/13 (pi 15f0 13f0))
(u:define-constant 16pi/13 (pi 16f0 13f0))
(u:define-constant 17pi/13 (pi 17f0 13f0))
(u:define-constant 18pi/13 (pi 18f0 13f0))
(u:define-constant 19pi/13 (pi 19f0 13f0))
(u:define-constant 20pi/13 (pi 20f0 13f0))
(u:define-constant 21pi/13 (pi 21f0 13f0))
(u:define-constant 22pi/13 (pi 22f0 13f0))
(u:define-constant 23pi/13 (pi 23f0 13f0))
(u:define-constant 24pi/13 (pi 24f0 13f0))
(u:define-constant 25pi/13 (pi 25f0 13f0))
(u:define-constant 26pi/13 2pi)

;; Circle with 14 pieces in a half.
(u:define-constant pi/14 (pi 1f0 14f0))
(u:define-constant 2pi/14 pi/7)
(u:define-constant 3pi/14 (pi 3f0 14f0))
(u:define-constant 4pi/14 2pi/7)
(u:define-constant 5pi/14 (pi 5f0 14f0))
(u:define-constant 6pi/14 3pi/7)
(u:define-constant 7pi/14 pi/2)
(u:define-constant 8pi/14 4pi/7)
(u:define-constant 9pi/14 (pi 9f0 14f0))
(u:define-constant 10pi/14 5pi/7)
(u:define-constant 11pi/14 (pi 11f0 14f0))
(u:define-constant 12pi/14 6pi/7)
(u:define-constant 13pi/14 (pi 13f0 14f0))
(u:define-constant 14pi/14 pi)
(u:define-constant 15pi/14 (pi 15f0 14f0))
(u:define-constant 16pi/14 8pi/7)
(u:define-constant 17pi/14 (pi 17f0 14f0))
(u:define-constant 18pi/14 9pi/7)
(u:define-constant 19pi/14 (pi 19f0 14f0))
(u:define-constant 20pi/14 5pi/7)
(u:define-constant 21pi/14 3pi/2)
(u:define-constant 22pi/14 11pi/7)
(u:define-constant 23pi/14 (pi 23f0 14f0))
(u:define-constant 24pi/14 12pi/7)
(u:define-constant 25pi/14 (pi 25f0 14f0))
(u:define-constant 26pi/14 13pi/7)
(u:define-constant 27pi/14 (pi 27f0 14f0))
(u:define-constant 28pi/14 2pi)

;; Circle with 15 pieces in a half.
(u:define-constant pi/15 (pi 1f0 15f0))
(u:define-constant 2pi/15 (pi 2f0 15f0))
(u:define-constant 3pi/15 pi/5)
(u:define-constant 4pi/15 (pi 4f0 15f0))
(u:define-constant 5pi/15 pi/3)
(u:define-constant 6pi/15 2pi/5)
(u:define-constant 7pi/15 (pi 7f0 15f0))
(u:define-constant 8pi/15 (pi 8f0 15f0))
(u:define-constant 9pi/15 3pi/5)
(u:define-constant 10pi/15 2pi/3)
(u:define-constant 11pi/15 (pi 11f0 15f0))
(u:define-constant 12pi/15 4pi/5)
(u:define-constant 13pi/15 (pi 13f0 15f0))
(u:define-constant 14pi/15 (pi 14f0 15f0))
(u:define-constant 15pi/15 pi)
(u:define-constant 16pi/15 (pi 16f0 15f0))
(u:define-constant 17pi/15 (pi 17f0 15f0))
(u:define-constant 18pi/15 6pi/5)
(u:define-constant 19pi/15 (pi 19f0 15f0))
(u:define-constant 20pi/15 4pi/3)
(u:define-constant 21pi/15 7pi/5)
(u:define-constant 22pi/15 (pi 22f0 15f0))
(u:define-constant 23pi/15 (pi 23f0 15f0))
(u:define-constant 24pi/15 6pi/5)
(u:define-constant 25pi/15 5pi/3)
(u:define-constant 26pi/15 (pi 26f0 15f0))
(u:define-constant 27pi/15 9pi/5)
(u:define-constant 28pi/15 (pi 28f0 15f0))
(u:define-constant 29pi/15 (pi 29f0 15f0))
(u:define-constant 30pi/15 2pi)

;; Circle with 16 pieces in a half.
(u:define-constant pi/16 (pi 1f0 16f0))
(u:define-constant 2pi/16 pi/8)
(u:define-constant 3pi/16 (pi 3f0 16f0))
(u:define-constant 4pi/16 pi/4)
(u:define-constant 5pi/16 (pi 5f0 16f0))
(u:define-constant 6pi/16 3pi/8)
(u:define-constant 7pi/16 (pi 7f0 16f0))
(u:define-constant 8pi/16 pi/2)
(u:define-constant 9pi/16 (pi 9f0 16f0))
(u:define-constant 10pi/16 5pi/8)
(u:define-constant 11pi/16 (pi 11f0 16f0))
(u:define-constant 12pi/16 3pi/4)
(u:define-constant 13pi/16 (pi 13f0 16f0))
(u:define-constant 14pi/16 7pi/8)
(u:define-constant 15pi/16 (pi 15f0 16f0))
(u:define-constant 16pi/16 pi)
(u:define-constant 17pi/16 (pi 17f0 16f0))
(u:define-constant 18pi/16 9pi/8)
(u:define-constant 19pi/16 (pi 19f0 16f0))
(u:define-constant 20pi/16 5pi/4)
(u:define-constant 21pi/16 (pi 21f0 16f0))
(u:define-constant 22pi/16 11pi/8)
(u:define-constant 23pi/16 (pi 23f0 16f0))
(u:define-constant 24pi/16 3pi/2)
(u:define-constant 25pi/16 (pi 25f0 16f0))
(u:define-constant 26pi/16 13pi/8)
(u:define-constant 27pi/16 (pi 27f0 16f0))
(u:define-constant 28pi/16 7pi/4)
(u:define-constant 29pi/16 (pi 29f0 16f0))
(u:define-constant 30pi/16 15pi/8)
(u:define-constant 31pi/16 (pi 31f0 16f0))
(u:define-constant 32pi/16 2pi)

;; Whole circle split into 2 parts.
(u:define-constant tau/2 pi)
(u:define-constant 2tau/2 tau)

;; Whole circle split into 3 parts
(u:define-constant tau/3 (tau 1f0 3f0))
(u:define-constant 2tau/3 (tau 2f0 3f0))
(u:define-constant 3tau/3 tau)

;; Whole circle split into 4 parts
(u:define-constant tau/4 (tau 1f0 4f0))
(u:define-constant 2tau/4 tau/2)
(u:define-constant 3tau/4 (tau 3f0 4f0))
(u:define-constant 4tau/4 tau)

;; Whole circle split into 5 parts
(u:define-constant tau/5 (tau 1f0 5f0))
(u:define-constant 2tau/5 (tau 2f0 5f0))
(u:define-constant 3tau/5 (tau 3f0 5f0))
(u:define-constant 4tau/5 (tau 4f0 5f0))
(u:define-constant 5tau/5 tau)

;; Whole circle split into 6 parts
(u:define-constant tau/6 (tau 1f0 6f0))
(u:define-constant 2tau/6 tau/3)
(u:define-constant 3tau/6 tau/2)
(u:define-constant 4tau/6 2tau/3)
(u:define-constant 5tau/6 (tau 5f0 6f0))
(u:define-constant 6tau/6 tau)

;; Whole circle split into 7 parts
(u:define-constant tau/7 (tau 1f0 7f0))
(u:define-constant 2tau/7 (tau 2f0 7f0))
(u:define-constant 3tau/7 (tau 3f0 7f0))
(u:define-constant 4tau/7 (tau 4f0 7f0))
(u:define-constant 5tau/7 (tau 5f0 7f0))
(u:define-constant 6tau/7 (tau 6f0 7f0))
(u:define-constant 7tau/7 tau)

;; Whole circle split into 8 parts
(u:define-constant tau/8 (tau 1f0 8f0))
(u:define-constant 2tau/8 tau/4)
(u:define-constant 3tau/8 (tau 3f0 8f0))
(u:define-constant 4tau/8 tau/2)
(u:define-constant 5tau/8 (tau 5f0 8f0))
(u:define-constant 6tau/8 3tau/4)
(u:define-constant 7tau/8 (tau 7f0 8f0))
(u:define-constant 8tau/8 tau)

;; Whole circle split into 9 parts
(u:define-constant tau/9 (tau 1f0 9f0))
(u:define-constant 2tau/9 (tau 2f0 9f0))
(u:define-constant 3tau/9 tau/3)
(u:define-constant 4tau/9 (tau 4f0 9f0))
(u:define-constant 5tau/9 (tau 5f0 9f0))
(u:define-constant 6tau/9 2tau/3)
(u:define-constant 7tau/9 (tau 7f0 9f0))
(u:define-constant 8tau/9 (tau 8f0 9f0))
(u:define-constant 9tau/9 tau)

;; Whole circle split into 10 parts
(u:define-constant tau/10 (tau 1f0 10f0))
(u:define-constant 2tau/10 tau/5)
(u:define-constant 3tau/10 (tau 3f0 10f0))
(u:define-constant 4tau/10 2tau/5)
(u:define-constant 5tau/10 tau/2)
(u:define-constant 6tau/10 3tau/5)
(u:define-constant 7tau/10 (tau 7f0 10f0))
(u:define-constant 8tau/10 4tau/5)
(u:define-constant 9tau/10 (tau 9f0 10f0))
(u:define-constant 10tau/10 tau)

;; Whole circle split into 11 parts
(u:define-constant tau/11 (tau 1f0 11f0))
(u:define-constant 2tau/11 (tau 2f0 11f0))
(u:define-constant 3tau/11 (tau 3f0 11f0))
(u:define-constant 4tau/11 (tau 4f0 11f0))
(u:define-constant 5tau/11 (tau 5f0 11f0))
(u:define-constant 6tau/11 (tau 6f0 11f0))
(u:define-constant 7tau/11 (tau 7f0 11f0))
(u:define-constant 8tau/11 (tau 9f0 11f0))
(u:define-constant 9tau/11 (tau 9f0 11f0))
(u:define-constant 10tau/11 (tau 10f0 11f0))
(u:define-constant 11tau/11 tau)

;; Whole circle split into 12 parts
(u:define-constant tau/12 (tau 1f0 12f0))
(u:define-constant 2tau/12 tau/6)
(u:define-constant 3tau/12 tau/4)
(u:define-constant 4tau/12 tau/3)
(u:define-constant 5tau/12 (tau 5f0 12f0))
(u:define-constant 6tau/12 tau/2)
(u:define-constant 7tau/12 (tau 7f0 12f0))
(u:define-constant 8tau/12 2tau/3)
(u:define-constant 9tau/12 3tau/4)
(u:define-constant 10tau/12 5tau/6)
(u:define-constant 11tau/12 (tau 11f0 12f0))
(u:define-constant 12tau/12 tau)

;; Whole circle split into 13 parts
(u:define-constant tau/13 (tau 1f0 13f0))
(u:define-constant 2tau/13 (tau 2f0 13f0))
(u:define-constant 3tau/13 (tau 3f0 13f0))
(u:define-constant 4tau/13 (tau 3f0 13f0))
(u:define-constant 5tau/13 (tau 5f0 13f0))
(u:define-constant 6tau/13 (tau 6f0 13f0))
(u:define-constant 7tau/13 (tau 7f0 13f0))
(u:define-constant 8tau/13 (tau 8f0 13f0))
(u:define-constant 9tau/13 (tau 9f0 13f0))
(u:define-constant 10tau/13 (tau 10f0 13f0))
(u:define-constant 11tau/13 (tau 11f0 13f0))
(u:define-constant 12tau/13 (tau 12f0 13f0))
(u:define-constant 13tau/13 tau)

;; Whole circle split into 14 parts
(u:define-constant tau/14 (tau 1f0 14f0))
(u:define-constant 2tau/14 tau/7)
(u:define-constant 3tau/14 (tau 3f0 14f0))
(u:define-constant 4tau/14 2tau/7)
(u:define-constant 5tau/14 (tau 5f0 14f0))
(u:define-constant 6tau/14 3tau/7)
(u:define-constant 7tau/14 tau/2)
(u:define-constant 8tau/14 4tau/7)
(u:define-constant 9tau/14 (tau 9f0 14f0))
(u:define-constant 10tau/14 5tau/7)
(u:define-constant 11tau/14 (tau 11f0 14f0))
(u:define-constant 12tau/14 6tau/7)
(u:define-constant 13tau/14 (tau 13f0 14f0))
(u:define-constant 14tau/14 tau)

;; Whole circle split into 15 parts
(u:define-constant tau/15 (tau 1f0 15f0))
(u:define-constant 2tau/15 (tau 2f0 15f0))
(u:define-constant 3tau/15 tau/5)
(u:define-constant 4tau/15 (tau 4f0 15f0))
(u:define-constant 5tau/15 tau/3)
(u:define-constant 6tau/15 2tau/5)
(u:define-constant 7tau/15 (tau 7f0 15f0))
(u:define-constant 8tau/15 (tau 8f0 15f0))
(u:define-constant 9tau/15 3tau/5)
(u:define-constant 10tau/15 2tau/3)
(u:define-constant 11tau/15 (tau 11f0 15f0))
(u:define-constant 12tau/15 4tau/5)
(u:define-constant 13tau/15 (tau 13f0 15f0))
(u:define-constant 14tau/15 (tau 14f0 15f0))
(u:define-constant 15tau/15 tau)

;; Whole circle split into 16 parts
(u:define-constant tau/16 (tau 1f0 16f0))
(u:define-constant 2tau/16 tau/8)
(u:define-constant 3tau/16 (tau 3f0 16f0))
(u:define-constant 4tau/16 tau/4)
(u:define-constant 5tau/16 (tau 5f0 16f0))
(u:define-constant 6tau/16 3tau/8)
(u:define-constant 7tau/16 (tau 7f0 16f0))
(u:define-constant 8tau/16 tau/2)
(u:define-constant 9tau/16 (tau 9f0 16f0))
(u:define-constant 10tau/16 5tau/8)
(u:define-constant 11tau/16 (tau 11f0 16f0))
(u:define-constant 12tau/16 3tau/4)
(u:define-constant 13tau/16 (tau 13f0 16f0))
(u:define-constant 14tau/16 7tau/8)
(u:define-constant 15tau/16 (tau 15f0 16f0))
(u:define-constant 16tau/16 tau)

;; Whole circle split into 17 parts
(u:define-constant tau/17 (tau 1f0 17f0))
(u:define-constant 2tau/17 (tau 2f0 17f0))
(u:define-constant 3tau/17 (tau 3f0 17f0))
(u:define-constant 4tau/17 (tau 4f0 17f0))
(u:define-constant 5tau/17 (tau 5f0 17f0))
(u:define-constant 6tau/17 (tau 6f0 17f0))
(u:define-constant 7tau/17 (tau 7f0 17f0))
(u:define-constant 8tau/17 (tau 8f0 17f0))
(u:define-constant 9tau/17 (tau 9f0 17f0))
(u:define-constant 10tau/17 (tau 10f0 17f0))
(u:define-constant 11tau/17 (tau 11f0 17f0))
(u:define-constant 12tau/17 (tau 12f0 17f0))
(u:define-constant 13tau/17 (tau 13f0 17f0))
(u:define-constant 14tau/17 (tau 14f0 17f0))
(u:define-constant 15tau/17 (tau 15f0 17f0))
(u:define-constant 16tau/17 (tau 16f0 17f0))
(u:define-constant 17tau/17 tau)

;; Whole circle split into 18 parts
(u:define-constant tau/18 (tau 1f0 18f0))
(u:define-constant 2tau/18 tau/9)
(u:define-constant 3tau/18 tau/6)
(u:define-constant 4tau/18 2tau/9)
(u:define-constant 5tau/18 (tau 5f0 18f0))
(u:define-constant 6tau/18 tau/3)
(u:define-constant 7tau/18 (tau 7f0 18f0))
(u:define-constant 8tau/18 4tau/9)
(u:define-constant 9tau/18 tau/2)
(u:define-constant 10tau/18 5tau/9)
(u:define-constant 11tau/18 (tau 11f0 18f0))
(u:define-constant 12tau/18 2tau/3)
(u:define-constant 13tau/18 (tau 13f0 18f0))
(u:define-constant 14tau/18 7tau/9)
(u:define-constant 15tau/18 5tau/6)
(u:define-constant 16tau/18 8tau/9)
(u:define-constant 17tau/18 (tau 17f0 18f0))
(u:define-constant 18tau/18 tau)

;; Whole circle split into 19 parts
(u:define-constant tau/19 (tau 1f0 19f0))
(u:define-constant 2tau/19 (tau 2f0 19f0))
(u:define-constant 3tau/19 (tau 3f0 19f0))
(u:define-constant 4tau/19 (tau 4f0 19f0))
(u:define-constant 5tau/19 (tau 5f0 19f0))
(u:define-constant 6tau/19 (tau 6f0 19f0))
(u:define-constant 7tau/19 (tau 7f0 19f0))
(u:define-constant 8tau/19 (tau 8f0 19f0))
(u:define-constant 9tau/19 (tau 9f0 19f0))
(u:define-constant 10tau/19 (tau 10f0 19f0))
(u:define-constant 11tau/19 (tau 11f0 19f0))
(u:define-constant 12tau/19 (tau 12f0 19f0))
(u:define-constant 13tau/19 (tau 13f0 19f0))
(u:define-constant 14tau/19 (tau 14f0 19f0))
(u:define-constant 15tau/19 (tau 15f0 19f0))
(u:define-constant 16tau/19 (tau 16f0 19f0))
(u:define-constant 17tau/19 (tau 17f0 19f0))
(u:define-constant 18tau/19 (tau 18f0 19f0))
(u:define-constant 19tau/19 tau)

;; Whole circle split into 20 parts
(u:define-constant tau/20 (tau 1f0 20f0))
(u:define-constant 2tau/20 tau/10)
(u:define-constant 3tau/20 (tau 3f0 20f0))
(u:define-constant 4tau/20 tau/5)
(u:define-constant 5tau/20 tau/4)
(u:define-constant 6tau/20 3tau/10)
(u:define-constant 7tau/20 (tau 7f0 20f0))
(u:define-constant 8tau/20 2tau/5)
(u:define-constant 9tau/20 (tau 9f0 20f0))
(u:define-constant 10tau/20 tau/2)
(u:define-constant 11tau/20 (tau 11f0 20f0))
(u:define-constant 12tau/20 3tau/5)
(u:define-constant 13tau/20 (tau 13f0 20f0))
(u:define-constant 14tau/20 7tau/10)
(u:define-constant 15tau/20 3tau/4)
(u:define-constant 16tau/20 4tau/5)
(u:define-constant 17tau/20 (tau 17f0 20f0))
(u:define-constant 18tau/20 9tau/10)
(u:define-constant 19tau/20 (tau 19f0 20f0))
(u:define-constant 20tau/20 tau)

;; Whole circle split into 21 parts
(u:define-constant tau/21 (tau 1f0 21f0))
(u:define-constant 2tau/21 (tau 2f0 21f0))
(u:define-constant 3tau/21 tau/7)
(u:define-constant 4tau/21 (tau 4f0 21f0))
(u:define-constant 5tau/21 (tau 5f0 21f0))
(u:define-constant 6tau/21 2tau/7)
(u:define-constant 7tau/21 tau/3)
(u:define-constant 8tau/21 (tau 8f0 21f0))
(u:define-constant 9tau/21 3tau/7)
(u:define-constant 10tau/21 (tau 10f0 21f0))
(u:define-constant 11tau/21 (tau 11f0 21f0))
(u:define-constant 12tau/21 4tau/7)
(u:define-constant 13tau/21 (tau 13f0 21f0))
(u:define-constant 14tau/21 2tau/3)
(u:define-constant 15tau/21 5tau/7)
(u:define-constant 16tau/21 (tau 16f0 21f0))
(u:define-constant 17tau/21 (tau 17f0 21f0))
(u:define-constant 18tau/21 6tau/7)
(u:define-constant 19tau/21 (tau 19f0 21f0))
(u:define-constant 20tau/21 (tau 20f0 21f0))
(u:define-constant 21tau/21 tau)

;; Whole circle split into 22 parts
(u:define-constant tau/22 (tau 1f0 22f0))
(u:define-constant 2tau/22 tau/11)
(u:define-constant 3tau/22 (tau 3f0 22f0))
(u:define-constant 4tau/22 2tau/11)
(u:define-constant 5tau/22 (tau 5f0 22f0))
(u:define-constant 6tau/22 3tau/11)
(u:define-constant 7tau/22 (tau 7f0 22f0))
(u:define-constant 8tau/22 4tau/11)
(u:define-constant 9tau/22 (tau 9f0 22f0))
(u:define-constant 10tau/22 5tau/11)
(u:define-constant 11tau/22 tau/2)
(u:define-constant 12tau/22 6tau/11)
(u:define-constant 13tau/22 (tau 13f0 22f0))
(u:define-constant 14tau/22 7tau/11)
(u:define-constant 15tau/22 (tau 15f0 22f0))
(u:define-constant 16tau/22 8tau/11)
(u:define-constant 17tau/22 (tau 17f0 22f0))
(u:define-constant 18tau/22 9tau/11)
(u:define-constant 19tau/22 (tau 19f0 22f0))
(u:define-constant 20tau/22 10tau/11)
(u:define-constant 21tau/22 (tau 21f0 22f0))
(u:define-constant 22tau/22 tau)

;; Whole circle split into 23 parts
(u:define-constant tau/23 (tau 1f0 23f0))
(u:define-constant 2tau/23 (tau 2f0 23f0))
(u:define-constant 3tau/23 (tau 3f0 23f0))
(u:define-constant 4tau/23 (tau 4f0 23f0))
(u:define-constant 5tau/23 (tau 5f0 23f0))
(u:define-constant 6tau/23 (tau 6f0 23f0))
(u:define-constant 7tau/23 (tau 7f0 23f0))
(u:define-constant 8tau/23 (tau 8f0 23f0))
(u:define-constant 9tau/23 (tau 9f0 23f0))
(u:define-constant 10tau/23 (tau 10f0 23f0))
(u:define-constant 11tau/23 (tau 11f0 23f0))
(u:define-constant 12tau/23 (tau 12f0 23f0))
(u:define-constant 13tau/23 (tau 13f0 23f0))
(u:define-constant 14tau/23 (tau 14f0 23f0))
(u:define-constant 15tau/23 (tau 15f0 23f0))
(u:define-constant 16tau/23 (tau 16f0 23f0))
(u:define-constant 17tau/23 (tau 17f0 23f0))
(u:define-constant 18tau/23 (tau 18f0 23f0))
(u:define-constant 19tau/23 (tau 19f0 23f0))
(u:define-constant 20tau/23 (tau 20f0 23f0))
(u:define-constant 21tau/23 (tau 21f0 23f0))
(u:define-constant 22tau/23 (tau 22f0 23f0))
(u:define-constant 23tau/23 tau)

;; Whole circle split into 24 parts
(u:define-constant tau/24 (tau 1f0 24f0))
(u:define-constant 2tau/24 tau/12)
(u:define-constant 3tau/24 tau/8)
(u:define-constant 4tau/24 tau/6)
(u:define-constant 5tau/24 (tau 5f0 24f0))
(u:define-constant 6tau/24 tau/4)
(u:define-constant 7tau/24 (tau 7f0 24f0))
(u:define-constant 8tau/24 tau/3)
(u:define-constant 9tau/24 3tau/8)
(u:define-constant 10tau/24 5tau/12)
(u:define-constant 11tau/24 (tau 11f0 24f0))
(u:define-constant 12tau/24 tau/2)
(u:define-constant 13tau/24 (tau 13f0 24f0))
(u:define-constant 14tau/24 7tau/12)
(u:define-constant 15tau/24 5tau/8)
(u:define-constant 16tau/24 2tau/3)
(u:define-constant 17tau/24 (tau 17f0 24f0))
(u:define-constant 18tau/24 3tau/4)
(u:define-constant 19tau/24 (tau 19f0 24f0))
(u:define-constant 20tau/24 5tau/6)
(u:define-constant 21tau/24 7tau/8)
(u:define-constant 22tau/24 11tau/12)
(u:define-constant 23tau/24 (tau 23f0 24f0))
(u:define-constant 24tau/24 tau)

;; Whole circle split into 25 parts
(u:define-constant tau/25 (tau 1f0 25f0))
(u:define-constant 2tau/25 (tau 2f0 25f0))
(u:define-constant 3tau/25 (tau 3f0 25f0))
(u:define-constant 4tau/25 (tau 4f0 25f0))
(u:define-constant 5tau/25 tau/5)
(u:define-constant 6tau/25 (tau 6f0 25f0))
(u:define-constant 7tau/25 (tau 7f0 25f0))
(u:define-constant 8tau/25 (tau 8f0 25f0))
(u:define-constant 9tau/25 (tau 9f0 25f0))
(u:define-constant 10tau/25 2tau/5)
(u:define-constant 11tau/25 (tau 11f0 25f0))
(u:define-constant 12tau/25 (tau 12f0 25f0))
(u:define-constant 13tau/25 (tau 13f0 25f0))
(u:define-constant 14tau/25 (tau 14f0 25f0))
(u:define-constant 15tau/25 3tau/5)
(u:define-constant 16tau/25 (tau 16f0 25f0))
(u:define-constant 17tau/25 (tau 17f0 25f0))
(u:define-constant 18tau/25 (tau 18f0 25f0))
(u:define-constant 19tau/25 (tau 19f0 25f0))
(u:define-constant 20tau/25 4tau/5)
(u:define-constant 21tau/25 (tau 21f0 25f0))
(u:define-constant 22tau/25 (tau 22f0 25f0))
(u:define-constant 23tau/25 (tau 23f0 25f0))
(u:define-constant 24tau/25 (tau 24f0 25f0))
(u:define-constant 25tau/25 tau)

;; Whole circle split into 26 parts
(u:define-constant tau/26 (tau 1f0 26f0))
(u:define-constant 2tau/26 tau/13)
(u:define-constant 3tau/26 (tau 3f0 26f0))
(u:define-constant 4tau/26 2tau/13)
(u:define-constant 5tau/26 (tau 5f0 26f0))
(u:define-constant 6tau/26 3tau/13)
(u:define-constant 7tau/26 (tau 7f0 26f0))
(u:define-constant 8tau/26 4tau/13)
(u:define-constant 9tau/26 (tau 9f0 26f0))
(u:define-constant 10tau/26 5tau/13)
(u:define-constant 11tau/26 (tau 11f0 26f0))
(u:define-constant 12tau/26 6tau/13)
(u:define-constant 13tau/26 tau/2)
(u:define-constant 14tau/26 7tau/13)
(u:define-constant 15tau/26 (tau 15f0 26f0))
(u:define-constant 16tau/26 8tau/13)
(u:define-constant 17tau/26 (tau 17f0 26f0))
(u:define-constant 18tau/26 9tau/13)
(u:define-constant 19tau/26 (tau 19f0 26f0))
(u:define-constant 20tau/26 10tau/13)
(u:define-constant 21tau/26 (tau 21f0 26f0))
(u:define-constant 22tau/26 11tau/13)
(u:define-constant 23tau/26 (tau 23f0 26f0))
(u:define-constant 24tau/26 12tau/13)
(u:define-constant 25tau/26 (tau 25f0 26f0))
(u:define-constant 26tau/26 tau)

;; Whole circle split into 27 parts
(u:define-constant tau/27 (tau 1f0 27f0))
(u:define-constant 2tau/27 (tau 2f0 27f0))
(u:define-constant 3tau/27 tau/9)
(u:define-constant 4tau/27 (tau 4f0 27f0))
(u:define-constant 5tau/27 (tau 5f0 27f0))
(u:define-constant 6tau/27 2tau/9)
(u:define-constant 7tau/27 (tau 7f0 27f0))
(u:define-constant 8tau/27 (tau 8f0 27f0))
(u:define-constant 9tau/27 tau/3)
(u:define-constant 10tau/27 (tau 10f0 27f0))
(u:define-constant 11tau/27 (tau 11f0 27f0))
(u:define-constant 12tau/27 4tau/9)
(u:define-constant 13tau/27 (tau 13f0 27f0))
(u:define-constant 14tau/27 (tau 14f0 27f0))
(u:define-constant 15tau/27 5tau/9)
(u:define-constant 16tau/27 (tau 16f0 27f0))
(u:define-constant 17tau/27 (tau 17f0 27f0))
(u:define-constant 18tau/27 2tau/3)
(u:define-constant 19tau/27 (tau 19f0 27f0))
(u:define-constant 20tau/27 (tau 20f0 27f0))
(u:define-constant 21tau/27 7tau/9)
(u:define-constant 22tau/27 (tau 22f0 27f0))
(u:define-constant 23tau/27 (tau 23f0 27f0))
(u:define-constant 24tau/27 8tau/9)
(u:define-constant 25tau/27 (tau 25f0 27f0))
(u:define-constant 26tau/27 (tau 26f0 27f0))
(u:define-constant 27tau/27 tau)

;; Whole circle split into 28 parts
(u:define-constant tau/28 (tau 1f0 28f0))
(u:define-constant 2tau/28 tau/14)
(u:define-constant 3tau/28 (tau 3f0 28f0))
(u:define-constant 4tau/28 tau/7)
(u:define-constant 5tau/28 (tau 5f0 28f0))
(u:define-constant 6tau/28 3tau/14)
(u:define-constant 7tau/28 tau/4)
(u:define-constant 8tau/28 2tau/7)
(u:define-constant 9tau/28 (tau 9f0 28f0))
(u:define-constant 10tau/28 5tau/14)
(u:define-constant 11tau/28 (tau 11f0 28f0))
(u:define-constant 12tau/28 3tau/7)
(u:define-constant 13tau/28 (tau 13f0 28f0))
(u:define-constant 14tau/28 tau/2)
(u:define-constant 15tau/28 (tau 15f0 28f0))
(u:define-constant 16tau/28 4tau/7)
(u:define-constant 17tau/28 (tau 17f0 28f0))
(u:define-constant 18tau/28 9tau/14)
(u:define-constant 19tau/28 (tau 19f0 28f0))
(u:define-constant 20tau/28 5tau/7)
(u:define-constant 21tau/28 3tau/4)
(u:define-constant 22tau/28 11tau/14)
(u:define-constant 23tau/28 (tau 23f0 28f0))
(u:define-constant 24tau/28 6tau/7)
(u:define-constant 25tau/28 (tau 25f0 28f0))
(u:define-constant 26tau/28 13tau/14)
(u:define-constant 27tau/28 (tau 27f0 28f0))
(u:define-constant 28tau/28 tau)

;; Whole circle split into 29 parts
(u:define-constant tau/29 (tau 1f0 29f0))
(u:define-constant 2tau/29 (tau 2f0 29f0))
(u:define-constant 3tau/29 (tau 3f0 29f0))
(u:define-constant 4tau/29 (tau 4f0 29f0))
(u:define-constant 5tau/29 (tau 5f0 29f0))
(u:define-constant 6tau/29 (tau 6f0 29f0))
(u:define-constant 7tau/29 (tau 7f0 29f0))
(u:define-constant 8tau/29 (tau 8f0 29f0))
(u:define-constant 9tau/29 (tau 9f0 29f0))
(u:define-constant 10tau/29 (tau 10f0 29f0))
(u:define-constant 11tau/29 (tau 11f0 29f0))
(u:define-constant 12tau/29 (tau 12f0 29f0))
(u:define-constant 13tau/29 (tau 13f0 29f0))
(u:define-constant 14tau/29 (tau 14f0 29f0))
(u:define-constant 15tau/29 (tau 15f0 29f0))
(u:define-constant 16tau/29 (tau 16f0 29f0))
(u:define-constant 17tau/29 (tau 17f0 29f0))
(u:define-constant 18tau/29 (tau 18f0 29f0))
(u:define-constant 19tau/29 (tau 19f0 29f0))
(u:define-constant 20tau/29 (tau 20f0 29f0))
(u:define-constant 21tau/29 (tau 21f0 29f0))
(u:define-constant 22tau/29 (tau 22f0 29f0))
(u:define-constant 23tau/29 (tau 23f0 29f0))
(u:define-constant 24tau/29 (tau 24f0 29f0))
(u:define-constant 25tau/29 (tau 25f0 29f0))
(u:define-constant 26tau/29 (tau 26f0 29f0))
(u:define-constant 27tau/29 (tau 27f0 29f0))
(u:define-constant 28tau/29 (tau 28f0 29f0))
(u:define-constant 29tau/29 tau)

;; Whole circle split into 30 parts
(u:define-constant tau/30 (tau 1f0 30f0))
(u:define-constant 2tau/30 tau/15)
(u:define-constant 3tau/30 tau/10)
(u:define-constant 4tau/30 2tau/15)
(u:define-constant 5tau/30 tau/6)
(u:define-constant 6tau/30 tau/5)
(u:define-constant 7tau/30 (tau 7f0 30f0))
(u:define-constant 8tau/30 4tau/15)
(u:define-constant 9tau/30 3tau/10)
(u:define-constant 10tau/30 tau/3)
(u:define-constant 11tau/30 (tau 11f0 30f0))
(u:define-constant 12tau/30 2tau/5)
(u:define-constant 13tau/30 (tau 13f0 30f0))
(u:define-constant 14tau/30 7tau/15)
(u:define-constant 15tau/30 tau/2)
(u:define-constant 16tau/30 8tau/15)
(u:define-constant 17tau/30 (tau 17f0 30f0))
(u:define-constant 18tau/30 3tau/5)
(u:define-constant 19tau/30 (tau 19f0 30f0))
(u:define-constant 20tau/30 2tau/3)
(u:define-constant 21tau/30 7tau/10)
(u:define-constant 22tau/30 11tau/15)
(u:define-constant 23tau/30 (tau 23f0 30f0))
(u:define-constant 24tau/30 4tau/5)
(u:define-constant 25tau/30 5tau/6)
(u:define-constant 26tau/30 13tau/15)
(u:define-constant 27tau/30 9tau/10)
(u:define-constant 28tau/30 14tau/15)
(u:define-constant 29tau/30 (tau 29f0 30f0))
(u:define-constant 30tau/30 tau)

;; Whole circle split into 31 parts
(u:define-constant tau/31 (tau 1f0 31f0))
(u:define-constant 2tau/31 (tau 2f0 31f0))
(u:define-constant 3tau/31 (tau 3f0 31f0))
(u:define-constant 4tau/31 (tau 4f0 31f0))
(u:define-constant 5tau/31 (tau 5f0 31f0))
(u:define-constant 6tau/31 (tau 6f0 31f0))
(u:define-constant 7tau/31 (tau 7f0 31f0))
(u:define-constant 8tau/31 (tau 8f0 31f0))
(u:define-constant 9tau/31 (tau 9f0 31f0))
(u:define-constant 10tau/31 (tau 10f0 31f0))
(u:define-constant 11tau/31 (tau 11f0 31f0))
(u:define-constant 12tau/31 (tau 12f0 31f0))
(u:define-constant 13tau/31 (tau 13f0 31f0))
(u:define-constant 14tau/31 (tau 14f0 31f0))
(u:define-constant 15tau/31 (tau 15f0 31f0))
(u:define-constant 16tau/31 (tau 16f0 31f0))
(u:define-constant 17tau/31 (tau 17f0 31f0))
(u:define-constant 18tau/31 (tau 18f0 31f0))
(u:define-constant 19tau/31 (tau 19f0 31f0))
(u:define-constant 20tau/31 (tau 20f0 31f0))
(u:define-constant 21tau/31 (tau 21f0 31f0))
(u:define-constant 22tau/31 (tau 22f0 31f0))
(u:define-constant 23tau/31 (tau 23f0 31f0))
(u:define-constant 24tau/31 (tau 24f0 31f0))
(u:define-constant 25tau/31 (tau 25f0 31f0))
(u:define-constant 26tau/31 (tau 26f0 31f0))
(u:define-constant 27tau/31 (tau 27f0 31f0))
(u:define-constant 28tau/31 (tau 28f0 31f0))
(u:define-constant 29tau/31 (tau 29f0 31f0))
(u:define-constant 30tau/31 (tau 30f0 31f0))
(u:define-constant 31tau/31 tau)

;; Whole circle split into 32 parts
(u:define-constant tau/32 (tau 1f0 32f0))
(u:define-constant 2tau/32 tau/16)
(u:define-constant 3tau/32 (tau 2f0 32f0))
(u:define-constant 4tau/32 tau/8)
(u:define-constant 5tau/32 (tau 5f0 32f0))
(u:define-constant 6tau/32 3tau/16)
(u:define-constant 7tau/32 (tau 7f0 32f0))
(u:define-constant 8tau/32 tau/4)
(u:define-constant 9tau/32 (tau 9f0 32f0))
(u:define-constant 10tau/32 5tau/16)
(u:define-constant 11tau/32 (tau 11f0 32f0))
(u:define-constant 12tau/32 3tau/8)
(u:define-constant 13tau/32 (tau 13f0 32f0))
(u:define-constant 14tau/32 7tau/16)
(u:define-constant 15tau/32 (tau 15f0 32f0))
(u:define-constant 16tau/32 tau/2)
(u:define-constant 17tau/32 (tau 17f0 32f0))
(u:define-constant 18tau/32 9tau/16)
(u:define-constant 19tau/32 (tau 19f0 32f0))
(u:define-constant 20tau/32 5tau/8)
(u:define-constant 21tau/32 (tau 21f0 32f0))
(u:define-constant 22tau/32 11tau/16)
(u:define-constant 23tau/32 (tau 23f0 32f0))
(u:define-constant 24tau/32 3tau/4)
(u:define-constant 25tau/32 (tau 25f0 32f0))
(u:define-constant 26tau/32 13tau/16)
(u:define-constant 27tau/32 (tau 27f0 32f0))
(u:define-constant 28tau/32 7tau/8)
(u:define-constant 29tau/32 (tau 29f0 32f0))
(u:define-constant 30tau/32 15tau/16)
(u:define-constant 31tau/32 (tau 31f0 32f0))
(u:define-constant 32tau/32 tau)
