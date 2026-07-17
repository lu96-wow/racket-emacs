#lang racket

;; platform/termios.rkt — Terminal control: raw mode + window size
;;
;; FFI-based. No internal editor dependencies.

(require ffi/unsafe)

(provide
 screen-init! screen-cleanup!
 terminal-width terminal-height get-window-size
 detect-terminal-size!
 terminal? STDIN_FILENO
 ;; resize event (green thread polls ioctl, writes channel)
 resize-channel)

(define libc (ffi-lib #f))
(define tcgetattr (get-ffi-obj 'tcgetattr libc (_fun _int _pointer -> _int)))
(define tcsetattr (get-ffi-obj 'tcsetattr libc (_fun _int _int _pointer -> _int)))
(define ioctl     (get-ffi-obj 'ioctl libc (_fun _int _int _pointer -> _int)))
(define isatty    (get-ffi-obj 'isatty libc (_fun _int -> _int)))

(define (terminal?) (not (zero? (isatty STDIN_FILENO))))

(define STDIN_FILENO 0)
(define STDOUT_FILENO 1)
(define TCSAFLUSH 2)
(define TERMIOS-SIZE 60)
(define TIOCGWINSZ #x5413)

;; ============================================================
;; Flag bits
;; ============================================================

(define ICANON 2) (define ECHO 8)
(define ISIG 1) (define IEXTEN 32768)
(define IXON 1024) (define ICRNL 256) (define INLCR 64) (define IGNCR 128)
(define OPOST 1) (define OCRNL 8) (define ONLCR 4)
(define VMIN 6) (define VTIME 5)

(define LFLAG-OFFSET 12) (define IFLAG-OFFSET 0) (define OFLAG-OFFSET 4)

;; ============================================================
;; termios helpers
;; ============================================================

(define (make-termios) (make-bytes TERMIOS-SIZE 0))
(define (copy-termios src)
  (define dst (make-termios))
  (bytes-copy! dst 0 src 0 TERMIOS-SIZE) dst)

(define (flag-ref t off)
  (for/fold ([v 0]) ([i (in-range 3 -1 -1)])
    (+ (arithmetic-shift v 8) (bytes-ref t (+ off i)))))

(define (flag-set! t off v)
  (for ([i 4])
    (bytes-set! t (+ off i) (bitwise-and v #xff))
    (set! v (arithmetic-shift v -8))) t)

(define (lflag-ref t)  (flag-ref t LFLAG-OFFSET))
(define (lflag-set! t v) (flag-set! t LFLAG-OFFSET v))
(define (iflag-ref t)  (flag-ref t IFLAG-OFFSET))
(define (iflag-set! t v) (flag-set! t IFLAG-OFFSET v))
(define (oflag-ref t)  (flag-ref t OFLAG-OFFSET))
(define (oflag-set! t v) (flag-set! t OFLAG-OFFSET v))

(define (set-vmin-vtime! t vmin vtime)
  (bytes-set! t (+ 17 VMIN) vmin)
  (bytes-set! t (+ 17 VTIME) vtime) t)

;; ============================================================
;; State
;; ============================================================

(define saved-terminal #f)
(define terminal-width  (make-parameter 80))
(define terminal-height (make-parameter 24))

;; ============================================================
;; Enter / exit raw mode
;; ============================================================

(define (enter-raw-mode!)
  (define t (make-termios))
  (when (not (= 0 (tcgetattr STDIN_FILENO t)))
    (error 'screen-init! "tcgetattr failed — is stdin a TTY?"))
  (set! saved-terminal (copy-termios t))
  (lflag-set! t (bitwise-and (lflag-ref t)
                 (bitwise-not (bitwise-ior ICANON ECHO ISIG IEXTEN))))
  (iflag-set! t (bitwise-and (iflag-ref t)
                 (bitwise-not (bitwise-ior IXON ICRNL INLCR IGNCR))))
  (oflag-set! t (bitwise-and (oflag-ref t)
                 (bitwise-not (bitwise-ior OPOST OCRNL ONLCR))))
  (set-vmin-vtime! t 0 1)  ; VMIN=0 VTIME=1: 100ms poll, non-blocking for escape seq
  (when (not (= 0 (tcsetattr STDIN_FILENO TCSAFLUSH t)))
    (error 'screen-init! "tcsetattr failed — cannot enter raw mode")))

(define (exit-raw-mode!)
  (when saved-terminal
    (tcsetattr STDIN_FILENO TCSAFLUSH saved-terminal)
    (set! saved-terminal #f))
  (display "\e[?25h") (display "\e[0m") (flush-output))

;; ============================================================
;; Window size
;; ============================================================

(define (get-window-size [fd STDOUT_FILENO])
  (define ws (make-bytes 8 0))
  (if (= (ioctl fd TIOCGWINSZ ws) -1)
      (values #f #f)
      (values (+ (bytes-ref ws 0) (arithmetic-shift (bytes-ref ws 1) 8))
              (+ (bytes-ref ws 2) (arithmetic-shift (bytes-ref ws 3) 8)))))

(define (detect-terminal-size!)
  (define-values (rows cols) (get-window-size))
  (when rows (terminal-height rows))
  (when cols (terminal-width cols)))

;; ============================================================
;; Resize monitor — green thread polls TIOCGWINSZ every 0.1s.
;; On change: detect-terminal-size! + channel-put.
;; read-key sync multiplexes stdin + resize-channel together.
;; ============================================================

(define RESIZE-POLL-INTERVAL 0.1)
(define resize-channel (make-channel))

(define (start-resize-monitor!)
  (thread
    (λ ()
      (let loop ([prev-w (terminal-width)] [prev-h (terminal-height)])
        (sleep RESIZE-POLL-INTERVAL)
        (detect-terminal-size!)
        (define w (terminal-width))
        (define h (terminal-height))
        (unless (and (= w prev-w) (= h prev-h))
          (channel-put resize-channel 'resize))
        (loop w h)))))

;; ============================================================
;; Public entry points
;; ============================================================

(define (screen-init!)
  (unless (terminal?) (error 'screen-init! "stdin is not a TTY"))
  (enter-raw-mode!) (detect-terminal-size!)
  (start-resize-monitor!))

(define (screen-cleanup!)
  (display "\e[?25h") (display "\e[0m")
  (display "\e[?1049l") (display "\e[?2004l")
  (display "\e[?1006l\e[?1002l\e[?1000l")
  (flush-output) (exit-raw-mode!) (flush-output))
