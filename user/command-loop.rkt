#lang racket

;; user/command-loop.rkt — Main command loop

(require "../kernel/buffer.rkt"
         "../kernel/gap.rkt"
         "../kernel/marker.rkt"
         "../kernel/keymap.rkt"
         "../core/window.rkt"
         "../kernel/event-chain.rkt"
         "../kernel/bottom-input.rkt"
         "../core/minibuffer.rkt"
         "../base/edit.rkt"
         "../base/keybind.rkt"
         "../base/isearch.rkt"
         "../platform/event.rkt"
         "../platform/termios.rkt"
         "../display/render.rkt"
         "minibuffer-loop.rkt"
         "completion-ui.rkt")

(provide command-loop run-command lookup-key-in-buffer
         init-minibuffer-bindings!)

(define needs-render? (box #t))

(define (lookup-key-in-buffer buf keys)
  (buffer-lookup-key buf keys))

(define (run-command cmd [arg #f])
  (bottom-line-clear-doc!)
  ;; Ensure the minibuffer shrinks back to 1 row if doc was just dismissed.
  (define frm0 (current-frame))
  (when frm0
    (define mini (frame-minibuffer-window frm0))
    (when (and mini (> (window-desired-rows mini) 1))
      (set-window-desired-rows! mini 1)
      (layout-frame! frm0)
      (invalidate-frame-cache! frm0)))
  (define buf (current-buffer))
  (define urec (buffer-undo-rec buf))
  (unless (and (last-command) (eq? cmd (last-command)))
    (undo-recorder-push-boundary! urec))
  (last-command (this-command))
  (this-command cmd)
  (if arg (cmd arg) (cmd))
  (last-command (this-command))
  (define frm (current-frame))
  (when frm
    (define sel (frame-selected-window frm))
    (when (and sel (not (window-mini? sel)))
      (set-window-point! sel (buffer-point (window-buffer sel)))))
  (set-box! needs-render? #t))

(define (handle-mouse-event mevt frm)
  (define action (mouse-event-action mevt))
  (define row (mouse-event-y mevt))
  (define col (mouse-event-x mevt))
  (define-values (buf-pos win part) (screen-coord->buffer-pos frm row col))
  (cond
    [(eq? action 'press)
     (when (and (eq? part 'text) win)
       (define buf (window-buffer win))
       (when buf
         (define old-win (frame-selected-window frm))
         (unless (eq? win old-win)
           (when (and old-win (window-leaf? old-win) (not (window-mini? old-win)))
             (set-window-point! old-win (buffer-point (window-buffer old-win))))
           (when old-win (set-window-selected?! old-win #f))
           (set-window-selected?! win #t)
           (set-frame-selected-window! frm win)
           (set-buffer buf)
           (set-buffer-point! buf (window-point win)))
         (set-buffer-point! buf buf-pos)
         (set-window-point! win buf-pos)
         (set-box! needs-render? #t)))]
    [(or (eq? action 'scroll-up) (eq? action 'scroll-down))
     (define target-win
       (if (and win (memq part '(text mode-line))) win
           (frame-selected-window frm)))
     (scroll-window target-win (if (eq? action 'scroll-up) -3 3) frm)]))

(define (scroll-window win delta frm)
  (when (and win (window-leaf? win) (not (window-mini? win)))
    (define buf (window-buffer win))
    (define gb (buffer-gap buf))
    (define len (gap-byte-length gb))
    (define content-rows (max 1 (sub1 (window-rows win))))
    (define (nl? b) (= b #x0A))
    (define (scan-backward p n)
      (if (or (<= p 0) (<= n 0)) (max 0 p)
          (let ([nl (gap-scan-backward-byte gb p nl?)])
            (if (>= nl 0) (scan-backward nl (sub1 n)) 0))))
    (define (scan-forward p n)
      (if (or (>= p len) (<= n 0)) (min p len)
          (let ([nl (gap-scan-forward-byte gb p nl?)])
            (if (< nl len) (scan-forward (add1 nl) (sub1 n)) len))))
    (define max-window-start
      (let loop ([p (sub1 len)] [remaining content-rows])
        (cond [(< p 0) 0]
              [(= (gap-byte-ref gb p) #x0A)
               (if (zero? remaining) (add1 p) (loop (sub1 p) (sub1 remaining)))]
              [else (loop (sub1 p) remaining)])))
    (define ws (marker-pos (window-start win)))
    (define selected? (eq? win (frame-selected-window frm)))
    (define pt (if selected? (buffer-point buf) (window-point win)))
    (define adelta (abs delta))
    (if (positive? delta)
        (begin
          (set-marker-pos! (window-start win)
                           (min (scan-forward ws adelta) max-window-start))
          (when selected?
            (set-buffer-point! buf (scan-forward pt adelta))
            (set-window-point! win (buffer-point buf))))
        (begin
          (set-marker-pos! (window-start win) (scan-backward ws adelta))
          (when selected?
            (set-buffer-point! buf (scan-backward pt adelta))
            (set-window-point! win (buffer-point buf)))))
    (set-box! needs-render? #t)))

(define resize-poll-interval 0.5)

(define (command-loop input-decode-map lookup-key)
  (let loop ([prefix-keys '()])
    (define frm (current-frame))
    (let-values ([(rows cols) (get-window-size)])
      (when (and rows cols frm
                 (or (not (= rows (frame-height frm)))
                     (not (= cols (frame-width frm)))))
        (terminal-height rows)
        (terminal-width cols)
        (set-frame-width! frm cols)
        (set-frame-height! frm rows)
        (layout-frame! frm)
        (invalidate-frame-cache! frm)
        (set-box! needs-render? #t)))
    (when (or (unbox needs-render?) (isearch-active?))
      (set-box! needs-render? #f)
      (display-frame frm))
    (define raw-ke
      (if resize-poll-interval
          (read-key-event/timeout! resize-poll-interval input-decode-map lookup-key)
          (read-key-event! input-decode-map lookup-key)))
    (unless raw-ke (loop prefix-keys))
    (when (mouse-event? raw-ke)
      (handle-mouse-event raw-ke frm)
      (loop prefix-keys))
    ;; ── Bracketed paste: insert all text as one undo-able operation ──
    (when (string? raw-ke)
      ;; Normalize line endings: \r\n → \n, \r → \n
      (define cleaned
        (string-replace (string-replace raw-ke "\r\n" "\n") "\r" "\n"))
      (run-command (λ () (insert cleaned)))
      (loop '()))
    (define ke (dispatch-event! raw-ke))
    (unless ke (loop prefix-keys))
    (when (key-event-cancel? ke)
      (deactivate-mark)
      (bottom-line-clear-echo!)
      (set-box! needs-render? #t)
      (loop '()))
    (define full-seq (append prefix-keys (list ke)))
    (define binding (lookup-key-in-buffer (current-buffer) full-seq))
    (cond
      [(procedure? binding)
       (bottom-line-clear-echo!)
       (run-command binding)
       (loop '())]
      [(keymap? binding)
       (bottom-line-set-echo! (key-sequence->echo-string full-seq))
       (set-box! needs-render? #t)
       (loop full-seq)]
      [(pair? prefix-keys)
       (bottom-line-set-echo! (key-sequence->echo-string full-seq))
       (set-box! needs-render? #t)
       (loop '())]
      [(key-event-self-insert? ke)
       (run-command (λ () (insert (string (key-event-char ke)))))
       (loop '())]
      [else (loop '())])))

(define (init-minibuffer-bindings!)
  (bind-key minibuffer-local-map "RET" 'minibuffer-exit)
  (bind-key minibuffer-local-map "C-g" 'minibuffer-abort)
  (bind-key minibuffer-local-map "C-j" 'minibuffer-exit)
  (bind-key minibuffer-local-map "C-a" 'minibuffer-beginning-of-line)
  (bind-key minibuffer-local-map "C-e" 'minibuffer-end-of-line)
  (bind-key minibuffer-local-map "C-b" 'minibuffer-backward-char)
  (bind-key minibuffer-local-map "C-f" 'minibuffer-forward-char)
  (bind-key minibuffer-local-map "left" 'minibuffer-backward-char)
  (bind-key minibuffer-local-map "right" 'minibuffer-forward-char)
  (bind-key minibuffer-local-map "DEL" 'minibuffer-delete-backward-char)
  (bind-key minibuffer-local-map "C-d" 'minibuffer-delete-char)
  (bind-key minibuffer-local-map "C-k" 'minibuffer-kill-line)
  (bind-key minibuffer-local-map "C-y" 'minibuffer-yank)
  (bind-key minibuffer-local-map "M-p" 'minibuffer-previous-history)
  (bind-key minibuffer-local-map "M-n" 'minibuffer-next-history)
  (bind-key minibuffer-local-map "TAB" minibuffer-complete))
