#lang racket

(require 
 "lib_xcb.rkt"
 "lib_x11.rkt"
 "config.rkt"
 "const_data.rkt")
(require racket/string)
(require racket/list)

;; 调试开关
(define DEBUG? #t)

(define-syntax-rule (debug-printf fmt arg ...)
  (when DEBUG?
    (begin
      (printf fmt arg ...)
      (flush-output))))

;; 启动后台进程（自动附加 &）
(define (spawn cmd)
  (unless (string=? cmd "")
    (system (string-append cmd " &"))))

;; 全局 XCB 状态
(define conn #f)
(define root #f)
(define screen-width 0)
(define screen-height 0)

;; 键盘可见性控制
(define keyboard-visible? #t)

;; 工作区管理
(define WORKSPACE-COUNT 10)
(define current-workspace (make-parameter 0))
(define winid->workspace (make-hash))

;; 全屏状态：ws → win-id 或 #f
(define workspace-fullscreen? (make-hash))

;; 布局结构
(struct layout-region (x y w h) #:transparent)
(struct layout-desc (name match? region tile-fn) #:transparent)

(define registered-layouts '())
(define active-layouts (make-hash))
(define layout-windows (make-hash))   ; key: 'keyboard 或 '(main . ws)
(define winid->layout (make-hash))

;; 窗口宽度偏移管理
;; (cons 'main ws) → (hash win-id → delta)
(define win-width-deltas (make-hash))

;; 按顺序：'1'..'9','0' → 对应工作区 0~9
(define CONFIG_WORKSPACE_KEYCODES
  (list KEY_1 KEY_2 KEY_3 KEY_4 KEY_5 KEY_6 KEY_7 KEY_8 KEY_9 KEY_0))

;; --- X11 Modifier Masks ---
(define MOD1_SHIFT (bitwise-ior MOD1_MASK SHIFT_MASK))

;; 辅助：列表交换函数
(define (list-swap lst i j)
  "返回新列表，交换索引 i 和 j 处的元素"
  (define vec (list->vector lst))
  (define temp (vector-ref vec i))
  (vector-set! vec i (vector-ref vec j))
  (vector-set! vec j temp)
  (vector->list vec))

;; 快捷键动作函数

(define (toggle-keyboard-layout!)
  (define kb-wins (hash-ref layout-windows 'keyboard '()))
  (cond
    [(null? kb-wins)
     (spawn CONFIG_KEYBOARD_CMD)]
    [else
     (set! keyboard-visible? (not keyboard-visible?))
     (if keyboard-visible?
         (for ([win kb-wins]) (xcb-map-window conn win))
         (for ([win kb-wins]) (xcb-unmap-window conn win)))
     (coordinate-layouts!)
     (debug-printf "Keyboard layout ~a\n" (if keyboard-visible? "shown" "hidden"))]))

(define (switch-to-workspace! ws)
  (unless (= ws (current-workspace))
    (debug-printf "Switching to workspace ~a\n" ws)
    (current-workspace ws)
    (coordinate-layouts!)
    (re-tile! (cons 'main ws))
    (define wins (hash-ref layout-windows (cons 'main ws) '()))
    (when (pair? wins)
      (focus-window (car wins)))))

;; 宽度调整函数（智能借/还）

(define (adjust-focused-window-width delta)
  (define win (current-focus))
  (when (> win 0)
    (define ws (hash-ref winid->workspace win #f))
    (when ws
      (define key (cons 'main ws))
      (define wins (hash-ref layout-windows key '()))
      (when (member win wins)
        (define n (length wins))
        (define idx (index-of wins win))
        (cond
          [(= n 1)
           ;; 单窗口：直接调整
           (define dh (hash-ref win-width-deltas key))
           (hash-set! dh win (+ (hash-ref dh win 0) delta))
           (re-tile! key)
           (debug-printf "Single window adjusted: #x~a\n" (number->string win 16))]
          [else
           ;; 多窗口：选择供体
           (define donor-idx
             (if (= idx (- n 1))
                 (- idx 1)     ; 最后一个 → 向左借
                 (+ idx 1)))   ; 其他 → 向右借
           (define donor-win (list-ref wins donor-idx))

           (define dh (hash-ref win-width-deltas key))
           (define my-delta (hash-ref dh win 0))
           (define donor-delta (hash-ref dh donor-win 0))

           (define r (layout-region-from-name 'main))
           (when r
             (define total-w (layout-region-w r))
             (define default-w (quotient total-w n))
             (define rem (- total-w (* default-w n)))
             (define (base i) (+ default-w (if (= i (- n 1)) rem 0)))
             (define my-base (base idx))
             (define donor-base (base donor-idx))

             (define my-new (+ my-base my-delta delta))
             (define donor-new (+ donor-base donor-delta (- delta)))

             (when (and (>= my-new MIN-WINDOW-WIDTH)
                        (>= donor-new MIN-WINDOW-WIDTH))
               (hash-set! dh win (+ my-delta delta))
               (hash-set! dh donor-win (- donor-delta delta))
               (re-tile! key)
               (debug-printf "Width adjusted: #x~a (~a) ↔ #x~a (~a)\n"
                             (number->string win 16) delta
                             (number->string donor-win 16) (- delta))))])))))

(define (reset-focused-window-width!)
  (define win (current-focus))
  (when (> win 0)
    (define ws (hash-ref winid->workspace win #f))
    (when ws
      (define key (cons 'main ws))
      (define delta-hash (hash-ref win-width-deltas key))
      (hash-set! delta-hash win 0)
      (re-tile! key)
      (debug-printf "Reset window #x~a to default width\n"
                    (number->string win 16)))))


;; 全屏切换函数
(define (toggle-main-fullscreen!)
  (define win (current-focus))
  (define ws (current-workspace))

  (cond
    [(<= win 0)
     (debug-printf "No focused window to fullscreen.\n")]
    [(not (hash-has-key? winid->workspace win))
     (debug-printf "Focused window #x~a not assigned to any workspace.\n"
                   (number->string win 16))]
    [else
     (define win-ws (hash-ref winid->workspace win))
     (unless (= win-ws ws)
       (debug-printf "Focused window is in workspace ~a, not current ~a.\n" win-ws ws)
       (set! win #f))

     (when win
       (define current-full (hash-ref workspace-fullscreen? ws #f))

       (cond
         [(and current-full (= current-full win))
          ;; 当前窗口已是全屏 → 退出
          (hash-set! workspace-fullscreen? ws #f)
          (debug-printf "Exited fullscreen for window #x~a in ws ~a\n"
                        (number->string win 16) ws)]
         [else
          ;; 切换为该窗口全屏
          (hash-set! workspace-fullscreen? ws win)
          (debug-printf "Entered fullscreen for window #x~a in ws ~a\n"
                        (number->string win 16) ws)])

       (re-tile! (cons 'main ws)))]))


;; 窗口顺序调整函数（新增）

(define (move-focused-window-in-list delta)
  ;; delta = -1 表示前移（Alt+Z），+1 表示后移（Alt+C）
  (define win (current-focus))
  (when (> win 0)
    (define ws (current-workspace))
    (define key (cons 'main ws))
    (define wins (hash-ref layout-windows key '()))
    (define idx (index-of wins win))

    (when idx
      (define new-idx (+ idx delta))
      (cond
        [(or (< new-idx 0) (>= new-idx (length wins)))
         (debug-printf "Cannot move window #x~a ~a (at boundary)\n"
                       (number->string win 16)
                       (if (= delta -1) "forward" "backward"))]
        [else
         (define new-wins (list-swap wins idx new-idx))
         (hash-set! layout-windows key new-wins)
         (re-tile! key)
         (debug-printf "Moved window #x~a ~a in workspace ~a\n"
                       (number->string win 16)
                       (if (= delta -1) "forward" "backward")
                       ws)]))))

(define (move-focused-window-forward!)   ; Alt+Z: 前移（向左）
  (move-focused-window-in-list -1))

(define (move-focused-window-backward!)  ; Alt+C: 后移（向右）
  (move-focused-window-in-list +1))


;; 快捷键配置（使用 KEY_* 常量）

(define workspace-bindings
  (for/list ([ws (in-range WORKSPACE-COUNT)]
             [kc CONFIG_WORKSPACE_KEYCODES])
    (cons (cons MOD1_MASK kc)
          (λ () (switch-to-workspace! ws)))))

(define move-window-bindings
  (for/list ([ws (in-range WORKSPACE-COUNT)]
             [kc CONFIG_WORKSPACE_KEYCODES])
    (cons (cons MOD4_MASK kc)
          (λ ()
            (define win (current-focus))
            (when (> win 0)
              (define old-ws (hash-ref winid->workspace win #f))
              (cond
                [(not old-ws)
                 (debug-printf "Window #x~a has no workspace.\n" (number->string win 16))]
                [(= old-ws ws)
                 (debug-printf "Window already in workspace ~a.\n" ws)]
                [else
                 ;; 从所有工作区移除窗口引用
                 (for ([w (in-range WORKSPACE-COUNT)])
                   (hash-set! layout-windows (cons 'main w)
                              (remove win (hash-ref layout-windows (cons 'main w) '()))))
                 ;; 迁移宽度 delta
                 (define old-key (cons 'main old-ws))
                 (define new-key (cons 'main ws))
                 (define old-dh (hash-ref win-width-deltas old-key #f))
                 (define new-dh (hash-ref win-width-deltas new-key #f))
                 (define delta (if old-dh (hash-ref old-dh win 0) 0))
                 (when old-dh (hash-remove! old-dh win))
                 (when new-dh (hash-set! new-dh win delta))
                 ;; 更新归属 + 插入到末尾 
                 (hash-set! winid->workspace win ws)
                 (hash-set! layout-windows new-key
                            (append (hash-ref layout-windows new-key '()) (list win)))
                 ;; 显示/隐藏
                 (if (= ws (current-workspace))
                     (begin (xcb-map-window conn win) (focus-window win))
                     (xcb-unmap-window conn win))
                 ;; 重排
                 (re-tile! new-key)
                 (re-tile! old-key)
                 (debug-printf "Moved window #x~a from ws ~a to ~a (delta=~a)\n"
                               (number->string win 16) old-ws ws delta)]))))))

(define keybindings
  (append
   workspace-bindings
   move-window-bindings
   (list
     (cons (cons MOD1_MASK KEY_A)        ; Alt+A: 增宽
           (λ () (adjust-focused-window-width 50)))
     (cons (cons MOD1_MASK KEY_D)        ; Alt+D: 缩窄
           (λ () (adjust-focused-window-width -50)))
     (cons (cons MOD1_MASK KEY_R)        ; Alt+R: 重置宽度
           reset-focused-window-width!)
     (cons (cons MOD1_MASK KEY_ENTER)    ; Alt+Enter: 启动终端
           (λ () (spawn CONFIG_TERMINAL_CMD)))
     (cons (cons MOD1_MASK KEY_F)    ; Alt+F: 启动菜单
           (λ () (spawn CONFIG_MENU_CMD)))
     (cons (cons MOD1_MASK KEY_BACKSPACE) ; Alt+Backspace: 关闭窗口
           (λ ()
             (define win (current-focus))
             (when (> win 0)
               (xcb-delete-window conn win))))
     (cons (cons MOD1_MASK KEY_L)        ; Alt+L: 切换虚拟键盘
           toggle-keyboard-layout!)
     (cons (cons MOD1_MASK KEY_M)        ; Alt+M: 全屏切换
           toggle-main-fullscreen!)
     (cons (cons MOD1_MASK KEY_Z)        ; Alt+Z: 前移窗口
           move-focused-window-forward!)
     (cons (cons MOD1_MASK KEY_C)        ; Alt+C: 后移窗口
           move-focused-window-backward!)
     (cons (cons MOD1_SHIFT KEY_Q)       ; Alt+Shift+Q: 退出 WM
           (λ () (exit 0))))))


;; 辅助函数
(define (window-name-contains? win-id substrings)
  (define name (xcb-get-window-name conn win-id))
  (and name
       (ormap (λ (pat)
                (string-contains? (string-downcase name) (string-downcase pat)))
              substrings)))

(define IGNORED_WINDOW_TYPES '())

(define (init-ignored-types! conn)
  (set! IGNORED_WINDOW_TYPES
        (list (xcb-intern-atom conn "_NET_WM_WINDOW_TYPE_SPLASH")
              (xcb-intern-atom conn "_NET_WM_WINDOW_TYPE_NOTIFICATION"))))

(define (should-ignore-window? win-id)
  (define type-atom (xcb-get-window-type conn win-id))
  (and (> type-atom 0)
       (member type-atom IGNORED_WINDOW_TYPES)))

(define (should-ignore-focus? win-id)
  (window-name-contains? win-id CONFIG_FOCUS_IGNORE_WINDOW_NAMES))

;; 布局策略
(define (tile-main wins)
  (define current-ws (current-workspace))
  (define full-win (hash-ref workspace-fullscreen? current-ws #f))

  (cond
    [(and full-win (> full-win 0))
     ;; 全屏模式:只显示 full-win
     (define r (layout-region-from-name 'main))
     (when r
       (xcb-move-resize conn full-win
                        (layout-region-x r)
                        (layout-region-y r)
                        (layout-region-w r)
                        (layout-region-h r))
       (xcb-map-window conn full-win)
       ;; 隐藏其他窗口
       (for ([w wins])
         (unless (= w full-win)
           (xcb-unmap-window conn w))))]
    [else
     ;; 正常平铺
     (define r (layout-region-from-name 'main))
     (when (and r (> (length wins) 0))
       (define x (layout-region-x r))
       (define y (layout-region-y r))
       (define total-w (layout-region-w r))
       (define h (layout-region-h r))
       (define n (length wins))
       (define delta-hash (hash-ref win-width-deltas (cons 'main current-ws) #f))
       (unless delta-hash (set! delta-hash (make-hash)))

       (define default-w (quotient total-w n))
       (define remainder (- total-w (* default-w n)))

       (define adjusted-widths
         (for/list ([win wins] [i (in-naturals)])
           (define base (+ default-w (if (= i (- n 1)) remainder 0)))
           (define delta (hash-ref delta-hash win 0))
           (max CONFIG_MIN_WINDOW_WIDTH (+ base delta))))

       (let loop ([wins wins] [widths adjusted-widths] [cur-x x])
         (cond
           [(null? wins) (void)]
           [else
            (define w (car widths))
            (xcb-move-resize conn (car wins) cur-x y w h)
            (xcb-map-window conn (car wins))
            (loop (cdr wins) (cdr widths) (+ cur-x w))])))]))

(define (tile-keyboard wins)
  (define r (layout-region-from-name 'keyboard))
  (when (and r (pair? wins))
    (define win (car wins))
    (xcb-move-resize conn win
                     (layout-region-x r)
                     (layout-region-y r)
                     (layout-region-w r)
                     (layout-region-h r))))

(define (layout-region-from-name name)
  (define desc (hash-ref active-layouts name #f))
  (and desc (layout-desc-region desc)))

(define (set-layout-region! name x y w h)
  (define desc (hash-ref active-layouts name #f))
  (when desc
    (hash-set! active-layouts name
               (struct-copy layout-desc desc [region (layout-region x y w h)]))
    (for ([i (in-range WORKSPACE-COUNT)])
      (re-tile! (cons 'main i)))))

(define (coordinate-layouts!)
  (define kb-wins (hash-ref layout-windows 'keyboard '()))
  (define should-show-keyboard-area?
    (and (not (null? kb-wins)) keyboard-visible?))

  (if should-show-keyboard-area?
      (begin
        (set-layout-region! 'main 0 0 screen-width (- screen-height CONFIG_KEYBOARD_HEIGHT))
        (set-layout-region! 'keyboard 0 (- screen-height CONFIG_KEYBOARD_HEIGHT) screen-width CONFIG_KEYBOARD_HEIGHT))
      (begin
        (set-layout-region! 'main 0 0 screen-width screen-height)
        (set-layout-region! 'keyboard 0 screen-height screen-width 0)))

  (debug-printf "Layout coordination: main=(0,0 ~ax~a), kb=~a\n"
                screen-width
                (if should-show-keyboard-area?
                    (- screen-height CONFIG_KEYBOARD_HEIGHT)
                    screen-height)
                (if should-show-keyboard-area? "visible" "hidden")))

;; 布局初始化
(define (register-layout! name match? region tile-fn)
  (set! registered-layouts
        (append registered-layouts (list (layout-desc name match? region tile-fn)))))

(define (init-layouts!)
  (hash-clear! active-layouts)
  (hash-clear! layout-windows)
  (hash-clear! winid->layout)
  (hash-clear! winid->workspace)
  (hash-clear! win-width-deltas)
  (hash-clear! workspace-fullscreen?)

  (register-layout! 'keyboard
                    (λ (win-id) (window-name-contains? win-id CONFIG_KEYBOARD_WINDOW_NAMES))
                    (layout-region 0 0 0 0)
                    tile-keyboard)

  (register-layout! 'main
                    (λ (_) #t)
                    (layout-region 0 0 0 0)
                    tile-main)

  (for ([desc registered-layouts])
    (hash-set! active-layouts (layout-desc-name desc) desc))

  (for ([i (in-range WORKSPACE-COUNT)])
    (hash-set! layout-windows (cons 'main i) '())
    (hash-set! win-width-deltas (cons 'main i) (make-hash))
    (hash-set! workspace-fullscreen? i #f)) ; 初始化全屏状态

  (hash-set! layout-windows 'keyboard '())
  (coordinate-layouts!)
  (debug-printf "Initialized layouts with ~a workspaces.\n" WORKSPACE-COUNT))

;; 窗口管理
(define (assign-window-to-layout! win-id)
  (define matched-desc
    (for/or ([desc registered-layouts])
      (and ((layout-desc-match? desc) win-id) desc)))

  (cond
    [(not matched-desc)
     (debug-printf "No layout matched window #x~a\n" (number->string win-id 16))]
    [else
     (define name (layout-desc-name matched-desc))
     (define final-name
       (if (and (eq? name 'keyboard)
                (not (null? (hash-ref layout-windows 'keyboard '()))))
           'main
           name))
     (cond
       [(eq? final-name 'keyboard)
        (debug-printf "Assigned window #x~a to layout 'keyboard'\n"
                      (number->string win-id 16))
        (hash-set! winid->layout win-id 'keyboard)
        (hash-set! layout-windows 'keyboard (list win-id))
        (when keyboard-visible?
          (xcb-map-window conn win-id))
        (coordinate-layouts!)
        (re-tile! 'keyboard)]
       [else
        (define ws (current-workspace))
        (hash-set! winid->layout win-id 'main)
        (hash-set! winid->workspace win-id ws)
        (define key (cons 'main ws))
        ;; 插入到末尾
        (define wins (append (hash-ref layout-windows key '()) (list win-id)))
        (hash-set! layout-windows key wins)
        (debug-printf "Assigned window #x~a to main workspace ~a (appended)\n"
                      (number->string win-id 16) ws)
        (re-tile! key)])]))

(define (remove-from-layout! win-id)
  (define name (hash-ref winid->layout win-id #f))
  (when name
    (debug-printf "Removed window #x~a from layout '~a'\n"
                  (number->string win-id 16) name)
    (hash-remove! winid->layout win-id)
    (cond
      [(eq? name 'keyboard)
       (hash-set! layout-windows 'keyboard '())
       (coordinate-layouts!)
       (re-tile! 'keyboard)]
      [(eq? name 'main)
       (define ws (hash-ref winid->workspace win-id #f))
       (when ws
         ;; 清理全屏状态
         (define full-win (hash-ref workspace-fullscreen? ws #f))
         (when (and full-win (= full-win win-id))
           (hash-set! workspace-fullscreen? ws #f)
           (debug-printf "Cleared fullscreen state for ws ~a (window destroyed)\n" ws))
         ;; 清理归属
         (hash-remove! winid->workspace win-id)
         (define key (cons 'main ws))
         (define wins (remove win-id (hash-ref layout-windows key '())))
         (hash-set! layout-windows key wins)
         ;; 清理宽度 delta
         (define dh (hash-ref win-width-deltas key #f))
         (when dh (hash-remove! dh win-id))
         (re-tile! key))])))

(define (re-tile! key)
  (cond
    [(eq? key 'keyboard)
     (define desc (hash-ref active-layouts 'keyboard #f))
     (when desc
       (define wins (hash-ref layout-windows 'keyboard '()))
       (debug-printf "Re-tiling keyboard with ~a windows\n" (length wins))
       ((layout-desc-tile-fn desc) wins))]
    [(and (pair? key) (eq? (car key) 'main))
     (define desc (hash-ref active-layouts 'main #f))
     (when desc
       (define wins (hash-ref layout-windows key '()))
       (define ws (cdr key))
       (when (= ws (current-workspace))
         (debug-printf "Tiling workspace ~a: ~a windows\n" ws (length wins))
         ((layout-desc-tile-fn desc) wins))
       (for ([win wins])
         (if (= ws (current-workspace))
             (xcb-map-window conn win)
             (xcb-unmap-window conn win))))]))

;; 焦点管理
(define current-focus (make-parameter 0))

(define (focus-window win-id)
  (current-focus win-id)
  (xcb-set-active-window conn root win-id)
  (xcb-set-input-focus conn win-id)
  (debug-printf "Focused: #x~a (~a)\n"
                (number->string win-id 16)
                (or (xcb-get-window-name conn win-id) "unnamed")))

;; 主事件循环
(define (main)
  (set! conn (xcb-connect))
  (set! root (xcb-get-root conn))
  (set! screen-width (xcb-get-root-width conn))
  (set! screen-height (xcb-get-root-height conn))
  (init-ignored-types! conn)
  (init-layouts!)
  (xcb-select-wm-events conn root)

  (spawn CONFIG_BACKGROUND_CMD)  ; 启动背景

  (for ([binding keybindings])
    (define modifiers (car (car binding)))
    (define keycode   (cdr (car binding)))
    (xcb-grab-key conn keycode modifiers root))

  (debug-printf "WM started. Screen: ~ax~a\n" screen-width screen-height)
  (debug-printf "Registered ~a keybinding(s)\n" (length keybindings))

  (let loop ()
    (define event (wm-wait-event conn))
    (when event
      (match event
        [(map-request win-id)
         (cond
           [(should-ignore-window? win-id)
            (debug-printf "Ignored (unmanaged): #x~a\n" (number->string win-id 16))
            (xcb-map-window conn win-id)]
           [else
            (debug-printf "Mapping window: #x~a (~a)\n"
                          (number->string win-id 16)
                          (or (xcb-get-window-name conn win-id) "unnamed"))
            (xcb-set-window-borderless conn win-id)
            (xcb-select-input conn win-id XCB_EVENT_MASK_ENTER_WINDOW)
            (xcb-map-window conn win-id)
            (assign-window-to-layout! win-id)
            (define ws (hash-ref winid->workspace win-id #f))
            (when (and ws (= ws (current-workspace))
                       (= (length (hash-ref layout-windows (cons 'main ws) '())) 1))
              (focus-window win-id))])]
        [(destroy-notify win-id)
         (debug-printf "Destroyed: #x~a\n" (number->string win-id 16))
         (remove-from-layout! win-id)]
        [(enter-notify win-id)
         (define layout (hash-ref winid->layout win-id #f))
         (when layout
           (cond
             [(eq? layout 'keyboard)
              (unless (should-ignore-focus? win-id)
                (focus-window win-id))]
             [(eq? layout 'main)
              (define ws (hash-ref winid->workspace win-id #f))
              (when (and ws (= ws (current-workspace)))
                (unless (should-ignore-focus? win-id)
                  (focus-window win-id)))]))]
        [(key-press detail state x y)
         (define modifiers (bitwise-and state #xFF))
         (define action
           (for/or ([binding keybindings])
             (and (= (car (car binding)) modifiers)
                  (= (cdr (car binding)) detail)
                  (cdr binding))))
         (when action (action))]
        [(button-press detail state x y child)
         (debug-printf " BUTTON_PRESS: button=~a at (~a,~a), child=#x~a, state=~a\n"
                       detail x y (number->string child 16) state)]
        [(button-release detail state x y child)
          (debug-printf " BUTTON_RELEASE: button=~a at (~a,~a), child=#x~a, state=~a\n"
                        detail x y (number->string child 16) state)
          ;; 检测：左键（detail=1） + 点击在根窗口（child=0）
          (when (and (= detail 1)        ; 左键
                     (= child 0))        ; 空白处（无子窗口）
            (debug-printf " Clicked on desktop background → toggling keyboard layout!\n")
            (toggle-keyboard-layout!))]
        [(motion-notify x y state)
         (debug-printf " MOTION_NOTIFY: (~a,~a), state=~a\n" x y state)]
        [(unknown-event type)
         (debug-printf " Unknown event type: ~a\n" type)]
        [_ (void)])
      (loop)))

  (xcb-disconnect conn))

;; 启动
(main)
