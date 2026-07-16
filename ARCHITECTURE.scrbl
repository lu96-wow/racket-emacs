#lang scribble/manual

@title{racket-emacs-rebuild — 架构文档}

@section{设计原则}

@itemlist[
  @item{每一层只实现自己关心的核心原语，不感知上层或下层实现细节}
  @item{高层通过组合底层原语构建，不侵入底层代码}
  @item{数据变更信息通过返回值显式传递，不依赖隐藏状态}
  @item{纯计算与副作用严格分离——compute 产生值，apply 执行效果}
]

@section{整体分层}

@verbatim{
kernel/                     display/                   platform/
  data/   存储原语            vbuffer.rkt   cell 网格     ansi.rkt
  buffer  组合层              layout.rkt    布局计算
  dirty   脏视图              face.rkt      样式系统
  edit    编辑命令             char-width.rkt 字符宽度
  undo/   撤销协议             render.rkt    渲染填充
  key-event 输入类型           flush.rkt     终端输出
  kill-ring 剪切环
  lang/   语法配置
}

@subsection{数据流}

@verbatim{
key-event ─┐
           ├── edit ──→ dirty-buffer ──→ layout ──→ render ──→ flush ──→ terminal
kill-ring ─┘                                      ↑          ↑
                                             char-width    face
}

@section{kernel — 编辑器核心}

@subsection{kernel/data — 存储原语}

@subsubsection{gap.rkt — Gap Buffer}

字节级可变文本容器。逻辑布局：

@verbatim{[text-before] [gap] [text-after]}

公开接口（7个）： @racket[make-gap-buffer], @racket[gap-length],
@racket[gap-byte-ref], @racket[gap-subbytes], @racket[gap-insert!],
@racket[gap-delete!].

关键抽象：物理索引 → 逻辑索引的映射，gap 移动和扩容对上层完全透明。

@subsubsection{marker.rkt — 位置标记}

纯数据结构：@racket[(marker pos insertion-type)]。

@racket[insertion-type] 为 @racket[#t] 时标记留在插入文本之后，
为 @racket[#f] 时留在之前。调整逻辑不在 marker 内部，由 text.rkt 统一管理。

@subsubsection{utf8.rkt — UTF-8 编解码}

纯函数，零依赖。提供：@racket[utf8-start-byte?], @racket[utf8-char-len],
@racket[utf8-encode], @racket[utf8-decode], @racket[utf8-next-pos],
@racket[utf8-prev-pos].

@subsubsection{query.rkt — 字符级查询}

组合 gap.rkt + utf8.rkt。提供字符访问、字符串提取、导航（前后字符）、
扫描（byte/char predicate 匹配）、行首检测、单词读取。

关键函数：@racket[gap-char+len], @racket[gap-substring],
@racket[gap-next-char-pos], @racket[gap-prev-char-pos],
@racket[gap-scan-byte], @racket[gap-scan-char].

@subsubsection{text.rkt — Text 原子}

组合 gap.rkt + marker.rkt 为不可分割的编辑原子。

核心职责：insert/delete 时同步调整所有 marker 位置。
@racket[adjust-markers-insert!] 和 @racket[adjust-markers-delete!]
是纯函数，独立可测试。

@subsubsection{textprop.rkt — 文本属性}

基于 @racket[data/interval-map] 的区间属性容器。

在任意字节区间上挂 key-value 属性。insert/delete 时自动跟随文本扩展/收缩。
由 buffer 层调用 @racket[textprop-adjust-insert!] / @racket[textprop-adjust-delete!]。

@subsection{kernel/buffer.rkt — Buffer 组合层}

组合 text + textprop + undo + point/mark 为完整的编辑器 buffer。

所有 mutation 返回变更区间：@racket[(values start end)]。
不存储任何脏标记或显示状态。

关键接口：
@itemlist[
  @item{@racket[buffer-insert!] → (values byte-pos byte-pos+blen)}
  @item{@racket[buffer-delete!] → (values pos pos)}
  @item{@racket[buffer-undo!] → (values start end) or (values #f #f)}
  @item{@racket[buffer-redo!] → (values start end) or (values #f #f)}
]

@subsection{kernel/dirty.rkt — 脏视图包装}

包装 buffer，在 mutation 返回值基础上累积变更区间。

类型：@racket[(dirty-buffer buf changes)]，其中 changes 为
@racket[(listof (cons start end))]。

关键设计：
@itemlist[
  @item{每个 mutation 返回 @racket[struct-copy] 的新 dirty-buffer}
  @item{buffer 对象不变（同一引用），only changes 列表累积}
  @item{@racket[dirty-extent] 合并所有变更为最小区间}
  @item{@racket[dirty-clear!] 返回清空 changes 的新值}
  @item{零全局可变状态——db 值由事件循环持有并传递}
]

@subsection{kernel/edit.rkt — 编辑命令}

组合 dirty-buffer + key-event + kill-ring。所有命令统一签名：

@racketblock{dirty-buffer × key-event → dirty-buffer}

point/mark 移动不产生内容变更，
dirty-buffer 的 changes 列表保持为空。

@subsection{kernel/undo — 撤销协议}

三层结构：
@itemlist[
  @item{@racket[record.rkt] — 纯数据类型：undo-insert, undo-delete, undo-group}
  @item{@racket[recorder.rkt] — 录制器：存储 + 合并相邻 insert + commit}
  @item{@racket[exec.rkt] — 执行引擎：@racket[execute-undo!] / @racket[execute-redo!]}
]

@subsection{kernel/key-event.rkt — 输入事件}

纯数据类型：@racket[key-event] 和 @racket[mouse-event]。
提供分类函数：@racket[self-insert-key?], @racket[backspace-key?],
@racket[return-key?], @racket[cancel-key?].

@subsection{kernel/kill-ring.rkt — 剪切环}

字符串环形缓冲区。可变模块级状态（全局共享）。
接口：@racket[kill-new], @racket[kill-ring-yank], @racket[kill-ring-pop].

@subsection{kernel/lang — 语言配置}

@subsubsection{lang/syntax.rkt — 语法表}

字符→语法分类的规则系统。提供 @racket[syntax-table] 类型、
@racket[char-syntax], @racket[make-standard-syntax-table],
@racket[make-racket-syntax-table].

9 种语法类别：word, whitespace, open, close, string-quote,
comment-start, escape, expression-prefix, punctuation.

@section{display — 显示层}

@subsection{display/vbuffer.rkt — Cell 网格}

屏幕离屏缓冲区。纯数据结构，零依赖。

@racketblock[(struct cell (ch attrs face-id))]
@racketblock[(struct vbuffer (rows cols cells))]

提供：@racket[vbuffer-put-char!], @racket[vbuffer-put-string!],
@racket[vbuffer-fill!], @racket[vbuffer-blit!],
@racket[vbuffer-row-changed?]（用于增量刷新）。

@subsection{display/char-width.rkt — 字符宽度}

纯函数，将 Unicode codepoint 映射为 0/1/2 列。

@racket[gap-display-width] 计算区间显示列数。
@racket[scan-display-width] 向前扫描最多 max-width 列，返回字节位置。
被 layout.rkt 消费。

@subsection{display/layout.rkt — 布局计算}

纯函数：gap-buffer × viewport 参数 → layout 值。

layout 类型打包 visual-lines + cursor 位置 + 参数。

两种行生成模式：
@itemlist[
  @item{@racket[truncate] — 每逻辑行一个 visual-line，溢出截断标记 @litchar{$}}
  @item{@racket[wrap] — 按 max-cols 拆分逻辑行，续行标记 @racket[continued?]}
]

每个 @racket[visual-line] 包含 @racket[end-buf-pos]，
使 @racket[layout-query-pos]（屏幕坐标→buffer位置）可以 O(row) 完成。

所有参数通过 @racket[#:wrap-mode], @racket[#:left-col] 等显式传入，
无副作用表。

@subsection{display/face.rkt — 样式系统}

三层：
@itemlist[
  @item{@racket[face-attrs] — 逻辑属性（前景色、背景色、粗细、斜体、下划线、反相）}
  @item{@racket[face-cache] — attrs → face-id → realized-face（含缓存 ANSI 字节）}
  @item{@racket[define-face!] — 命名 face（@racket['keyword], @racket['comment] 等）}
]

renderer 用 @racket[face-id] 设置 cell，flusher 在 face-id 变化时输出 ANSI 转义码。

@section{platform — 平台抽象}

@subsection{platform/ansi.rkt — ANSI 转义码常量}

纯格式字符串：光标控制、屏幕清除、文本属性、颜色、鼠标、bracketed paste。
@racket[detect-color-depth!] 检测终端色彩能力（truecolor / 256 / 16 / none）。

@section{组合链示例}

@verbatim{
;; 事件循环伪代码
(define (event-loop db)
  (cond [(dirty-dirty? db)
         ;; safe point: 有脏数据 → 渲染
         (let* ([buf  (dirty-buffer-buf db)]
                [gb   (text-gap (buffer-text buf))]
                [ly   (compute-layout gb (buffer-point buf) #:max-rows 24 #:max-cols 80)]
                [vb   (render-to-vbuffer ly faces)]
                [_    (flush-vbuffer-delta! vb cached-vb)]
                [_    (move-cursor! (layout-cursor-row ly) (layout-cursor-col ly))])
           (event-loop (dirty-clear! db)))]
        [else
         ;; 读取事件 → 查找命令 → 执行
         (let* ([evt    (read-key-event!)]
                [cmd    (lookup-command db evt)]
                [new-db (cmd db evt)])
           (event-loop new-db))]))
}

@section{与原始 racket-emacs 的关键差异}

@itemlist[
  @item{@bold{dirty 标记}：从 3 处隐式状态（global box + buffer box + command macro）→ 1 层显式包装（dirty-buffer）}
  @item{@bold{layout}：从副作用表（buffer-wrap-mode, buffer-hscroll）→ 纯函数显式参数}
  @item{@bold{buffer mutation}：从 void → 返回变更区间，上层自由组合}
  @item{@bold{visual-line}：增加 @racket[end-buf-pos]，支持 O(row) 逆查询}
  @item{@bold{命令签名}：从 @racket[(buf evt) → void] → @racket[dirty-buffer × evt → dirty-buffer]}
  @item{@bold{零全局状态}：事件循环持有 @racket[db] 值并线程传递，不依赖全局 box}
]
