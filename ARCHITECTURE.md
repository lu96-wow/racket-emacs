# racket-emacs-rebuild — 架构与 elisp 兼容性分析

## 1. 分层架构

```
user/         用户功能、keybinding、mode、command-loop
  ↓ 依赖
display/      渲染管线 (face、vbuffer、render、bottom-line)
  ↓ 依赖
base/         高层算法组合 (edit、isearch、font-lock、completion)
  ↓ 依赖
core/         内核原语的最小组合 (buffer-factory、window-layout、textprop、search)
  ↓ 依赖
kernel/       纯数据结构 + 协议 (struct、predicate、accessor — 零扫描循环)
  ↓ 依赖
platform/     终端/OS 交互 (event、termios、ansi、file-io)
```

**规则：上层可依赖下层，下层不可依赖上层。kernel 零 IO、零 display、零算法循环。**

### 各层职责

| 层 | 职责 | 允许的操作 |
|----|------|-----------|
| **kernel/** | 数据结构 + 协议定义 | struct、predicate、accessor、零循环 |
| **core/** | 内核原语的最小组合 | make-buffer、layout-frame!、textprop map 操作、字符串搜索 |
| **base/** | 高层算法 | 编辑命令、font-lock 引擎、增量搜索、补全匹配 |
| **display/** | 终端渲染 | vbuffer、face、渲染管线 |
| **user/** | 用户功能 | keybinding、mode、command-loop |

## 2. 模块清单

### kernel/ — 纯数据结构 + 协议（零依赖、零循环、零算法）

| 文件 | 职责 |
|------|------|
| `gap.rkt` | UTF-8 字节级 gap buffer（insert、delete、scan、char-at） |
| `gap-util.rkt` | gap buffer 公共 helper（char-at、match-str-at、at-bol? 等） |
| `marker.rkt` | Position marker（insert/delete 时自动调整） |
| `undo.rkt` | 线性 undo/redo 记录类型 |
| `keymap.rkt` | 稀疏前缀树 keymap（define-key、lookup-key） |
| `key-event.rkt` | Key event struct、分类 predicate |
| `syntax.rkt` | Syntax table（char classes + multi-char rules） |
| `category.rkt` | 语义分类符号常量（keyword、string、comment、…） |
| `completion.rkt` | 补全协议（source 类型、completion-style struct） |
| `bottom-input.rkt` | 底部行状态机 struct（idle/echo/doc/input） |
| `kill-ring.rkt` | 内部 kill ring（kill-new、yank、yank-pop） |
| `command.rkt` | 命令注册表（define-command、lookup-command） |
| `event-chain.rkt` | 分层事件分发（node chain） |
| `buffer.rkt` | Buffer struct + point/mark/region + buffer-local 变量 + make-buffer 工厂 |

### core/ — 内核原语的最小组合（产生可工作的核心对象）

| 文件 | 职责 |
|------|------|
| `window.rkt` | Window/frame struct + layout-frame! + 构造器 |
| `textprop.rkt` | interval-map 文本属性（face + paren-depth 双图） |
| `minibuffer.rkt` | Minibuffer 窗口激活/停用 + history |
| `search.rkt` | 纯文本正向/反向搜索 |
| `char-width.rkt` | 字符显示宽度 + gap 级 display-width 扫描 |

### base/ — 高层算法（组合 core + kernel 原语）

| 文件 | 职责 |
|------|------|
| `edit.rkt` | 编辑命令：move、insert、delete、kill、yank、undo/redo、symbol-at-point |
| `font-lock.rkt` | 三级字体化引擎：syntax pass → keyword pass → paren-depth pass |
| `syntax-cache.rkt` | 增量 parse-state 缓存（buffer-parse-state、matching-paren） |
| `completion-algo.rkt` | 补全算法（candidates 匹配、默认 style 实现） |
| `isearch.rkt` | 增量搜索（forward/backward） |
| `registry.rkt` | Buffer 注册表（get-buffer-create、kill-buffer、rename） |
| `window-ops.rkt` | 窗口操作：split、delete、other、switch-buffer |
| `keybind.rkt` | 字符串→key-event DSL（"C-x C-f" 等） |

### display/ — 渲染

| 文件 | 职责 |
|------|------|
| `render.rkt` | 多窗口渲染管线：recenter → compose → delta flush |
| `face.rkt` | Face 系统：defface、face-cache、face merging、ANSI 输出 |
| `vbuffer.rkt` | 虚拟屏幕缓冲区（cell grid） |
| `bottom-line.rkt` | 底部行渲染（单行 echo + 多行 doc 模式） |

### platform/ — 终端/OS

| 文件 | 职责 |
|------|------|
| `event.rkt` | 原始字节→key-event（UTF-8、CSI、SGR mouse、bracketed paste） |
| `termios.rkt` | Raw mode、窗口尺寸检测 |
| `ansi.rkt` | ANSI escape 常量（纯数据） |
| `file-io.rkt` | file→string、string→file |

### user/ — 用户层

| 文件 | 职责 |
|------|------|
| `command-loop.rkt` | 主循环：读事件→查键→run-command→渲染 |
| `minibuffer-loop.rkt` | Minibuffer 读取循环 |
| `global-bindings.rkt` | 全局键绑定（C-x C-f/s/b、C-g、C-c C-d） |
| `fundamental.rkt` | Fundamental mode：keymap + 标准 syntax-table |
| `racket.rkt` | Racket mode：Lisp syntax + font-lock 激活 |
| `racket-keywords.rkt` | Racket 关键词表（define、lambda、let、if 等） |
| `racket-doc.rkt` | Racket 文档查询（xref → bluebox → 底部行） |
| `font-lock-activate.rkt` | 字体化激活 + after-change hook 注册 |
| `standard-syntax.rkt` | 标准 syntax-table（[a-zA-Z0-9_] = word） |
| `mode.rkt` | 模式注册（setup-function + 文件扩展名） |
| `file-io.rkt` | find-file、save-buffer（组合 platform + mode） |

---

## 3. Emacs Lisp 原语兼容性

> 目标：未来通过 Racket 模块 (#lang elisp 或类似) 支持 Emacs Lisp，复用 Emacs 插件生态。

### 3.1 已实现 ✅

#### Buffer
`current-buffer` `buffer-name` `buffer-file-name` `buffer-modified-p` `set-buffer`
`get-buffer-create` `get-buffer` `kill-buffer` `buffer-list` `with-current-buffer`
`buffer-size` `buffer-substring` `buffer-string` `rename-buffer` `buffer-live-p`
`buffer-modified-tick` `generate-new-buffer`

#### Point / Marker
`point` `point-min` `point-max` `goto-char` `mark` `set-mark` `markerp`
`make-marker` `marker-position` `set-marker` `marker-buffer`
`marker-insertion-type` `region-active-p` `region-beginning` `region-end`

#### Text 编辑
`insert` `delete-char` `delete-backward-char` `delete-region`

#### Text Properties
`put-text-property` `get-text-property` `remove-text-properties`

#### Window / Frame
`selected-window` `window-buffer` `window-point` `set-window-point`
`window-start` `set-window-start` `window-height` `window-width`
`split-window` `delete-window` `delete-other-windows` `other-window`
`window-list` `selected-frame` `frame-selected-window`

#### Keymap
`make-keymap` `make-sparse-keymap` `define-key` `lookup-key` `keymapp`
`current-local-map` `use-local-map` `current-global-map`

#### Syntax Table
`make-syntax-table` `char-syntax` `set-syntax-table` `syntax-table`

#### Search
`search-forward` `search-backward`（纯文本，不支持 regex）

#### Files
`find-file` `save-buffer`

#### Minibuffer
`read-from-minibuffer` `read-string` `minibufferp`

#### Hooks
`before-change-functions` `after-change-functions`
`make-local-variable` `buffer-local-value`

#### Undo
`undo` `undo-boundary` `redo`

#### Display
`defface` `face-attribute`

#### Misc
`read-event` `save-excursion` `this-command` `last-command` `format`

### 3.2 缺失 ❌ — 按优先级

#### 🔴 P0 — 阻塞 elisp 兼容（没有这些插件无法运行）

| # | 原语 | 说明 | 难度 |
|---|------|------|------|
| 1 | `interactive` / `commandp` / `call-interactively` | Emacs 命令入口机制。`(interactive …)` 声明参数读取方式。 | 中 |
| 2 | `completing-read` + 补全框架 | 所有补全（M-x、find-file、switch-buffer）的根基。需要 completion table、collection、predicate。 | 中高 |
| 3 | `re-search-forward`/`re-search-backward` + match-data | 正则搜索。font-lock、isearch、无数 package 的基础。 | 中 |
| 4 | `pre-command-hook` / `post-command-hook` | 每个命令前后执行的 hook。eldoc、hl-line、global-*-mode 依赖。 | 低 |
| 5 | `start-process` + filter/sentinel | 外部进程。LSP、shell、compile、grep 模式必须。 | 高 |
| 6 | Overlay API（`make-overlay`…） | flymake、company、highlight 等大量 package 用 overlay 做视觉标记。 | 中 |
| 7 | `keymap-parent` / `set-keymap-parent` | Emacs mode 继承体系核心。derive-mode 依赖。 | 低 |
| 8 | `unread-command-events` | 事件回退。key-translation、macro、package 常用。 | 低 |
| 9 | `next-property-change` / `previous-property-change` | 文本属性扫描。font-lock 核心依赖。 | 低 |
| 10 | `advice-add` / `advice-remove` | 函数 advice。无数 package 通过 advice 修改行为。 | 中 |

#### 🟡 P1 — 重要但可逐步补充

| 原语 | 说明 |
|------|------|
| `looking-at` / `looking-back` | 当前位置正则匹配 |
| `match-string` / `match-beginning` / `match-end` | 正则匹配信息 |
| `replace-match` | 正则替换 |
| `where-is-internal` | 反向查找命令绑键 |
| `substitute-key-definition` | 批量替换键绑定 |
| `erase-buffer` | 清空 buffer |
| `write-file` | 另存为 |
| `insert-file-contents` | 插入文件内容 |
| `push-mark` / `pop-mark` / mark-ring | Mark 环 |
| `modify-syntax-entry` | 动态修改 syntax-table |
| `add-text-properties` / `set-text-properties` | text-prop 补充操作 |
| `universal-argument` / `prefix-numeric-value` | C-u 前缀参数 |
| `process-send-string` / `accept-process-output` | 进程通信 |
| `get-buffer-window` | 查找 buffer 所在的 window |
| `message` (format版) | 带格式的回显 |
| `sit-for` | 延时等待输入 |
| `current-time` | 时间戳 |
| `directory-files` | 目录列表 |
| `file-exists-p` / `expand-file-name` | 文件操作 |

#### 🟢 P2 — 高级功能

| 原语 | 说明 |
|------|------|
| `make-char-table` / char-table API | 字符查找表 |
| `progv` / 动态绑定 | elisp 的变量作用域机制 |
| `copy-syntax-table` / `with-syntax-table` | Syntax-table 复制/临时切换 |
| `define-prefix-command` | 前缀命令定义 |
| `suppress-keymap` | 抑制自插入键的 keymap |
| `pos-visible-in-window-p` | 判断位置是否可见 |
| `window-edges` | 窗口边界坐标 |
| `set-window-configuration` | 保存/恢复窗口布局 |
| `recenter` | 居中重绘 |
| `scroll-up` / `scroll-down` | 程序化滚动 |

---

## 4. 实现路线建议

### 第一阶段：elisp 命令模型（P0 #1, #4, #7, #8）
- `interactive` spec 解析
- `commandp` / `call-interactively`
- `pre-command-hook` / `post-command-hook`
- `keymap-parent` 继承
- `unread-command-events`

### 第二阶段：搜索与文本属性（P0 #3, #9）
- Regex search + match-data
- `looking-at` / `looking-back`
- `next-property-change` / `previous-property-change`

### 第三阶段：补全框架（P0 #2）
- completion table 协议
- `completing-read`
- TAB 补全 UI

### 第四阶段：Overlay + 进程（P0 #5, #6）
- Overlay API（基于 interval-map）
- `start-process` + filter/sentinel

### 第五阶段：Advice + 补充（P0 #10, P1）
- `advice-add` / `advice-remove`
- 剩余 P1 原语
