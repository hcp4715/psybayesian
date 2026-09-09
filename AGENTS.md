# Agent Permanent Memory

## 1. 身份与使命 (Identity)
为南京师范大学心理学院贝叶斯统计课程（《Bayesian Statistics with Python》）制作可放映的 Quarto revealjs 课件（Lecture1.qmd → 16:9 HTML），核心要求：**每张 slide 内容必须在 1600×900 设计稿内单页完整显示**，R 代码真实执行，结果可信可缓存。

## 2. 铁律与工作流 (Rules & SOPs)

- **版式验证双通道**：① 静态（CSS 挂载与否必须 grep `<link>` 确认）；② 运行时用 playwright 实测——按 `(rect.bottom - secRect.top)/Reveal.getScale()` 算 local bottom，阈值 ≤897（slide 高 900）；必须 `?nocache=` 重载否则读到旧 CSS 假阴性。
- **渲染 SOP**：改完 qmd/css → 同步到 preview 目录（`/var/folders/1_/.../T/opencode/preview`）→ `SMOKE_TEST=true quarto render` 快速验证版式（1-2 min）→ 确认后再全量渲染。
- **长渲染（>30min）必须 nohup 后台化**：`nohup quarto render ... > log 2>&1 &`，记录 PID，周期 `sleep 600` 轮询，**禁止前台同步等待**（60min 必超时被 kill）。
- **MCMC 缓存用工具原生机制**：brms 加 `file=` 参数即可（存在即加载），smoke/full 文件名必须区分（`tmpdata/xxx_smoke` vs `tmpdata/xxx`），目录入 `.gitignore`。
- 溢出修复优先级：合并多图 > 拆 slide > 全局 CSS 压字号/行距 > 截断输出加滚动（`code-overflow:scroll` 只对源码生效，**stdout 输出必须自定义 max-height**）。

## 2.5 Skills 使用指南 (Skills Guide)

| Skill | 触发场景 | 用法 |
|---|---|---|
| **quarto-pptx-creator**（项目级，`.agents/skills/`；opencode 旧路径已弃用，DSH/多 agent 均从此目录读取） | 设计新课件/新章节的 qmd 结构、把素材拆成逐页 slide、内容组织方法论 | `skill(name="quarto-pptx-creator")` 加载其流程参考；做 revealjs 时借鉴其"素材→结构化 qmd"骨架，但输出格式仍遵循本文件渲染 SOP |
| **playwright / dev-browser** | 渲染产物视觉验证、溢出检测、页面截图/操作 | 溢出检测核心工具（见 3. 节场景 1）；必须配 `browser_run_code_unsafe` 跑 DOM 测量脚本 |
| **frontend-ui-ux** | slide 视觉/布局调优（两栏、字号、图排版） | 委派 UI 类任务时 `task(category="visual-engineering", load_skills=["frontend-ui-ux"], ...)`，勿用 quick/unspecified 类 |
| **git-master** | 任何 git 操作（提交、历史检索） | `task(category="quick", load_skills=["git-master"], ...)` 委派，节省主上下文 |
| **review-work** | 较大实现完成后自查 | 委派 5 路并行审阅；本环境无图像能力，审查以数值/DOM 证据为准 |
| **ai-slop-remover** | 清理代码中的 AI 风格冗余注释 | 单文件逐个调用 |

注意：**委派任何 subagent 时都必须传 `load_skills`**（匹配的 skill 优先；无匹配传 `[]`）。本项目无图像输入能力，凡需"看图"一律改用 DOM/数值证据，勿依赖视觉类 skill。

## 3. 避坑指南 / 经验库 (Lessons Learned)

> **场景**: 渲染后视觉/溢出问题排查
> **❌ 踩坑记录**: ① DOM 全局检测把视口内正常元素误报溢出，且未排除折叠 `<details>` 内不可见 PRE；② 用 file:// 直接访问被浏览器阻止；③ 改 CSS 后复测数值原封不动=浏览器缓存（非 CSS 无效）。
> **✅ 正确姿势**: 只测当前 present slide；排除 `details:not([open])`；局部坐标系除以 scale 且过滤 height≤1；起 `python3 -m http.server` 后访问；URL 加 `?nocache=timestamp` 强刷。
> **🔔 预警信号**: 数值诡异没变化 → 先查缓存；元素有 rect 但折叠 → 查 closest('details').open。

> **场景**: 调用 rstan/brms 缓存
> **❌ 踩坑记录**: 误以为 `rstan::stan(file=...)` 是结果缓存——实际 `file` 是 **Stan 模型代码路径**，传 .rds 会当代码读而报错；只有 `brms::brm(file=)` 是结果缓存（`file_refit="never"`）。
> **✅ 正确姿势**: 先 `args()`/help 核实 API 语义再下结论；smoke 与 full 采样量不同，缓存名必须区分否则 full 加载 smoke 小样本；改模型后需手动删 `tmpdata/*.rds`。
> **🔔 预警信号**: 用户说"XX 自带缓存"时，先本地验证该包参数再设计，避免直接照搬。

> **场景**: 视觉确认截图
> **❌ 踩坑记录**: 当前主模型与 multimodal-looker 用的 big-pickle **均不支持图像输入**（look_at 直接报错，agent 挂死 3min）。
> **✅ 正确姿势**: 放弃读图，全部改用精确 DOM 数值测量（block 级 local top/bottom 定位溢出元素），证据更强。
> **🔔 预警信号**: 图分析任务长时间 running → 立即 cancel，不等待。

> **场景**: 第三方 MCP 安装
> **❌ 踩坑记录**: bayes-msp 仓库代码缺失（schemas/inputs.py、outputs.py 不存在）、pyproject 包结构错误致 `pip install` 失败、`/mcp` 端点是非标准 JSON-RPC（opencode 连不上）、PyMC6 与旧代码不兼容。README 声称的 URL 与实际仓库名还不一致。
> **✅ 正确姿势**: 先 clone 完整审查 + 用标准 MCP initialize 探测协议兼容性，再决定修复/放弃，别急着写 opencode.json。
> **🔔 预警信号**: star 少（4★）的 MCP 仓库，协议与可运行性都需实测。

## 4. 上下文默认值 (Context Defaults)

- **语言**：中文交流，学术内容保留英文术语；回复精炼、多用表格/代码块，避免赘述。
- **工作目录**：真目录 `/Users/hcp4715/Library/CloudStorage/OneDrive-Personal/Teaching/Bayesian/PsyBayesian/`（OneDrive，勿动其文件结构）；preview 渲染目录在系统 temp。
- **文件**：Lecture1.qmd 主交付物 + `lecture1.css`；数据 `data/flanker_1.csv`(6.7万行)、`data/SMS_Well_being.csv`；图 `figs/lec1/`(13张)。
- **渲染**：默认 smoke 验证、正式版后台跑；82→83 slides；figs 有 lecture1_files/figure-revealjs 6 张 R 图。
- **引用规范**：参考文献页倾向只保留与内容直接相关条目（用户曾因"英文引用疑似错误且无关"要求删）。

## 5. 待确认事项 (Pending Clarifications)

- 全量渲染每次仍跑 rstan 例1（约 5min）——是否也要缓存（当前明确"仅 brms 缓存，rstan 照跑"）。
- `figs/lec1/` 未纳入 git（一直 untracked）；`Lecture1.qmd/html` 亦未入库——是否整体纳入版本管理？
- `figs/lec1/` 中未被引用的 3 张图（lec1_IIT.png、lec1_Ma_Griffiths_BookCover.png、meme.jpg）是否删除？
- 恢复文件 `Lecture1_backup.qmd` 保留作安全网——何时可删？
