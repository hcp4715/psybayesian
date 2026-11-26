# 关于本仓库

本仓库是南京师范大学心理学院胡传鹏教授在2026年秋季学期中《高级心理统计》中的课件及相关内容.

本仓库内容由本人与助教共同完成，基于本课程[2023年学期课件](https://github.com/hcp4715/psybayesian/tree/2023%E7%A7%8B%E5%AD%A3%E5%AD%A6%E6%9C%9F)和[2024年学期课件](https://github.com/hcp4715/psybayesian/releases/tag/2024%E5%B9%B4%E7%A7%8B%E5%AD%A3%E5%AD%A6%E6%9C%9F%E5%AD%98%E6%A1%A3)。

## 助教信息

### 2026
- 王继贤
- 黄逸杰
- 陈思羽

### 2025
- 刘茗钰
- 陈思羽
- 邬思宇
- 蔡振辛


我们鼓励重复使用本仓库中的内容，但需遵守本仓库的版本协议。使用前请联系胡传鹏教授，邮箱：hcp4715@hotmail.com

This is a repo for teaching Bayesian analysis.

Author: Prof. Dr. HU Chuan-Peng; teaching assistants of 2026: Jixian Wang, Yijie Huang, Siyu Chen (王继贤、黄逸杰、陈思羽), of 2025: Mingyu Liu, Siyu Wu, Zhenxin Cai (刘茗钰、邬思宇、蔡振辛) et al.

Affiliation: School of Psychology, Nanjing Normal University, Nanjing, China

Please contact Prof. Hu before re-using materials in this repo.

Email: hcp4715@hotmail.com

## 相关资源
本课进行了录屏，并对录屏进行了文字转录。[在线电子书](https://hcp4715.github.io/PsyBayesianBook)；[2025年B站录屏](https://space.bilibili.com/252509184/lists/6346227),[2024年B站录屏](https://space.bilibili.com/252509184/channel/collectiondetail?sid=3799210)

## Outlines

| 序号  |                    课程内容                     |
| :--: | :--------------------------------------------: |
|  1   |                    课程介绍                     |  
|  2   |                  Bayes' Rule                   |
|  3   |        The Beta-Binomial Bayesian Model        |
|  4   | Balance and Sequentiality in Bayesian Analyses |
|  5   |          Approximating the Posterior           |
|  6   |              MCMC under the Hood               |
|  7   |        Posterior Inference & Prediction        |
|  8   |      Hypothesis Testing & Bayes Factors        |
|  9   |  A Simple Linear Regression Model with brms    |
|  10  |              Multiple regression               |
|  11  |         Evaluating Regression Models           |
|  12  |            GLM: Logistic Regression            |
|  13  |             Bayesian Item Response Modeling    |
|  14  |             Hierarchical Models 1              |
|  15  |             Hierarchical Models 2              |

## 文件夹结构

```bash
PyBayesian/
├── .github/                     # 存放GitHub相关配置，如workflows（用于自动化任务）
│
├── data/                        # 数据文件
│   ├── flanker_1.csv            # Flanker任务数据
│   ├── SMS_Well_being.csv       # SMS心理幸福感数据
│   └── ...                      # 各讲课件用到的其他数据
│
├── code/                        # 各讲配套的 R/Python 脚本（code/lec{id}.R / code/lec{id}.py）
├── assets/                      # 图片与库：figs/lec{id}/（手工素材 + 渲染图）+ libs/（全课共享库）
├── archive/                     # 历史归档：旧版 .ipynb 课件与旧 Dockerfile
├── tmpdata/                     # 贝叶斯模型的采样缓存（.rds/.RData，不入库）
├── 闯关题/                      # 课堂练习（闯关题）
├── 教学大纲及相关内容/          # 课程大纲（docx/pdf）等教学文档
├── .gitignore                   # Git忽略文件
├── _quarto.yml                  # Quarto 项目配置（lib-dir: assets/libs 指定共享库目录）
├── lec{id}.qmd / .html          # 课件源码与渲染结果（Quarto revealjs，浏览器放映）
├── lec.css                      # 全课共用的幻灯片样式
├── LICENSE                      # 许可证文件
└── README.md                    # 本仓库说明文件
```

# 环境配置和使用

本项目有三种环境使用方式：

- 自行本地环境配置（即在自己电脑上安装R/Python语言），见 [本地环境配置](#本地环境配置)
- 和鲸云服务器中的Bayesian镜像，无需额外配置环境
- dockerhub 镜像，所有用户可拉取镜像使用，见 [dockerhub镜像使用](#dockerhub镜像使用)

### 本地 R 配置

安装 [R](https://www.r-project.org/)、[RStudio](https://posit.co/download/rstudio-desktop/) 或者 [Positron](https://positron.posit.co/)。

安装贝叶斯推断常用的 R 包（如 brms、rstan、bayesplot、tidybayes、bayestestR、loo 等）：

```r
pacman::p_load( "brms", "rstan", "bayesplot", "tidybayes", "bayestestR", "loo")
```

## 使用 Quarto 渲染课件（终端）

课件使用 [Quarto](https://quarto.org/) 的 revealjs 格式：`lec{id}.qmd` 是源文件，渲染后生成同名的 `lec{id}.html`（1600×900，浏览器直接放映）。

**. 安装 Quarto**

```bash
# macOS（Homebrew）
brew install --cask quarto

# 其他系统见 https://quarto.org/docs/get-started/

quarto --version    # 确认安装成功
```

**. 渲染单讲**

在仓库根目录执行。渲染时会真实运行 `.qmd` 里的 R 代码，请先完成上一节的 R 包配置：

```bash
cd path/to/PsyBayesian

quarto render lec3.qmd    # 生成 lec3.html
```

- 产物为 `lec{id}.html`、`assets/figs/lec{id}/`（本讲的图）与 `assets/libs/`（全课共享的 revealjs 库，只存一份）；由 `_quarto.yml` 统一收纳，**不再生成 `lec{id}_files/`**
- 首次渲染要下载并缓存 revealjs 等资源，之后会明显加快
- 渲染需要写 Quarto 的缓存目录（macOS 为 `~/Library/Caches/quarto`）；若在容器或受限环境中报 `unable to open database file`，即该目录不可写

**3. 渲染全部 / 实时预览**

```bash
quarto render                 # 渲染根目录下全部 .qmd（含 MCMC 的讲次耗时较长，建议后台运行）

quarto preview lec3.qmd   # 本地起服务并预览，保存后自动刷新
```

**4. 常见问题**

| 情况 | 处理方式 |
|---|---|
| 想快速确认代码能不能跑通 | `SMOKE_TEST=true quarto render lec1.qmd`（目前仅第 1 讲支持：会减小 MCMC 迭代量） |
| 想重跑贝叶斯模型 | 删除 `tmpdata/` 下对应的 `.rds`；`brms::brm(file=)` 只要缓存存在就直接加载 |
| 内容超出 1600×900 被截断 | 优先合并多图 → 拆分 slide → 再调 `lec.css` 的字号与行距 |
| 打开 HTML 后图片不显示 | 用 `python3 -m http.server` 起本地服务再访问，避开浏览器对 `file://` 的限制 |

## dockerhub镜像使用 [2025年及以及课件适用]

我们已经将 docker 镜像上传至 [dockerhub](https://hub.docker.com/repository/docker/hcp4715/pybayesian)，你可以使用以下命令进行使用。

如果你已经安装了 docker desktop 或 docker engine，你可以使用以下命令拉取镜：

```bash
docker pull hcp4715/pybayesian
```

下载或者克隆 pybayesian 镜像，并运行docker 容器：

```bash
docker run -it --rm -v path/to/pybayesian:/home/jovyan -p 8888:8888 hcp4715/pybayesian
```
- 注意：请将 `path/to/pybayesian` 替换为你本地的 pybayesian 仓库路径。
- 例如，在 windows 下，下载或者克隆本pybayesian仓库到 D 盘，你可以执行以命令：`docker run -it --rm -v D:/pybayesian:/home/jovyan -p 8888:8888 hcp4715/pybayesian`
- 之后在浏览器中输入返回的 url，即可打开 jupyter notebook。在根目录下可以找到pybayesian仓库中的所有notebooks。

### 如何在VS Code打开的jupyter notebook中使用docker container的kernel：

https://medium.com/@FredAsDev/connect-vs-code-jupyter-notebook-to-a-jupyter-container-a63293f29325

1. 运行docker container：
   `docker run -it --rm -v ${PWD}:/home/jovyan/ -p 8888:8888 hcp4715/pybayesian:latest` Note: 根据系统不同，有可能需要使用 `${pwd}` 来指定的当前目录。
2. 在VS Code中安装jupyter扩展
3. 打开 jupyter notebook,在右上角的选择kernel中选择；
4. 在正上方的下拉选项中，选择“existing jupyter server”
5. Copy URL with port and add at the end /tree. Like this http://127.0.0.1:8888/tree
6. Press Enter go back to the terminal log: each time the container starts it generates a new token. Copy the token value (the part after `?token=` in the printed URL) and paste it when VS Code asks for the password, then hit enter:
7. Confirm if it is correct: 127.0.0.1
8. Select Python Kernel: