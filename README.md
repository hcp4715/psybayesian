# 关于本仓库

本仓库是南京师范大学心理学院胡传鹏教授在2024年秋季学期中《高级心理统计》中的课件及相关内容.

本仓库内容由胡传鹏、潘晚坷、朱珊珊三人共同完成，基于本课程[2023年学期课件](./Archive/2023/README.md)。

我们鼓励重复使用本仓库中的内容，但需遵守本仓库的版本协议。使用前请联系胡传鹏教授，邮箱：hcp4715@hotmail.com

This is a repo for teaching Bayesian analysis.

Author: Prof. Dr. HU Chuan-Peng; PAN, Wanke; Zhu Shanshan.

Affiliation: School of Psychology, Nanjing Normal University, Nanjing, China

Please contact Prof. Hu before re-using materials in this repo.

Email: hcp4715@hotmail.com

## 相关资源
本课进行了录屏，并对录屏进行了文字转录，见[B站录屏](https://space.bilibili.com/252509184/channel/collectiondetail?sid=3799210)和[在线电子书](https://hcp4715.github.io/PyBayesianBook/)

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
|  8   |           A Simple Normal Regression           |
|  9   |                  Bayes factors                 |
|  10  |              Multiple regression               |
|  11  |         Evaluating Regression Models           |
|  12  |            GLM: Logistic Regression            |
|  13  |             Hierarchical Models 1              |
|  14  |             Hierarchical Models 2              |
|  15  |                  特邀专家报告                    |

## 文件夹结构

```bash
PyBayesian/
├── .github/                     # 存放GitHub相关配置，如workflows（用于自动化任务）  
│
├── Archive/                     # 归档文件  
│   ├── 2022/                    # 2022年课件及相关数据  
│   └── 2023/                    # 2023年课件及相关数据  
│
├── data/                        # 数据文件  
│   ├── flanker_1.csv            # Flanker任务数据  
│   └── SMS_Well_being.csv       # SMS心理幸福感数据  
│
├── figs/                        # 存在使用的图片
├── .gitignore                   # Git忽略文件  
├── dockerfile                   # Docker配置文件  
├── Lecture{id}.ipynb            # 2024年第{id}讲课件  
├── LICENSE                      # 许可证文件   
├── README.md                    # 本仓库说明文件  
└── Syllabus_CN.md               # 课程大纲 (中文)
```

# 环境配置和使用

本项目有三种环境使用方式：

- 和鲸云服务器平台，专门为选课同学使用，无需额外配置环境
- dockerhub 镜像，所有用户可拉取镜像使用，见 [dockerhub镜像使用](#dockerhub镜像使用)
- 自行本地环境配置，见 [本地环境配置](#本地环境配置)

## dockerhub镜像使用

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


## 本地环境配置

有两种环境配置方式，即Docker配置和本地 python 配置。

### 本地 docoker 部署配置

首先确保你已经安装了 docker。

请克隆我们的仓库或者下载 dockerfile。

之后执行以下部署命令：

```bash
docker build -t {username}/{imagename}:2024
```

请将 `{username}` 和 `{imagename}` 替换为你自己的用户名和任意镜像名称。

之后运行：

```bash
docker run -it --rm -v path/to/pybayesian:/home/jovyan -p 8888:8888 {username}/{imagename}
```
- 注意：请将 `path/to/pybayesian` 替换为你本地的 pybayesian 仓库路径。
- 具体见[dockerhub镜像使用](#dockerhub镜像使用)

#### 如何在VS Code打开的jupyter notebook中使用docker container的kernel：

https://medium.com/@FredAsDev/connect-vs-code-jupyter-notebook-to-a-jupyter-container-a63293f29325

1. 运行docker container：
   `docker run -it --rm -v ${PWD}:/home/jovyan/ -p 8888:8888 hcp4715/pybayesian:latest` Note: 根据系统不同，有可能需要使用 `${pwd}` 来指定的当前目录。
2. 在VS Code中安装jupyter扩展
3. 打开 jupyter notebook,在右上角的选择kernal中选择；
4. 在正上方的下拉选项中，选择“existing jupyter server”
5. Copy URL with port and add at the end /tree. Like this http://127.0.0.1:8888/tree
6. Press Enter go back to the log, and copy the token value. Paste it when it asks for the password (0cca3493bcfddba8451ecfe0f9e2ccf30cae85026154b397) and hit enter:
7. Confirm if it is correct: 127.0.0.1
8. Select Python Kernel:

### 本地 python 配置

如果你已经安装了 python 3.10-3.11，你可以使用以下命令安装依赖：

```bash
pip install -r requirements.txt
```
