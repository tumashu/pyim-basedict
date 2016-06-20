- [chinese-pyim-basedict README](#chinese-pyim-basedict-readme)
  - [简介](#简介)
  - [安装和使用](#安装和使用)

# chinese-pyim-basedict README<a id="orgheadline3"></a>

## 简介<a id="orgheadline1"></a>

Chinese-pyim-basedict 是 chinese-pyim 的默认词库，词库的内容的来源:

1.  libpinyin 项目内置的词库
2.  chinese-pyim 用户贡献的个人词库

## 安装和使用<a id="orgheadline2"></a>

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET chinese-pyim-basedict RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：

        (require 'chinese-pyim-basedict)
