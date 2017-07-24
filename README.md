- [pyim-basedict README](#org9c0ad64)
  - [简介](#org9738d87)
  - [安装和使用](#orgad28538)
  - [如何为这个项目贡献词条](#org4bcc69d)


<a id="org9c0ad64"></a>

# pyim-basedict README


<a id="org9738d87"></a>

## 简介

pyim-basedict 是 pyim 的默认词库，词库的内容的来源:

1.  libpinyin 项目的内置词库
2.  pyim 用户贡献的个人词库

注意：这个词库的词条量大概在1万左右，是一个 **非常小** 得词库，只能确保 pyim 可以正常工作，如果用户想让 pyim 更加顺手，需要添加附加的词库， 一个比较好的选择是安装 pyim-greatdict, 不过这个词库非常庞大，词条量 超过300万，不适合计算机 cpu 和内存不足的用户。另外，用户也可以使用其它方式 添加词库，具体请参考 pyim README 中的相关章节：

<https://github.com/tumashu/pyim>


<a id="orgad28538"></a>

## 安装和使用

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET pyim-basedict RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：

        (require 'pyim-basedict)
        (pyim-basedict-enable)


<a id="org4bcc69d"></a>

## 如何为这个项目贡献词条

pyim 的用户，如果想让自己常用词条进入 pyim 默认词库的话， 可以运行 \`pyim-contribute-words' , 然后按照它的提示操作就可以了。


Converted from pyim-basedict.el by [el2org](https://github.com/tumashu/el2org) .