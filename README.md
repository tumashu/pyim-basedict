Note: this file is auto converted from pyim-basedict.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [pyim-basedict README](#orgd067e30)
    1.  [简介](#org0a5c058)
    2.  [安装和使用](#orgfc7f697)


<a id="orgd067e30"></a>

# pyim-basedict README


<a id="org0a5c058"></a>

## 简介

pyim-basedict 是 pyim 输入法的默认词库，词库数据来源为 libpinyin 项目。

<https://github.com/libpinyin/libpinyin/releases> (Data files we need is in release tarball)

注意：这个词库的词条量大概在 10 万左右，是一个 **比较小** 的词库，只能确保 pyim
可以正常工作，如果用户想让 pyim 更加顺手，需要添加其它附加词库，具体添加词库的方式可以参考 pyim 的 README.


<a id="orgfc7f697"></a>

## 安装和使用

1.  M-x package-install RET pyim-basedict RET
2.  在 Emacs 配置文件中（比如: "~/.emacs"）添加如下代码：
    
        (pyim-basedict-enable)

