;;; pyim-basedict.el --- The default pinyin dict of pyim  -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim-basedict
;; Version: 0.5.4
;; Keywords: convenience, Chinese, pinyin, input-method, complete

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; * pyim-basedict README                         :README:doc:

;; ** 简介
;; pyim-basedict 是 pyim 输入法的默认词库，词库数据来源为 libpinyin 项目。

;;  https://github.com/libpinyin/libpinyin/releases (Data files we need is in release tarball)

;; 注意：这个词库的词条量大概在 10 万左右，是一个 *比较小* 的词库，只能确保 pyim
;; 可以正常工作，如果用户想让 pyim 更加顺手，需要添加其它附加词库，具体添加词库的
;; 方式可以参考 pyim 的 README.

;; ** 安装和使用
;; 1. M-x package-install RET pyim-basedict RET
;; 2. 在 Emacs 配置文件中（比如: "~/.emacs"）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (pyim-basedict-enable)
;;    #+END_EXAMPLE

;;; Code:
;; * 代码                                                               :code:
(require 'pyim-dict)

;;;###autoload
(defun pyim-basedict-enable ()
  "Add pyim-basedict.pyim file to `pyim-extra-dicts'."
  (interactive)
  (let ((file (expand-file-name
               "pyim-basedict.pyim"
               (file-name-directory
                (locate-library "pyim-basedict.el")))))
    (when (file-exists-p file)
      (pyim-extra-dicts-add-dict
       `(;; Make Indent beautiful :-)
         :name "Basedict-elpa"
         :file ,file
         :coding utf-8-unix
         :dict-type pinyin-dict
         :elpa t)))))

;; * Footer
(provide 'pyim-basedict)

;;; pyim-basedict.el ends here
