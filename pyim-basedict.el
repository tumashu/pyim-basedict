;;; pyim-basedict.el --- The default pinyin dict of pyim  -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015 Feng Shu <tumashu@163.com>

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim-basedict
;; Version: 0.4.0
;; Keywords: convenience, Chinese, pinyin, input-method, complete

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; * pyim-basedict README                         :README:doc:

;; ** 简介
;; pyim-basedict 是 pyim 输入法的默认词库，词库数据来源为 libpinyin 项目。

;;           https://github.com/libpinyin/libpinyin

;; 注意：这个词库的词条量大概在 10 万左右，是一个 *比较小* 的词库，只能确保 pyim
;; 可以正常工作，如果用户想让 pyim 更加顺手，需要添加其它附加词库，具体添加词库的
;; 方式可以参考 pyim 的 README.

;; ** 安装和使用
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET pyim-basedict RET
;; 3. 在Emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (pyim-basedict-enable)
;;    #+END_EXAMPLE

;;; Code:
;; * 代码                                                               :code:
(defvar pyim-basedict-libpinyin-data-files
  (list "society.table"
        "life.table"
        "people.table"
        "culture.table"
        "economy.table"
        "technology.table"
        "science.table"
        "nature.table"
        "history.table"
        "art.table"
        "sport.table"
        "geology.table"
        "merged.table"
        "opengram.table"
        "gb_char.table"
        "gbk_char.table")
  "Libpinyin data files")

;;;###autoload
(defun pyim-basedict-enable ()
  "Add basedict to pyim."
  (interactive)
  (let* ((file (expand-file-name "pyim-basedict.pyim"
                                 (file-name-directory
                                  (locate-library "pyim-basedict.el")))))
    (when (file-exists-p file)
      ;; FIXME: If `pyim-basedict-enable' is called early enough, pyim
      ;; won't be loaded yet and this (featurep 'pyim) will return nil.
      ;; Maybe we should just (require 'pyim) and call
      ;; `pyim-extra-dicts-add-dict' unconditionally, or maybe we should
      ;; use `with-eval-after-load'.
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(;; Make Indent beautiful :-)
             :name "Basedict-elpa"
             :file ,file
             :coding utf-8-unix
             :dict-type pinyin-dict
             :elpa t))
        (message "pyim 没有安装，pyim-basedict 启用失败。")))))


(declare-function 'pyim-dline-parse "pyim")
(declare-function 'pyim-pymap-cchar< "pyim-pymap")

(defun pyim-basedict-build-file ()
  "使用 libpinyin 自带的 data 文件创建 pyim-basedict.pyim."
  (interactive)
  (let ((dir (read-directory-name "请选择 libpinyin data 所在的目录："))
        (hash-table (make-hash-table :test #'equal)))
    (with-temp-buffer
      (erase-buffer)
      (dolist (filename pyim-basedict-libpinyin-data-files)
        (when (file-exists-p (expand-file-name filename dir))
          (insert-file-contents (expand-file-name filename dir))
          (goto-char (point-max))))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((contents (pyim-dline-parse))
               (code (replace-regexp-in-string
                      "'" "-"
                      (car contents)))
               (word (cadr contents)))
          (puthash code (push word (gethash code hash-table))
                   hash-table))
        (forward-line 1)))
    (with-temp-buffer
      (maphash
       (lambda (key value)
         (setq value (delete-dups (reverse value)))
         (unless (string-match-p "-" key)
           (setq value (sort value #'pyim-pymap-cchar<)))
         (insert (format "%s %s\n" key (mapconcat #'identity value " "))))
       hash-table)
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (insert ";; -*- coding: utf-8 -*--\n")
      (write-file "pyim-basedict.pyim" t))))

;; * Footer
(provide 'pyim-basedict)

;;; pyim-basedict.el ends here
