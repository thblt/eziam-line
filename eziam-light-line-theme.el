;;; eziam-light-line-theme.el --- Light theme for Eziam-Line

;; Copyright (c) 2020 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintainer: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: faces
;; Homepage: https://github.com/thblt/eziam-line
;; Version: 1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A theme for Eziam Line, suitable for light themes.

;;; Code:

(require 'eziam-line)

(deftheme eziam-light-line
  "A light theme for Eziam Line")

(eziam-line--apply-basic-theme 'eziam-light-line
  :line "#000000"
  :line-inactive "#cccccc"
  :default-bg "#ffffff"
  :default-bg-inactive "#dddddd"
  :default-fg "#222244"
  :default-fg-inactive "#dddddd"
  :buffid-bg "#3C434A"
  :buffid-bg-inactive "#dddddd"
  :buffid-fg "#ffffee"
  :buffid-fg-inactive "#444444"
  :buffmodified-fg "#ff0000"
  :buffmodified-fg-inactive "#660000")

(provide 'eziam-light-line-theme)

;; eziam-light-line-theme ends here
;; (progn (save-buffer) (load-theme 'eziam-light-line t))
