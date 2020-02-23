;;; eziam-dark-line-theme.el --- Dark theme for Eziam-Line

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

;; A theme for Eziam Line, suitable for dark themes.

;;; Code:

(require 'eziam-line)

(deftheme eziam-dark-line
  "A dark theme for Eziam Line")

(eziam-line--apply-basic-theme 'eziam-dark-line
  :line "#7D7D75"
  :line-inactive nil
  :default-bg "#333344"
  :default-bg-inactive "#1B1B28"
  :default-fg "#ffe"
  :default-fg-inactive "#1B1B28"
  :buffid-bg "#ffe"
  :buffid-bg-inactive "#1B1B28"
  :buffid-fg "#333344"
  :buffid-fg-inactive "#666666"
  :buffmodified-fg "#ff0000"
  :buffmodified-fg-inactive "#660000")

(provide 'eziam-dark-line-theme)

;; eziam-dark-line-theme ends here
