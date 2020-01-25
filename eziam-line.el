;;; eziam-line.el --- Yet another Emacs mode-line

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

;; A simple and, I hope, aesthetic mode-line for Emacs.

;;; Code:

(require 'kurecolor)
(require 'powerline)
(require 'server)

(setq x-underline-at-descent-line t
      powerline-gui-use-vcs-glyph t)

;;;;; Faces (and cursor!)

(defun thblt/mode-line-set-faces (&rest _) ; I'm hooking this on theme change so it needs to accept arguments
  "Configure faces for the mode-line."
  (interactive)
  (pl/reset-cache)

  (let* ((dark (< (kurecolor-hex-get-brightness (face-attribute 'default :background)) .5))

         (buffid-bg (if dark "#fff" "#000"))
         (buffid-bg-inactive (if dark "#777" "#aaa"))

         (inactive (if dark "#111111" "#dddddd")))

    ;; Modeline
    (face-spec-set 'mode-line
                   `((t
                      :background ,(if dark "#334" "#667")
                      :foreground ,(if dark "#ffe" "#ffe")
                      :overline ,(if dark "#667" "#334")
                      :underline ,(if dark "#667" "#334"))))

    ;; Inactive mode line (invisible)
    (face-spec-set 'mode-line-inactive
                   `((t
                      :background ,inactive
                      :foreground ,inactive
                      :overline ,(if dark "#444" "#fff")
                      :underline ,(if dark "#444" "#fff"))))


    (face-spec-set 'thblt/mode-line--window-id
                   `((t
                      :background "#ffcc33"
                      :foreground "#000000")))

    (face-spec-set 'thblt/mode-line--buffer-id
                   `((t
                      :background ,buffid-bg
                      :foreground ,(if dark "#000" "#fff"))))

    (face-spec-set 'thblt/mode-line--buffer-id-inactive
                   `((t
                      :background "#555555"
                      :foreground ,(if dark "#000" "#fff"))))


    (face-spec-set 'thblt/mode-line--window-id-inactive
                   `((t
                      :background "#444444"
                      :foreground "#ffcc33")))

    (face-spec-set 'thblt/mode-line--buffer-modified
                   `((t
                      :background ,buffid-bg
                      :foreground "#ff0000")))

    (face-spec-set 'thblt/mode-line--buffer-modified-inactive
                   `((t
                      :inherit thblt-mode-line--buffer-modified
                      :background ,buffid-bg-inactive)))

    (face-spec-set 'thblt/mode-line--buffer-read-only
                   `((t
                      :background ,buffid-bg-inactive
                      :foreground "#ff0000"))) ;

    ;; Narrowing indicator
    (face-spec-set 'thblt/mode-line--buffer-narrowed
                   `((t
                      :background ,buffid-bg
                      :foreground "#888888")))

    ;; Minor mode lighter
    (face-spec-set 'thblt/mode-line--minor-mode
                   `((t)))

    ;; Server ON face
    (face-spec-set 'thblt/mode-line--server
                   `((t
                      :foreground "#fe3"
                      :weight bold)))

    ;; Delimiter
    (face-spec-set 'thblt/mode-line--delimiter
                   `((t
                      :foreground "#97978D")))))

;;;;; Theme

(defmacro thblt/face-maybe-inactive (face)
  (let ((inac (intern (format "%s-inactive" face))))
    `(if active ',face ',inac)))

(defun thblt/powerline-theme ()
  "Setup the default mode-line."

  ;; MAINTENANCE NOTES
  ;;
  ;; the `mode-line' face isn't used, because the whole modeline color
  ;; is determined by the current Evil mode.

  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (face (if active 'mode-line 'mode-line-inactive))
             (last-face) ; The last used face, to show the
                                        ; correct separator after conditional
                                        ; segments
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (delim-face (if active 'thblt/mode-line--delimiter face))
             (space (powerline-raw " " face))
             (open (powerline-raw " [" delim-face))
             (open* (powerline-raw "[" delim-face))
             (close (powerline-raw "]" delim-face))
             (lhs
              (list
               ;; Window ID
               (progn
                 (setq last-face (thblt/face-maybe-inactive thblt/mode-line--window-id))
                 (setq face (thblt/face-maybe-inactive thblt/mode-line--buffer-id))
                 (propertize " 1 " 'face last-face))

               (funcall separator-left last-face face)

               ;; Buffer id
               ;; Modified?
               (when (and buffer-file-name (buffer-modified-p))
                 (powerline-raw " ●"  (thblt/face-maybe-inactive thblt/mode-line--buffer-modified)))
               ;; Read-only?
               (when buffer-read-only
                 (powerline-raw " "  'thblt/mode-line--buffer-read-only))
               ;; Not read-only, has a file, but isn't modified: spaces where the modified marker will appear
               (when (and buffer-file-name
                          (not (or (buffer-modified-p)
                                   buffer-read-only)))
                 ;; @Notice: we're borrowing the narrow face here
                 (powerline-raw " -" 'thblt/mode-line--buffer-narrowed))

               ;; Buffer name
               (progn
                 (setq last-face (thblt/face-maybe-inactive thblt/mode-line--buffer-id))
                 ( powerline-raw
                   " %b "
                   `(:weight ,(if buffer-file-name 'bold 'normal) :inherit ,last-face)))

               ;; Narrowing indicator
               (when (buffer-narrowed-p)
                 (powerline-raw "[Narrow] " `(:inherit thblt/mode-line--buffer-narrowed :inherit thblt/mode-line--buffer-id )))

               (progn
                 (setq face (thblt/face-maybe-inactive mode-line))
                 (funcall separator-left last-face face))
               ;; Position
               (powerline-raw " %2l:%3c [%o]    " face)

               ;; Major mode
               open
               (powerline-major-mode `(:inherit ,face :weight bold 'r))
               ;; Minor modes
               (powerline-minor-modes  face 'l)
               close

               space space space space

               ;; open
               ;; (powerline-raw "⯃ 3 ⯅ 14" face)
               ;; close
               ))
             (rhs (list
                   ;; Version control
                   (when buffer-file-name
                     (concat
                      open
                      (powerline-raw
                       (concat
                        (projectile-project-name)
                        (powerline-vc))
                       face)
                      close))

                   space
                   (when  (window-parameter (selected-window) 'thblt/window-at-bottom-right)
                     (powerline-raw
                      (if server-process
                          (propertize (format " [%s] " server-name) 'face 'thblt/mode-line--server)
                        ""))))

                  ))

        (concat (powerline-render lhs)
                (powerline-fill face (powerline-width rhs))
                (powerline-render rhs)))))))

;;;;; Window position tracker

(defun thblt/window-at-bottom-left-p (win)
  "Return non-nil if WIN is at the bottom left of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'left win))))

(defun thblt/window-at-bottom-right-p (win)
  "Return non-nil if WIN is at the bottom right of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'right win))))

(defun thblt/update-window-position-parameters (&optional frame)
  (unless frame (setq frame (selected-frame)))
  (mapc (lambda (win)
          (set-window-parameter win 'thblt/window-at-bottom-left (thblt/window-at-bottom-left-p win))
          (set-window-parameter win 'thblt/window-at-bottom-right (thblt/window-at-bottom-right-p win)))
        (window-list frame nil)))

(add-hook 'window-configuration-change-hook 'thblt/update-window-position-parameters)

;;;;; Installation

(thblt/mode-line-set-faces)
(advice-add 'load-theme :after 'thblt/mode-line-set-faces)

(add-to-list 'after-make-frame-functions 'thblt/update-window-position-parameters)
(unless (daemonp)
  (thblt/update-window-position-parameters)) ; This is required for
                                        ; non-daemon instances
                                        ; where the frame is
                                        ; created before init.el
                                        ; gets to run.

(thblt/powerline-theme)

(provide 'eziam-line)
