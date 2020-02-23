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

(require 'server)

;;;; Variables

(defvar eziam-line--selected-window nil
  "The currently selected windom.")

;;;; Faces

;;;;; Faces declaration

(defgroup eziam-line-faces nil
  "Faces for the Eziam mode line."
  :group 'faces)

(defface eziam-line-buffer-id
  '((t (:inherit mode-line)))
  "Buffer ID segment in Eziam-Line (active)"
  :group 'eziam-line-faces)

(defface eziam-line-project-id
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-project-id-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-project-buffer-transition
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-id-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-modified
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-modified-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-read-only
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-read-only-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-narrowed
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-buffer-narrowed-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-project-id
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-project-id-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-delimiter
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-delimiter-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-minor-mode
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-minor-mode-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-server
  '((t (:inherit mode-line)))
  ""
  :group 'eziam-line-faces)

(defface eziam-line-server-inactive
  '((t (:inherit mode-line-inactive)))
  ""
  :group 'eziam-line-faces)

;;;;; Theme template

(cl-defmacro eziam-line--apply-basic-theme
    (name &key
          line
          line-inactive
          default-bg
          default-bg-inactive
          default-fg
          default-fg-inactive
          buffid-bg
          buffid-bg-inactive
          buffid-fg
          buffid-fg-inactive
          buffmodified-fg
          buffmodified-fg-inactive)
  "Set Eziam-Line faces for a theme called NAME with provided colors.

The keyword arguments LINE, LINE-INACTIVE, DEFAULT-BG, ,
DEFAULT-BG-INACTIVE, DEFAULT-FG, DEFAULT-FG-INACTIVE, BUFFID-BG,
BUFFID-BG-INACTIVE, BUFFID-FG, BUFFID-FG-INACTIVE,
BUFFMODIFIED-FG and BUFFMODIFIED-FG-INACTIVE configure the
colors.  See the source for their use."
  (declare (indent 1))
  `(progn
     (custom-theme-set-faces
      ,name
      '(mode-line                            ((t (:background ,default-bg :foreground ,default-fg :overline ,line :underline ,line))))
      '(mode-line-inactive                   ((t (:background ,default-bg-inactive :foreground ,default-fg-inactive :overline ,line-inactive :underline ,line-inactive))))
      '(eziam-line-project-id                ((t (:background "#77cc00" :foreground "#000000"))))
      '(eziam-line-project-id-inactive       ((t ())))
      '(eziam-line-project-buffer-transition ((t (:background ,buffid-bg :foreground "#77cc00" ))))
      '(eziam-line-buffer-id                 ((t (:background ,buffid-bg :foreground ,buffid-fg))))
      '(eziam-line-buffer-id-inactive        ((t (:background ,buffid-bg-inactive :foreground ,buffid-fg-inactive))))
      '(eziam-line-buffer-modified           ((t (:background ,buffid-bg :foreground ,buffmodified-fg))))
      '(eziam-line-buffer-modified-inactive  ((t (:background ,buffid-bg-inactive :foreground ,buffmodified-fg-inactive))))
      '(eziam-line-buffer-read-only          ((t (:background ,buffid-bg :foreground ,buffmodified-fg))))
      '(eziam-line-buffer-read-only-inactive ((t (:background ,buffid-bg-inactive :foreground ,buffmodified-fg-inactive))))
      '(eziam-line-narrowing                 ((t ())))
      '(eziam-line-narrowing-inactive        ((t ())))
      '(eziam-line-delimiter                 ((t ())))
      '(eziam-line-delimiter-inactive        ((t ())))
      '(eziam-line-minor-mode                ((t ())))
      '(eziam-line-minor-mode-inactive       ((t ())))
      '(eziam-line-server                    ((t ())))
      '(eziam-line-server-inactive           ((t ()))))
     (custom-theme-set-variables
      ,name
      '(x-underline-at-descent-line t)
      '(powerline-gui-use-vcs-glyph t))))

;;;; Line


(defmacro eziam-line--face-maybe-inactive (face)
  (let ((inac (intern (format "%s-inactive" face))))
    `(if active ',face ',inac)))

(defun eziam-line-separator-left (left right)
  (let ((bg (face-attribute right :background))
        (fg (face-attribute left :background)))
    (propertize "" 'face `(:background ,bg :foreground ,fg))))
    ;; (message "%s %s %s %s" bg fg left right)

(defmacro eziam-line--maybe (&rest body)
  `(or
    ,@body
    ""))

(defmacro eziam-line--when-let (def &rest body)
  `(let ((it ,def))
     (if (not it)
         ""
       ,@body)))

(setq-default mode-line-format
'("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
  (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))



;;;###autoload
(defun eziam-line-install ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '(:eval
     (let* ((active (eq eziam-line--selected-window (selected-window)))
            (face)
            (space " ")
            (open  " [" )
            (close  "]")
            (left
            (concat
              ;; Project ID, if there's a project
              (eziam-line--maybe
               (when (projectile-project-p)
                 (propertize (format " %s " (projectile-project-name)) 'face (eziam-line--face-maybe-inactive eziam-line-project-id))))

              ;; Buffer id
              ;; Modified?
              (when (and buffer-file-name (buffer-modified-p))
                (powerline-raw " ●"  (eziam-line--face-maybe-inactive eziam-line-buffer-modified)))
              ;; Read-only?
              (when buffer-read-only
                (powerline-raw " "  (eziam-line--face-maybe-inactive eziam-line-buffer-read-only)))
              ;; Not read-only, has a file, but isn't modified: spaces where the modified marker will appear
              (when (and buffer-file-name
                         (not (or (buffer-modified-p)
                                  buffer-read-only)))
                ;; @Notice: we're borrowing the narrow face here
                (powerline-raw " -" (eziam-line--face-maybe-inactive eziam-line-buffer-id)))

              ;; Buffer name
              (progn
                (setq face (eziam-line--face-maybe-inactive eziam-line-buffer-id))
                (powerline-raw
                 (format " %s " (car mode-line-buffer-identification))
                 `(:weight ,(if buffer-file-name 'bold 'normal) :inherit ,face)))

              ;; Narrowing indicator
              (when (buffer-narrowed-p)
                (propertize " " 'face (eziam-line--face-maybe-inactive eziam-line-buffer-narrowed)))

              space
              ;; Position
              (format " %2s:2c " (line-number-at-pos))

              ;; Major mode
              open
              (powerline-major-mode `(:inherit ,(eziam-line--face-maybe-inactive mode-line) :weight bold 'r))
              ;; Minor modes
              (powerline-minor-modes (eziam-line--face-maybe-inactive mode-line) 'l)
              close))
            (right
             (format-mode-line
              (list
               ;; Version control
               (when buffer-file-name
                 (concat
                  open
                  (powerline-raw
                   (concat
                    "▕ "
                    (projectile-project-name)
                    (powerline-vc))
                   face)
                  close))

               space
               (when (window-parameter (selected-window) 'eziam-line--window-at-bottom-right)
                 (powerline-raw
                  (if server-process
                      (propertize (format " [%s] " server-name) 'face 'eziam-line-server)
                    ""))))))
            (available-width (- (window-width) (length left) (length right))))
       (format "%s%s%s" left (make-string available-width ? ) (format-mode-line right)))))

  ;; Install utilities
  (add-hook 'window-configuration-change-hook 'eziam-line--update-state)
  (add-function :after after-focus-change-function 'eziam-line--update-state)
  (advice-add 'select-window :after 'eziam-line--update-state)

  (eziam-line--update-state))

(defun eziam-line-uninstall nil
  "Remove support hooks for eziam-line."
  (interactive)
  (setq after-make-frame-functions (delete  'eziam-line--update-state after-make-frame-functions))
  (remove-hook 'window-configuration-change-hook 'eziam-line--update-state)
  (remove-function after-focus-change-function 'eziam-line--update-state))


;;;; Utilities

(defun eziam-line--window-at-bottom-left-p (win)
  "Return non-nil if WIN is at the bottom left of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'left win))))

(defun eziam-line--window-at-bottom-right-p (win)
  "Return non-nil if WIN is at the bottom right of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'right win))))

(defun eziam-line--update-state (&rest _)
  "Update relevant state information"

  (let ((frame (selected-frame)))
    ;; Set selected window
    ;; (when (not (minibuffer-window-active-p (frame-selected-window frame)))
    (setq eziam-line--selected-window (frame-selected-window frame))
    (setq eziam-line--selected-window nil)

    (dolist (f (frame-list))
      (when (frame-focus-state f)
        ;; (message "%s = %s" f (frame-selected-window f))
        (setq eziam-line--selected-window (frame-selected-window f))))

    (mapc (lambda (win)
            (set-window-parameter win 'eziam-line--window-at-bottom-left (eziam-line--window-at-bottom-left-p win))
            (set-window-parameter win 'eziam-line--window-at-bottom-right (eziam-line--window-at-bottom-right-p win)))
          (window-list frame nil))

    (force-mode-line-update t)))

;;;; Conclusion

;; Make themes discoverable
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'eziam-line)

;;; eziam-line.el ends here.
