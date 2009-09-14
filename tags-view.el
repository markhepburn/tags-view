;;; tags-view.el --- Display and navigate tags browsing history.

;; Copyright (C) 2009  Mark Hepburn

;; Author: Mark Hepburn <Mark.Hepburn@gmail.com>
;; Keywords: extensions, tools, convenience, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Functionality to supplement the different tags operations, usually
;;; bound to M-./M-* Currently supports etags.el and gtags.el; any
;;; others?

;;; As you navigate through a source tree it can become easy to forget
;;; how you got to your current location -- you could call M-*
;;; repeatedly to pop back up the stack, but this would lose your
;;; current location.  This module allows you to view the path taken,
;;; and if desired to jump immediately back to any intermediate
;;; location.

;;; I don't think this exists, but I haven't looked too hard -- I'd
;;; like the practice of writing something to completion for emacs for
;;; once!

;;; TODO:
;;; * support for gtags as well as etags
;;; * manipulation of the tags list (deletion, etc)
;;; * autodetection of etags/gtags etc

;;; Code:

;;; etags.el.  Locations are in global variable `tags-location-ring',
;;; a ring data structure.

;;; gtags.el.  Locations are in two global variables, one for the
;;; buffer and one for the point: `gtags-buffer-stack' and
;;; `gtags-point-stack'.  These are just lists, semantically treated
;;; as stacks.  This will require some adjustment; easiest is probably
;;; to create a marker for each location and return that as a list,
;;; using save-buffer -> save-excursion -> goto-char -> point-marker
;;; to create each marker.

(require 'cl)

(defvar tv-separator-string "----"
  "Text used to separate entries in the browser window.  May be nil.")

(defvar tv-context-lines 0
  "The number of preceding and following lines to include around
  each location displayed.")

(defface tv-header-face
  '((t (:foreground "gray" :weight 'light)))
  "Face used to display the header of each tag entry.")

(defvar tv-determine-backend-function 'tv-determine-backend-directory-search
  "Value should be a function of no arguments that returns a
  symbol indicating which backend should be used, or 'none if not
  applicable.")

(defun tv-determine-backend ()
  "Returns a symbol indicating which backend should be used (eg,
  'etags, 'gtags, etc)."
  (funcall tv-determine-backend-function))

(defun tv-determine-backend-directory-search ()
  "Determine which of gtags, etags, etc we should be assuming by
recursively searching parent directories looking for TAGS, GTAGS,
etc."
  ;; Try just looking through parent directories for tell-tale files:
  (let ((working-dir (or (and (buffer-file-name)
                              (file-name-directory (buffer-file-name)))
                         (pwd))))
    (labels
        ((rec (dir)
              (cond
               ((file-exists-p (concat dir "GTAGS"))
                (throw 'exit 'gtags))
               ((file-exists-p (concat dir "TAGS"))
                (throw 'exit 'etags))
               ;; if we've reached the end of the road, we're done:
               ((string= dir (file-name-directory (directory-file-name dir)))
                (throw 'exit 'none))
               ;; else, keep recursing:
               (t (rec (file-name-directory (directory-file-name dir)))))))
      (catch 'exit (rec working-dir)))))

(defun tv-get-tags-marker-list ()
  (let* ((backend (tv-determine-backend))
         (backend-fn (intern (concat "tv-get-tags-marker-list-for-"
                                     (symbol-name backend)))))
    (if (or (eq backend 'none) (not (fboundp backend-fn)))
        (error "Can't find a usable backend")
      (funcall backend-fn))))
(defun tv-get-tags-marker-list-for-etags ()
  (copy-list (ring-elements tags-location-ring)))
(defun tv-get-tags-marker-list-for-gtags ()
  (let ((points-and-buffers (map 'list 'cons gtags-point-stack gtags-buffer-stack))
        (gtags-markers nil))
    (dolist (pb points-and-buffers gtags-markers)
      (with-current-buffer (cdr pb)
        (save-excursion
          (goto-char (car pb))
          (setq gtags-markers (cons (point-marker) gtags-markers)))))))

(defun tv-view-history ()
  "The main entry point; pops open a buffer with the list of
locations on the tag stack that can then optionally be operated
on (eg, jumping to that location, deleting it from the list,
etc).  The following options will be available:

\\{tags-history-mode-map}"
  (interactive)
  (let ((buf (get-buffer-create "*tags history*")))
    (pop-to-buffer buf)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (tags-history-mode)
    (let ((tag-items (tv-get-tags-marker-list)))
      (tv-insert-items tag-items))
    (setq buffer-read-only t)
    (goto-char 0)))

(defun tv-what-line (marker)
  "Return the line number of a marker"
  (save-current-buffer
    (set-buffer (marker-buffer marker))
    (line-number-at-pos (marker-position marker))))

(defun tv-insert-items (items &optional count)
  "Insert the formatted list of tags with context"
  (unless count (setq count 0))
  (if items
      (progn
        (tv-insert-single-item (car items) count)
        (if (cdr items)
            (progn
              (if tv-separator-string
                  (insert tv-separator-string "\n"))
              (tv-insert-items (cdr items) (1+ count)))))))

(defun tv-insert-single-item (marker posn)
  "Insert a single formatted item, including overlays etc.
Argument is a marker that will be displayed, along with
`tv-context-lines' of context, if non-zero."
  (let ((beg (point)))
    (insert (propertize (format "Buffer %s, line %d:\n"
                                (buffer-name (marker-buffer marker))
                                (tv-what-line marker))
                        'face 'tv-header-face))
    (insert (tv-get-lines-with-context marker tv-context-lines) "\n")
    (let ((o (make-overlay beg (point))))
      (overlay-put o 'mouse-face 'highlight)
      (overlay-put o 'tv-ring-posn posn)
      (overlay-put o 'tv-marker marker))))

(defun tv-get-lines-with-context (marker &optional num-context)
  "Grabs the line at the specified marker; if optional
  num-context is specified, it will also grab that number of
  preceding and following lines, assuming sufficient lines exist.
  For example, if 2 context lines are specified, a total of 5
  lines wil lbe returned: 2 preceding, the line the marker is
  located on, and 2 following lines.  If not enough context lines
  exist in either direction, as many as possible will be used."
  (unless num-context (setq num-context 0))
  (if (< num-context 0) (setq (num-context (- num-context))))
  (save-current-buffer
    (set-buffer (marker-buffer marker))
    (let (start end)
      (goto-char (marker-position marker))
      (forward-line (- num-context))
      (setq start (point))
      (goto-char (marker-position marker))
      (forward-line num-context)
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))))

;;; to implement; different methods of operating on the current selection:
(defun tv-display-tag-other-window ())
(defun tv-jump-to-tag-and-quit (location)
  (interactive "d")
  (let* ((o (or (car-safe (overlays-at location))
                (car (overlays-at (next-overlay-change location)))))
         (marker (overlay-get o 'tv-marker))
         (buf    (marker-buffer marker))
         (posn   (marker-position marker)))
    (switch-to-buffer buf)
    (goto-char posn)
    (delete-other-windows)))
(defun tv-clear-tag-at-point ())

;;; Navigation:
(defun tv-next-tag (&optional arg)
  "Move point forward to the next tag.  Optional numeric argument
  moves forward that many tags."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (if (> arg 0)
        (progn
          (decf arg)
          (if (overlays-at (point))
              (progn
                (goto-char (overlay-end (car (overlays-at (point)))))
                (goto-char (next-overlay-change (point))))
            (goto-char (next-overlay-change (point)))
            (unless (eobp)
              (goto-char (overlay-start (car (overlays-at (point))))))))
      (progn
        (incf arg)
        (if (overlays-at (point))
            (progn
              (goto-char (overlay-start (car (overlays-at (point)))))
              (goto-char (previous-overlay-change (point)))
              (goto-char (previous-overlay-change (point))))
          (progn
            (goto-char (previous-overlay-change (point)))
            (unless (bobp)
              (goto-char (overlay-start (car (overlays-at (point))))))))))))

(defun tv-previous-tag (&optional arg)
  "Move point backwards to the previous tag.  Optional numeric
  argument moves backwards that many tags."
  (interactive "p")
  (tv-next-tag (- arg)))

;;; major mode for displaying the history:
(define-derived-mode tags-history-mode
  nil "Tags-History"
  "View history of tags locations, with the most recent on the top.

\\{tags-history-mode-map}"
  (let ((km tags-history-mode-map))
    ;; first, clear all other bindings:
    (suppress-keymap km)

    ;; navigation:
    (define-key km "n"    'tv-next-tag)
    (define-key km "\C-n" 'tv-next-tag)
    (define-key km "j"    'tv-next-tag)
    
    (define-key km "p"    'tv-previous-tag)
    (define-key km "\C-p" 'tv-previous-tag)
    (define-key km "k"    'tv-previous-tag)

    ;; operation:
    (define-key km "\C-m"  'tv-jump-to-tag-and-quit)

    ;; cleanup:
    (define-key km "q"    'delete-window)))

(provide 'tags-view)