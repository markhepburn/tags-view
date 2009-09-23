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
;;; location, delete any extranous locations, etc.

;;; I don't think this exists, but I haven't looked too hard -- I'd
;;; like the practice of writing something to completion for emacs for
;;; once, before realising someone else has already provided it!
;;; Update: ok, I went looking, and of course it exists:
;;; http://www.emacswiki.org/emacs/EtagsStack
;;; I'd like to think this offers a little bit different; it supports
;;; gtags as well out of the box and in theory at least can be
;;; extended to others, and it offers a few more operations on the
;;; chronological trace.  Of course, if you need any of that is up to
;;; you :)

;;; Bugs: Most probably.  The gtags backend in particular really
;;; hasn't copped much testing yet.

;;; Code:


(require 'cl)

(defvar tv-separator-string "----"
  "Text used to separate entries in the browser window.  May be nil.")

(defvar tv-context-lines 0
  "The number of preceding and following lines to include around
  each location displayed.")

(defface tv-header-face
  '((t (:foreground "gray" :weight light)))
  "Face used to display the header of each tag entry.")

(defvar tv-determine-backend-function 'tv-determine-backend-directory-search
  "Value should be a function of no arguments that returns a
  symbol indicating which backend should be used, or 'none if not
  applicable.")

(defvar tv-backend-list
  '((etags
     (get-tags-list . tv-get-tags-list-for-etags)
     (clear-tag . tv-delete-tag-for-etags))
    (gtags
     (get-tags-list . tv-get-tags-list-for-gtags)
     (clear-tag . tv-delete-tag-for-gtags)))
  "Assoc list keyed by the symbol returned by
  `tv-determine-backend', whose values are also assoc lists
  mapping the functionality keys to functions implementing that
  functionality for that backend.")

;;; Use a datastructure containing point and buffer instead of
;;; markers, for backends such as gtags that don't use markers:
(defun tv--make-pb (point buffer) (cons point buffer))
(defun tv--pb-from-marker (marker)
  (tv--make-pb (marker-position marker) (marker-buffer marker)))
(defun tv--pb-point  (pb) (car pb))
(defun tv--pb-buffer (pb) (cdr pb))

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

(defun tv--call-fn-for-backend (fn-sym backend &rest args)
  (condition-case nil
      (let* ((backend-list (cdr (assoc backend tv-backend-list)))
             (impl         (cdr (assoc fn-sym backend-list))))
        (apply impl args))
      (error
       (error "Couldn't find implementation of %s for backend %s" fn-sym backend))))

(defun tv-get-tags-list (backend)
  (tv--call-fn-for-backend 'get-tags-list backend))
(defun tv-get-tags-list-for-etags ()
  (mapcar 'tv--pb-from-marker (ring-elements tags-location-ring)))
(defun tv-get-tags-list-for-gtags ()
  (map 'list 'tv--make-pb gtags-point-stack gtags-buffer-stack))

(defun tv-view-history ()
  "The main entry point; pops open a buffer with the list of
locations on the tag stack that can then optionally be operated
on (eg, jumping to that location, deleting it from the list,
etc).  The following options will be available:

\\{tags-history-mode-map}"
  (interactive)
  (let* ((buf (get-buffer-create "*tags history*"))
         (backend (tv-determine-backend))
         (tag-items (tv-get-tags-list backend)))
    (pop-to-buffer buf)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (tags-history-mode)
    (set (make-local-variable 'tv-tags-backend) backend)
    (tv-insert-items tag-items)
    (setq buffer-read-only t)
    (goto-char 0)))

(defun tv-what-line (pb)
  "Return the line number of a point-buffer structure."
  (with-current-buffer (tv--pb-buffer pb)
    (line-number-at-pos (tv--pb-point pb))))

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

(defun tv-insert-single-item (pb posn)
  "Insert a single formatted item, including overlays etc.
Argument is a marker that will be displayed, along with
`tv-context-lines' of context, if non-zero."
  (let ((beg (point)))
    (insert (propertize (format "Buffer %s, line %d:\n"
                                (buffer-name (tv--pb-buffer pb))
                                (tv-what-line pb))
                        'face 'tv-header-face))
    (insert (tv-get-lines-with-context pb tv-context-lines) "\n")
    (let ((o (make-overlay beg (point))))
      (overlay-put o 'mouse-face 'highlight)
      (overlay-put o 'tv-stack-posn posn)
      (overlay-put o 'tv-buffer (tv--pb-buffer pb))
      (overlay-put o 'tv-point (tv--pb-point pb)))))

(defun tv-get-lines-with-context (pb &optional num-context)
  "Grabs the line at the specified point-and-buffer; if optional
  num-context is specified, it will also grab that number of
  preceding and following lines, assuming sufficient lines exist.
  For example, if 2 context lines are specified, a total of 5
  lines wil lbe returned: 2 preceding, the line the marker is
  located on, and 2 following lines.  If not enough context lines
  exist in either direction, as many as possible will be used."
  (unless num-context (setq num-context 0))
  (if (< num-context 0) (setq (num-context (- num-context))))
  (with-current-buffer (tv--pb-buffer pb)
    (save-excursion
      (let (start end)
        (goto-char (tv--pb-point pb))
        (forward-line (- num-context))
        (setq start (point))
        (goto-char (tv--pb-point pb))
        (forward-line num-context)
        (end-of-line)
        (setq end (point))
        (buffer-substring start end)))))

;;; to implement; different methods of operating on the current selection:

(defmacro with-tag-info (locn args &rest body)
  "Macro to facilitate writing tag-stack operations.  First
  argument is the location (point) in the buffer, the second is
  an \"argument list\" of buffer, position, and stack position,
  all taken from the tag under point, and the remainder is the
  body.  The arg-list args will be bound within the body to the
  values corresponding to the tag under point."
  (declare (indent 2))
  (let ((o (gensym "tv-overlay-")))
    `(let* ((,o (or (car-safe (overlays-at ,locn))
                    (car (overlays-at (next-overlay-change ,locn)))))
            (,(car   args) (overlay-get ,o 'tv-buffer))
            (,(cadr  args) (overlay-get ,o 'tv-point))
            (,(caddr args) (overlay-get ,o 'tv-stack-posn)))
       ,@body)))

(defun tv-display-tag-other-window (location)
  (interactive "d")
  (with-tag-info location (buf posn stack)
    (let ((tags-win (selected-window)))
      (with-current-buffer (pop-to-buffer buf)
        (goto-char posn)
        (recenter))
      (select-window tags-win))))
(defun tv-jump-to-tag-and-quit (location)
  (interactive "d")
  (with-tag-info location (buf posn stack)
    (switch-to-buffer buf)
    (goto-char posn)
    (delete-other-windows)))
(defun tv-delete-tag-at-point (location)
  (interactive "d")
  (with-tag-info location (buf posn stack-pos)
    (tv--call-fn-for-backend 'clear-tag tv-tags-backend stack-pos)
    ;; redraw; hack here to make sure the same backend is used.  I
    ;; don't like this, and will probably refactor to fix it soon.  It
    ;; smells, to me:
    (let ((tv-determine-backend-function (lambda () tv-tags-backend)))
      (tv-view-history))))
;;; implementations:
(defun tv-delete-tag-for-etags (stack-position)
  (ring-remove tags-location-ring stack-position))
(defun tv-delete-tag-for-gtags (stack-position)
  (macrolet
      ((delete-nth (n lst)
                   `(if (zerop ,n)
                        (setq ,lst (cdr ,lst))
                      (setcdr (nthcdr (1- ,n) ,lst) (nthcdr (1+ ,n) ,lst)))))
    (delete-nth stack-position gtags-point-stack)
    (delete-nth stack-position gtags-buffer-stack)))

;;; Navigation (mostly borrowed from browse-kill-ring):
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
    (define-key km "\C-m" 'tv-jump-to-tag-and-quit)
    (define-key km "o"    'tv-display-tag-other-window)
    (define-key km "d"    'tv-delete-tag-at-point)

    ;; cleanup:
    (define-key km "q"    'delete-window)))

(provide 'tags-view)