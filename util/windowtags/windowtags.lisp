;; Copyright 2009 Michael Raskin
;;
;; Maintainer: Michael Raskin
;;
;; This file is part of StumpWM.
;;

(in-package #:windowtags)

;;; String parsing for commands

;; TODO: Rename split-tags
(defun string-split-by-spaces (string)
  (split-sequence #\Space string :remove-empty-subseqs t))

;;; Basic operations

(defcommand window-tags (&optional (argwin nil)) ()
  "Show window tags"
  (let* ((win (or argwin (current-window)))
         (tags (xlib:get-property (window-xwin win) :STUMPWM_TAGS))
         (tagstring (octets-to-string tags))
         (taglist (when tags
                    (string-split-by-spaces #\Space tagstring))))
    (if argwin
        taglist
        (message "Tags: ~{~%~A~}" taglist))))

(defun (setf window-tags) (newtags &optional (argwin nil))
  "Set the window tag set for a window"
  (let*
      ((win (or argwin (current-window)))
       (tagstring (format nil "~{~a ~}" (mapcar 'string-upcase newtags))))
    (xlib:change-property (window-xwin win)
                          :STUMPWM_TAGS
                          (string-to-octets tagstring)
                          :UTF8_STRING 8)))

(defun clear-tags-if (clearp &optional (argwin nil))
  "Remove tags matched by predicate"
  (let*
      ((win (or argwin (current-window)))
       (new-tags (remove-if clearp (window-tags win))))
    (setf (window-tags win) new-tags)))

;;; Commands for basic operations

(defcommand clear-tags (&optional (argtags nil) (argwin nil)) (:rest :rest)
  "Remove specified or all tags."
  (let*
      ((tags (string-split-by-spaces argtags))
       (condition (if tags (lambda(x) (find x tags :test 'equalp)) (lambda (x) t)))) 
    (clear-tags-if condition argwin)))

;; TODO: Should iterate through all the screens
(defcommand clear-all-tags () ()
  "Remove all tags and start afresh."
  (mapcar (lambda (x)
            (clear-tags nil x))
          (screen-windows (current-screen))))

(defcommand tag-window (argtag &optional (window (current-window))) ((:rest "Tag to set: ") :rest)
  "Add a tag to ARGWIN."
  (let ((tags (string-split-by-spaces argtag)))
    (setf (window-tags window)
          (union tags
                 (window-tags win)
                 :test 'equalp))))

;; TODO: Rename this to SHOW-WINDOWS-WITH-TAGS. Make a similar function that
;; lists all tags.
(defcommand all-tags () ()
  "List all windows with their tags"
  (let ((*suppress-echo-timeout* t))
    (message 
     "Window list: ~{~%~{[ ~A ] ( ~A | ~A | ~A ) ~% ->~{~A, ~}~}~}"
     (mapcar
      (lambda(x)
        (list
         (window-title x)
         (window-class x)
         (window-res x)
         (window-role x)
         (window-tags x)))
      (screen-windows (current-screen))))))

;;; Selection of tags and windows by tags

;; Q: Why Tag T is assigned implicitly?
(defun tags-from (argtags &optional (window (current-window)))
  "Check whether (current) window has one of the specified tags.
  Tag T is implicitly assigned to all windows."
  (let* ((tags (string-split-by-spaces argtags))
         (wtags (union (list "T") (window-tags window) :test 'equalp)))
    (intersection tags wtags :test 'equalp)))

(defun select-by-tags (argtags &optional (without nil))
  "Select windows with (without) one of the specified tags 
  (any of the specified tags) from current screen. Tag T
  is implicitly assigned to every window"
  (let* ((tags (string-split-by-spaces argtags))
         (condition (lambda (w) (tags-from tags w)))
         (windows (screen-windows (current-screen)))
         (fn (if without #'remove-if #'remove-if-not)))
    (funcall fn condition windows)))

;;; Window manipulations using tags

;;; And convenient instances

(defcommand pull-tag (argtag) ((:rest "Tag(s) to pull: "))
  "Pull all windows with the tag (any of the tags) to current group"
  (move-windows-to-group (select-by-tags (string-split-by-spaces argtag))))

(defcommand push-without-tag (argtag) ((:rest "Tag(s) needed to stay in the group: "))
  "Push windows not having the tag (any of the tags) to .tag-store"
  (move-windows-to-group (select-by-tags (string-split-by-spaces argtag) T) ".tag-store"))

(defcommand push-tag (argtag) ((:rest "Tag(s) to push: "))
  "Push windows having the tag (any of the tags) to .tag-store"
  (move-windows-to-group (select-by-tags (string-split-by-spaces argtag)) ".tag-store"))

(defcommand pull+push (argtag) ((:rest "Tag(s) to select: "))
  "Pull all windows with the tag, push all without"
  (pull-tag argtag)
  (push-without-tag argtag))

(defcommand push-window () ()
  "Push window to tag store"
  (move-windows-to-group (list (current-window)) ".tag-store"))

;;; Manage window numbers by tags..

(defun window-number-from-tag (window)
  "Find a numeric tag, if any, and parse it"
  (let* ((tags (window-tags window))
         (numtag (find-if (lambda (x) (cl-ppcre:scan "^[0-9]+$" x)) tags)))
    (and numtag (parse-integer numtag))))

(defcommand number-by-tags () ()
  "Every window tagged <number> will have a chance to have that number. The
remaining windows will have packed numbers"
  ;; First, assign impossible numbers.
  (mapcar
   (lambda(x)
     (setf (window-number x) -1))
   (group-windows (current-group)))
  ;; Now try to assign numbers to windows holding corresponding tags.
  (mapcar
   (lambda (x) 
     (let* 
         ((num (window-number-from-tag x))
          (occupied (mapcar 'window-number (group-windows (current-group)))))
       (if (and num (not (find num occupied)))
           (setf (window-number x) num))))
   (group-windows (current-group)))
  ;; Give up and give smallest numbers possible
  (repack-window-numbers 
   (mapcar 'window-number
           (remove-if-not 
            (lambda(x) (equalp (window-number x) (window-number-from-tag x)))
            (group-windows (current-group))))))

(defcommand tag-visible (&optional (argtags nil)) (:rest)
  "IN-CURRENT-GROUP or another specified tag will be assigned to all windows 
	    in current group and only to them"
  (let* ((tags (if (or (equalp argtags "") (not argtags))
                   "IN-CURRENT-GROUP"
                   argtags)))
    (mapcar (lambda (x) (clear-tags tags x)) (screen-windows (current-screen)))
    (mapcar (lambda (x) (tag-window tags x)) (group-windows (current-group)))))

(defcommand raise-tag (tag) ((:rest "Tag to pull: "))
  "Make window current by tag"
  (when-let ((window (car (select-by-tags tag))))
    (move-windows-to-group (list window))
    (really-raise-window window)
    window))

(defcommand search-tag (tag-regex) ((:rest "Tag regex to select: "))
  (only)
  (fclear)
  (let* ((current (current-group (current-screen)))
         (tag-store (find-group (current-screen) ".tag-store")))
    (loop
      :for w :in (screen-windows (current-screen))
      :do (if (find-if (lambda (s)
                         (cl-ppcre:scan (concatenate 'string "(?i)" tag-regex) s))
                       (window-tags w))
           (move-window-to-group w current)
           (move-window-to-group w tag-store)))))

(defcommand search-tag-pull (tag-regex) ((:rest "Tag regex to pull: "))
  (only)
  (fclear)
  (let* ((current (current-group (current-screen)))
         (tag-store (find-group (current-screen) ".tag-store")))
    (loop
      :for w :in (screen-windows (current-screen))
      :when (find-if (lambda (tag)
                       (cl-ppcre:scan (concatenate 'string "(?i)" tag-regex) tag))
                     (window-tags w))
        :do (move-window-to-group w current))))

(defcommand select-by-title-regexp (regex) ((:rest "Title regex to select: "))
  (only)
  (fclear)
  (let* ((current (current-group (current-screen)))
         (tag-store (find-group (current-screen) ".tag-store")))
    (loop
      :for w :in (screen-windows (current-screen))
      :do (if (cl-ppcre:scan regex (window-title w))
              (move-window-to-group w current)
              (move-window-to-group w tag-store)))))

(defcommand pull-by-title-regexp (regex) ((:rest "Title regex to select: "))
  (only)
  (fclear)
  (let* ((current (current-group (current-screen)))
         (tag-store (find-group (current-screen) ".tag-store")))
    (loop
      :for w :in (screen-windows (current-screen))
      :when (cl-ppcre:scan regex (window-title w))
        :do (move-window-to-group w current))))
