;; Gitgraph is a tool to display and edit graphical tree of the commit history.
;;
;; Copyright (C) 2020 by Jonggyu Lee (jglee1027@gmail.com)
;;
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:gitgraph)

(defparameter *default-color* +gray+)
(defparameter *default-new-color* +yellow+)
(defparameter *branch-x* 100)
(defparameter *branch-y* 50)
(defparameter *branch-name-height* 30)
(defparameter *branch-margin* 100)
(defparameter *branch-name-pad-width* 30)
(defparameter *commit-merge-offset-y* 35)
(defparameter *commit-margin* 50)
(defparameter *commit-new-id* 0)
(defparameter *commit-radius* 15)
(defparameter *repo*
  '((:branch-name feature/a :root-commit d3 :color +magenta+
     :commits ((d3 a1) a2 a3 a4))
    (:branch-name feature/b :root-commit d3 :color +magenta+
     :commits (b1 b2 b3 (d7 b4) b5 b6))
    (:branch-name develop :root-commit m1 :color +light-yellow+
     :commits (d1 d2 d3 d4 (h1 d5) (b3 d6) (r2 d7) (r4 d8) (b6 d9) (a4 d10) (r5 d11)))
    (:branch-name release/r :color +light-green+
     :commits ((d6 r1) r2 r3 r4 (d10 r5)))
    (:branch-name hotfix/h :root-commit m1 :color +indian-red+
     :commits (h1))
    (:branch-name master :color +light-blue+
     :commits (m1 (h1 m2) (r4 m3) (r5 m4)))))

(defclass branch ()
  ((name :accessor name :initarg :name)
   (root-commit :accessor root-commit :initarg :root-commit :initform nil)
   (commits :accessor commits :initarg :commits)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (color :accessor color :initarg :color))
  (:default-initargs :name "noname" :x 0 :y 0 :color *default-color*))

(defmethod initialize-instance :after ((branch branch) &key)
  (if (null (color branch))
      (setf (color branch) *default-color*)))

(defmethod print-object ((obj branch) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name x y commits) obj
      (format stream "~a ~a ~a ~a" name x y commits))))

(defclass commit ()
  ((commit :accessor commit :initarg :commit)
   (merge-commit :accessor merge-commit :initarg :merge-commit :initform nil)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (color :accessor color :initarg :color))
  (:default-initargs :commit "?" :x 0 :y 0 :color +light-gray+))

(defmethod initialize-instance :after ((commit commit) &key)
  (if (null (color commit))
      (setf (color commit) *default-color*)))

(defmethod print-object ((obj commit) stream)
  (with-slots (commit y) obj
    (format stream "(~a ~a)" commit y)))

(defgeneric draw (sheet obj))
(defgeneric copy (obj))

(defmethod draw (sheet (obj branch))
  (with-slots (name root-commit commits x y color) obj
    (with-output-as-presentation (sheet obj 'branch)
      (let* ((text-width (text-width sheet name))
             (x1 (- x (/ (+ text-width *branch-name-pad-width*) 2)))
             (y1 (- y (/ *branch-name-height* 2)))
             (x2 (+ x (/ (+ text-width *branch-name-pad-width*) 2)))
             (y2 (+ y (/ *branch-name-height* 2))))
        (draw-rectangle* sheet x1 y1 x2 y2 :ink color)))
    (draw-text* sheet name x y :align-x :center :align-y :center)
    (loop for commit in commits
          for i from 1
          for y = y
          do (draw sheet commit)
          when (and (= i 1) root-commit) do
            (draw-commit-arrow sheet root-commit commit)
          when (> i 1) do
            (draw-arrow* sheet x y x (- (y commit) *commit-radius*))
          when (merge-commit commit) do
            (draw-commit-arrow sheet (merge-commit commit) commit)
          do (setf y (+ (y commit) *commit-radius*))
          finally (draw-arrow* sheet x y x (+ y (- *commit-margin* (* *commit-radius* 2)))))))

(defmethod draw (sheet (obj commit))
  (with-slots (commit x y color) obj
    (draw-circle* sheet x y (1+ *commit-radius*)
                  :ink +gray50+
                  :filled nil
                  :line-thickness 2)
    (with-output-as-presentation (sheet obj 'commit)
      (draw-circle* sheet x y *commit-radius*
                    :ink color))
    (draw-text* sheet commit x y
                :align-x :center
                :align-y :center)))

(defmethod copy ((obj branch))
  (with-slots (name root-commit commits x y color) obj
    (let ((new (make-instance 'branch
                              :name name
                              :root-commit root-commit
                              :commits (mapcar #'copy commits)
                              :x x
                              :y y
                              :color color)))
      new)))

(defmethod copy ((obj commit))
  (with-slots (commit merge-commit x y color) obj
    (let ((new (make-instance 'commit
                              :commit commit
                              :merge-commit merge-commit
                              :x x
                              :y y
                              :color color)))
      new)))

(define-presentation-method present (commit (type commit) stream
                                            (view textual-view) &key)
  (write-string (commit commit) stream))

(define-presentation-method present (branch (type branch) stream
                                            (view textual-view) &key)
  (write-string (name branch) stream))

(defclass input-view (textual-view) ())

(defclass input-pointer-documentation-view
    (input-view pointer-documentation-view)
  ())

(defparameter +input-view+ (make-instance 'input-view))
(defparameter +input-pointer-documentation-view+
  (make-instance 'input-pointer-documentation-view))

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view input-view)
                     &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
                              :acceptably acceptably :for-context-type for-context-type))

(define-presentation-method accept :around
  ((type sequence) stream (view input-view) &key default default-type)
  (declare (ignorable default default-type))
  (let* ((token (read-token stream))
         (result (handler-case (read-from-string token)
                   (error (c)
                     (declare (ignore c))
                     (simple-parse-error
                      "Error parsing ~S for presentation type ~S"
                      token type)))))
    (if (presentation-typep result type)
        (values result type)
        (input-not-of-required-type result type))))

(define-application-frame gitgraph ()
  ((branches :accessor branches :initform (build *repo*))
   (current-color :accessor current-color :initform +green+)
   (scale :accessor scale :initform 1.0)
   (undo-list :accessor undo-list :initform nil)
   (redo-list :accessor redo-list :initform nil))
  (:panes
   (black-button     (make-colored-button +gray50+))
   (blue-button      (make-colored-button +dodger-blue+))
   (green-button     (make-colored-button +green+))
   (cyan-button      (make-colored-button +cyan+))
   (red-button       (make-colored-button +red+))
   (magenta-button   (make-colored-button +magenta+))
   (yellow-button    (make-colored-button +yellow+))
   (white-button     (make-colored-button +white+))
   (turquoise-button (make-colored-button +turquoise+))
   (grey-button      (make-colored-button +grey+))
   (brown-button     (make-colored-button +rosy-brown+))
   (orange-button    (make-colored-button +orange+))
   (change-layout-button    (make-change-layout-button +gray+ :width 10))
   (source :text-editor
           :value (repo-to-string *repo*)
           :min-height 800
           :min-width 600
           :scroll-bars t
           :text-style (make-text-style nil nil 18))
   (graph :application
          :min-width 800 :min-height 600
          :scroll-bars t
          :display-function #'display)
   (input :interactor :default-view +input-view+)
   (pointer-doc :pointer-documentation :max-width 200))
  (:layouts
   (default-layout
    (horizontally ()
      (vertically ()
        (horizontally ()
          (tabling (:max-height 10)
            (list black-button blue-button green-button cyan-button)
            (list red-button magenta-button yellow-button white-button)
            (list turquoise-button grey-button brown-button orange-button))
          (18 change-layout-button))
        input
        pointer-doc)
      (with-tab-layout ('tab-page)
        ("graph" graph)
        ("source" source))))
   (zoom-output-layout
    (horizontally ()
      (10 change-layout-button)
      (vertically ()
        (with-tab-layout ('tab-page)
          ("graph" graph)
          ("source" source))
        (70 input)))))
  (:top-level
   (default-frame-top-level :prompt #'gitgraph-prompt)))

(define-presentation-method accept :around
  ((type sequence) stream (view input-view) &key default default-type)
  (declare (ignorable default default-type))
  (let* ((token (read-token stream))
         (result (handler-case (read-from-string token)
                   (error (c)
                     (declare (ignore c))
                     (simple-parse-error
                      "Error parsing ~S for presentation type ~S"
                      token type)))))
    (if (presentation-typep result type)
        (values result type)
        (input-not-of-required-type result type))))

(defgeneric display (frame pane))

(defmethod display ((frame gitgraph) pane)
  (with-drawing-options (pane :transformation (compose-transformation-with-scaling
                                               (sheet-transformation pane)
                                               (scale frame)
                                               (scale frame)))
    (with-slots (branches) frame
      (layout-branches-vertically-beside (update-merge-commits branches) branches)
      (dolist (branch branches)
        (draw pane branch)))))

(defmethod note-tab-page-changed :after ((layout tab-layout-pane) page)
  (format t "~a~%" page))

(defun gitgraph ()
  (run-frame-top-level (make-application-frame 'gitgraph)))

(defun gitgraph-prompt (pane frame)
  (declare (ignore frame))
  (with-text-face (pane :roman)
    (write-string "$ " pane)))

(defun dump (repo)
  (dolist (i repo)
    (format t "~{~a: ~a~%~}~%" i)))

(defun text-width (sheet text)
  (multiple-value-bind (left top right bottom)
      (climb:text-bounding-rectangle* (sheet-medium sheet) text)
    (declare (ignore left top bottom))
    right))

(defun draw-commit-arrow (sheet commit1 commit2)
  (with-accessors ((cx1 x) (cy1 y)) commit1
    (with-accessors ((cx2 x) (cy2 y)) commit2
      (let* ((gradient (abs (/ (- cy2 cy1)
                               (- cx2 cx1))))
             (off-x (sqrt (/ (expt *commit-radius* 2)
                             (+ (expt gradient 2) 1))))
             (off-y (* gradient (sqrt (/ (expt *commit-radius* 2)
                                         (+ (expt gradient 2) 1)))))
             (x1 (if (< cx1 cx2) (+ cx1 off-x) (- cx1 off-x)))
             (x2 (if (< cx1 cx2) (- cx2 off-x) (+ cx2 off-x)))
             (y1 (if (< cy1 cy2) (+ cy1 off-y) (- cy1 off-y)))
             (y2 (if (< cy1 cy2) (- cy2 off-y) (+ cy2 off-y))))
        (draw-arrow* sheet x1 y1 x2 y2)))))

(defun find-commit (commit branches)    ; TODO: (make-hash-table :test #'equal)
  (if (null commit)
      (return-from find-commit nil))
  (if (eq (type-of commit) 'commit)
      (setf commit (commit commit)))
  (dolist (branch branches)
    (dolist (c (commits branch))
      (if (string-equal commit (commit c))
          (return-from find-commit (cons c branch))))))

(defun make-colored-button (color &key width height)
  (make-pane 'push-button
             :label " "
             :activate-callback
             (lambda (gadget)
               (setf (current-color (gadget-client gadget)) color))
             :width width :height height
             :background color :foreground color
             :normal color :pushed-and-highlighted color
             :highlighted color))

(defun make-change-layout-button (color &key width height)
  (make-pane 'push-button
             :label "<"
             :activate-callback
             (lambda (gadget)
               (with-accessors ((layout frame-current-layout)) *application-frame*
                 (setf layout (cond ((eq layout 'default-layout)
                                     (setf (gadget-label gadget) ">")
                                     'zoom-output-layout)
                                    (t
                                     (setf (gadget-label gadget) "<")
                                     'default-layout)))))
             :width width :height height
             :background color :foreground +black+
             :normal color :pushed-and-highlighted color
             :highlighted color))

(defun make-commits (commits &optional (color *default-new-color*))
  (mapcar
   (lambda (c)
     (cond
       ((consp c)
        (make-instance 'commit
                       :commit (symbol-name (cadr c))
                       :merge-commit (symbol-name (car c))
                       :color color))
       (t
        (make-instance 'commit
                       :commit (format nil "~a" c)
                       :color color))))
   commits))

(defun make-branch (branch)
  (destructuring-bind (&key branch-name color root-commit commits) branch
    (make-instance 'branch
                   :name (symbol-name branch-name)
                   :color (symbol-value color)
                   :root-commit root-commit
                   :commits (make-commits commits
                                          (symbol-value color)))))

(defun make-branches (repo)
  (let ((branches nil))
    (dolist (branch repo)
      (setf branches (nconc branches (list (make-branch branch)))))
    branches))

(defun layout-merge-commit-vertically-beside (merge-commit merge-commits branches)
  (labels ((update-y-of-commits-in-branch (merge-commit commit)
             (let ((diff-y (- (y commit) (y merge-commit))))
               (if (= diff-y *commit-margin*)
                   (return-from update-y-of-commits-in-branch))
               (let* ((base-commit (if (<= diff-y 0)
                                       commit
                                       merge-commit))
                      (branch (cdr (find-commit base-commit branches))))
                 (setf diff-y (+ (abs diff-y) (if (eq base-commit commit)
                                                  *commit-margin*
                                                  (- *commit-margin*))))
                 (with-slots (commits) branch
                   (loop for c in commits
                         for mc = (merge-commit c)
                         for c2 = (gethash c merge-commits)
                         with new-y = 0
                         when (> new-y 0) do
                           (setf (y c) (incf new-y *commit-margin*))
                           (if mc
                               (layout-merge-commit-vertically-beside mc merge-commits branches))
                         when (eq c base-commit) do
                           (setf new-y (incf (y c) diff-y))
                           (if (eq c (car commits))
                               (setf (y branch) (- new-y *commit-margin*)))
                         ))))))
    (let ((commits (gethash merge-commit merge-commits)))
      (loop for commit-cons in commits
            for commit = (car commit-cons)
            for commits2 = (gethash commit merge-commits)
            do (update-y-of-commits-in-branch merge-commit commit)
            when commits2 do
              (layout-merge-commit-vertically-beside commit merge-commits branches)))))

(defun layout-branches-vertically-beside (merge-commits branches)
  (dolist (branch branches)
    (with-slots (commits) branch
      (loop for commit in commits
            when (gethash commit merge-commits) do
              (layout-merge-commit-vertically-beside commit merge-commits branches))))
  branches)

(defun layout-branch (branch &optional  x y)
  (if x
      (setf (x branch) x)
      (setf x (x branch)))
  (if y
      (setf (y branch) y)
      (setf y (y branch)))

  (incf y *commit-margin*)
  (dolist (commit (commits branch))
    (setf (x commit) x)
    (setf (y commit) y)
    (incf y *commit-margin*)))

(defun layout-branches-vertically (branch branches)
  (let* ((root (find-commit (root-commit branch) branches))
         (root-branch (cdr root))
         (root-commit (car root)))
    (if root-branch
        (layout-branches-vertically root-branch branches))
    (when root-commit
      (setf (root-commit branch) root-commit)
      (layout-branch branch
                     (x branch)
                     (y root-commit)))))

(defun update-merge-commits (branches)
  (let ((merge-commits (make-hash-table)))
    (dolist (branch branches)
      (loop for commit in (commits branch)
            for merge-commit = (merge-commit commit)
            when merge-commit do
              (setf (merge-commit commit) (car (find-commit merge-commit branches)))
              (setf (gethash (merge-commit commit) merge-commits)
                    (append (gethash (merge-commit commit) merge-commits)
                            (list (cons commit branch))))))
    merge-commits))

(defun layout-branches (branches)
  ;; horizontally
  (let ((x *branch-x*)
        (y *branch-y*))
    (dolist (branch branches)
      (layout-branch branch x y)
      (incf x *branch-margin*)))

  ;; vertically
  (loop for branch in branches do
    (layout-branches-vertically branch branches))

  (layout-branches-vertically-beside (update-merge-commits branches) branches)
  branches)

(defun build (repo)
  (layout-branches (make-branches repo)))

(defun insert-at (item list index)
  (append (subseq list 0 index)
          (list item)
          (nthcdr index list)))

(defun insert-new-commit(commit &key (before t))
  (with-slots (branches undo-list) *application-frame*
    (push (copy-branches branches) undo-list)
    (let ((branch (cdr (find-commit commit branches))))
      (with-slots (commits) branch
        (let ((pos (position commit commits))
              (new-commit (make-instance 'commit :commit (format nil "'~a" (incf *commit-new-id*))
                                                 :color *default-new-color*)))
          (setf commits
                (if before
                    (insert-at new-commit commits pos)
                    (insert-at new-commit commits (1+ pos))))))
      (layout-branch branch))))

(defun delete-commit(commit)
  (with-slots (branches undo-list) *application-frame*
    (push (copy-branches branches) undo-list)
    (let ((branch (cdr (find-commit commit branches))))
      (with-slots (commits) branch
        (setf commits
              (remove commit commits)))
      (layout-branch branch))))

(defun new-branch (name root-commit &key (left t))
  (with-slots (branches undo-list) *application-frame*
    (push (copy-branches branches) undo-list)
    (let* ((commits `(,(format nil "'~a" (incf *commit-new-id*))
                      ,(format nil "'~a" (incf *commit-new-id*))))
           (root-branch (cdr (find-commit root-commit branches)))
           (pos (position root-branch branches))
           (branch (make-instance 'branch :name name
                                          :color *default-new-color*
                                          :root-commit root-commit
                                          :commits (make-commits commits))))
      (setf branches
            (if left
                (insert-at branch branches pos)
                (insert-at branch branches (1+ pos))))
      (layout-branches branches))))

(defun delete-branch (branch)
  (with-slots (branches undo-list) *application-frame*
    (push (copy-branches branches) undo-list)
    (setf branches
          (remove branch branches))
    (layout-branches branches)))

(defun copy-branches (branches)
  (mapcar #'copy branches))

(defun write-repo (repo filepath)
  (with-open-file (out filepath :direction :output
                                :if-exists :supersede)
    (with-standard-io-syntax (print repo out))))

(defun read-repo (filepath)
  (with-open-file (in filepath)
    (with-standard-io-syntax (read in))))

(defun repo-to-string (repo)
  (cl-ppcre:regex-replace-all "gitgraph::|common-lisp-user::"
                              (write-to-string repo :case :downcase
                                                    :pretty t
                                                    :right-margin 500
                                                    :miser-width 500)
                              ""))

(defun set-source (repo)
  (let ((source (find-pane-named *application-frame* 'source)))
    (setf (gadget-value source)
          (repo-to-string repo))))

(defun get-repo-from-source ()
  (let ((source (find-pane-named *application-frame* 'source)))
    (read-from-string (gadget-value source))))

(define-gitgraph-command (com-open :name "Open" :menu t)
    ((repo-path pathname :default (user-homedir-pathname) :insert-default t))
  (when (probe-file repo-path)
    (with-slots (branches) *application-frame*
      (setf *repo* (read-repo repo-path))
      (set-source *repo*)
      (setf branches (build *repo*))
      (layout-branches branches))))

(define-gitgraph-command (com-save :name "Save" :menu t)
    ((repo-path pathname :default (user-homedir-pathname) :insert-default t))
  (write-repo (get-repo-from-source) repo-path))

(define-gitgraph-command (com-reload :name "Reload" :menu t) ()
  (setf (branches *application-frame*) (build (get-repo-from-source)))
  (setf *commit-new-id* 0)
  (setf (undo-list *application-frame*) nil)
  (setf (redo-list *application-frame*) nil))

(define-gitgraph-command (com-pretty :name "Pretty" :menu t) ()
  (with-slots (branches undo-list) *application-frame*
    (push (copy-branches branches) undo-list)
    (layout-branches-vertically-beside (update-merge-commits branches) branches)))

(define-gitgraph-command (com-undo :name "Undo" :menu t :keystroke (:left :meta)) ()
  (with-slots (branches undo-list redo-list) *application-frame*
    (let ((undo-branches (pop undo-list)))
      (when undo-branches
        (push branches redo-list)
        (setf branches undo-branches)
        (layout-branches branches)))))

(define-gitgraph-command (com-redo :name "Redo" :menu t :keystroke (:right :meta)) ()
  (with-slots (branches undo-list redo-list) *application-frame*
    (let ((redo-branches (pop redo-list)))
      (when redo-branches
        (push branches undo-list)
        (setf branches redo-branches)
        (layout-branches branches)))))

(define-gitgraph-command (com-zoom-in :name "Zoom in" :menu t) ()
  (incf (scale *application-frame*) 0.2))

(define-gitgraph-command (com-zoom-out :name "Zoom out" :menu t) ()
  (decf (scale *application-frame*) 0.2))

(define-gitgraph-command (com-quit :name "Quit" :menu t) ()
  (frame-exit *application-frame*))

(define-gitgraph-command (com-update :name nil) ())

(define-gitgraph-command (com-commit-insert :name "commit before")
    ((commit commit :prompt " before which commit? "))
  (insert-new-commit commit :before t))

(define-gitgraph-command (com-commit-append :name "commit after")
    ((commit commit :prompt " after which commit? "))
  (insert-new-commit commit :before nil))

(define-gitgraph-command (com-commit-delete :name "commit delete")
    ((commit commit :prompt " which commit? "))
  (delete-commit commit))

(define-gitgraph-command (com-merge :name "merge")
    ((from-commit commit :prompt "From")
     (to-commit commit :prompt "To"))
  (setf (color to-commit) +green+)
  (setf (merge-commit to-commit) from-commit))

(define-gitgraph-command (com-branch-new-left :name "branch new left")
    ((new-branch-name string :prompt " branch name? ")
     (root-commit commit :prompt " start point(commit)? "))
  (new-branch new-branch-name root-commit :left t))

(define-gitgraph-command (com-branch-new-right :name "branch new right")
    ((new-branch-name string :prompt " branch name? ")
     (root-commit commit :prompt " start point(commit)? "))
  (new-branch new-branch-name root-commit :left nil))

(define-gitgraph-command (com-branch-rm :name "branch rm")
    ((branch branch :prompt " which branch? "))
  (delete-branch branch))

(define-drag-and-drop-translator com-drag-drop-commit
    (commit command commit gitgraph :pointer-documentation "Drop Commit to Merge")
    (object destination-object)
  (cond ((eq object destination-object)
         (with-slots (branches undo-list) *application-frame*
           (push (copy-branches branches) undo-list))
         (setf (color object) (current-color *application-frame*))
         `(com-update))
        (t
         (with-slots (branches undo-list) *application-frame*
           (push (copy-branches branches) undo-list))
         `(com-merge ,object ,destination-object))))
