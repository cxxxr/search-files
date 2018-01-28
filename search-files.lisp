(in-package :search-files)

(defstruct search-result
  pathname
  base-directory
  position
  line
  character
  string)

(defun print-search-result (search-result)
  (format nil "~A:~D:~D: ~A"
          (enough-namestring (search-result-pathname search-result)
                             (search-result-base-directory search-result))
          (search-result-line search-result)
          (search-result-character search-result)
          (search-result-string search-result)))

(defun position-to-line-column (text position)
  (with-input-from-string (in text)
    (loop :with offset := position
          :for line :from 1
          :for str := (read-line in nil)
          :while str
          :do (let ((len (1+ (length str))))
                (if (> 0 (- offset len))
                    (return (values line offset str))
                    (decf offset len))))))

(defun search-from-file (pathname function base-directory)
  (let* ((text (uiop:read-file-string pathname))
         (start 0)
         (search-results '()))
    (loop
     (multiple-value-bind (position next-position)
         (funcall function text start)
       (unless position
         (return (nreverse search-results)))
       (push (multiple-value-bind (line column string)
                 (position-to-line-column text position)
               (make-search-result :pathname pathname
                                   :base-directory base-directory
                                   :line line
                                   :character column
                                   :position position
                                   :string string))
             search-results)
       (setf start
             (if (< position next-position)
                 next-position
                 (1+ position)))))))

(defun walk-directory (directory function)
  (dolist (file (uiop:directory-files directory))
    (funcall function file))
  (dolist (subdir (uiop:subdirectories directory))
    (walk-directory subdir function)))

(defgeneric search-file (searcher pathname))

(defclass abstract-searcher ()
  ((pattern :initarg :pattern :reader pattern-of)
   (case-insensitive :initarg :case-insensitive :reader case-insensitive-of)
   (directory :initarg :directory :reader directory-of)
   (types :initarg :types :reader types-of)))

(defclass string-searcher (abstract-searcher)
  ())

(defclass regex-searcher (abstract-searcher)
  ((regex :accessor searcher-regex)))

(defclass form-searcher (abstract-searcher)
  ((form :accessor searcher-form)))

(defun get-searcher-class (name)
  (ecase name
    (:string 'string-searcher)
    (:regex 'regex-searcher)
    (:form 'form-searcher)))

(defmethod initialize-instance :after ((searcher regex-searcher) &rest initargs)
  (setf (searcher-regex searcher)
        (ppcre:create-scanner (pattern-of searcher)
                              :case-insensitive-mode (case-insensitive-of searcher))))

(defmethod initialize-instance :after ((searcher form-searcher) &rest initargs)
  (setf (searcher-form searcher)
        (read-from-string (pattern-of searcher))))

(defmethod search-file ((searcher string-searcher) pathname)
  (let ((pattern (pattern-of searcher))
        (test (if (case-insensitive-of searcher)
                  #'char-equal
                  #'char=)))
    (search-from-file pathname
                      (lambda (text start)
                        (let ((position (search pattern text
                                                :start2 start
                                                :test test)))
                          (when position
                            (values position (+ position (length pattern))))))
                      (directory-of searcher))))

(defmethod search-file ((searcher regex-searcher) pathname)
  (search-from-file pathname
                    (lambda (text start)
                      (multiple-value-bind (start end)
                          (ppcre:scan (searcher-regex searcher) text :start start)
                        (values start end)))
                    (directory-of searcher)))

(defun match (pattern form)
  (cond ((and (consp pattern) (consp form))
         (and (match (car pattern)
                     (car form))
              (match (cdr pattern)
                     (cdr form))))
        ((eq pattern :*))
        (t
         (eql pattern form))))

(defun find-match (pattern tree fn)
  (labels ((f (tree path ncdr)
             (cond ((match pattern tree)
                    (funcall fn (reverse path)))
                   ((consp tree)
                    (f (car tree) (cons ncdr path) 0)
                    (f (cdr tree) path (1+ ncdr))))))
    (f tree nil 0)))

(defun traverse-tree (point position path)
  (setf (editor:point-position point) position)
  (dolist (n path)
    (editor:skip-whitespace point)
    (let ((char (editor:character-at point 0)))
      (cond ((eql char #\()
             (editor:character-offset point 1)
             (editor:form-offset point n))
            ((eql char #\')
             (editor:character-offset point 1))
            ((and (eql char #\#)
                  (eql (editor:character-at point 1) #\'))
             (editor:character-offset point 2)))))
  (editor:skip-whitespace point)
  point)

(defun point-to-line-number (point)
  (editor:with-point ((p point))
    (loop :for i :from 1
          :while (editor:line-offset p -1)
          :finally (return i))))

(defmethod search-file ((searcher form-searcher) pathname)
  (let* ((buffer (editor:make-buffer pathname :temporary t))
         (point (editor:buffer-point buffer))
         (search-results '())
         (package *package*))
    (editor:insert-file-command point pathname buffer)
    (editor:buffer-start point)
    (loop
     (let ((end (editor:form-offset point 1)))
       (unless end (return))
       (let* ((start (editor:form-offset (editor:copy-point end :temporary) -1))
              (position (editor:point-position start))
              (form (let ((*package* package))
                      (read-from-string
                       (editor:points-to-string start end)))))
         (cond ((and (consp form) (eq 'in-package (first form)))
                (setf package
                      (or (find-package (second form))
                          (find-package :cl-user))))
               (t
                (find-match (searcher-form searcher) form
                            (lambda (path)
                              (editor:with-point ((point point))
                                (let* ((point (traverse-tree point position path))
                                       (search-result
                                        (make-search-result
                                         :pathname pathname
                                         :base-directory (directory-of searcher)
                                         :line (point-to-line-number point)
                                         :character (editor:point-column point)
                                         :position (editor:point-position point)
                                         :string (editor:line-string point))))
                                  (push search-result search-results))))))))
       (editor:move-point point end)))
    (nreverse search-results)))

(defun search-directory (searcher)
  (let ((acc '()))
    (walk-directory (directory-of searcher)
                    (lambda (file)
                      (when *interrupted-flag*
                        (return-from search-directory acc))
                      (when (or (null (types-of searcher))
                                (member (pathname-type file)
                                        (types-of searcher)
                                        :test #'string=))
                        (setf acc (nconc (search-file searcher file) acc)))))
    acc))
