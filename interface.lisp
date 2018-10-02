(in-package :search-files)

(defun current-directory ()
  (let ((pathname (current-pathname)))
    (make-pathname :directory (pathname-directory pathname)
                   :host (pathname-host pathname))))

(defvar *current-directory* (current-directory))

(capi:define-interface search-files-pane ()
  ()
  (:panes
   (directory-input-pane
    capi:text-input-pane
    :title "Directory"
    :buttons '(:browse-file (:directory t) :ok nil)
    :file-completion t
    :text (namestring *current-directory*)
    :reader directory-input-pane-of)
   (file-type-input-pane
    capi:text-input-pane
    :title "File type"
    :text "lisp,asd"
    :reader file-type-input-pane-of)
   (search-input-pane
    capi:text-input-pane
    :title "Search"
    :reader search-input-pane-of)
   (search-option-pane
    capi:option-pane
    :items '(:string :regex :form)
    :selection 0
    :reader search-option-pane-of
    :print-function #'string-capitalize)
   (case-insensitive-button
    capi:check-button
    :text "Case insensitive"
    :reader case-insensitive-button-of)
   (search-button
    capi:push-button
    :text "Search"
    :callback-type :interface
    :callback 'start-search-files)
   (interrupt-button
    capi:push-button
    :text "Interrupt"
    :callback-type :none
    :callback 'interrupt-search-files)
   (search-result-panel
    capi:list-panel
    :items 'nil
    :print-function 'print-search-result
    :accessor search-result-panel-of
    ::action-callback 'select-search-result))
  (:layouts
   (search-files-layout
    capi:column-layout
    '(directory-input-pane file-type-input-pane search-string-layout case-insensitive-button button-layout search-result-panel)
    :title-position :frame
    :title "Search Specification")
   (search-string-layout
    capi:row-layout
    '(search-input-pane search-option-pane)
    :x-ratios '(10 1))
   (button-layout
    capi:row-layout
    '(search-button interrupt-button)))
  (:default-initargs
   :best-height 500
   :best-width 400
   :layout 'search-files-layout
   :message-area t))

(defun parse-csv (string)
  (remove-if (lambda (s) (string= s ""))
             (split-sequence "," string)))

(defun start-search-files (self)
  (let ((directory (capi:text-input-pane-text (directory-input-pane-of self)))
        (types (parse-csv (capi:text-input-pane-text (file-type-input-pane-of self))))
        (string (capi:text-input-pane-text (search-input-pane-of self)))
        (searcher-name (capi:choice-selected-item (search-option-pane-of self)))
        (case-insensitive (capi:button-selected (case-insensitive-button-of self))))
    (cond ((probe-file directory)
           (setf (capi:titled-object-message self) "Searching...")
           (start-search-thread (make-instance (get-searcher-class searcher-name)
                                               :pattern string
                                               :directory directory
                                               :types types
                                               :case-insensitive case-insensitive)
                                self))
          (t
           (setf (capi:titled-object-message self)
                 (format nil "Directory '~A' does not exist" directory))))))

(defun start-search-thread (searcher self)
  (mp:process-run-function
   *search-process-name*
   nil
   (lambda ()
     (let ((items (search-directory searcher))
           (message "Searching... Done"))
       (when *interrupted-flag*
         (setf *interrupted-flag* nil)
         (setf message "Interrupted"))
       (capi:apply-in-pane-process
        self
        (lambda ()
          (setf (capi:titled-object-message self) message)
          (setf (capi:collection-items (search-result-panel-of self)) items)))))))

(defun interrupt-search-files ()
  (when-let (process (mp:get-process *search-process-name*))
    (mp:process-interrupt process
                          (lambda ()
                            (setf *interrupted-flag* t)))))

(defun select-search-result (data interface)
  (declare (ignore interface))
  (let ((pathname (search-result-pathname data))
        (line (search-result-line data)))
    (capi:find-interface 'lw-tools:editor)
    (editor:find-line-in-file pathname line)))

(defun show-search-files (&optional (*current-directory* (current-directory)))
  (capi:display (make-instance 'search-files-pane)))
