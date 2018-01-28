(defsystem "search-files"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "search-files")
               (:file "interface")))
