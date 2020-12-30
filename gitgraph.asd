;;;; gitgraph.asd

(asdf:defsystem #:gitgraph
  :description "gitgraph is a CLIM-based tool to display and editor graphical representation of the commit history in s-expression."
  :author "Jonggyu Lee"
  :license  "GNU General Public License Version 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim
               #:mcclim-layouts/tab)
  :components ((:file "package")
               (:file "gitgraph")))
