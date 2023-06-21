(require :asdf)
(defsystem "watch"
  :description "File update watcher for linux."
  :pathname "src"
  :licence "MIT"
  :depends-on (#:cffi)
  :serial t
  :components
  ((:file "watch")))
