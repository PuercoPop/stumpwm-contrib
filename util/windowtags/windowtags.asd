(defsystem "windowtags"
  :serial t
  :description "Add metadata to windows to manipulate them en mass."
  :author "Michael Raskin"
  :license "GPLv3"
  :depends-on ("stumpwm"
               "alexandria"
               "split-sequence")
  :components ((:file "package")
               (:file "windowtags")))

