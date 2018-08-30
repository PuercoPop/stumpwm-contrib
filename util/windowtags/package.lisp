(defpackage #:windowtags
  (:use #:cl
        #:stumpwm
        #:alexandria
        #:split-sequence)
  (:import-from #:sb-ext
                #:string-to-octets
                #:octets-to-string )
  (:import-from #:stumpwm
                #:really-raise-window)
  (:export #:window-tags
           #:clear-tags))
