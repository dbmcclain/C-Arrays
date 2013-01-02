
(asdf:defsystem "c-arrays"
  :description "c-arrays: adds foreign C-Arrays data types"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "c-arrays"))
  :depends-on  ("useful-macros"))

