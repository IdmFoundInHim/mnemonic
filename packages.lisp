(asdf:defsystem mnemonic
  :depends-on (:qtools :qtcore :qtgui))

(defpackage :idmfoundinhim.mnemonic-transcription
  (:use :common-lisp)
  (:export :transcription-reigon-handling))

(defpackage :idmfoundinhim.mnemonic-gui
  (:use :cl+qt :idmfoundinhim.mnemonic-transcription))
