(defpackage :idmfoundinhim.mnemonic-transcription ; main.lisp
  (:use :common-lisp)
  (:export :transcription-reigon-handling))

(defpackage :idmfoundinhim.mnemonic-gui ; gui.lisp
  (:use :cl+qt :idmfoundinhim.mnemonic-transcription)
  (:export :mnemonic))
