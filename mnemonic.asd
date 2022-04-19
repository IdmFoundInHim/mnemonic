;;;; Deployment form

(defsystem "mnemonic"
    :description "Transcribes text for easy typing"
    :version "0.0.1"
    :author "IdmFoundInHim <idmfoundinhim@gmail.com>"
    :licence "MIT License"
    :source-control "git@github.com:IdmFoundInHim/mnemonic.git"
    :depends-on (:qtools :qtcore :qtgui)
    :defsystem-depends-on (:qtools)
    :components ((:file "packages")
		 (:file "main" :depends-on ("packages"))
		 (:file "gui" :depends-on ("main")))
    :build-operation "qt-program-op"
    :build-pathname "mnemonic"
    :entry-point "idmfoundinhim.mnemonic-gui:mnemonic")
