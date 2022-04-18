(in-package :idmfoundinhim.mnemonic-gui)
(in-readtable :qtools)

(define-widget mnemonic (QWidget)
  ())

(define-subwidget (mnemonic input) (q+:make-qtextedit mnemonic)
  )

(define-subwidget (mnemonic output) (q+:make-qtextedit mnemonic)
  (setf (q+:read-only output) t))

;; (define-subwidget (mnemonic button) (q+:make-qpushbutton "Wait" mnemonic))

(define-subwidget (mnemonic layout) (q+:make-qvboxlayout mnemonic)
  (q+:add-widget layout input)
  (q+:add-widget layout output))

(define-slot (mnemonic input-changed) ()
  (declare (connected input (text-changed)))
  (setf (q+:plain-text output) (transcription-reigon-handling (format nil "::~a" (q+:text input)))))
