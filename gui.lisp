#| Qt-based Graphical User Interface

Element Tree:
mnemonic (top-level container)
|-- layout (display container)
|   |-- input (editable text box, changing this refreshes output)
|   |-- output (view-only text box)
|#

(in-package :idmfoundinhim.mnemonic-gui)
(in-readtable :qtools)

(defparameter *font-size* 24)

(define-widget mnemonic (QWidget)
  ())

(define-initializer (mnemonic frameup)
  (setf (q+:window-title mnemonic) "Mnemonic")
  (setf (q+:fixed-size mnemonic) (q+:make-qsize 1400 800)))

(define-subwidget (mnemonic input) (q+:make-qtextedit mnemonic)
  (setf (q+:font-point-size input) *font-size*))

(define-subwidget (mnemonic output) (q+:make-qtextedit mnemonic)
  (setf (q+:font-point-size output) *font-size*)
  (setf (q+:read-only output) t))

(define-subwidget (mnemonic layout) (q+:make-qvboxlayout mnemonic)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout input)
  (q+:add-widget layout output))

(define-slot (mnemonic input-changed) ()
  (declare (connected input (text-changed)))
  (setf (q+:plain-text output)
	(transcription-reigon-handling (q+:to-plain-text input))))

