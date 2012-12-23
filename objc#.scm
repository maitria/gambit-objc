(namespace ("objc#"
  ;; Public interface
  class 
  object?
  selector?
  string->selector

  ;; Internal stuff
  call-method
  extract-args-from-arg-list
  extract-selector-name-from-arg-list
  make-object-tags
  make-selector-tags
  raw-class
  raw-object?
  raw-object->object
  *object-table*
  ))
