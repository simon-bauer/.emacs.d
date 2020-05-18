(describe "make-reaction-shell"
  (it "adds a new entry in the global registry"
    (make-reaction-shell "new-test-shell" ())
    (expect (gethash "new-test-shell" reaction-shell-registry) :not :to-equal nil)))
