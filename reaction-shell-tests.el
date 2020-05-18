(describe "reaction-shell"
  (after-each
    (setq reaction-shell-registry (make-hash-table :test 'equal)))
  
  (describe "make-reaction-shell"
    (it "adds a new entry in the global registry"
      (make-reaction-shell "test-shell" ())
      (expect (gethash "test-shell" reaction-shell-registry) :not :to-equal nil)))

  (describe "reaction-shell-queue-cmd"
    (before-each
      (make-reaction-shell "test-shell" '("init cmd")))
    (it "starts a new shell buffer if it is not already running"
      (reaction-shell-queue-cmd "test-shell" "ls" 'ignore)
      (let ((first-buffer (get-buffer (reaction-shell-buffer-name "test-shell"))))
        (expect first-buffer :not :to-equal nil)
        (reaction-shell-queue-cmd "test-shell" "ls" 'ignore)
        (expect (get-buffer (reaction-shell-buffer-name "test-shell")) :to-be first-buffer)))
    (it "queues the initial commands when run for the first time"
      (spy-on 'reaction-shell-queue-cmd-pairs-internal)
      (reaction-shell-queue-cmd "test-shell" "ls" 'ignore)
      (expect 'reaction-shell-queue-cmd-pairs-internal :to-have-been-called-with "test-shell" '(("init cmd" . ignore))))
    (it "queues the shell cmd together with the reaction"
      (spy-on 'reaction-shell-queue-cmd-pairs-internal)
      (reaction-shell-queue-cmd "test-shell" "ls" 'ignore)
      (expect 'reaction-shell-queue-cmd-pairs-internal :to-have-been-called-with "test-shell" '(("ls" . ignore)))))

  (describe "reaction-shell-queue-cmd-pairs-internal"
    (it "appends the given cmd pairs to the queue in the registry"
      (make-reaction-shell "test-shell" ())
      (reaction-shell-queue-cmd-pairs-internal "test-shell" '(("ls" . ignore)))
      (expect (gethash :queue (gethash "test-shell" reaction-shell-registry)) :to-equal '(("ls" . ignore)))) ))
