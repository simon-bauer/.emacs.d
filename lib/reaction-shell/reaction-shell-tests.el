(describe
    "reaction-shell"
  (after-each
    (setq rs-registry (make-hash-table :test 'equal)))
  
  (describe "rs-init"
    (it "adds a new entry in the global registry"
      (rs-init "test-shell" ())
      (expect (gethash "test-shell" rs-registry) :not :to-equal nil)))

  (describe "rs-cmd"
    (before-each
      (rs-init "test-shell" '("init cmd")))
    (it "starts a new shell buffer if it is not already running"
      (rs-cmd "test-shell" "ls" 'ignore)
      (let ((first-buffer (get-buffer (rs-buffer-name "test-shell"))))
        (expect first-buffer :not :to-equal nil)
        (rs-cmd "test-shell" "ls" 'ignore)
        (expect (get-buffer (rs-buffer-name "test-shell")) :to-be first-buffer)))
    (it "queues the initial commands when run for the first time"
      (spy-on 'rs-cmd-internal)
      (rs-cmd "test-shell" "ls" 'ignore)
      (expect 'rs-cmd-internal :to-have-been-called-with "test-shell" '(("init cmd" . ignore))))
    (it "queues the shell cmd together with the reaction"
      (spy-on 'rs-cmd-internal)
      (rs-cmd "test-shell" "ls" 'ignore)
      (expect 'rs-cmd-internal :to-have-been-called-with "test-shell" '(("ls" . ignore)))))

  (describe "rs-cmd-internal"
    (before-each
      (spy-on 'rs-trigger)
      (rs-init "test-shell" ())
      (rs-cmd-internal "test-shell" '(("ls" . ignore))))
    (it "appends the given cmd pairs to the queue in the registry"
      (expect (rs-get "test-shell" :queue) :to-equal '(("ls" . ignore))))
    (it "triggers the check if the next cmd pair can be executed"
      (expect 'rs-trigger :to-have-been-called)))

  (describe "rs-trigger"
    (before-each
      (spy-on 'rs-process-next)
      (rs-init "test-shell" ()))
    (it "does not call process next when queue empty"
      (rs-trigger "test-shell")
      (expect 'rs-process-next :not :to-have-been-called))
    (it "does not call process next when shell busy"
      (rs-set "test-shell" :queue '(("ls" . ignore)))
      (rs-set "test-shell" :shell-busy t)
      (rs-trigger "test-shell")
      (expect 'rs-process-next :not :to-have-been-called))
    (it "does call next process when when queue not empty and shell not busy"
      (rs-set "test-shell" :queue '(("ls" . ignore)))
      (rs-trigger "test-shell")
      (expect 'rs-process-next :to-have-been-called)))

  (describe "rs-process-next"
    (defun my-ignore ())
    (before-each
      (spy-on 'rs-process-current)
      (rs-init "test-shell" ())
      (rs-set "test-shell" :queue '(("ls" . my-ignore))))
    (it "pops first element of queue and sets shell to busy"
      (rs-process-next "test-shell")
      (expect (rs-get "test-shell" :queue) :to-equal '())
      (expect (rs-get "test-shell" :shell-busy) :to-be t))
    (it "inserts the callback in the current callback slot"
      (rs-process-next "test-shell")
      (expect (rs-get "test-shell" :current-callback) :to-equal 'my-ignore))
    (it "inserts cmd in the current cmd slot"
      (rs-process-next "test-shell")
      (expect (rs-get "test-shell" :current-cmd) :to-equal "ls"))
    (it "triggers process current"
      (rs-process-next "test-shell")
      (expect 'rs-process-current :to-have-been-called)))

  (describe "rs-process-current"
    (it "adds output filter function to comint output filter of shell")
    (it "inserts cmd and end marker in shell"))

  (describe "rs-output-filter"
    (it "sends input to tmp buffer")
    (it "triggers callback if end marker is found in tmp buffer")
    (it "cleans up after callback by removing itself from comint output filter of shell, deleting current-callback and current-cmd, setting shell to non busy and sending rs-trigger")))
