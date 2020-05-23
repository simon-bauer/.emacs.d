(describe "reaction-shell"
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
                    (before-each
                     (rs-init "test-shell" ())
                     (rs-set "test-shell" :queue '(("ls" . ignore))))
                    (it "pops first element of queue and sets shell to busy"
                        (rs-process-next "test-shell")
                        (expect (rs-get "test-shell" :queue) :to-equal '())
                        (expect (rs-get "test-shell" :shell-busy) :to-be t))
                    (it "inserts the callback in the callback slot")
                    (it "calls run shell cmd")))
