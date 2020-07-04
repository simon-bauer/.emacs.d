(require 'org-element)

(describe
    "kauri"
  
  (describe "kauri-config-plist"
    (it "describe here"
      (with-temp-buffer
        (insert "* key
** val
")
        (expect 
         (kauri-config-plist
          (org-element-parse-buffer 'headline))
         :to-equal
         '("key" "val"))))))
