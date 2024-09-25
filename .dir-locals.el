((nil
  . ((eval
      . (setq-local org-roam-directory
                    (file-name-concat
                     (expand-file-name
                      (locate-dominating-file
                       default-directory ".dir-locals.el"))
                     "docs/roam")))
     (eval
      . (setq-local org-roam-db-location
                    (expand-file-name "org-roam.db"
                                      org-roam-directory)))))
 (c-mode
  . ((c-basic-offset . 4)
     (c-default-style . "bsd"))))
