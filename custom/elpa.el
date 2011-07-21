(labels ((add-repo (name url)
                   (add-to-list 'package-archives
                                (cons name url))))
  (add-repo "marlalade" "http://marmalade-repo.org/packages/")
  (add-repo "elpa" "http://tromey.com/elpa/"))
