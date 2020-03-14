(defpackage :software-evolution-library/software/super-mutant-project
  (:nicknames :sel/software/super-mutant-project :sel/sw/super-mutant-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/super-mutant
        :software-evolution-library/software/project))
(in-package :software-evolution-library/software/super-mutant-project)
(in-readtable :curry-compose-reader-macros)

(defmethod create-super-soft ((base project) mutants)
  (assert (every (lambda (mutant)
                   (and (eq (length (evolve-files base))
                            (length (evolve-files mutant)))
                        (every #'string=
                               (mapcar #'car (evolve-files base))
                               (mapcar #'car (evolve-files mutant)))))
                 mutants)
          nil
          "All project mutants must have the same file names.")

  (let ((super (copy base)))
    (nest (setf (evolve-files super))
          (mapcar (lambda (base-file super-file) ; combine with filenames
                    (cons (car base-file) super-file))
                  (evolve-files base))
          (apply #'mapcar                ; create super-soft for each file
                 (lambda (&rest mutants)
                   (create-super-soft (car mutants) mutants))
                 (mapcar [{mapcar #'cdr} #'evolve-files] mutants)))

    super))
