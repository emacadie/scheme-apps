#lang racket/base

(require cKanren)


(require (prefix-in ck-mk: cKanren/miniKanren))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
)

