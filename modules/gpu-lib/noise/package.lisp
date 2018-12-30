(in-package :defpackage+-user-1)

(defpackage+ #:first-light.gpu.noise
  (:nicknames #:fl.gpu.noise)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  (:export #:perlin
           #:perlin/derivs
           #:perlin-surflet
           #:perlin-surflet/derivs
           #:perlin-improved
           #:cellular
           #:cellular/derivs
           #:cellular-fast
           #:polkadot
           #:polkadot-box
           #:hermite
           #:hermite/derivs
           #:simplex-perlin
           #:simplex-perlin/derivs
           #:simplex-cellular
           #:simplex-polkadot
           #:value
           #:value/derivs
           #:value-perlin
           #:value-hermite
           #:cubist
           #:stars))
