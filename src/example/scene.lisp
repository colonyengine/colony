(:universe
 (:player-ship
  ((transform))
  (:turret
   ((transform :translation/current (1 2 0))
    (gun-manager
     :guns (:laser :missile)))
   (:laser
    ((transform)
     (gun :shots 10
          :type :beam)))
   (:missile
    ((transform)
     (gun :shots 10
          :type :homing))))))
