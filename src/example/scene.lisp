(:universe
 (:player-ship
  ((transform :translation/current (0 0 0)))
  (:turret
   ((transform :translation/current (1 2 0))
    (gun-manager
     :guns (list :laser :missile)))
   (:laser
    ((transform)
     (gun :shots 10
          :type :beam)))
   (:missile
    ((transform)
     (gun :shots 10
          :type :homing))))))
