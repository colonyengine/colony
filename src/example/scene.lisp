(:universe
 (:player-ship
  ((transform
    :tx 0 :ty 0 :tz 0
    :rx 0 :ry 0 :rz 0
    :sx 1 :sy 1 :sz 1))
  (:turret
   ((transform
     :tx 1 :ty 2 :tz 0
     :rx 0 :ry 0 :rz 0
     :sx 1 :sy 1 :sz 1)
    (gun-manager
     :guns (list :laser :missile)))
   (:laser
    ((transform
      :tx 0 :ty 0 :tz 0
      :rx 0 :ry 0 :rz 0
      :sx 1 :sy 1 :sz 1)
     (gun
      :shots 10
      :type :beam)))
   (:missile
    ((transform
      :tx 0 :ty 0 :tz 0
      :rx 0 :ry 0 :rz 0
      :sx 1 :sy 1 :sz 1)
     (gun
      :shots 10
      :type :homing))))))

;; questions
Why is gun2 a child of gun1 in the example DSL?

Are the guns supposed to be children of the turret in the example DSL?

Do you think components in the DSL should have default values? For example, the current DSL example has `:sx 1 :sy 1 :sz 1` for every entity. I think it would aid in much more terse scene descriptions if only the parts of a component that change from the default values are written.

What was your intention with the transform component in the dsl? I mean, there is no distinction whether it is specifying current, incremental, etc state variables.
