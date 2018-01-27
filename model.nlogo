extensions [ gis csv rnd vid ]
globals [ initialization dow hour dow-gen hour-gen moscow-zones wheely-zone airports-zone online-cars online-cars-by-zone preorders-share-by-zone orders-matrix transition-matrix classes-matrix
  speed-matrix ticks-per-hour retarget awakening-by-zone preorders-total-delay-time preorders-total-delayed preorders-total-advanced preorders-total-advance-time preorders-total-number
  speed speed-matrix-old total-busy-time total-online-time preorders-by-zone preorders-delays-by-zone polygons-centroids polygons-distances-matrix polygons-online-cars-differences
  online-cars-by-zone-actual orders-by-zone-actual asap-orders-by-zone-actual preorders-by-zone-actual orders-duration-by-zone orders-distance-by-zone completed-orders-by-zone ]
breed [ orders order ]
breed [ cars car ]
patches-own [ random-orders random-orders-target-zones random-orders-ticks ]
orders-own [ from-zone to-zone dest class has-dropoff preorder ticks-to-pickup reserved ticks-to-show taken waiting-time ticks-to-dispatch dispatch-time-calculated initial-ticks-to-pickup
  shown-ticks advance-time-written-down ]
cars-own [ class busy? reserved-order dest kids drunk ticks-to-start ticks-to-show order-taken waiting-time hold offline? eta current-zone order-start-ticks order-from-zone order-dist ]

to-report avg-preorders-delay
  if preorders-total-delayed > 0 [
    report preorders-total-delay-time * (60. / ticks-per-hour) / preorders-total-delayed ; in minutes
  ]
end

to-report preorders-delays-share
  if preorders-total-number > 0 [
    report preorders-total-delayed * 100. / preorders-total-number ; in %
  ]
end

to-report busy-time-share
  report total-busy-time * 100. / total-online-time
end

to-report avg-preorders-advance
  if preorders-total-advanced > 0 [
    report preorders-total-advance-time * (60. / ticks-per-hour) / preorders-total-advanced ; in minutes
  ]
end

to-report get-eta [ car-target order-target gen? ]
  let heading-to-store [ heading ] of car-target
  let model-ticks ticks
  let model-hour hour
  let model-dow dow
  let model-speed speed

  if gen? [ set model-hour hour-gen
    set model-dow dow-gen
    set model-speed item 2 item (model-dow * 24 + model-hour) speed-matrix
    set model-speed model-speed * (3600 / 1000.) * (81 / 185.) * (2 / 3.) / ticks-per-hour ]

  let current-point list [xcor] of car-target [ycor] of car-target
  let center-distance ((item 0 current-point) ^ 2 + (item 1 current-point) ^ 2) ^ 0.5
  let step model-speed * (e ^ (-2. / (center-distance + 2)) + 0.3)

  ask car-target [ face order-target ]

  while [ ((item 0 current-point - [xcor] of order-target) ^ 2 + (item 1 current-point - [ycor] of order-target) ^ 2) ^ 0.5 >
    ((item 0 current-point + [dx] of car-target * step - [xcor] of order-target) ^ 2 + (item 1 current-point + [dy] of car-target * step - [ycor] of order-target) ^ 2) ^ 0.5 ] [; gets closer
    set current-point list (item 0 current-point + [dx] of car-target * step) (item 1 current-point + [dy] of car-target * step)
    set model-ticks model-ticks + 1
    if model-ticks mod ticks-per-hour = 0 [
      ifelse model-hour = 23 [set model-hour 0 ifelse model-dow = 6 [set model-dow 0] [set model-dow model-dow + 1]] [set model-hour model-hour + 1]
    ]
    set model-speed item 2 item (model-dow * 24 + model-hour) speed-matrix
    set model-speed model-speed * (3600 / 1000.) * (81 / 185.) * (2 / 3.) / ticks-per-hour
    set center-distance ((item 0 current-point) ^ 2 + (item 1 current-point) ^ 2) ^ 0.5
    set step model-speed * (e ^ (-2. / (center-distance + 2)) + 0.3)
  ]

  ask car-target [ set heading heading-to-store ]
  report (model-ticks - ticks)
end

to-report show-date [date-type]
  ifelse ticks <= ticks-per-hour * 3 [report "preparing"]
  [ifelse date-type = "dow" [
    if dow = 0 [report "Sunday"]
    if dow = 1 [report "Monday"]
    if dow = 2 [report "Tuesday"]
    if dow = 3 [report "Wednesday"]
    if dow = 4 [report "Thursday"]
    if dow = 5 [report "Friday"]
    if dow = 6 [report "Saturday"]
  ] [ report hour ] ]
end

to order-setup [ target-zone ]
  set hidden? true
  set size 0.1
  set from-zone target-zone

  let shift-x (random-float 1) - 0.5
  let shift-y (random-float 1) - 0.5
  let shifted-xcor (pxcor + shift-x)
  let shifted-ycor (pycor + shift-y)
  setxy shifted-xcor shifted-ycor

  let c 0
  while [not gis:contains? (gis:find-one-feature moscow-zones "FID" (word from-zone)) self]
    [ set c (c + 1)
      ; pass cases when current patch has small fraction of polygon's shape to other patches of the same target zone after 100 trials
      if c > 100 [
        set retarget from-zone
        ask one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word retarget))) [
          sprout-orders 1 [ order-setup retarget ]
        ]
        die
      ]
      set shift-x (random-float 1) - 0.5
      set shift-y (random-float 1) - 0.5
      set shifted-xcor (pxcor + shift-x)
      set shifted-ycor (pycor + shift-y)
      setxy shifted-xcor shifted-ycor ]

  let weights item from-zone transition-matrix
  let items n-values 269 [i -> i]
  let pairs (map list items weights)
  set to-zone first rnd:weighted-one-of-list pairs [ [p] -> last p ]

  ; determine destination point
  hatch 1 [
    set hidden? true

    let dest-patch one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word to-zone)))
    let dest-xcor [pxcor] of dest-patch
    let dest-ycor [pycor] of dest-patch
    set shift-x (random-float 1) - 0.5
    set shift-y (random-float 1) - 0.5
    setxy (dest-xcor + shift-x) (dest-ycor + shift-y)

    while [not gis:contains? (gis:find-one-feature moscow-zones "FID" (word to-zone)) self]
    [ set dest-patch one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word to-zone)))
      set dest-xcor [pxcor] of dest-patch
      set dest-ycor [pycor] of dest-patch
      set shift-x (random-float 1) - 0.5
      set shift-y (random-float 1) - 0.5
      setxy (dest-xcor + shift-x) (dest-ycor + shift-y)]

    set dest-xcor xcor
    set dest-ycor ycor
    set heading towards myself ; direction towards the origin
    let direction heading

    ask myself [
      set dest (list dest-xcor dest-ycor)
      set heading ((direction + 180) mod 360) ; direction towards the destination
    ]

    die
  ]

  ifelse member? to-zone [134 135 136] ; airports
  [ ifelse random-float 1 < 0.837491 [ set preorder 1 set shape "star" set size 0.2 ] [ set preorder 0 set shape "circle" ] ]
  [ ifelse random-float 1 < ( item 1 ( item from-zone preorders-share-by-zone ) ) [ set preorder 1 set shape "star" set size 0.2 ] [ set preorder 0 set shape "circle" ] ]

  ifelse preorder = 0 [ if random-float 1 < 0.361968 [ set has-dropoff 1 ] ]
  [ if random-float 1 < 0.656726 [ set has-dropoff 1 ] ]

  set weights item preorder classes-matrix
  set items (list "business" "vip" "viano" "kids" "drunk")
  set pairs (map list items weights)
  set class first rnd:weighted-one-of-list pairs [ [p] -> last p ]

  ifelse class = "business" [ set color gray ]
  [ ifelse class = "vip" [ set color white ]
    [ ifelse class = "viano" [ set color cyan ]
      [ ifelse class = "kids" [ set color magenta ]
        [ set color orange ] ] ] ]

  if preorder = 1 [

  ; determine time to pickup for preorders using precalculated distribution
  ; bins in minutes 0 12 24 36 48 60 72 84 96 108 120+
  let time-to-pickup-prob-list (list 0.04393591 0.16117932 0.14094208 0.08754795 0.05999818 0.05404694 0.03316701 0.02525227 0.01922512 0.01709462 0.3576105988684554)
  let time-to-pickup-bins (list 0 12 24 36 48 60 72 84 96 108 120 120) ; if >=120 it doesn't have an influence on the model

  set weights time-to-pickup-prob-list
  set items n-values 11 [i -> i] ; 11 intervals
  set pairs (map list items weights)
  let time-to-pickup-interval  first rnd:weighted-one-of-list pairs [ [p] -> last p ]

  let p random-float 1
  let minutes-to-pickup (item time-to-pickup-interval time-to-pickup-bins) * p + (item (time-to-pickup-interval + 1) time-to-pickup-bins) * (1 - p) ; assume uniform distribution within intervals
  set ticks-to-pickup round ((minutes-to-pickup / 60.) * ticks-per-hour)

  ; increase time-to-pickup to 1 hour of 2 hours if no free cars of this class are available
  if ticks >= ticks-per-hour * 3 [
      let order-class class
      let order-ticks-to-pickup ticks-to-pickup
      let available-cars cars with [hidden? = false and busy? = false and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1)
        or (member? order-class (list "business" "vip" "viano") and class = order-class)) and hold = 0]
      ask available-cars [ set eta get-eta self myself true ]
      if not any? available-cars with [ eta + (preorder-eta-additional-time / 60.) * ticks-per-hour <= order-ticks-to-pickup ] [
        ifelse any? available-cars with [ eta + (preorder-eta-additional-time / 60.) * ticks-per-hour <= ticks-per-hour ] [
          set ticks-to-pickup ticks-per-hour ; 1 hour
        ] [ set ticks-to-pickup 2 * ticks-per-hour ] ; 2 hours
    ]]

    set initial-ticks-to-pickup ticks-to-pickup

    ifelse minutes-to-pickup <= 12 [ ; corrections for small time to pickup
      set reserved 0 ] [ ifelse minutes-to-pickup <= 24 [
        if random-float 1 < 0.177004 [ set reserved 1 ] ]
        [ ifelse minutes-to-pickup <= 36 [
          if random-float 1 < 0.558089 [ set reserved 1 ] ]
          [
              let res-prob reservation-prob distancexy 0 0 distancexy (item 0 dest) (item 1 dest)
              if random-float 1 < res-prob [set reserved 1]
          ]
        ]
    ]
  ]

  ; create cars and disappear during the initialization phase
  if initialization [
    let origin-x xcor
    let origin-y ycor
    let destination dest
    let direction heading
    ask one-of patches [ sprout-cars 1 [ car-init origin-x origin-y destination direction ] ]
    die
  ]

  ; determine time from generation to activation
  ifelse preorder = 0 [ set ticks-to-show (2 * ticks-per-hour) ] [ set ticks-to-show (2 * ticks-per-hour - ticks-to-pickup) ]

  ; count created orders by district, dow-gen and hour-gen
  let t [ ]
  set t item (dow-gen * 24 + hour-gen) orders-by-zone-actual
  set t replace-item from-zone t (item from-zone t + 1)
  set orders-by-zone-actual replace-item (dow-gen * 24 + hour-gen) orders-by-zone-actual t

  ; count preorders and asap orders by zone
  ifelse preorder = 0 [
    set asap-orders-by-zone-actual replace-item from-zone asap-orders-by-zone-actual (item from-zone asap-orders-by-zone-actual + 1)
  ][
    set preorders-by-zone-actual replace-item from-zone preorders-by-zone-actual (item from-zone preorders-by-zone-actual + 1)
  ]

end

; calculate reservation probability for orders using prefitted logit model
to-report reservation-prob [ center-distance order-distance ]
  ; distance conversion: 81 (full model space range) = 185 km (measured using QGIS), *81/185
  if has-dropoff = 0 [ set order-distance 17.915816614551044 * 81 / 185 ] ; mean preorder distance, variance incorporated into the model through other variables
  let class-coef (list 0 0.448038627975 -0.399231111436 0.0775765435596 0.81513389302) ; business, vip, viano, kids, drunk
  let dow-coef (list 0 -0.118736548169 -0.116739354915 -0.149340328726 -0.186161346484 -0.256171178993 -0.101586947084)
  let hour-coef (list 0 0.00898559375828 0.203785873515 0.277397949146 0.825802700814 1.33639335156 1.83061318351 1.58881486732 0.894116643668 0.687140062641 0.409366115887 0.489760702583
    0.451169806916 0.47795833043 0.366807210095 0.362807500175 0.28736958722 0.424373379052 0.289549057481 0.174709896837 0.275344554268 0.446185229793 0.341789075796 0.296220314427)
  let center-dist-coef 0.0133406753614 * 185 / 81
  let order-dist-coef 0.0155211890581 * 185 / 81
  let airport-coef 1.21689218655
  let ticks-to-pickup-coef -4.02509155603e-08 * (60 / ticks-per-hour)
  let incept -0.25969409054976778

  let classes-dict (list "business" "vip" "viano" "kids" "drunk")
  let airport-correction 0
  if (member? to-zone [134 135 136] or member? from-zone [134 135 136]) [set airport-correction airport-coef] ; airports

  let z (incept + item (position class classes-dict) class-coef + item dow-gen dow-coef + item hour-gen hour-coef
    + center-dist-coef * center-distance + order-dist-coef * order-distance + airport-correction + ticks-to-pickup-coef * ticks-to-pickup)
  report 1 / (1 + exp (-1 * z))
end

to car-init [ origin-x origin-y destination direction ]
  setxy origin-x origin-y
  set heading direction
  set offline? false
  set busy? true
  set hidden? true
  set ticks-to-start 2 * ticks-per-hour ; start movement in 2 hours after generation (1 hour before the model time starts)
  set ticks-to-show 3 * ticks-per-hour - ticks ; show cars when the model time starts
  set dest destination
end

to cars-control-population
  if ticks >= ticks-per-hour * 3 [ ; after the model time starts
    let random-factor 0.9 + random-float 0.2
    let cars-to-add (round (item 2 item (dow * 24 + hour) online-cars * random-factor) - count cars)

    if cars-to-add < 0 [
      repeat abs cars-to-add [
        if any? cars with [busy? = false and reserved-order = 0] [
          ask one-of cars with [busy? = false and reserved-order = 0] [ die ]
        ]
      ]
    ]

    let weights awakening-by-zone
    let items n-values 269 [i -> i] ; 269 zones
    let pairs (map list items weights)

    if cars-to-add > 0 [ repeat cars-to-add [
      let target-zone first rnd:weighted-one-of-list pairs [ [p] -> last p ]
      ask one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word target-zone))) [
        sprout-cars 1 [ setxy (pxcor + random-float 1 - 0.5) (pycor + random-float 1 - 0.5) set busy? false ]
      ]
    ] ]

    ask cars with [class = 0] [
      set offline? false
      set shape "car top"
      set size 0.3

      set weights (list 1064 285 24)
      set items (list "business" "vip" "viano")
      set pairs (map list items weights)
      set class first rnd:weighted-one-of-list pairs [ [p] -> last p ]

      if class = "business" [
        set color gray
        if random-float 1 < 0.618 [ set drunk 1 ]
        if random-float 1 < 0.063 [ set kids 1 ]
      ]

      if class = "vip" [
        set color white
        if random-float 1 < 0.218 [ set drunk 1 ]
      ]

      if class = "viano" [
        set color cyan
        if random-float 1 < 0.125 [ set drunk 1 ]
      ]
    ]
  ]
end

to dispatch-order [ order-who ]
  let order-class [ class ] of order order-who
  let order-type [ preorder ] of order order-who
  let order-reserved [ reserved ] of order order-who
  let order-ticks-to-pickup [ ticks-to-pickup ] of order order-who
  let order-ticks-to-dispatch [ ticks-to-dispatch ] of order order-who

  ; ASAP orders
  let search-radius 0
  ifelse order-class = "business" [ set search-radius 10 * 81 / 185. ] [ set search-radius 11 * 81 / 185. ] ; 10 km for business class and 11 km for the rest

  if order-type = 0 [
    let available-cars cars with [busy? = false and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1)
      or (member? order-class (list "business" "vip" "viano") and class = order-class))
      and (reserved-order = 0 or order reserved-order = nobody or ([ticks-to-pickup] of order reserved-order > ticks-per-hour / 2.))] ; drivers with preorders within half an hour can't take ASAPs
      ifelse any? available-cars [
        if distance min-one-of available-cars [distance myself] <= search-radius [
          set taken 1
          ask min-one-of available-cars [distance myself] [ receive-order order-who ]
      ]
  ] [ ifelse order-class = "business" [ ; vip and viano cars can perform business orders
        let additional-available-cars cars with [busy? = false and member? class list "vip" "viano"
            and (reserved-order = 0 or order reserved-order = nobody or ([ticks-to-pickup] of order reserved-order > ticks-per-hour / 2.))]
        ifelse any? additional-available-cars in-radius search-radius [
         ; each additional car accepts order with 0.7 probability (0.7 because in distant area)
          let acceptance 1 - (1 - 0.7) ^ (count additional-available-cars in-radius search-radius)
          ifelse random-float 1 < acceptance [
            set taken 1
            ask one-of additional-available-cars in-radius search-radius [receive-order order-who]
          ] [ask order order-who [ die ]]
         ] [ask order order-who [ die ]]
      ] [ask order order-who [ die ]]
    ]
  ]

 ; new preorders dispatch algorithm
 if order-type = 1 and preorder-dispatch-algorithm = "new" [
    if order-ticks-to-pickup <= (preorder-dispatch-start-time / 60.) * ticks-per-hour [ ; start preorder dispatch algorithm
      ifelse order-reserved = 1 [ ; if preorder was reserved
        let reserved-cars cars with [hidden? = false and reserved-order = order-who and offline? = false]
        ifelse any? reserved-cars [ ; if driver who reserved this order is online
          let reserved-driver one-of reserved-cars
          let eta-plus-x (get-eta reserved-driver order order-who false) + (preorder-eta-additional-time / 60.) * ticks-per-hour
          ifelse eta-plus-x <= order-ticks-to-pickup [ ; if driver who reserved this order is not late
            ifelse ([busy?] of reserved-driver) = false [ ; if driver who reserved this order is not busy
              if order-ticks-to-pickup <= max list (eta-plus-x + 1) preorder-reserved-dispatch-time [ ; not too early to dispatch
                ask reserved-driver [ receive-order order-who ]
                set taken 1
              ]
            ] [ ask order order-who [ free-dispatch order-who ] ]
          ] [ ask order order-who [ free-dispatch order-who ] ]
        ] [ ask order order-who [ free-dispatch order-who ] ]
      ] [ ask order order-who [ free-dispatch order-who ] ]
      ]
    ]

  ; dispatch preorders with the old algorithm
  if order-type = 1 and preorder-dispatch-algorithm = "old" [
    if order-ticks-to-dispatch = 0 [
      ifelse order-reserved = 1 [
        let reserved-cars cars with [hidden? = false and reserved-order = order-who and offline? = false]
        ifelse any? reserved-cars [ ; if driver who reserved this order is online
          let reserved-driver one-of reserved-cars
          ask reserved-driver [ receive-order order-who ]
          set taken 1
        ] [ ; free dispatch
          let all-available-cars cars with [hidden? = false and busy? = false and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1)
            or (member? order-class (list "business" "vip" "viano") and class = order-class))]
          ask all-available-cars [ set eta get-eta self myself false ]
          if any? all-available-cars [ ask min-one-of all-available-cars [eta] [ receive-order order-who ]
          set taken 1 ]
        ]
      ] [ ; free dispatch
          let all-available-cars cars with [hidden? = false and busy? = false and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1)
            or (member? order-class (list "vip" "viano") and class = order-class) or class = "business")]
          ask all-available-cars [ set eta get-eta self myself false ]
          if any? all-available-cars [ ask min-one-of all-available-cars [eta] [ receive-order order-who ]
          set taken 1 ]
        ]
    ]
  ]
end

to free-dispatch [ order-who ]
  let order-class [ class ] of order order-who
  let order-type [ preorder ] of order order-who
  let order-reserved [ reserved ] of order order-who
  let order-ticks-to-pickup [ ticks-to-pickup ] of order order-who
  let available-cars cars with [hidden? = false and busy? = false
    and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1) or (member? order-class (list "business" "vip" "viano") and class = order-class)) and hold = 0]
  ask available-cars [ set eta get-eta self myself false ]
  set available-cars available-cars with [ eta + (preorder-eta-additional-time / 60.) * ticks-per-hour <= order-ticks-to-pickup ]
  ifelse count available-cars >= 1 [
    let nearest-available-car min-one-of available-cars [eta]
    let min-eta-plus-x [eta] of nearest-available-car + (preorder-eta-additional-time / 60.) * ticks-per-hour
    if order-ticks-to-pickup <= max list (min-eta-plus-x + 1) preorder-safe-time [
      ask nearest-available-car [ receive-order order-who ]
      set taken 1
      ]
    ] [
    let all-available-cars cars with [hidden? = false and busy? = false
      and ((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1) or (member? order-class (list "business" "vip" "viano") and class = order-class))]
    if order-class = "business" [set all-available-cars cars with [hidden? = false and busy? = false]]
    ask all-available-cars [ set eta get-eta self myself false ]
    if any? all-available-cars [ ask min-one-of all-available-cars [eta] [ receive-order order-who ]
       set taken 1 ]
    ]
end

to receive-order [ order-who ]
  face order order-who
  set busy? true
  set reserved-order 0
  set waiting-time 0
  set order-taken order-who
  set dest (list ([xcor] of order order-who) ([ycor] of order order-who))
end

to calculate-dispatch-time

  let order-type preorder
  let order-ticks-to-pickup ticks-to-pickup
  let order-center-distance ((xcor ^ 2 + ycor ^ 2) ^ 0.5) * 185 / 81. ; distance to km
  let order-dispatch-time-calculated dispatch-time-calculated

  ; calculate prebook hour & dow
  let model-hour hour
  let model-dow dow
  let advance order-ticks-to-pickup
  let model-ticks ticks
  while [ advance > 0 ] [
    set model-ticks model-ticks + 1
    if model-ticks mod ticks-per-hour = 0 [
        ifelse model-hour = 23 [set model-hour 0 ifelse model-dow = 6 [set model-dow 0] [set model-dow model-dow + 1]] [set model-hour model-hour + 1]]
    set advance advance - 1
  ]

  ; first iteration
  if model-dow = 0 [ set model-dow 7 ] ; sunday is the last weekday
  set model-dow max list 0 (model-dow - 4) ; mon-thu = 0, fri = 1, sat = 2, sun = 3
  let avg-speed item model-hour item model-dow speed-matrix-old
  let distrib-time 15 + (0.5 * 60 / avg-speed) * order-center-distance
  set distrib-time round (distrib-time * (ticks-per-hour / 60.)) ; minutes to ticks

  ; calculate ride hour & dow
  set model-hour hour
  set model-dow dow
  set advance order-ticks-to-pickup - distrib-time
  set model-ticks ticks
  while [ advance > 0 ] [
    set model-ticks model-ticks + 1
    if model-ticks mod ticks-per-hour = 0 [
        ifelse model-hour = 23 [set model-hour 0 ifelse model-dow = 6 [set model-dow 0] [set model-dow model-dow + 1]] [set model-hour model-hour + 1]]
    set advance advance - 1
  ]

  ; second iteration
  if model-dow = 0 [ set model-dow 7 ] ; sunday is the last weekday
  set model-dow max list 0 (model-dow - 4) ; mon-thu = 0, fri = 1, sat = 2, sun = 3
  set avg-speed item model-hour item model-dow speed-matrix-old
  set distrib-time 15 + (0.5 * 60 / avg-speed) * order-center-distance
  set distrib-time round (distrib-time * (ticks-per-hour / 60.)) ; minutes to ticks

  set ticks-to-dispatch max list 0 (ticks-to-pickup - distrib-time)
  set dispatch-time-calculated 1

end

to setup
  ca
  ask patches [set pcolor gray - 3]

  set initialization true

  set ticks-per-hour 20
  set dow 6
  set hour 21

  set dow-gen 6
  set hour-gen 23

  ; Load the dataset
  set moscow-zones gis:load-dataset "data/moscow_zones.shp"
  set wheely-zone gis:load-dataset "data/wheely.shp"
  set airports-zone gis:load-dataset "data/airports.shp"
  gis:set-world-envelope-ds gis:envelope-of moscow-zones

  ; Drawing zones boundaries from a shapefile
  gis:set-drawing-color blue - 3  gis:fill wheely-zone 1
  gis:set-drawing-color sky - 2   gis:fill airports-zone 1
  gis:set-drawing-color white    gis:draw moscow-zones 1

  set orders-matrix csv:from-file "data/orders.csv" ; index: dow * 24 + hour (all starting with 0), zones by columns
  set preorders-share-by-zone csv:from-file "data/preorders_share.csv"
  set transition-matrix csv:from-file "data/transition.csv"
  set classes-matrix csv:from-file "data/classes.csv" ; rows: 0 - ASAP, 1 - preorder, columns: 0 - business, 1 - vip, 2 - viano, 3 - kids, 4 - drunk
  set speed-matrix csv:from-file "data/speed.csv" ; index: dow * 24 + hour (all starting with 0), all city, speed in m/sec stored in item 2

  set online-cars csv:from-file "data/onlinecars.csv" ; index: 'dow'*24 + 'hour' (all starting with 0)
  set online-cars-by-zone csv:from-file "data/online_cars_by_zone.csv" ; index: 'dow'*24 + 'hour' (all starting with 0) by rows, zones by columns

  set awakening-by-zone (list 0 0 0 0 41 0 440 0 0 0 0 0 0 593 750 1081 346 0 1305 349 460 350 524 0 3109 1327 1335 1187 183 584 371 977 650 459 2023
  634 111 310 318 949 2081 3557 619 255 905 593 2117 4293 4926 8410 3533 1026 461 1894 2904 3233 0 1681 2411 76 1178 374 613 211 1374 198 1129 376 714
  172 590 683 429 548 79 641 1111 0 1211 2323 686 702 1697 236 594 274 658 405 999 792 272 158 1462 1151 147 3195 51 334 88 444 269 391 263 381 784 203
  340 294 515 0 2109 3517 1034 0 309 434 572 365 622 200 3880 694 549 546 597 752 0 779 2399 479 794 1088 2773 582 2075 1012 6014 0 260 570 1326 0 0 0
  0 0 307 1079 2139 261 154 0 434 1102 0 12 5 0 0 0 0 0 0 0 0 0 0 0 51 0 0 92 0 0 133 0 0 0 825 416 502 0 0 0 16 0 276 32 0 408 332 0 0 0 0 0 45 0 0 0
  45 0 0 0 0 0 171 0 0 0 0 37 0 462 567 0 283 0 53 80 0 0 0 0 171 316 0 8 1150 0 0 0 0 0 0 359 0 0 0 0 0 111 0 206 0 0 0 17 53 0 0 0 185 0 1120 232 495
  648 373 39 0 633 600 104 573 0 236 309 347)

  set speed-matrix-old csv:from-file "data/old_avg_speed.csv" ; has header, rows: 1 - friday, 2 - saturday, 3 - sunday, 0 - else, columns - hours

  set preorders-by-zone n-values 269 [i -> 0] ; 269 zones
  set preorders-delays-by-zone n-values 269 [i -> 0] ; 269 zones

  set orders-duration-by-zone n-values 269 [i -> 0] ; 269 zones
  set orders-distance-by-zone n-values 269 [i -> 0] ; 269 zones

  let zone 0
  set polygons-centroids [ ]
  while [ zone < 269 ] [
    set polygons-centroids lput gis:location-of (gis:centroid-of (gis:find-one-feature moscow-zones "FID" (word zone))) polygons-centroids
    set zone zone + 1
  ]

  set polygons-distances-matrix [ ]
  let i 0
  while [ i < 269 ] [
    let row [ ]
    let j 0
    while [ j < 269 ] [
      set row lput (((item 0 item i polygons-centroids - item 0 item j polygons-centroids) ^ 2 + (item 1 item i polygons-centroids - item 1 item j polygons-centroids) ^ 2) ^ 0.5) row
      set j j + 1
    ]
    set polygons-distances-matrix lput row polygons-distances-matrix
    set i i + 1
  ]

  set online-cars-by-zone-actual [ ]
  set i 0
  while [ i < 24 * 7 ] [
    set online-cars-by-zone-actual lput n-values 269 [ j -> 0 ] online-cars-by-zone-actual
    set i i + 1
  ]

  set orders-by-zone-actual [ ]
  set i 0
  while [ i < 24 * 7 ] [
    set orders-by-zone-actual lput n-values 269 [ j -> 0 ] orders-by-zone-actual
    set i i + 1
  ]

  set asap-orders-by-zone-actual n-values 269 [ jj -> 0 ]
  set preorders-by-zone-actual n-values 269 [ jjj -> 0 ]
  set completed-orders-by-zone n-values 269 [ jjjj -> 0 ]

  reset-ticks
end

to go
  ifelse ticks = ticks-per-hour [ set initialization false cars-control-population set dow-gen 0 set hour-gen 0 ]
  [if ticks > ticks-per-hour [
  ifelse ((ticks - ticks-per-hour) mod (ticks-per-hour * 24 * 7) = 0) ; add one additional hour to ticks for cars initialization
    [ set dow-gen 0 set hour-gen 0 ]
    [ ifelse ((ticks - ticks-per-hour) mod (ticks-per-hour * 24) = 0)
      [ set hour-gen 0 set dow-gen dow-gen + 1 ]
      [ if ((ticks - ticks-per-hour) mod ticks-per-hour = 0) [ set hour-gen hour-gen + 1 cars-control-population ] ; every hour
    ]
  ]
  ]]

  ; video recording
  ;if ticks = 60 + 24 * ticks-per-hour [ (vid:start-recorder 2025 2025) vid:record-view ]
  ;if ticks > 60 + 24 * ticks-per-hour and ticks < 60 + 48 * ticks-per-hour [ vid:record-view ]
  ;if ticks = 60 + 48 * ticks-per-hour [ vid:save-recording "monday.mp4" ]

  ; set speed
  set speed item 2 item (dow * 24 + hour) speed-matrix
  set speed speed * (3600 / 1000.) * (81 / 185.) * (2 / 3.) / ticks-per-hour ; convert from m/sec to km/h, from km to model distance units, divide by 1.5 to account for indirect routes

  ; infer model time from orders generation time (-gen)
  ifelse hour-gen < 2 [ ifelse dow-gen = 0 [set dow 0] [set dow dow-gen - 1]
    set hour 22 + hour-gen] [set dow dow-gen set hour hour-gen - 2]

  if (ticks mod ticks-per-hour = 0) [ ; every hour

    ask patches [
      set random-orders 0
      set random-orders-ticks [ ]
      set random-orders-target-zones [ ]
    ]

    let orders-matrix-row item (dow-gen * 24 + hour-gen) orders-matrix
    let num-orders sum orders-matrix-row
    let random-factor ( random-float 0.2 ) + 0.9
    set num-orders num-orders * random-factor * 1.2 ; to account for dead orders with car-not-founds

    let weights orders-matrix-row
    let items n-values 269 [i -> i] ; 269 zones
    let pairs (map list items weights)

    repeat (round num-orders) [
      let target-zone first rnd:weighted-one-of-list pairs [ [p] -> last p ]
      ask one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word target-zone))) [
        set random-orders random-orders + 1
        set random-orders-target-zones lput target-zone random-orders-target-zones
      ]
    ]

    ask patches [
      if random-orders > 0 [
        repeat random-orders [
          set random-orders-ticks lput (random ticks-per-hour) random-orders-ticks
        ]
      ]
    ]

  ]

  ask patches [
      foreach random-orders-ticks [
        x -> if (ticks mod ticks-per-hour) = x [
          let target-zone first random-orders-target-zones
          set random-orders-target-zones but-first random-orders-target-zones
          sprout-orders 1 [ order-setup target-zone ]
        ]
      ]
     ]

  ; randomly set a small fraction of drivers offline and back online
  ask cars [
    ifelse busy? [
      ifelse random-float 1 < 0.01 [set offline? true] [set offline? false]
    ] [
      ifelse random-float 1 < 0.05 [set offline? true] [set offline? false]
    ]
  ]

  ; assign reserved preorder to a free driver who can reach it in time
  ; busy drivers don't reserve orders if there is less than an hour to pickup
  ask orders [
    let order-who who
    if hidden? = false and preorder = 1 and reserved = 1 and not any? cars with [reserved-order = order-who] [
      let step speed
      let order-ticks-to-pickup ticks-to-pickup
      let order-class class
      let radius (order-ticks-to-pickup + (preorder-eta-additional-time / 60.) * ticks-per-hour) * step
      let available-cars cars with [((order-class = "kids" and kids = 1) or (order-class = "drunk" and drunk = 1)
        or (member? order-class (list "business" "vip" "viano") and class = order-class))
        and (busy? = false or order-ticks-to-pickup >= ticks-per-hour)] in-radius radius
        if any? available-cars [
          ask one-of available-cars [ set reserved-order order-who ]
        ]
      ]
    ]

  ; hold cars with preorders in preorder-car-hold-time
  ask cars [
    if reserved-order != 0 and order reserved-order != nobody [
      if [ticks-to-pickup] of order reserved-order <= preorder-car-hold-time * ticks-per-hour / 60. [
        set hold 1
      ]
    ]
  ]

  ; unhide (activate) orders
  ask orders [
    if hidden? [
      set ticks-to-show ticks-to-show - 1
      if ticks-to-show = 0 [ set hidden? false set shown-ticks ticks ]
    ]
  ]

  ; decrease ticks-to-pickup by 1 every tick for active preorders
  ask orders with [hidden? = false and preorder = 1] [ if ticks-to-pickup > 0 [set ticks-to-pickup ticks-to-pickup - 1 ] ]

  ; calculate dispatch time for peorders using the old algorithm
  if preorder-dispatch-algorithm = "old" [
    ask orders with [ hidden? = false and preorder = 1 and dispatch-time-calculated = 0 ] [ calculate-dispatch-time ]
  ]

  ; dispatch not taken active orders
  ask orders with [ hidden? = false and taken = 0 ] [
      dispatch-order who
  ]

  ; calculate current cars zones
  ask cars [
    let zone 0
    while [zone < 268 and not gis:contains? (gis:find-one-feature moscow-zones "FID" (word zone)) self] [
      set zone zone + 1
    ]
    set current-zone zone
  ]

  ; count cars within zones
  if ticks > ticks-per-hour * 3 [
    let t [ ]
    set t item (dow * 24 + hour) online-cars-by-zone-actual
    let zones n-values 269 [ i -> i ]
    let online-cars-d map [j -> (count cars with [current-zone = j]) * 1. / ticks-per-hour + item j t] zones
    set t online-cars-d
    set online-cars-by-zone-actual replace-item (dow * 24 + hour) online-cars-by-zone-actual t
  ]


  ; calculate online cars differences with desired distribution by polygons
  set polygons-online-cars-differences [ ]
  let zone 0
  let current-cars 0
  let desired-cars 0
  while [ zone < 269 ] [
    set current-cars count cars with [current-zone = zone]
    set desired-cars item zone item (24 * dow + hour) online-cars-by-zone
    ifelse random-float 1 < (desired-cars - floor desired-cars) [ set desired-cars ceiling desired-cars ] [ set desired-cars floor desired-cars ]
    set polygons-online-cars-differences lput (current-cars - desired-cars) polygons-online-cars-differences
    set zone zone + 1
  ]

  ; cars movement
  ask cars [
    ifelse ticks-to-show > 0 [ set ticks-to-show ticks-to-show - 1 ] [ set hidden? false ]
    ifelse ticks-to-start > 0 [ set ticks-to-start ticks-to-start - 1 ] [
      let center-distance distance patch 0 0
      let step speed * (e ^ (-2. / (center-distance + 2)) + 0.3) ; decrease speed closer to the city center
      ; add random delays due to unpredicted traffic jams
      let unexpected-jam-prob 0
      if member? dow (list 1 2 3 4 5) [
        if member? hour list 18 19 [ set unexpected-jam-prob 0.15 ]
        if member? hour list 17 20 [ set unexpected-jam-prob 0.10 ]
        if member? hour (list 8 9 10 11 12 13 14 15 16) [ set unexpected-jam-prob 0.05 ]
        ]
      if random-float 1 < unexpected-jam-prob [ set step step / 10. ]
      ifelse busy? [ ifelse ((xcor - item 0 dest) ^ 2 + (ycor - item 1 dest) ^ 2) ^ 0.5 > ((xcor + dx * step - item 0 dest) ^ 2 + (ycor + dy * step - item 1 dest) ^ 2) ^ 0.5 [ ; gets closer
        fd step ] [
        ifelse order order-taken = nobody [ ; if order is completed
          set busy? false
          let order-duration ticks - order-start-ticks
          set orders-distance-by-zone replace-item order-from-zone orders-distance-by-zone (item order-from-zone orders-distance-by-zone + order-dist)
          set orders-duration-by-zone replace-item order-from-zone orders-duration-by-zone (item order-from-zone orders-duration-by-zone + order-duration)
          set completed-orders-by-zone replace-item order-from-zone completed-orders-by-zone (item order-from-zone completed-orders-by-zone + 1)
        ] [
          ifelse [ticks-to-pickup] of order order-taken = 0 [
            set dest [dest] of order order-taken
            set order-start-ticks ticks
            set order-from-zone [from-zone] of order order-taken
            set order-dist ((([xcor] of order order-taken - item 0 dest) ^ 2 + ([ycor] of order order-taken - item 1 dest) ^ 2) ^ .5) * 1.5
            facexy (item 0 dest) (item 1 dest)
            ask order order-taken [
              if preorder = 1 and ticks >= ticks-per-hour * 4 [
                set preorders-total-number preorders-total-number + 1
                set preorders-by-zone replace-item from-zone preorders-by-zone (item from-zone preorders-by-zone + 1)
                if waiting-time > 0 [ set preorders-total-delay-time preorders-total-delay-time + waiting-time
                set preorders-total-delayed preorders-total-delayed + 1
                set preorders-delays-by-zone replace-item from-zone preorders-delays-by-zone (item from-zone preorders-delays-by-zone + 1)]
              ]
              die ]
            ] [ if ticks >= ticks-per-hour * 4 and [advance-time-written-down] of order order-taken = 0 [
                set preorders-total-advanced preorders-total-advanced + 1
                set preorders-total-advance-time preorders-total-advance-time + [ticks-to-pickup] of order order-taken
                ask order order-taken [set advance-time-written-down 1 ]
              ]
             ]
      ] ]
    ] [
        if hidden? = false
        [if waiting-time >= 1 [
          ifelse order reserved-order != nobody [
            face order reserved-order
            if distance order reserved-order > (5 * 81 / 185.) [ fd step ]
          ] [ ; move free cars from zones with surplus to closest zones with shortage
              if item current-zone polygons-online-cars-differences > 0 [
               let polygons-with-shortage [ ]
               let i 0
               while [i < 269] [
                 if item i polygons-online-cars-differences < 0 [set polygons-with-shortage lput i polygons-with-shortage]
                 set i i + 1
               ]
              if length polygons-with-shortage > 0 [
               let distances-to-polygons-with-shortage map [x -> item x item current-zone polygons-distances-matrix] polygons-with-shortage
               let min-distance min distances-to-polygons-with-shortage
               let target-zone item (position min-distance distances-to-polygons-with-shortage) polygons-with-shortage
               set polygons-online-cars-differences replace-item target-zone polygons-online-cars-differences (item target-zone polygons-online-cars-differences + 1)

               let dest-patch one-of (patches gis:intersecting (gis:find-one-feature moscow-zones "FID" (word target-zone)))
               face dest-patch
               fd step
              ]
             ]
            ]
          ]
          set waiting-time waiting-time + 1]
    ]
  ]]

  ; if the old algorithm is in use, decrease ticks-to-dispatch by 1 every tick for active preorders
  if preorder-dispatch-algorithm = "old" [
    ask orders with [hidden? = false and preorder = 1 and dispatch-time-calculated = 1] [ if ticks-to-dispatch > 0 [set ticks-to-dispatch ticks-to-dispatch - 1 ] ]
  ]

  ; add waiting time to orders
  ask orders with [hidden? = false] [
    ifelse preorder = 0 [set waiting-time waiting-time + 1]
    [if ticks-to-pickup = 0 [set color red set waiting-time waiting-time + 1 ]]
  ]

  ; kill orders if waiting time is too high (more than 30 minutes) or order is not taken after tickup time
  ask orders with [hidden? = false] [
    if waiting-time > ticks-per-hour / 2. [
      if preorder = 1 and ticks >= ticks-per-hour * 4 [ set preorders-total-delay-time preorders-total-delay-time + waiting-time
      set preorders-total-delayed preorders-total-delayed + 1
      set preorders-total-number preorders-total-number + 1
      set preorders-by-zone replace-item from-zone preorders-by-zone (item from-zone preorders-delays-by-zone + 1)
      set preorders-delays-by-zone replace-item from-zone preorders-delays-by-zone (item from-zone preorders-delays-by-zone + 1) ]
      if not (preorder = 0 and taken = 1) [ die ]
    ]
  ]

  ; calculate avg busy time share
  if any? cars with [ hidden? = false ] [ set total-busy-time total-busy-time + count cars with [ hidden? = false and busy? = true ]
    set total-online-time total-online-time + count cars with [ hidden? = false ] ]
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
266
12
6349
6096
-1
-1
75.0
1
10
1
1
1
0
0
0
1
-40
40
-40
40
1
1
1
ticks
1.0

BUTTON
-1
11
65
44
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
-2
61
66
94
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

MONITOR
74
10
160
55
dow
show-date \"dow\"
0
1
11

MONITOR
75
64
159
109
hour
show-date \"hour\"
0
1
11

MONITOR
166
169
266
214
active orders
count orders with [hidden? = False]
0
1
11

MONITOR
167
222
267
267
active preorders
count orders with [preorder = 1 and hidden? = false]
17
1
11

SLIDER
-5
333
268
366
preorder-dispatch-start-time
preorder-dispatch-start-time
30
120
60.0
15
1
minutes
HORIZONTAL

SLIDER
-5
376
268
409
preorder-car-hold-time
preorder-car-hold-time
30
180
120.0
30
1
minutes
HORIZONTAL

SLIDER
-5
421
267
454
preorder-safe-time
preorder-safe-time
10
30
15.0
5
1
minutes
HORIZONTAL

SLIDER
-5
464
267
497
preorder-reserved-dispatch-time
preorder-reserved-dispatch-time
10
60
30.0
10
1
minutes
HORIZONTAL

SLIDER
-4
509
268
542
preorder-eta-additional-time
preorder-eta-additional-time
5
15
10.0
5
1
minutes
HORIZONTAL

CHOOSER
-4
555
268
600
preorder-dispatch-algorithm
preorder-dispatch-algorithm
"old" "new"
1

MONITOR
-1
118
160
163
NIL
avg-preorders-delay
1
1
11

MONITOR
-1
223
162
268
NIL
preorders-delays-share
1
1
11

MONITOR
-1
277
164
322
NIL
busy-time-share
1
1
11

MONITOR
-2
170
160
215
NIL
avg-preorders-advance
1
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

car top
true
0
Polygon -7500403 true true 151 8 119 10 98 25 86 48 82 225 90 270 105 289 150 294 195 291 210 270 219 225 214 47 201 24 181 11
Polygon -16777216 true false 210 195 195 210 195 135 210 105
Polygon -16777216 true false 105 255 120 270 180 270 195 255 195 225 105 225
Polygon -16777216 true false 90 195 105 210 105 135 90 105
Polygon -1 true false 205 29 180 30 181 11
Line -7500403 false 210 165 195 165
Line -7500403 false 90 165 105 165
Polygon -16777216 true false 121 135 180 134 204 97 182 89 153 85 120 89 98 97
Line -16777216 false 210 90 195 30
Line -16777216 false 90 90 105 30
Polygon -1 true false 95 29 120 30 119 11

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

van top
true
0
Polygon -7500403 true true 90 117 71 134 228 133 210 117
Polygon -7500403 true true 150 8 118 10 96 17 85 30 84 264 89 282 105 293 149 294 192 293 209 282 215 265 214 31 201 17 179 10
Polygon -16777216 true false 94 129 105 120 195 120 204 128 180 150 120 150
Polygon -16777216 true false 90 270 105 255 105 150 90 135
Polygon -16777216 true false 101 279 120 286 180 286 198 281 195 270 105 270
Polygon -16777216 true false 210 270 195 255 195 150 210 135
Polygon -1 true false 201 16 201 26 179 20 179 10
Polygon -1 true false 99 16 99 26 121 20 121 10
Line -16777216 false 130 14 168 14
Line -16777216 false 130 18 168 18
Line -16777216 false 130 11 168 11
Line -16777216 false 185 29 194 112
Line -16777216 false 115 29 106 112
Line -7500403 false 210 180 195 180
Line -7500403 false 195 225 210 240
Line -7500403 false 105 225 90 240
Line -7500403 false 90 180 105 180

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="new algorithm's params optimization" repetitions="1" runMetricsEveryStep="false">
    <setup>random-seed 0
setup</setup>
    <go>go</go>
    <timeLimit steps="3440"/>
    <metric>avg-preorders-delay</metric>
    <metric>avg-preorders-advance</metric>
    <metric>preorders-delays-share</metric>
    <metric>busy-time-share</metric>
    <enumeratedValueSet variable="preorder-reserved-dispatch-time">
      <value value="15"/>
      <value value="30"/>
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-safe-time">
      <value value="15"/>
      <value value="30"/>
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-dispatch-start-time">
      <value value="60"/>
      <value value="90"/>
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-car-hold-time">
      <value value="30"/>
      <value value="60"/>
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-dispatch-algorithm">
      <value value="&quot;new&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-eta-additional-time">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="old algorithm" repetitions="1" runMetricsEveryStep="false">
    <setup>random-seed 0
setup</setup>
    <go>go</go>
    <timeLimit steps="3440"/>
    <metric>avg-preorders-delay</metric>
    <metric>avg-preorders-advance</metric>
    <metric>preorders-delays-share</metric>
    <metric>busy-time-share</metric>
    <enumeratedValueSet variable="preorder-reserved-dispatch-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-safe-time">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-dispatch-start-time">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-car-hold-time">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-dispatch-algorithm">
      <value value="&quot;old&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preorder-eta-additional-time">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
