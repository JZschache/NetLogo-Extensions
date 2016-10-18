extensions[ql games]

turtles-own[ is-player-x q-values frequencies rel-freqs exploration-rate exploration-method q-values-std last-action nash-action optimal-action last-field]
globals[ means-x-matrix means-y-matrix means-max optimal-fields nash-fields rel-freq-optimal rel-freq-nash nextTick nextWrite]

to-report read-means-matrix [ nr ]
  let row-string-list []
  let temp means-x
  if (nr = 2) [set temp means-y]
  while [temp != ""] [
    let line-break position "\n" temp
    ifelse line-break = false [ 
      set row-string-list lput temp row-string-list
      set temp ""
    ] [
      set row-string-list lput (substring temp 0 line-break) row-string-list
      set temp substring temp (line-break + 1) (length temp)
    ]
  ]
  report (map [read-from-string (word "[ " ? " ]")] row-string-list)
end

to-report write-means-matrix [ matrix ]
  let strings games:matrix-as-pretty-strings matrix "  "
  let result ""
  foreach strings [ set result (word result (reduce [(word ?1 "  " ?2 )] ?) "\n") ]
  report result
end


to set-game
  
  let pm1 0
  let pm2 0
  let game 0
  
  ifelse (member? game-name ["Custom" "CopyMeansX" "TransposeMeansX"])[
    if (game-name = "Custom") [
      set pm1 games:matrix-from-row-list (read-means-matrix 1)
      set pm2 games:matrix-from-row-list (read-means-matrix 2)
      set game games:two-persons-game pm1 pm2
    ] 
    if (game-name = "CopyMeansX") [
      set pm1 games:matrix-from-row-list (read-means-matrix 1)
      set pm2 pm1
      set game games:two-persons-game pm1 pm2
    ] 
    if (game-name = "TransposeMeansX") [
      set pm1 games:matrix-from-row-list (read-means-matrix 1)
      set pm2 games:matrix-transpose pm1
      set game games:two-persons-game pm1 pm2
    ]
  ][ ; else: use gamut
    set game games:two-persons-gamut-game game-name n-alt-x n-alt-y
    set pm1 games:game-matrix game 1
    set pm2 games:game-matrix game 2
  ]
  
  ; update interface
  let m games:matrix-as-pretty-strings pm1 "  "
  set n-alt-x length m
  set n-alt-y length first m  
  set means-x write-means-matrix pm1
  set means-y write-means-matrix pm2
  set sample-equilibria games:get-solutions-string game "  "
  set fields games:get-fields-string game "  "
end



to setup
  
  clear-all  
  
  let n-patches (1 + floor sqrt (n-pairs * 9))
  set-patch-size 400 / n-patches
  resize-world 0 n-patches 0 n-patches
  
  let rows-x read-means-matrix 1
  let rows-y read-means-matrix 2
  
  set means-max max map [max map [abs ?] ? ](sentence rows-x rows-y)
  
  set n-alt-x length rows-x
  set n-alt-y length first rows-x
  
  set means-x-matrix games:matrix-from-row-list rows-x
  set means-y-matrix games:matrix-from-row-list rows-y
  
  let game games:two-persons-game means-x-matrix means-y-matrix
  set optimal-fields games:pure-optima game
  set nash-fields games:pure-solutions game
  
  set rel-freq-optimal 0.0
  
  let group-structure []
  create-turtles n-pairs [
    setxy random-xcor random-ycor
    set is-player-x true
    let partner 0
    hatch 1 [ ; create a partner
      setxy (1 + [xcor] of myself) (1 + [ycor] of myself)
      set is-player-x false
      set group-structure lput (ql:create-group (list (list myself (n-values n-alt-x [?])) (list self (n-values n-alt-y [?])))) group-structure
      face myself
      set partner self
    ]
    face partner    
  ]
  
  ask turtles [
    set exploration-rate global-exploration
    set exploration-method global-exploration-method
  ]
  
  ql:init turtles
  ql:set-group-structure group-structure
  
  ask turtles [
    set optimal-action false
    set nash-action false
    set rel-freqs (n-values n-alt-y [ 0.0 ])
    if is-player-x [set rel-freqs (n-values n-alt-x [ 0.0 ])]
  ]
  
  set nextTick 0
  set nextWrite 10
  
  reset-ticks
end

to-report get-rewards [ env-id ]
  let dec-list ql:get-group-list env-id
  let result map [reward ?] dec-list
  report result
end

to-report reward [group-choice]
  
  let agents ql:get-agents group-choice
  let decisions ql:get-decisions group-choice
  
  let dec-x first decisions
  let dec-y last decisions
  
  let field dec-x * n-alt-y + dec-y
  let optimal item field optimal-fields
  let nash item field nash-fields
  ask first agents [
    set last-action dec-x
    set optimal-action optimal 
    set nash-action nash
    set last-field field
  ]
  ask last agents [
    set last-action dec-y
    set optimal-action optimal 
    set nash-action nash
    set last-field field
  ]
    
  let m1 games:get-reward means-x-matrix dec-x dec-y
  let m2 games:get-reward means-y-matrix dec-x dec-y
   
  let rewards (list (random-normal m1 sd) (random-normal m2 sd))
  report ql:set-rewards group-choice rewards
  
end

to wait-for-tick
  set nextTick nextTick + 1
  while [ticks < nextTick] [ 
    update-slow
  ]
end

;to update
;  update-slow
;  tick
;end

to update
  ;update-slow
  tick
  ;if ticks > 999990 [update-slow]
  if (ticks >= nextWrite) [
    file-open "/home/johannes/matchingLaw/qlExtension/twoPersonsGames/exp-two-persons-prisoner-revision.csv"
    file-print (word "\"x\",\"" global-exploration-method "\",\"" n-pairs "\",\"" sd "\",\"" (read-means-matrix 1) "\",\"" global-exploration "\",\"" ticks "\",\"" (count turtles with [last-action = 0]) "\"")
    file-close
    ;set nextWrite nextWrite + (10 ^ (ifelse-value (ticks > 0) [floor ((log ticks 10) + 0.00000000001)] [0]))
    set nextWrite nextWrite + 10
  ]
end

to update-slow
  
  let optimal-freq count turtles with [optimal-action]
  set rel-freq-optimal optimal-freq / 2 / n-pairs
  
  let nash-freq count turtles with [nash-action]
  set rel-freq-nash nash-freq / 2 / n-pairs
  
  ask turtles [
    let total-n sum frequencies 
    set q-values-std 0
    if (total-n > 0) [
      set rel-freqs map [ precision (? / total-n) 10] frequencies   
      let zipped (map [ (list ?1 ?2) ] q-values frequencies)
      let filtered filter [ ( filter? (item 1 ?) total-n) ] zipped
      let qvs map [ (item 0 ?) / means-max ] filtered
      if (length qvs > 1) [ 
        set q-values-std precision (standard-deviation qvs) 2
      ]
    ]
  ]
  
  ;tick
end

to-report filter? [ n total-n ]
  
  let expectation 0.05
  if (exploration-method = "epsilon-greedy") [
    set expectation exploration-rate / (length frequencies)
  ]
  
  report 2.33 < (n / total-n - expectation) * (sqrt total-n) / (sqrt (expectation * (1 - expectation)))
  
end
@#$#@#$#@
GRAPHICS-WINDOW
890
25
1302
458
-1
-1
2.150537634408602
1
10
1
1
1
0
1
1
1
0
186
0
186
0
0
1
ticks
30.0

SLIDER
30
25
215
58
n-pairs
n-pairs
0
10000
3824
1
1
NIL
HORIZONTAL

BUTTON
630
185
765
218
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
630
220
765
253
NIL
ql:start
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
630
255
765
288
NIL
ql:stop
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
30
60
227
93
global-exploration
global-exploration
0
16
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
580
75
765
108
sd
sd
0
10
1
0.1
1
NIL
HORIZONTAL

INPUTBOX
220
60
395
175
means-x
  9    2\n10    0\n
1
1
String

CHOOSER
30
95
215
140
global-exploration-method
global-exploration-method
"epsilon-greedy" "softmax" "Roth-Erev"
0

MONITOR
30
195
215
240
NIL
rel-freq-optimal
2
1
11

INPUTBOX
220
185
625
330
fields
|  1:  (  9,  9)  O  |  2:  (  2,10)  ON|\n|  3:  (10,  2)  ON|  4:  (  0,  0)      |\n
1
1
String

PLOT
30
295
215
435
fields-histogram
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 false "set-plot-x-range 0 (n-alt-x * n-alt-y)" "histogram [last-field] of turtles with [is-player-x]"

INPUTBOX
400
60
575
175
means-y
  9  10\n  2    0\n
1
1
String

CHOOSER
580
25
765
70
game-name
game-name
"Custom" "CopyMeansX" "TransposeMeansX" "BattleOfTheSexes" "Chicken" "CollaborationGame" "CoordinationGame" "DispersionGame" "GrabTheDollar" "GuessTwoThirdsAve" "HawkAndDove" "MajorityVoting" "MatchingPennies" "PrisonersDilemma" "RandomGame" "RandomZeroSum" "RockPaperScissors" "ShapleysGame"
4

BUTTON
580
110
765
143
NIL
set-game
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
220
335
765
496
sample-equilibria
        x1        x2        y1        y2  |        Ex        Ey  |        mx\n\n      2/3      1/3      2/3      1/3  |    20/3    20/3  |           \n
1
1
String

SLIDER
220
25
395
58
n-alt-x
n-alt-x
1
10
2
1
1
NIL
HORIZONTAL

SLIDER
400
25
575
58
n-alt-y
n-alt-y
1
10
2
1
1
NIL
HORIZONTAL

PLOT
30
440
215
575
q-values-std-hist
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "" "histogram [q-values-std] of turtles"

MONITOR
30
245
215
290
NIL
rel-freq-nash
2
1
11

MONITOR
30
580
212
625
mean [q-values-std]
mean [q-values-std] of turtles
2
1
11

MONITOR
30
145
217
190
mean [sum exploration]
mean [exploration-rate] of turtles
5
1
11

PLOT
220
500
380
625
freq-x-hist
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "set-plot-x-range 0 n-alt-x" "histogram [last-action] of turtles with [is-player-x]"

PLOT
385
500
545
625
freq-y-hist
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "set-plot-x-range 0 n-alt-y" "histogram [last-action] of turtles with [not is-player-x]"

BUTTON
580
145
765
178
NIL
ql:decay-exploration
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
610
540
727
573
NIL
update-slow
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
800
490
1415
730
plot 1
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [item 0 rel-freqs] of turtles with [is-player-x]"
"pen-1" 1.0 0 -7500403 true "" "plot mean [item 0 rel-freqs] of turtles with [not is-player-x]"
"pen-2" 1.0 0 -2674135 true "" "plot mean [item 1 rel-freqs] of turtles with [is-player-x]"
"pen-3" 1.0 0 -955883 true "" "plot mean [item 1 rel-freqs] of turtles with [not is-player-x]"

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
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="exp-two-persons-coordination-1" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-1-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-1-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-1-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-1-sd" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-2" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0 \n 10  8\n&quot;"/>
      <value value="&quot;10  1 \n 9  8\n&quot;"/>
      <value value="&quot;10  2 \n 8  8\n&quot;"/>
      <value value="&quot;10  3 \n 7  8\n&quot;"/>
      <value value="&quot;10  4 \n 6  8\n&quot;"/>
      <value value="&quot;10  5 \n 5  8\n&quot;"/>
      <value value="&quot;10  6 \n 4  8\n&quot;"/>
      <value value="&quot;10  7 \n 3  8\n&quot;"/>
      <value value="&quot;10  8 \n 2  8\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-3" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0 -20\n 0  2  0\n-20  0 10\n&quot;"/>
      <value value="&quot;10  0 -15\n 0  2  0\n-15  0 10\n&quot;"/>
      <value value="&quot;10  0 -10\n 0  2  0\n-10  0 10\n&quot;"/>
      <value value="&quot;10  0 -5\n 0  2  0\n-5  0 10\n&quot;"/>
      <value value="&quot;10  0 0\n 0  2  0\n0  0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-three-opt-nash" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 10 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-battle" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;0  10\n 8 0\n&quot;"/>
      <value value="&quot;8  10\n 8 0\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-chicken" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 2\n 10 0&quot;"/>
      <value value="&quot; 5 2\n 10 0&quot;"/>
      <value value="&quot; 7 2\n 10 0&quot;"/>
      <value value="&quot; 9 2\n 10 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-chicken-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 2\n 10 0&quot;"/>
      <value value="&quot; 5 2\n 10 0&quot;"/>
      <value value="&quot; 7 2\n 10 0&quot;"/>
      <value value="&quot; 9 2\n 10 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-chicken-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 2\n 10 0&quot;"/>
      <value value="&quot; 5 2\n 10 0&quot;"/>
      <value value="&quot; 7 2\n 10 0&quot;"/>
      <value value="&quot; 9 2\n 10 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-extreme-chicken" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;1  0\n 10 0\n&quot;"/>
      <value value="&quot;2  0\n 10 0\n&quot;"/>
      <value value="&quot;3  0\n 10 0\n&quot;"/>
      <value value="&quot;4  0\n 10 0\n&quot;"/>
      <value value="&quot;5  0\n 10 0\n&quot;"/>
      <value value="&quot;6  0\n 10 0\n&quot;"/>
      <value value="&quot;7  0\n 10 0\n&quot;"/>
      <value value="&quot;8  0\n 10 0\n&quot;"/>
      <value value="&quot;9  0\n 10 0\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-three-alt-games" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;DispersionGame&quot;"/>
      <value value="&quot;GrabTheDollar&quot;"/>
      <value value="&quot;GuessTwoThirdsAve&quot;"/>
      <value value="&quot;MajorityVoting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-four-alt-games" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [is-player-x and last-action = 3]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 3]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <metric>count turtles with [last-field = 9]</metric>
    <metric>count turtles with [last-field = 10]</metric>
    <metric>count turtles with [last-field = 11]</metric>
    <metric>count turtles with [last-field = 12]</metric>
    <metric>count turtles with [last-field = 13]</metric>
    <metric>count turtles with [last-field = 14]</metric>
    <metric>count turtles with [last-field = 15]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;DispersionGame&quot;"/>
      <value value="&quot;GrabTheDollar&quot;"/>
      <value value="&quot;GuessTwoThirdsAve&quot;"/>
      <value value="&quot;MajorityVoting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-dollar" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [is-player-x and last-action = 3]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 3]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <metric>count turtles with [last-field = 9]</metric>
    <metric>count turtles with [last-field = 10]</metric>
    <metric>count turtles with [last-field = 11]</metric>
    <metric>count turtles with [last-field = 12]</metric>
    <metric>count turtles with [last-field = 13]</metric>
    <metric>count turtles with [last-field = 14]</metric>
    <metric>count turtles with [last-field = 15]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;0 10 10\n 5 0 10\n 5 5 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-dollar-4" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [is-player-x and last-action = 3]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 3]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <metric>count turtles with [last-field = 9]</metric>
    <metric>count turtles with [last-field = 10]</metric>
    <metric>count turtles with [last-field = 11]</metric>
    <metric>count turtles with [last-field = 12]</metric>
    <metric>count turtles with [last-field = 13]</metric>
    <metric>count turtles with [last-field = 14]</metric>
    <metric>count turtles with [last-field = 15]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;0 10 10 10\n 5 0 10 10\n 5 5 0 10\n 5 5 5 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-matching" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-y">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;MatchingPennies&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-RPS-SG" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;RockPaperScissors&quot;"/>
      <value value="&quot;ShapleysGame&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 10\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum-test" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 6\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-shapley-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;ShapleysGame&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-shapley-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;ShapleysGame&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-matching-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-y">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;MatchingPennies&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-matching-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-y">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;MatchingPennies&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-chicken-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 2\n 10 0&quot;"/>
      <value value="&quot; 5 2\n 10 0&quot;"/>
      <value value="&quot; 7 2\n 10 0&quot;"/>
      <value value="&quot; 9 2\n 10 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-matching-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-y">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;MatchingPennies&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-shapley-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;ShapleysGame&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 6\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum-softmax" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 6\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum-softmax-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:decay-exploration
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;softmax&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 6\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-three-opt-nash-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 10 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-three-alt-games-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;DispersionGame&quot;"/>
      <value value="&quot;GrabTheDollar&quot;"/>
      <value value="&quot;GuessTwoThirdsAve&quot;"/>
      <value value="&quot;MajorityVoting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-four-alt-games-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [is-player-x and last-action = 3]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 3]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <metric>count turtles with [last-field = 9]</metric>
    <metric>count turtles with [last-field = 10]</metric>
    <metric>count turtles with [last-field = 11]</metric>
    <metric>count turtles with [last-field = 12]</metric>
    <metric>count turtles with [last-field = 13]</metric>
    <metric>count turtles with [last-field = 14]</metric>
    <metric>count turtles with [last-field = 15]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;DispersionGame&quot;"/>
      <value value="&quot;GrabTheDollar&quot;"/>
      <value value="&quot;GuessTwoThirdsAve&quot;"/>
      <value value="&quot;MajorityVoting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-1-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0\n 0 0\n&quot;"/>
      <value value="&quot;10  0\n 0 1\n&quot;"/>
      <value value="&quot;10  0\n 0 2\n&quot;"/>
      <value value="&quot;10  0\n 0 3\n&quot;"/>
      <value value="&quot;10  0\n 0 4\n&quot;"/>
      <value value="&quot;10  0\n 0 5\n&quot;"/>
      <value value="&quot;10  0\n 0 6\n&quot;"/>
      <value value="&quot;10  0\n 0 7\n&quot;"/>
      <value value="&quot;10  0\n 0 8\n&quot;"/>
      <value value="&quot;10  0\n 0 9\n&quot;"/>
      <value value="&quot;10  0\n 0 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-2-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;10  0 \n 10  8\n&quot;"/>
      <value value="&quot;10  1 \n 9  8\n&quot;"/>
      <value value="&quot;10  2 \n 8  8\n&quot;"/>
      <value value="&quot;10  3 \n 7  8\n&quot;"/>
      <value value="&quot;10  4 \n 6  8\n&quot;"/>
      <value value="&quot;10  5 \n 5  8\n&quot;"/>
      <value value="&quot;10  6 \n 4  8\n&quot;"/>
      <value value="&quot;10  7 \n 3  8\n&quot;"/>
      <value value="&quot;10  8 \n 2  8\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-coordination-3-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [is-player-x and last-action = 2]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 2]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <metric>count turtles with [last-field = 4]</metric>
    <metric>count turtles with [last-field = 5]</metric>
    <metric>count turtles with [last-field = 6]</metric>
    <metric>count turtles with [last-field = 7]</metric>
    <metric>count turtles with [last-field = 8]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;CopyMeansX&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;30  20 0\n 20  22  20\n0  20 30\n&quot;"/>
      <value value="&quot;30  20 5\n 20  22  20\n5  20 30\n&quot;"/>
      <value value="&quot;30  20 10\n 20  22  20\n10  20 30\n&quot;"/>
      <value value="&quot;30  20 15\n 20  22  20\n15  20 30\n&quot;"/>
      <value value="&quot;30  20 20\n 20  22  20\n20  20 30\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-battle-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;5  0\n 2 10\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;10  0\n 2 5\n&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-chicken-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 2\n 10 0&quot;"/>
      <value value="&quot; 5 2\n 10 0&quot;"/>
      <value value="&quot; 7 2\n 10 0&quot;"/>
      <value value="&quot; 9 2\n 10 0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-matching-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-x">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-alt-y">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;MatchingPennies&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-RPS-SG-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 2 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;RockPaperScissors&quot;"/>
      <value value="&quot;ShapleysGame&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-zero-sum-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;7 0\n 5 6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;3 10\n 5 4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-inspection" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;9 15\n 10 10&quot;"/>
      <value value="&quot;8 15\n 10 10&quot;"/>
      <value value="&quot;7 15\n 10 10&quot;"/>
      <value value="&quot;6 15\n 10 10&quot;"/>
      <value value="&quot;5 15\n 10 10&quot;"/>
      <value value="&quot;4 15\n 10 10&quot;"/>
      <value value="&quot;3 15\n 10 10&quot;"/>
      <value value="&quot;2 15\n 10 10&quot;"/>
      <value value="&quot;1 15\n 10 10&quot;"/>
      <value value="&quot;0 15\n 10 10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;15 10\n 5 10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-inspection-RE" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 20000</exitCondition>
    <metric>fields</metric>
    <metric>[q-values-std] of turtles</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [is-player-x]</metric>
    <metric>[(item 0 rel-freqs)] of turtles with [not is-player-x]</metric>
    <metric>[(item 1 rel-freqs)] of turtles with [not is-player-x]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot;9 15\n 10 10&quot;"/>
      <value value="&quot;8 15\n 10 10&quot;"/>
      <value value="&quot;7 15\n 10 10&quot;"/>
      <value value="&quot;6 15\n 10 10&quot;"/>
      <value value="&quot;5 15\n 10 10&quot;"/>
      <value value="&quot;4 15\n 10 10&quot;"/>
      <value value="&quot;3 15\n 10 10&quot;"/>
      <value value="&quot;2 15\n 10 10&quot;"/>
      <value value="&quot;1 15\n 10 10&quot;"/>
      <value value="&quot;0 15\n 10 10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-y">
      <value value="&quot;15 10\n 5 10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-test" repetitions="100" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 5 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-two-persons-prisoner-revision" repetitions="1" runMetricsEveryStep="false">
    <setup>set-game
setup
ql:start</setup>
    <final>ql:stop
wait 1</final>
    <exitCondition>ticks &gt; 2000</exitCondition>
    <metric>fields</metric>
    <metric>rel-freq-optimal</metric>
    <metric>rel-freq-nash</metric>
    <metric>mean [q-values-std] of turtles</metric>
    <metric>mean [exploration-rate] of turtles</metric>
    <metric>count turtles with [is-player-x and last-action = 0]</metric>
    <metric>count turtles with [is-player-x and last-action = 1]</metric>
    <metric>count turtles with [not is-player-x and last-action = 0]</metric>
    <metric>count turtles with [not is-player-x and last-action = 1]</metric>
    <metric>count turtles with [last-field = 0]</metric>
    <metric>count turtles with [last-field = 1]</metric>
    <metric>count turtles with [last-field = 2]</metric>
    <metric>count turtles with [last-field = 3]</metric>
    <enumeratedValueSet variable="global-exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
      <value value="&quot;Roth-Erev&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-pairs">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="means-x">
      <value value="&quot; 3 0\n 10 2&quot;"/>
      <value value="&quot; 5 0\n 10 2&quot;"/>
      <value value="&quot; 7 0\n 10 2&quot;"/>
      <value value="&quot; 9 0\n 10 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-exploration">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-name">
      <value value="&quot;TransposeMeansX&quot;"/>
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
1
@#$#@#$#@
