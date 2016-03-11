extensions[ql]

patches-own[ exploration-rate exploration-method q-values frequencies rel-freqs q-values-std last-action last-field]
globals[ alternative-ids alternative-names alternative-colors patches-list n-groups next-groups updated nextTick groups group-fields group-fields-sum mean-group-fields]

to setup
  clear-all
  set-patch-size 400 / n-patches  
  resize-world 0 (n-patches - 1) 0 (n-patches - 1)
   
  ask patches [
    set exploration-rate experimenting
    set exploration-method "epsilon-greedy"
    set rel-freqs [0 0]
    ifelse (random 2 = 1) [set pcolor blue] [set pcolor red]
  ]
  ql:init patches
  
  set alternative-names [ "C" "D" "P" "E"]
  set alternative-colors [ blue red green yellow ]
  set alternative-ids [ 0 1 ]
  if enable-punishment [ set alternative-ids lput 2 alternative-ids ]
  if enable-exit [ set alternative-ids lput 3 alternative-ids ]
  
  set patches-list [self] of patches
  set n-groups floor (n-patches ^ 2 / group-size)
  
  set groups []
  set group-fields n-values (n-groups) [0]
  set group-fields-sum n-values (n-groups) [0]
  ifelse spatial [
    let group-structure []
    let fixed-patches patches-list
    let i 0
    while [i < n-groups] [
      set i i + 1
      set group-structure lput ql:create-group map [(list ? alternative-ids)] sublist fixed-patches 0 group-size group-structure
      set groups lput sublist fixed-patches 0 group-size groups
      set fixed-patches sublist fixed-patches group-size length fixed-patches
    ]
    ql:set-group-structure group-structure
  ] [
    set next-groups get-next-groups
    set updated true  
  ] 
  
  set nextTick 0
  
  reset-ticks
end

to-report get-next-groups
  set groups []
  let group-structure []
  let random-patches shuffle patches-list
  let i 0
  while [i < n-groups] [
    set i i + 1
    set group-structure lput ql:create-group map [(list ? alternative-ids)] sublist random-patches 0 group-size group-structure
    set groups lput sublist random-patches 0 group-size groups
    set random-patches sublist random-patches group-size length random-patches
  ]
  report group-structure
end

to-report get-groups
  ifelse updated [
    set updated false
    report next-groups
  ] [
    set updated false
    report get-next-groups
  ]
end


to-report get-rewards [ env-id ]
  let dec-list ql:get-group-list env-id
  let result map [reward ?] dec-list
  report result
end

to-report reward [group-choice]
  
  let agents ql:get-agents group-choice
  let decisions ql:get-decisions group-choice
  let n 0
  let nc 0
  let np 0
  let field sum decisions
  (foreach agents decisions [
    ask ?1 [
      set last-action ?2
      set last-field field
      set pcolor item ?2 alternative-colors
      if (?2 != 3) [set n n + 1]
      if (?2 = 0 or ?2 = 2) [set nc nc + 1]
      if (?2 = 2) [set np np + 1]
    ]
  ])
  
  let nd n - nc 
  let pool 0
  if n > 0 [ set pool nc * r-by-n * group-size / n ]
  let rewards (list (pool - 1) (pool - np * s) (pool - 1 - nd * c-by-s * s) (l-by-r-1 * (group-size * r-by-n - 1)) )

  report ql:set-rewards group-choice map [ item ? rewards ] decisions
  
end

to wait-for-tick
  set nextTick nextTick + 1
  ;set mean-group-fields mean group-fields
  ;set group-fields map [[last-field] of (first ?)] groups
  while [ticks < nextTick] [
    
    ;set group-fields map [[last-field] of (first ?)] groups
    ifelse not spatial [
      set next-groups get-next-groups
      set updated true
    ] [
      set updated random 1
    ]
  ]
  ;while [ticks < nextTick] [ 
  ;  update-slow
  ;]
end

to update
  ;update-slow
  tick
end

to update-slow
  
  set group-fields map [[last-field] of (first ?)] groups
  set group-fields-sum (map [?2 + [last-field] of (first ?1)] groups group-fields-sum)
  ;let idx ([last-field] of first first groups)
  ;set group-fields replace-item idx group-fields (1 + (item idx group-fields))
  
  ask patches [
    let total-n sum frequencies
    ifelse (total-n > 0) [
      set rel-freqs map [? / total-n] frequencies
    ] [
      set rel-freqs [0 0 0 0]
    ]
    set q-values-std 0
    if (total-n > 0) [
      let zipped (map [ (list ?1 ?2) ] q-values frequencies)
      let filtered filter [ ( filter? (item 1 ?) total-n) ] zipped
      let qvs map [ (item 0 ?) / group-size / r-by-n ] filtered
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
    set expectation experimenting / 2
  ]
  report 2.33 < (n / total-n - expectation) * (sqrt total-n) / (sqrt (expectation * (1 - expectation)))
end
@#$#@#$#@
GRAPHICS-WINDOW
585
10
995
441
-1
-1
20.0
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
19
0
19
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
n-patches
n-patches
1
100
20
1
1
^2
HORIZONTAL

BUTTON
395
25
530
58
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
395
60
530
93
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
395
95
530
128
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
215
93
experimenting
experimenting
0
16
0.1
0.05
1
NIL
HORIZONTAL

PLOT
30
210
215
345
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
"default" 0.05 1 -16777216 true "" "histogram [q-values-std] of patches"

MONITOR
30
350
215
395
mean [q-values-std]
mean [q-values-std] of patches
2
1
11

PLOT
220
210
390
345
freq-hist
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
"default" 1.0 1 -16777216 true "set-plot-x-range 0 1 + max alternative-ids" "histogram [last-action] of patches"

SLIDER
220
25
390
58
group-size
group-size
2
20
20
1
1
NIL
HORIZONTAL

BUTTON
30
405
215
440
NIL
update-slow\n
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
220
60
390
93
r-by-n
r-by-n
0
0.9
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
220
95
390
128
s
s
0
1
1
0.1
1
NIL
HORIZONTAL

MONITOR
220
350
390
395
mean [last-action]
mean [last-action] of patches
17
1
11

SWITCH
395
200
570
233
spatial
spatial
0
1
-1000

SLIDER
220
130
390
163
c-by-s
c-by-s
0
1
0.75
0.05
1
NIL
HORIZONTAL

SLIDER
220
165
390
198
l-by-r-1
l-by-r-1
0
1
0.5
0.1
1
NIL
HORIZONTAL

SWITCH
395
130
570
163
enable-punishment
enable-punishment
1
1
-1000

SWITCH
395
165
570
198
enable-exit
enable-exit
1
1
-1000

PLOT
1110
465
1630
760
plot-rate-def
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
"default" 1.0 0 -16777216 true "" "if (ticks mod 10) = 0\n[plot count patches with [last-action = 1] / (n-patches ^ 2)]"

BUTTON
395
235
570
268
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

MONITOR
1210
20
1622
65
NIL
length filter [? > mean group-fields] group-fields
17
1
11

MONITOR
1210
70
1622
115
NIL
length filter [? < mean group-fields] group-fields
17
1
11

PLOT
1005
180
1620
440
plot 1
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
"default" 1.0 0 -16777216 true "" "plot length filter [? > mean-group-fields] group-fields"
"pen-1" 1.0 0 -7500403 true "" "plot length filter [? < mean-group-fields] group-fields"

PLOT
1000
20
1200
170
plot 2
NIL
NIL
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram group-fields"

PLOT
395
275
575
425
plot 3
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
"default" 0.05 1 -16777216 true "" "histogram [first rel-freqs] of patches"

BUTTON
245
530
377
563
NIL
wait-for-tick
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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
  <experiment name="exp-pri-plain-gs05" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs05" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs20-coop-dev" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000000</exitCondition>
    <metric>count patches with [last-action = 0]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs20-qvalues" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 50000</exitCondition>
    <metric>ticks</metric>
    <metric>map [[first q-values] of ?] first groups</metric>
    <metric>map [[last q-values] of ?] first groups</metric>
    <metric>map [[first rel-freqs] of ?] first groups</metric>
    <metric>map [[first q-values] of ?] item 1 groups</metric>
    <metric>map [[last q-values] of ?] item 1 groups</metric>
    <metric>map [[first rel-freqs] of ?] item 1 groups</metric>
    <metric>map [[first q-values] of ?] item 2 groups</metric>
    <metric>map [[last q-values] of ?] item 2 groups</metric>
    <metric>map [[first rel-freqs] of ?] item 2 groups</metric>
    <metric>map [[first q-values] of ?] item 3 groups</metric>
    <metric>map [[last q-values] of ?] item 3 groups</metric>
    <metric>map [[first rel-freqs] of ?] item 3 groups</metric>
    <metric>map [[first q-values] of ?] item 4 groups</metric>
    <metric>map [[last q-values] of ?] item 4 groups</metric>
    <metric>map [[first rel-freqs] of ?] item 4 groups</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs20-decay" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:decay-exploration
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-gs20-rel-freqs" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>ticks</metric>
    <metric>group-fields</metric>
    <metric>group-fields-sum</metric>
    <metric>[first rel-freqs] of patches</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs05-coop-dev-r2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs05-coop-dev-r4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0.25"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs20-coop-dev-r4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0.25"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-punish-gs20-coop-dev-r16" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0.25"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-exit-gs05" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="exp-pri-exit-gs20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 990 [update-slow]
wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="exp-pri-exit-gs05-coop-dev-r3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="exp-pri-punish-long" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 25000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
      <value value="0.5"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-plain-long" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 25000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp-pri-exit-gs20-spatial-long" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 24990 [update-slow]</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 25000</exitCondition>
    <metric>ticks</metric>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="exp-pri-exit-gs05-spatial-long" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
ql:start</setup>
    <go>if ticks &gt; 24990 [update-slow]</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 25000</exitCondition>
    <metric>ticks</metric>
    <metric>mean [q-values-std] of patches</metric>
    <metric>standard-deviation [q-values-std] of patches</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="exp-pri-exit-gs20-coop-dev-r12" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ql:start</setup>
    <go>wait-for-tick</go>
    <final>ql:stop 
wait 1</final>
    <exitCondition>ticks &gt; 100000</exitCondition>
    <metric>ticks</metric>
    <metric>count patches with [last-action = 0]</metric>
    <metric>count patches with [last-action = 1]</metric>
    <metric>count patches with [last-action = 2]</metric>
    <metric>count patches with [last-action = 3]</metric>
    <enumeratedValueSet variable="n-patches">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration-method">
      <value value="&quot;epsilon-greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experimenting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-exit">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-punishment">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-by-n">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c-by-s">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="l-by-r-1" first="0.1" step="0.2" last="0.9"/>
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
