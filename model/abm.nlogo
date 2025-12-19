;==============================================================
;                          MODEL
;==============================================================
; School racial composition and the emergence of Black-White within-school inequalities
; Joao M. Souto-Maior, Stanford University
; Last updated on August 2024

;==================================================================
;                         CITATION
;==================================================================
; Souto-Maior, J. M. (2025). School racial composition and the emergence of Black-White
; within-school inequalities: network-based foundations. The Journal of Mathematical Sociology, 1-42.

;==================================================================
;                       LICENSE DETAILS
;==================================================================
; Copyright © 2025 João M. Souto-Maior
; This NetLogo model is licensed under the Creative Commons
; Attribution–NonCommercial 4.0 International License (CC BY-NC 4.0).

;==================================================================
;                      MAIN VARIABLES
;==================================================================

extensions [csv]
undirected-link-breed [edges edge]

globals [
  ;================== general global variables
  dataset
  school
  filled-spots
  p-enrollment_w
  p-enrollment_b
  sim-error-squared

  ;================== outcome variable of interest
  pct-whites-enroll
  pct-blacks-enroll
  sim-wb-enrollment-gap
  true-wb-enrollment-gap
  sim-odds-whites-enrollment
  sim-odds-blacks-enrollment
  bw-OR

  ;================== input variables from data
  school-name
  district-location
  district-type
  school-pct-black-white
  total-spots
  n-whites
  n-blacks
  pct-blacks
  pct-acad-blacks
  pct-info-blacks
  pct-belonging-blacks
  p-belonging-given-info_w
  p-belonging-given-info_b
  pct-filled-spots

  ;================== network variables
  population-density
  true-avg-same-color-ties-blacks
  true-avg-same-color-ties-whites
  true-avg-same-color-ties-total
  sim-avg-same-color-ties-whites
  sim-avg-same-color-ties-blacks
  sim-avg-same-color-ties-total
  sim-avg-n-ties-blacks
  sim-avg-n-ties-whites
  sim-avg-n-ties-total
  true-IH-blacks
  true-IH-whites
  sim-IH-blacks
  sim-IH-whites
  true-IH-total
  sim-IH-total
  p-diffusion-intra
  odds-whites
  odds-blacks
  odds-same-color-ties
]

turtles-own [
  race
  info
  belonging
  enrolled?
  total-ties
  diff-color-ties
  same-color-ties
  n-ties-to-create
]

;==================================================================
;                      SET UP ENVIRONMENT
;==================================================================

to setup
  clear-all
  reset-ticks

  ifelse specific-seed = TRUE ; for sensitivity chekcs: avoid randomness in the network formation procedure
  [
    with-local-randomness [
      random-seed run-seed
      create-school
      create-agents
      create-network
    ]
  ]
  [
    create-school
    create-agents
    create-network
  ]

  ask links [
    set thickness 0
    set color [0 0 0 25]
  ]
end

;==================================================================
;                      CREATE SCHOOL
;==================================================================

to create-school
  file-open "data_model_initialization.csv"
  set dataset csv:from-file "data_model_initialization.csv"

  ;================== general variables
  set school item (school-number) dataset
  set school-name (word "School " school-number)
  if school-number = 531 [
    set school-name "Average school (default)"
  ]

  set district-type item 2 school
  set district-location item 3 school
  set school-pct-black-white item 7 school

  ifelse n-manual = FALSE
  [set number-students item 4 school]
  [set number-students number-students]

  ifelse comp-manual = FALSE
  [set pct-whites (item 5 school) / 100]
  [set pct-whites pct-whites]

  set n-whites round(pct-whites * number-students)
  set n-blacks number-students - n-whites

  ifelse pct-open-manual = FALSE
  [set pct-open-spots (item 14 school) / 100]
  [set pct-open-spots pct-open-spots]

  set total-spots round(pct-open-spots * number-students)
  set filled-spots []

  ;================== academic background
  ifelse pct-acad-manual = FALSE
  [set pct-acad-whites (item 8 school) / 100]
  [set pct-acad-whites pct-acad-whites]

  ifelse ratio-acad-manual = FALSE
  [set bw-acad-ratio (item 10 school) / 100]
  [set bw-acad-ratio bw-acad-ratio]

  if pct-acad-whites >= pct-acad-blacks [
    set p-enrollment_w 100
    set p-enrollment_b bw-enroll-prob-ratio * 100 * bw-acad-ratio
  ]

  if pct-acad-whites < pct-acad-blacks [
   set p-enrollment_b bw-enroll-prob-ratio * 100
   set p-enrollment_w 100 * (pct-acad-whites / pct-acad-blacks)
  ]

  ;================== belonging
  ifelse pct-belonging-manual = FALSE
  [set pct-belonging-whites precision(((item 8 school) / 100) ^ 2) 2]
  [set pct-belonging-whites pct-belonging-whites]

  ifelse ratio-belonging-manual = FALSE
  [set bw-belonging-ratio precision((((item 9 school) / 100) ^ 2) / (((item 8 school) / 100) ^ 2)) 2]
  [set bw-belonging-ratio bw-belonging-ratio]

  ;================== information access variables
  ifelse pct-info-manual = FALSE
  [set pct-info-whites precision(((item 11 school) / 100) ^ 2) 2] ;
  [set pct-info-whites pct-info-whites]

  ifelse ratio-info-manual = FALSE
  [set bw-info-ratio precision((((item 12 school) / 100) ^ 2) /  (((item 11 school) / 100) ^ 2)) 2]
  [set bw-info-ratio bw-info-ratio]

  ;================== outcome variable
  set true-wb-enrollment-gap item 17 school
  file-close-all

  ;================== calculation of variables for black students
  set pct-blacks 1 - pct-whites
  set pct-acad-blacks pct-acad-whites * bw-acad-ratio
  set pct-info-blacks pct-info-whites * bw-info-ratio
  set pct-belonging-blacks pct-belonging-whites * bw-belonging-ratio

  ;================== network characteristics
  ifelse manual-IH = FALSE
  [
    set true-IH-blacks 0.032 + 2.15 * pct-blacks - 2.35 * (pct-blacks ^ 2)
    set true-IH-whites 0.032 + 2.15 * pct-whites - 2.35 * (pct-whites ^ 2)
  ]
  [
    set true-IH-blacks chosen-IH
    set true-IH-whites chosen-IH
  ]
  set true-avg-same-color-ties-blacks true-IH-blacks * (1 - pct-blacks) + pct-blacks
  set true-avg-same-color-ties-whites true-IH-whites * (1 - pct-whites) + pct-whites
  set p-diffusion-intra 100

  ;================== change color of patches
  ask patches [
    set pcolor white
  ]

  ;================== school desity calculation ;only for vizualization purposes (does not influence outcomes of the model)
  set population-density 0.6
  let total-patches number-students / population-density ; to make sure that population-density is fixed when we vary school size
  let new-world-width sqrt total-patches
  resize-world 0 new-world-width 0 new-world-width
end

;==================================================================
;                      CREATE AGENTS
;==================================================================

to create-agents
  set-default-shape turtles "circle"

  create-turtles n-whites [
    set race "White"
    set color green
  ]

  create-turtles n-blacks [
    set race "Black"
    set color orange
  ]

  ask turtles [
    set size 0.4
    set enrolled? 0
    set info 0
    set belonging 0
    let empty-patches patches with [not any? turtles-here]
    move-to one-of empty-patches
  ]

  if DB = FALSE and DI = FALSE [
    ask turtles [
      set belonging 1
      set info 1
    ]
  ]

  if DB = TRUE and DI = FALSE [
        ask turtles [
      set info 1
    ]
    access-to-belonging
  ]

  if DB = FALSE and DI = TRUE [
    ask turtles [
      set belonging 1
    ]
    access-to-info
  ]

  if DB = TRUE and DI = TRUE [
    access-to-info
    access-to-belonging-given-info
  ]

end

;================== initial access to information
to access-to-info
  let n_w round(pct-info-whites * n-whites)
  if n_w > 0 [
    ask n-of n_w turtles with [race = "White"][
      set info 1
    ]
  ]

  let n_b round(pct-info-blacks * n-blacks)
  if n_b > 0 [
    ask n-of n_b turtles with [race = "Black"][
      set info 1
    ]
  ]
end

;================== initial access to belonging
to access-to-belonging
  let n-belonging-whites round(pct-belonging-whites * n-whites)
  if n-belonging-whites > 0 [
    ask n-of n-belonging-whites turtles with [race = "White"][
      set belonging 1
    ]
  ]
  let n-belonging-blacks round(pct-belonging-blacks * n-blacks)
  if n-belonging-blacks > 0 [
    ask n-of n-belonging-blacks turtles with [race = "Black"][
      set belonging 1
    ]
  ]
end

;================== initial access to belonging given access to information
to access-to-belonging-given-info
  if pct-belonging-whites >= pct-belonging-blacks [
    set p-belonging-given-info_w 1
    set p-belonging-given-info_b pct-belonging-blacks / pct-belonging-whites
  ]

  if pct-belonging-blacks >= pct-belonging-whites [
    set p-belonging-given-info_b 1
    set p-belonging-given-info_w pct-belonging-whites / pct-belonging-blacks
  ]

  let n-belonging-whites round(p-belonging-given-info_w * (count turtles with [color = green and info = 1]))
  if n-belonging-whites > 0 [
    ask n-of n-belonging-whites turtles with [color = green and info = 1][
      set belonging 1
    ]
  ]

  let n-belonging-blacks round(p-belonging-given-info_b * (count turtles with [race = "Black" and info = 1]))
  if n-belonging-blacks > 0 [
    ask n-of n-belonging-blacks turtles with [race = "Black" and info = 1][
      set belonging 1
    ]
  ]
end

;==================================================================
;                        GO PROCEDURES
;==================================================================
to go
  ifelse model-type = "Network formation only"
  [
    stop
  ]
  [
    tick
    assessment-of-eligibility
    update-stats-course

    if length filled-spots >= total-spots [
      stop
    ]

    if DI = TRUE [
      diffusion-of-info
    ]
    if DB = TRUE [
      diffusion-of-belonging
    ]
  ]
end

;==================================================================
;                        NETWORK FORMATION
;==================================================================

to form-new-tie-blacks
  let my-color color
  ifelse manual-n-ties = FALSE
  [set n-ties-to-create random-float (5 + 6 * pct-blacks)]
  [set n-ties-to-create random-float chosen-n-ties]

  repeat n-ties-to-create
  [
    ifelse random-float 100 <= (100 * true-avg-same-color-ties-blacks)
    [
      let already-ties edge-neighbors
      let non-ties other turtles with [(not member? self already-ties)]
      let candidates non-ties
      let same-race-candidates (candidates with [color = my-color])
      if any? same-race-candidates [
        let same-race-tie one-of same-race-candidates
        create-edge-with same-race-tie
      ]
    ]
    [
      let already-ties edge-neighbors
      let non-ties other turtles with [(not member? self already-ties)]
      let candidates non-ties
      let diff-race-candidates (candidates with [color != my-color])
      if any? diff-race-candidates [
        let diff-race-tie one-of diff-race-candidates
        create-edge-with diff-race-tie
      ]
    ]
  ]
end

to form-new-tie-whites
  let my-color color
  ifelse manual-n-ties = FALSE
  [set n-ties-to-create random-float (5 + 6 * pct-whites)]
  [set n-ties-to-create random-float chosen-n-ties]

  repeat n-ties-to-create
  [
    ifelse random-float 100 <= (100 * true-avg-same-color-ties-whites)
    [
      let already-ties edge-neighbors
      let non-ties other turtles with [(not member? self already-ties)]
      let candidates non-ties
      let same-race-candidates (candidates with [color = my-color])
      if any? same-race-candidates [
        let same-race-tie one-of same-race-candidates
        create-edge-with same-race-tie
      ]
    ]
    [
      let already-ties edge-neighbors
      let non-ties other turtles with [(not member? self already-ties)]
      let candidates non-ties
      let diff-race-candidates (candidates with [color != my-color])
      if any? diff-race-candidates [
        let diff-race-tie one-of diff-race-candidates
        create-edge-with diff-race-tie
      ]
    ]
  ]
end

to create-network
  ask turtles [
    if race = "Black" [
      form-new-tie-blacks
    ]
    if race = "White" [
      form-new-tie-whites
    ]
  ]
  update-network-ties
  update-network-stats
end

to update-network-ties
ask turtles [
    let my-ties edge-neighbors
    if any? my-ties [
      set same-color-ties count my-ties with [color = [color] of myself]
      set diff-color-ties count my-ties with [color != [color] of myself]
      set total-ties count my-ties
    ]
  ]
end

to update-network-stats
  ;; odds of same color friendship nomination
  let s_1 sum [same-color-ties] of turtles
  let c_1 sum [diff-color-ties] of turtles
  let all (number-students * (number-students - 1)) / 2
  let all-white (n-whites * (n-whites - 1)) / 2
  let all-black (n-blacks * (n-blacks - 1)) / 2
  let all-same all-black + all-white
  let all-cross all - all-same
  let c_0 all-cross - c_1 ;all potential cross-color ties that do not become cross ties
  let s_0 all-same - s_1 ;all potential same-color ties that do not become cross ties
  ifelse (c_1 * s_0) > 0
  [
    set odds-same-color-ties (s_1 * c_0) / (c_1 * s_0)
  ]
  [
    set odds-same-color-ties "NA"
  ]

  ;; simulated avg same color ties by race
  let s_w sum [same-color-ties] of turtles with [race = "White"]
  let t_w sum [total-ties] of turtles with [race = "White"]
  if t_w > 0 [
    set sim-avg-same-color-ties-whites (s_w / t_w)
  ]
  let s_b sum [same-color-ties] of turtles with [race = "Black"]
  let t_b sum [total-ties] of turtles with [race = "Black"]

  if t_b > 0 [
    set sim-avg-same-color-ties-blacks (s_b / t_b)
  ]

  let s sum [same-color-ties] of turtles
  let t sum [total-ties] of turtles

  if t > 0 [
    set sim-avg-same-color-ties-total (s / t)
  ]

  set sim-avg-n-ties-whites ((sum [total-ties] of turtles with [race = "White"]) / n-whites)
  set sim-avg-n-ties-blacks ((sum [total-ties] of turtles with [race = "Black"]) / n-blacks)
  set sim-avg-n-ties-total ((sum [total-ties] of turtles / (n-blacks + n-whites)))
  set sim-IH-whites ((sim-avg-same-color-ties-whites) - pct-whites) / (1 - pct-whites)
  set sim-IH-blacks ((sim-avg-same-color-ties-blacks)- pct-blacks) / (1 - pct-blacks)
end

;==================================================================
;                     DIFFUSION OF BELONGING
;==================================================================
to diffusion-of-belonging

  ask turtles with [belonging = 0][
    let influential-ties edge-neighbors with [enrolled? = 1]
    if any? influential-ties [
      ifelse any? influential-ties with [race = [race] of myself]
      [
        if random-float 100 <= p-diffusion-intra [
          set belonging 1
        ]
      ]
      [
        if any? influential-ties with [race != [race] of myself]
        [
          if random-float 100 <= p-diffusion-inter
          [
            set belonging 1
          ]
        ]
      ]
    ]
  ]
end

;==================================================================
;                   DIFFUSION OF INFORMATION
;==================================================================
to diffusion-of-info
  let influential-ties turtles with [info = 1]
  if count influential-ties > 0 [
  ask influential-ties [
      let race-of-my-friend race
      let my-ties edge-neighbors with [info = 0]
      if any? my-ties
      [
        ask one-of my-ties [
          ifelse race = race-of-my-friend
          [
            if random-float 100 <= p-diffusion-intra [
              set info 1
            ]
          ]
          [
            if random-float 100 <= p-diffusion-inter [
              set info 1
            ]
          ]
        ]
      ]
    ]
  ]
end

;==================================================================
;                  ASSESSMENT OF ELIGIBILITY
;==================================================================
to assessment-of-eligibility
  let available-spots total-spots - length filled-spots

  if available-spots > 0 [
    let eligible-agents turtles with [enrolled? = 0 and info = 1 and belonging = 1]

    if any? eligible-agents [
      ifelse (count eligible-agents) <= available-spots
      [ask eligible-agents [enrollment]]
      [ask n-of available-spots eligible-agents [enrollment]]
    ]

    if ticks > 1000 [
      ask n-of available-spots turtles [enrollment]
    ]
  ]
end

;==================================================================
;                         ENROLLMENT
;==================================================================
to enrollment
  if race = "White" [
    if random-float 100 < p-enrollment_w [
      set filled-spots lput 0 filled-spots
      set enrolled? 1
    ]
  ]

  if race = "Black" [
    if random-float 100 < p-enrollment_b [
      set filled-spots lput 1 filled-spots
      set enrolled? 1
    ]
  ]
end

;==================================================================
;                    UPDATE PLOTS / STATS
;==================================================================
to update-stats-course
if length filled-spots >= 1 [

    ;==================== course characteristics
    let course-size length filled-spots
    let pct-program-black mean filled-spots
    let pct-program-white 1 - mean filled-spots
    set pct-filled-spots course-size / total-spots

    ;==================== number enrolled by group
    let n-blacks-enroll pct-program-black * course-size
    let n-whites-enroll pct-program-white * course-size

    ;==================== rates of enrollment by race
    set pct-blacks-enroll n-blacks-enroll / n-blacks
    set pct-whites-enroll n-whites-enroll / n-whites

    ;==================== compare model outcomes with true values
    set sim-wb-enrollment-gap 100 * (pct-whites-enroll - pct-blacks-enroll)
    let sim-error (sim-wb-enrollment-gap - true-wb-enrollment-gap)
    set sim-error-squared (sim-error) ^ 2

    ;===================== calculate odds of enrollment for whites
    ifelse n-whites-enroll > 0
    [
      ifelse (n-whites - n-whites-enroll) > 0
      [set sim-odds-whites-enrollment (n-whites-enroll / (n-whites - n-whites-enroll))]
      [set sim-odds-whites-enrollment "NA"]
    ]
    [
      set sim-odds-whites-enrollment 0
    ]

    ;===================== calculate odds of enrollment for blacks
    ifelse n-blacks-enroll > 0
    [
      ifelse (n-blacks - n-blacks-enroll) > 0
      [set sim-odds-blacks-enrollment (n-blacks-enroll / (n-blacks - n-blacks-enroll))]
      [set sim-odds-blacks-enrollment "NA"]
    ]
    [
      set sim-odds-blacks-enrollment 0
    ]

    ;===================== calculate black-white odds ratio
    ifelse sim-odds-whites-enrollment != "NA" and sim-odds-blacks-enrollment != "NA" and sim-odds-whites-enrollment > 0
    [set bw-OR sim-odds-blacks-enrollment / sim-odds-whites-enrollment]
    [set bw-OR "NA"]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
365
210
718
564
-1
-1
18.2
1
14
1
1
1
0
1
1
1
0
18
0
18
1
1
1
ticks
30.0

BUTTON
365
165
440
198
setup
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
530
165
620
198
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
440
165
530
198
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
915
235
1110
280
Filled spots
length filled-spots
17
1
11

SWITCH
365
50
455
83
DI
DI
1
1
-1000

SWITCH
455
50
545
83
DB
DB
1
1
-1000

SLIDER
365
120
545
153
bw-enroll-prob-ratio
bw-enroll-prob-ratio
0
2
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
25
210
345
243
school-number
school-number
1
531
530.0
1
1
NIL
HORIZONTAL

SLIDER
200
460
350
493
pct-whites
pct-whites
0
1
0.94
0.01
1
NIL
HORIZONTAL

MONITOR
25
250
145
295
NIL
school-name
17
1
11

SLIDER
200
425
350
458
number-students
number-students
0
2000
215.0
10
1
NIL
HORIZONTAL

SLIDER
200
495
350
528
pct-open-spots
pct-open-spots
0
1
0.22
0.01
1
NIL
HORIZONTAL

SLIDER
200
670
350
703
bw-info-ratio
bw-info-ratio
-1
1
0.33
0.01
1
NIL
HORIZONTAL

SLIDER
200
565
350
598
pct-info-whites
pct-info-whites
0
1
0.59
0.01
1
NIL
HORIZONTAL

CHOOSER
25
50
280
95
model-type
model-type
"Network formation only" "Full"
1

SLIDER
200
705
350
738
bw-belonging-ratio
bw-belonging-ratio
-100
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
200
600
350
633
pct-belonging-whites
pct-belonging-whites
0
1
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
200
530
350
563
pct-acad-whites
pct-acad-whites
0
1
0.14
0.01
1
NIL
HORIZONTAL

SLIDER
200
635
350
668
bw-acad-ratio
bw-acad-ratio
-1
1
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
145
250
235
295
NIL
district-location
17
1
11

MONITOR
915
280
1110
325
NIL
pct-blacks-enroll
3
1
11

MONITOR
735
280
915
325
NIL
pct-whites-enroll
3
1
11

MONITOR
735
235
915
280
NIL
total-spots
17
1
11

PLOT
735
370
1110
590
White-Black relative risk of enrollment
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Simulated" 1.0 0 -7500403 true "" "plot sim-wb-enrollment-gap"
"True" 1.0 0 -2674135 true "" "plot true-wb-enrollment-gap"

MONITOR
235
250
345
295
NIL
district-type
17
1
11

SWITCH
25
425
200
458
n-manual
n-manual
1
1
-1000

SWITCH
25
460
200
493
comp-manual
comp-manual
1
1
-1000

SWITCH
25
495
200
528
pct-open-manual
pct-open-manual
1
1
-1000

SWITCH
25
565
200
598
pct-info-manual
pct-info-manual
1
1
-1000

SWITCH
25
600
200
633
pct-belonging-manual
pct-belonging-manual
1
1
-1000

SWITCH
25
635
200
668
ratio-acad-manual
ratio-acad-manual
1
1
-1000

SWITCH
25
670
200
703
ratio-info-manual
ratio-info-manual
1
1
-1000

SWITCH
25
705
200
738
ratio-belonging-manual
ratio-belonging-manual
1
1
-1000

SWITCH
25
530
200
563
pct-acad-manual
pct-acad-manual
1
1
-1000

MONITOR
735
100
905
145
NIL
sim-avg-same-color-ties-whites
2
1
11

MONITOR
1075
100
1245
145
NIL
sim-avg-same-color-ties-blacks
2
1
11

MONITOR
735
55
905
100
NIL
true-avg-same-color-ties-whites
2
1
11

MONITOR
1075
55
1245
100
NIL
true-avg-same-color-ties-blacks
2
1
11

MONITOR
915
325
1110
370
NIL
sim-wb-enrollment-gap
2
1
11

MONITOR
905
55
1075
100
NIL
sim-avg-n-ties-whites
2
1
11

MONITOR
905
100
1075
145
NIL
sim-avg-n-ties-blacks
2
1
11

MONITOR
735
145
905
190
NIL
odds-same-color-ties
2
1
11

MONITOR
735
325
915
370
NIL
true-wb-enrollment-gap
3
1
11

SLIDER
365
85
545
118
p-diffusion-inter
p-diffusion-inter
0
100
100.0
1
1
NIL
HORIZONTAL

SWITCH
25
300
175
333
manual-IH
manual-IH
1
1
-1000

SLIDER
175
300
347
333
chosen-IH
chosen-IH
0
1
0.0
0.1
1
NIL
HORIZONTAL

SWITCH
25
335
175
368
manual-n-ties
manual-n-ties
0
1
-1000

SLIDER
175
335
347
368
chosen-n-ties
chosen-n-ties
0
10
8.0
1
1
NIL
HORIZONTAL

SWITCH
25
95
205
128
specific-seed
specific-seed
1
1
-1000

INPUTBOX
205
95
280
155
run-seed
211995.0
1
0
Number

TEXTBOX
30
385
330
416
Manually define each initial school characteristic (optional)
13
0.0
1

TEXTBOX
735
25
975
56
Characteristics of simulated network
13
0.0
1

TEXTBOX
735
205
940
236
Simulated enrollment patterns
13
0.0
1

TEXTBOX
30
170
290
231
Select empirical school for initializaton\n(school 531 = average school)
13
0.0
1

TEXTBOX
365
25
605
140
Parameters for mechanisms of interest
13
0.0
1

TEXTBOX
25
30
235
61
Choose type of model to run
13
0.0
1

@#$#@#$#@
## Model

* School racial composition and the emergence of Black-White within-school inequalities
* Joao M. Souto-Maior, Stanford University
* Last updated on August 2024 

## Citation
 
Souto-Maior, J. M. (2025). School racial composition and the emergence of Black-White within-school inequalities: network-based foundations. The Journal of Mathematical Sociology, 1-42.

## LICENSE DETAILS

Copyright © 2025 João M. Souto-Maior

This NetLogo model is licensed under the Creative Commons Attribution–NonCommercial 4.0 International License (CC BY-NC 4.0).
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
setup
repeat 20 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="simulated-pattern_empirical-conditions" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-wb-enrollment-gap</metric>
    <metric>true-wb-enrollment-gap</metric>
    <metric>pct-blacks-enroll</metric>
    <metric>pct-whites-enroll</metric>
    <metric>bw-OR</metric>
    <metric>pct-filled-spots</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.05" step="0.01" last="0.95"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="network-validation" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-avg-n-ties-whites</metric>
    <metric>sim-avg-n-ties-blacks</metric>
    <metric>sim-avg-n-ties-total</metric>
    <metric>sim-avg-same-color-ties-total</metric>
    <metric>sim-avg-same-color-ties-whites</metric>
    <metric>sim-avg-same-color-ties-blacks</metric>
    <metric>odds-same-color-ties</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Network formation only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.01" step="0.05" last="0.99"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="empirical-validation" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-wb-enrollment-gap</metric>
    <metric>true-wb-enrollment-gap</metric>
    <metric>pct-blacks-enroll</metric>
    <metric>pct-whites-enroll</metric>
    <metric>bw-OR</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="school-number" first="1" step="1" last="530"/>
    <enumeratedValueSet variable="manual-IH">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-whites">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="simulated-pattern-no-n-ties-no-homophily-variation" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-wb-enrollment-gap</metric>
    <metric>true-wb-enrollment-gap</metric>
    <metric>pct-blacks-enroll</metric>
    <metric>pct-whites-enroll</metric>
    <metric>bw-OR</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.05" step="0.01" last="0.95"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="network-validation-no-n-ties-no-homophily-variation" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-avg-n-ties-whites</metric>
    <metric>sim-avg-n-ties-blacks</metric>
    <metric>sim-avg-n-ties-total</metric>
    <metric>sim-avg-same-color-ties-total</metric>
    <metric>sim-avg-same-color-ties-whites</metric>
    <metric>sim-avg-same-color-ties-blacks</metric>
    <metric>odds-same-color-ties</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Network formation only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.01" step="0.05" last="0.99"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="in-group-favoritism" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>sim-wb-enrollment-gap</metric>
    <metric>true-wb-enrollment-gap</metric>
    <metric>pct-blacks-enroll</metric>
    <metric>pct-whites-enroll</metric>
    <metric>bw-OR</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.05" step="0.01" last="0.95"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-diffusion-inter" first="50" step="10" last="100"/>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="simulated-pattern_null-version" repetitions="100" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number + 123
setup</setup>
    <go>go</go>
    <metric>sim-wb-enrollment-gap</metric>
    <metric>true-wb-enrollment-gap</metric>
    <metric>pct-blacks-enroll</metric>
    <metric>pct-whites-enroll</metric>
    <metric>bw-OR</metric>
    <metric>pct-filled-spots</metric>
    <enumeratedValueSet variable="specific-seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="school-number">
      <value value="531"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-IH">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-IH">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-n-ties">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chosen-n-ties">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-type">
      <value value="&quot;Full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DI">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DB">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-students">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comp-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct-whites" first="0.05" step="0.01" last="0.95"/>
    <enumeratedValueSet variable="ratio-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-acad-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-belonging-ratio">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-info-ratio">
      <value value="0.47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-manual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-acad-whites">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-info-whites">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-belonging-whites">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-manual">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct-open-spots">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-diffusion-inter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw-enroll-prob-ratio">
      <value value="1"/>
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
