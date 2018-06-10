;;;======================================================
;;; Programa del Mundo de los Bloques
;;; Para ejecutarlo solamente carguelo en CLIPS, de reset y ejecutelo (run)
;;;======================================================
(defglobal ?*sal* = 0)
(defmodule MAIN (export ?ALL))
(defmodule CLEAR (import MAIN ?ALL))
(defmodule RECORRIDOS (import MAIN ?ALL))

(deftemplate MAIN::item
        (slot type)
        (slot name)
        (slot zone)
        (slot image)
        (slot attributes)
        (multislot pose)
        (slot belongs)
        (slot status)
)
(deftemplate MAIN::save-order
        (multislot objects)
)
(deftemplate MAIN::human
        (slot name)
        (slot zone)
        (slot image)
        (slot attributes)
        (multislot pose)
        (slot carring)
        (slot status)
)
(deftemplate MAIN::where-to
        (slot obj)
        (slot dest)
)
(deftemplate MAIN::robot
        (slot name)
        (slot zone)
        (slot image)
        (slot attributes)
        (multislot pose)
        (slot carring)
        (slot status)
)
(deftemplate MAIN::on-top-of
        (slot upper)
        (slot lower)
)
(deftemplate CLEAR::goal 
        (slot move)
        (slot on-top-of)
)
(deftemplate RECORRIDOS::auxiliar
        (slot valor)
)
(deftemplate RECORRIDOS::ir-a
        (slot zone)
)
(deftemplate RECORRIDOS::go-to
        (multislot pose)
)
(deftemplate RECORRIDOS::find-obj
        (slot obj)
        (slot order)
)

(deftemplate RECORRIDOS::grasp-obj
        (slot obj)
)
(deftemplate RECORRIDOS::release-obj
        (slot obj)
        (slot dest)
)
(deftemplate RECORRIDOS::take-obj
        (slot obj)
)
(deftemplate RECORRIDOS::change-door-state
        (slot status)
        (slot door)
        (slot obj)
)

(deffacts MAIN::initial-state
        ;;; Objects
        (item (type Objects) (name apple) (zone corridor) (image apple) (attributes pick) (pose 0.08 0.57 2.0))
        (item (type Objects) (name sushi) (zone corridor) (image sushi) (attributes pick) (pose 0.08 0.57 1.0))
        (item (type Objects) (name milk) (zone corridor) (image milk) (attributes pick) (pose 0.08 0.57 0.0))
        (item (type Objects) (name soap) (zone corridor) (image soap) (attributes pick) (pose 0.07 0.52 0.0))
        (item (type Objects) (name shampoo) (zone corridor) (image shampoo) (attributes pick) (pose 0.07 0.52 0.0))
        (item (type Objects) (name book) (zone deposit) (image book) (attributes pick) (pose 0.22 1.10 0.0))
        (item (type Objects) (name hammer) (zone deposit) (image hammer) (attributes pick) (pose 0.22 1.10 2.0))
        ;;; Stacks
        (on-top-of (upper nothing)(lower apple))
        (on-top-of (upper apple)(lower sushi))
        (on-top-of (upper sushi)(lower milk))
        (on-top-of (upper milk)(lower floor))
        (on-top-of (upper nothing)(lower hammer))
        (on-top-of (upper hammer)(lower book))
        (on-top-of (upper book)(lower floor))
        ;(goal (move A)(on-top-of E))

        ;;; Destinos
        (where-to (obj apple)(dest fridge))
        (where-to (obj sushi)(dest fridge))
        (where-to (obj milk)(dest fridge))
        (where-to (obj soap)(dest service_table))
        (where-to (obj shampoo)(dest service_table))
        (where-to (obj book)(dest Father))
        (where-to (obj hammer)(dest Mother))
        ;;;ROOMS
        (item (type Room) (name deposit) (pose 0.29 0.93 0.0))
        (item (type Room) (name kitchen) (pose 0.68 1.10 0.0))
        (item (type Room) (name corridor) (pose 0.39 0.58 0.0))
        (item (type Room) (name studio) (pose 0.13 0.24 0.0))
        (item (type Room) (name bedroom) (pose 0.40 0.28 0.0))
        (item (type Room) (name service) (pose 0.67 0.29 0.0))
        ;;; DOORS
        (item (type Door) (name fridgedoor) (zone kitchen) (status closed) (belongs fridge)(pose 0.72 0.89 0.0))
        (item (type Door) (name entrance) (zone corridor) (status closed) (belongs corridoor_wall) (pose 0.6 0.57 0.0))
        ;;; FURNITURE
        (item (type Furniture) (name bedroom_table) (zone bedroom) (pose 0.38 0.16 0.0))
        (item (type Furniture) (name deposit_table) (zone deposit) (pose 0.19 1.13 0.0))
        (item (type Furniture) (name fridge) (zone kitchen) (pose 0.65 0.82 0.0))
        (item (type Furniture) (name service_table) (zone service) (pose 0.63 0.17 0.0))
        ;;; HUMANS
        (human (name Mother) (zone bedroom) (pose 0.59 0.25 0.0))
        (human (name Father) (zone service) (pose 0.73 0.21 0.0))
        ;;; ROBOTS
        (robot (name Justina) (zone studio) (pose 0.09 0.16 0.0)(carring false)(status 0))

        ;;; ORDER
        
)
(defrule MAIN::initial-state
  =>
    (remove "/home/paconava/data.txt")
    (focus CLEAR RECORRIDOS)
    
)
(deffacts CLEAR::start
    (open "data.txt" mydata "w")
    (close mydata)
    (goal (move milk)(on-top-of floor))
    (goal (move apple)(on-top-of floor))
    (goal (move sushi)(on-top-of floor))

)
(deffacts RECORRIDOS::ejec
    (find-obj (obj milk)(order 0))
    (find-obj (obj apple)(order 1))
    (find-obj (obj sushi)(order 2))
    (find-obj (obj soap)(order 3))
    (find-obj (obj shampoo)(order 4))
    (auxiliar (valor ?*sal*))
)

(defrule RECORRIDOS::no-viaja
        ?destino <- (ir-a (zone ?zone))
        (item (type Room) (name ?zone))
        (robot (name Justina) (zone ?zone))
        =>
        (retract ?destino)
        (printout t "Justina ya se encuentra en " ?zone "." crlf)
)
(defrule RECORRIDOS::viaja
        ?destino <- (ir-a (zone ?zone))
        (item (type Room) (name ?zone)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x1 ?y1 ?z1))
        =>
        (retract ?destino ?pose1)
        (assert (robot (name Justina)(zone ?zone)(pose ?x2 ?y2 ?z2)))
        (printout t "Justina se movió de " ?actual " a " ?zone "." crlf)
)
(defrule RECORRIDOS::encuentra-obj
        ?val <- (auxiliar (valor ?val1))
        ?obj <- (find-obj (obj ?obj1)(order ?val1))
        (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x1 ?y1 ?z1)(carring false)(status 0))
        =>
        (bind ?*sal* (+ ?*sal* 1))
        (retract ?obj ?pose1 ?val)
        (assert (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring false)(status 1))
                (auxiliar (valor ?*sal*))
                (grasp-obj (obj ?obj1))
        )
        (printout t "Justina encontró " ?obj1 "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "find " ?obj1 crlf)
        (printout mydata "goto " ?x2 "," ?y2 "," ?z2 crlf)
        (close mydata)
)
(defrule RECORRIDOS::agarra-obj
        ?obj <- (grasp-obj (obj ?obj1))
        (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x2 ?y2 ?z2)(carring false)(status 1))
        =>
        (retract ?obj ?pose1)
        (assert (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring true)(status 1))
                (take-obj (obj ?obj1))
        )
        (printout t "Justina agarró " ?obj1 "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "grasp " ?obj1 crlf)
        (close mydata)
)
(defrule RECORRIDOS::lleva-obj-c-puerta
        ?take <- (take-obj (obj ?obj1))
        ?obj <- (item (type Objects)(name apple | sushi | milk)(zone ?zone2)(image ?obj1)(attributes pick)(pose ?x1 ?y1 ?z1))
        (where-to (obj ?obj1)(dest ?dest1))
        (item (type Furniture) (name ?dest1) (zone ?zone1) (pose ?x3 ?y3 ?z3))
        (item (type Door) (name ?name1) (zone ?zone1) (status closed) (belongs ?dest1)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?zone2) (pose ?x1 ?y1 ?z1)(carring true)(status 1))
        =>
        (retract ?take ?obj ?pose1)
        (assert (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
                (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring true)(status 1))
                (change-door-state (door ?name1)(status closed)(obj ?obj1))
        )
        (printout t "Justina llevó " ?obj1 " a " ?dest1 crlf)
        (open "data.txt" mydata "a")
        (printout mydata "goto " ?x2 "," ?y2 "," ?z2 crlf)
        (close mydata)
)
(defrule RECORRIDOS::lleva-obj-a-persona
        ?take <- (take-obj (obj ?obj1))
        ?obj <- (item (type Objects)(name ?obj1)(zone ?zone2)(image ?obj1)(attributes pick)(pose ?x1 ?y1 ?z1))
        (where-to (obj ?obj1)(dest ?dest1))
        (human (name ?dest1) (zone ?zone1) (pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?zone2) (pose ?x1 ?y1 ?z1)(carring true)(status 1))
        =>
        (retract ?take ?obj ?pose1)
        (assert (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
                (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring true)(status 1))
                (release-obj (obj ?obj1)(dest ?dest1))
        )
        (printout t "Justina llevó " ?obj1 " a " ?dest1 crlf)
        (open "data.txt" mydata "a")
        (printout mydata "goto " ?x2 "," ?y2 "," ?z2 crlf)
        (close mydata)
)

(defrule RECORRIDOS::lleva-obj-s-puerta
        ?take <- (take-obj (obj ?obj1))
        ?obj <- (item (type Objects)(name hammer | book | shampoo | soap)(zone ?zone2)(image ?obj1)(attributes pick)(pose ?x1 ?y1 ?z1))
        (where-to (obj ?obj1)(dest ?dest1))
        (item (type Furniture) (name ?dest1) (zone ?zone1) (pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?zone2) (pose ?x1 ?y1 ?z1)(carring true)(status 1))
        =>
        (retract ?take ?obj ?pose1)
        (assert (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
                (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring true)(status 1))
                (release-obj (obj ?obj1)(dest ?dest1))
        )
        (printout t "Justina llevó " ?obj1 " a " ?dest1 crlf)
        (open "data.txt" mydata "a")
        (printout mydata "goto " ?x2 "," ?y2 "," ?z2 crlf)
        (close mydata)
)
(defrule RECORRIDOS::suelta-obj-c-puerta
        ?rel <- (release-obj (obj ?obj1)(dest ?dest))
        (item (type Door) (name ?dest) (zone ?zone) (status open) (belongs ?which)(pose ?x2 ?y2 ?z2))
        ?obj <- (item (type Objects)(name apple | sushi | milk)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x2 ?y2 ?z2)(carring true)(status 1))
        =>
        (retract ?rel ?obj ?pose1)
        (assert (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring false)(status 1))
                (item (type Objects)(name ?obj1)(zone ?zone1)(image ?obj1)(attributes done)(pose ?x2 ?y2 ?z2))
                (change-door-state (door ?dest)(status open)(obj ?obj1))
        )
        (printout t "Justina soltó " ?obj1 "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "release " ?obj1 crlf)
        (close mydata)
)
(defrule RECORRIDOS::suelta-obj-s-puerta
        ?obj <- (release-obj (obj ?obj1)(dest ?dest))
        (item (type Objects)(name hammer | book | shampoo | soap)(zone ?zone1)(image ?obj1)(attributes pick)(pose ?x2 ?y2 ?z2))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x2 ?y2 ?z2)(carring true)(status 1))
        =>
        (retract ?obj ?pose1)
        (assert (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring false)(status 0)))
        (printout t "Justina soltó " ?obj1 "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "release " ?obj1 crlf)
        (close mydata)
)
(defrule RECORRIDOS::abre-puerta
        ?edo <- (change-door-state (door ?door)(status closed)(obj ?obj1))
        ?pose1 <- (robot (name Justina) (zone ?zone1) (pose ?x1 ?y1 ?z1)(carring true)(status 1))
        ?pta <- (item (type Door)(name ?door)(zone ?zone1)(status closed)(belongs ?which)(pose ?x2 ?y2 ?z2))
        =>
        (retract ?edo ?pta ?pose1)
        (assert (item (type Door)(name ?door)(zone ?zone1)(status open)(belongs ?which)(pose ?x2 ?y2 ?z2))
                (robot (name Justina) (zone ?zone1) (pose ?x2 ?y2 ?z2)(carring true)(status 1))
                (release-obj (obj ?obj1)(dest ?door))
        )
        (printout t "Justina abrió la puerta " ?door "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "open " ?door crlf)
        (close mydata)
)
(defrule RECORRIDOS::cierra-puerta
        ?edo <- (change-door-state (door ?door)(status open)(obj ?obj1))
        ?pta <- (item (type Door)(name ?door)(zone ?zone1)(status open)(belongs ?which)(pose ?x1 ?y1 ?z1))
        ?pose1 <- (robot (name Justina) (zone ?actual) (pose ?x2 ?y2 ?z2)(carring false)(status 1))
        =>
        (retract ?edo ?pta ?pose1)
        (assert (robot (name Justina)(zone ?zone1)(pose ?x2 ?y2 ?z2)(carring false)(status 0))
                (item (type Door)(name ?door)(zone ?zone1)(status closed)(belongs ?which)(pose ?x1 ?y1 ?z1)))
        (printout t "Justina cerró la puerta " ?door "." crlf)
        (open "data.txt" mydata "a")
        (printout mydata "close " ?door crlf)
        (close mydata)
)
(defrule CLEAR::move-directly
        ?goal <- (goal (move ?block1) (on-top-of ?block2))
        (item (type Objects) (name ?block1) (zone ?zone1) (image ?block1) (attributes pick) (pose ?x1 ?y1 ?z1))
        (item (type Objects) (name ?block2) (zone ?zone2) (image ?block2) (attributes pick) (pose ?x2 ?y2 ?z2))
        (on-top-of (upper nothing) (lower ?block1))
        ?stack-1 <- (on-top-of (upper ?block1)(lower ?block3))
        ?stack-2 <- (on-top-of (upper nothing)(lower ?block2))
        =>
        (retract ?goal ?stack-1 ?stack-2)
        (assert (on-top-of (upper ?block1)(lower ?block2))
        (on-top-of (upper nothing)(lower ?block3)))
        (printout t ?block1 " moved on top of " ?block2 "." crlf)
)
(defrule CLEAR::move-to-floor
        ?goal <- (goal (move ?block1) (on-top-of floor))
        (item (type Objects) (name ?block1) (zone ?zone1) (image ?block1) (attributes pick) (pose ?x1 ?y1 ?z1))
        (on-top-of (upper nothing) (lower ?block1))
        ?stack <- (on-top-of (upper ?block1) (lower ?block2))
        =>
        (retract ?goal ?stack)
        (assert (on-top-of (upper ?block1)(lower floor))
        (on-top-of (upper nothing)(lower ?block2)))
        (printout t ?block1 " moved on top of floor. " crlf)
)
(defrule CLEAR::clear-upper-block
        (goal (move ?block1))
        (item (type Objects) (name ?block1) (zone ?zone1) (image ?block1) (attributes pick) (pose ?x1 ?y1 ?z1))
        (on-top-of (upper ?block2) (lower ?block1))
        (item (type Objects) (name ?block2) (zone ?zone2) (image ?block2) (attributes pick) (pose ?x2 ?y2 ?z2))
        =>
        (assert (goal (move ?block2)(on-top-of floor)))
)
(defrule CLEAR::clear-lower-block
        (goal (on-top-of ?block1))
        (item (type Objects) (name ?block1) (zone ?zone1) (image ?block1) (attributes pick) (pose ?x1 ?y1 ?z1))
        (on-top-of (upper ?block2) (lower ?block1))
        (item (type Objects) (name ?block2) (zone ?zone2) (image ?block2) (attributes pick) (pose ?x2 ?y2 ?z2))
        =>
        (assert (goal (move ?block2)(on-top-of floor)))
)
