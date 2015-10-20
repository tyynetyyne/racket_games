#lang racket
(require 2htdp/universe 2htdp/image picturing-programs racket/trace)
;
; tetris-peli
;
(define PALA 30)
(define REUNA 5)
(define PALOJA 7)
(define ISO-PALA 5)
(define PIENI-PALA 3)
(define PELIN-KORKEUS 20)
(define PELIN-LEVEYS 16)
(define NOPEUS 0.5)
(define TYHJÄ-KENTTÄ (empty-scene (* PELIN-LEVEYS PALA) (* PELIN-KORKEUS PALA)))
(define LÄHTÖ-X 9)
(define LÄHTÖ-Y 0)
(define TEKSTI-KOKO 30)
(define PELIN-VASEN-LAITA 0)

;tehdään Tetris-palikat:
(define (tee-pala koko reuna väri)
  (overlay (rectangle (- koko reuna) (- koko reuna) 255 väri)(rectangle koko koko 100 "blue")))

(define pala-a (tee-pala PALA REUNA "red"))
(define pala-b (tee-pala PALA REUNA "blue"))
(define pala-c (tee-pala PALA REUNA "violet"))
(define pala-d (tee-pala PALA REUNA "green"))
(define pala-e (tee-pala PALA REUNA "yellow"))
(define pala-f (tee-pala PALA REUNA "orange"))
(define pala-g (tee-pala PALA REUNA "dark-green"))

(define tyyppiA 1)
(define tyyppiB 2)
(define tyyppiC 3)
(define tyyppiD 4)
(define tyyppiE 5)
(define tyyppiF 6)
(define tyyppiG 7)

(define asento1 1)
(define asento2 2)
(define asento3 3)
(define asento4 4)
(define YKSI-PALA 5)

; tietorakenteet:
; ------------------------------------------------------------------------
; sijainti:
; - x: x-koordinaatti
; - y: y-koordinaatti
(struct sijainti ([x #:mutable] [y #:mutable]))
;-------------------------------------------------------------------------

;--------------------------------------------------------------------------
; palikka:
; - tyyppi: tyyppiA --> tyyppiG
; - paikka: koordinaatti keskellä 3 x 3 tai 5 x 5 kehikkoa
; - asento: 1 --> 4 
(struct palikka ([tyyppi #:mutable] [paikka #:mutable] [asento #:mutable]))
; --------------------------------------------------------------------------

;--------------------------------------------------------------------------
; palikoiden rakenteet:
;--------------------------------------------------------------------------
(define palikka-a-as1 (list 0 0 0 1 0 0 1 1 1)) 
(define palikka-a-as2 (list 1 1 0 1 0 0 1 0 0))
(define palikka-a-as3 (list 0 0 0 1 1 1 0 0 1)) 
(define palikka-a-as4 (list 0 0 1 0 0 1 0 1 1))   

(define palikka-b-as1 (list 0 0 0 0 0 1 1 1 1)) 
(define palikka-b-as2 (list 1 0 0 1 0 0 1 1 0)) 
(define palikka-b-as3 (list 0 0 0 1 1 1 1 0 0)) 
(define palikka-b-as4 (list 0 1 1 0 0 1 0 0 1)) 

(define palikka-c-as1 (list 0 0 0 0 1 1 1 1 0)) 
(define palikka-c-as2 (list 0 1 0 0 1 1 0 0 1)) 
(define palikka-c-as3 (list 0 0 0 0 1 1 1 1 0)) 
(define palikka-c-as4 (list 0 1 0 0 1 1 0 0 1)) 

(define palikka-d-as1 (list 0 0 0 1 1 0 0 1 1)) 
(define palikka-d-as2 (list 0 1 0 1 1 0 1 0 0)) 
(define palikka-d-as3 (list 0 0 0 1 1 0 0 1 1)) 
(define palikka-d-as4 (list 0 1 0 1 1 0 1 0 0))

(define palikka-e-as1 (list 0 0 0 1 1 0 1 1 0)) 
(define palikka-e-as2 (list 0 0 0 1 1 0 1 1 0)) 
(define palikka-e-as3 (list 0 0 0 1 1 0 1 1 0))  
(define palikka-e-as4 (list 0 0 0 1 1 0 1 1 0)) 

(define palikka-f-as1 (list 0 0 0 0 1 0 1 1 1)) 
(define palikka-f-as2 (list 1 0 0 1 1 0 1 0 0)) 
(define palikka-f-as3 (list 0 0 0 1 1 1 0 1 0)) 
(define palikka-f-as4 (list 0 0 1 0 1 1 0 0 1))

(define palikka-g-as1 (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)) 
(define palikka-g-as2 (list 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0)) 
(define palikka-g-as3 (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)) 
(define palikka-g-as4 (list 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0)) 

(define pala-as1 (list 1))

; -----------------------------
; off-set muunnos matriisi:
; -----------------------------
(define muunnokset3x3 (list (sijainti -1 -1) (sijainti 0 -1) (sijainti 1 -1) 
                            (sijainti -1 0) (sijainti 0 0) (sijainti 1 0)
                            (sijainti -1 1) (sijainti 0 1) (sijainti 1 1)))
                       
(define muunnokset5x5 (list (sijainti -2 -2) (sijainti -1 -2) (sijainti 0 -2) (sijainti 1 -2) (sijainti 2 -2)
                            (sijainti -2 -1) (sijainti -1 -1) (sijainti 0 -1) (sijainti 1 -1) (sijainti 2 -1)
                            (sijainti -2 0) (sijainti -1 0) (sijainti 0 0) (sijainti 1 0) (sijainti 2 0)
                            (sijainti -2 1) (sijainti -1 1) (sijainti 0 1) (sijainti 1 1) (sijainti 2 1)
                            (sijainti -2 2) (sijainti -1 2) (sijainti 0 2) (sijainti 1 2) (sijainti 2 2)))

(define muunnokset1x1 (list (sijainti 0 0)))

;anna muoto
(define (anna-muoto p)
  (define t (palikka-tyyppi p))
  (define a (palikka-asento p))
  (cond [(and (= t tyyppiA) (= a asento1)) palikka-a-as1]
        [(and (= t tyyppiA) (= a asento2)) palikka-a-as2]
        [(and (= t tyyppiA) (= a asento3)) palikka-a-as3]
        [(and (= t tyyppiA) (= a asento4)) palikka-a-as4]
        [(and (= t tyyppiB) (= a asento1)) palikka-b-as1]
        [(and (= t tyyppiB) (= a asento2)) palikka-b-as2]
        [(and (= t tyyppiB) (= a asento3)) palikka-b-as3]
        [(and (= t tyyppiB) (= a asento4)) palikka-b-as4]
        [(and (= t tyyppiC) (= a asento1)) palikka-c-as1]
        [(and (= t tyyppiC) (= a asento2)) palikka-c-as2]
        [(and (= t tyyppiC) (= a asento3)) palikka-c-as3]
        [(and (= t tyyppiC) (= a asento4)) palikka-c-as4]
        [(and (= t tyyppiD) (= a asento1)) palikka-d-as1]
        [(and (= t tyyppiD) (= a asento2)) palikka-d-as2]
        [(and (= t tyyppiD) (= a asento3)) palikka-d-as3]
        [(and (= t tyyppiD) (= a asento4)) palikka-d-as4]
        [(and (= t tyyppiE) (= a asento1)) palikka-e-as1]
        [(and (= t tyyppiE) (= a asento2)) palikka-e-as2]
        [(and (= t tyyppiE) (= a asento3)) palikka-e-as3]
        [(and (= t tyyppiE) (= a asento4)) palikka-e-as4]
        [(and (= t tyyppiF) (= a asento1)) palikka-f-as1]
        [(and (= t tyyppiF) (= a asento2)) palikka-f-as2]
        [(and (= t tyyppiF) (= a asento3)) palikka-f-as3]
        [(and (= t tyyppiF) (= a asento4)) palikka-f-as4]
        [(and (= t tyyppiG) (= a asento1)) palikka-g-as1]
        [(and (= t tyyppiG) (= a asento2)) palikka-g-as2]
        [(and (= t tyyppiG) (= a asento3)) palikka-g-as3]
        [(and (= t tyyppiG) (= a asento4)) palikka-g-as4]
        [else pala-as1] ))
           
;anna muunnosmatriisi
(define (anna-matriisi p)
  (define t (palikka-tyyppi p))
  (define a (palikka-asento p))
  (cond [(and (= t tyyppiG) (not (= a YKSI-PALA)) muunnokset5x5)]
        [(= a YKSI-PALA) muunnokset1x1] 
        [else muunnokset3x3]))

;anna pala
(define (anna-pala p)
  (define t (palikka-tyyppi p))
    (cond [(= t tyyppiA) pala-a]
          [(= t tyyppiB) pala-b]
          [(= t tyyppiC) pala-c]
          [(= t tyyppiD) pala-d]
          [(= t tyyppiE) pala-e]
          [(= t tyyppiF) pala-f]
          [(= t tyyppiG) pala-g]))

;arvotaan yksi palikka
;voit testata tämän funktion kutsumalla
(define (anna-palikka x y)
  (define palanro (add1 (random PALOJA)))
  (palikka palanro (sijainti x y) asento1))

; kuva+kenttä: apufunktio piirtämiseen
; -----------------------------
 
(define (kuva+kenttä sij kuva kenttä)
  (place-image kuva (+ (/ PALA 2) (* (sijainti-x sij) PALA)) (- (* (sijainti-y sij) PALA) (/ PALA 2)) kenttä))

; ------------------------------
; piirrä-mittakaavassa
; 
; - palikat: lista sijainteja joihin piirretään pala
; - kuva: piirrettävän palan kuva
; -------------------------------
(define (piirrä-mittakaavaan palikat kuva kenttä)
  (cond [(empty? palikat) kenttä]
        [else (piirrä-mittakaavaan (rest palikat) kuva (kuva+kenttä (first palikat) kuva kenttä))])) 
         
; apufunktio koordinaattien laskemiseen
(define (laske-koordinaatit s p m)
  (cond [(= p 0) empty]
        [else (sijainti (+ (sijainti-x s) (sijainti-x m)) (+ (sijainti-y s) (sijainti-y m)))]))

; antaa palojen koordinaatit sijainti-listana
(define (anna-koordinaatit s p m)
  (cond [(empty? p) empty]
        [else (filter (lambda (f) (not (empty? f))) 
                      (cons (laske-koordinaatit s (first p) (first m)) (anna-koordinaatit s (rest p) (rest m))))]))
   
; palikan piirtäminen
(define (piirrä-palikka p kenttä)
  (piirrä-mittakaavaan (anna-koordinaatit (palikka-paikka p) (anna-muoto p) (anna-matriisi p)) (anna-pala p) kenttä))

(define (vala-pohja x p)
  (cond [(= x PELIN-LEVEYS) p]
        [else (vala-pohja (add1 x) (cons (palikka tyyppiA (sijainti x PELIN-KORKEUS) YKSI-PALA) p))]))

(struct pelikenttä ([liikkuva-palikka #:mutable] [pohja #:mutable]))
 
(define (piirrä-pohja pohja kenttä)
  (cond [(empty? pohja) kenttä]
        [else (piirrä-pohja (rest pohja) 
                            (piirrä-mittakaavaan (list (palikka-paikka (first pohja))) (anna-pala (first pohja)) kenttä))]))

(define (piirrä-peli pk)
  (piirrä-palikka (pelikenttä-liikkuva-palikka pk) (piirrä-pohja (pelikenttä-pohja pk) TYHJÄ-KENTTÄ)))

(define (törmääkö-seiniin? pk dx)
  (define pa (pelikenttä-liikkuva-palikka pk))
  (define sij (palikka-paikka pa))
  (define uusi-sij (sijainti (+ dx (sijainti-x sij)) (sijainti-y sij)))
  (define uusi-s (anna-koordinaatit uusi-sij (anna-muoto pa) (anna-matriisi pa)))
  (ormap (lambda (f) (or (> PELIN-VASEN-LAITA (sijainti-x f)) (<= PELIN-LEVEYS (sijainti-x f)))) uusi-s))

(define (liikuta-pala pk key)
  (define p (pelikenttä-liikkuva-palikka pk))
  (define a (palikka-asento p))
  (define s (palikka-paikka p))
  (cond [(and (string=? key "left") (not (törmääkö-seiniin? pk -1))) (set-sijainti-x! s (sub1 (sijainti-x s)))] 
        [(and (string=? key "right") (not (törmääkö-seiniin? pk 1))) (set-sijainti-x! s (add1 (sijainti-x s)))] 
        [(and (string=? key "up") (= a 4)) (set-palikka-asento! p (- a 3))]
        [(and (string=? key "up") (not (= a 4))) (set-palikka-asento! p (add1 a))]
        [(string=? key "down") (uusi-tilanne pk)]) 
  pk)
  
(define (yhdistä-pohjaan pa ps po)
  (cond [(empty? ps) po]
        [else (yhdistä-pohjaan pa (rest ps) (cons (palikka (palikka-tyyppi pa) (first ps) YKSI-PALA) po))]))   
   
(define (anna-pohjan-koordinaatit pk)
  (define p (pelikenttä-pohja pk))
  (map (lambda (f) (palikka-paikka f)) p))

(define (sijainti=? s1 s2)
  (and (= (sijainti-x s1)(sijainti-x s2))
       (= (sijainti-y s1)(sijainti-y s2))))
  
; törmääkö pohjaan?
; - sij : lista sijainteja joissa on palikan osia
; - po : lista sijainteja joissa on pohjan paloja
(define (törmääkö-pohjaan? sij po)
  (cond [(empty? sij) #f]
        [else (or (ormap (lambda (f) (sijainti=? (first sij) f)) po) (törmääkö-pohjaan? (rest sij) po))]))

; apufunktio joka testaa törmäsikö palikan palat yhteen pohjapalaan
; - p = palikan palojen sijainnit
; - po1 = yhden pohjan palan sijainti

;(define (törmäsikö? sij po1)
;  (cond [(empty? sij) #f]
;        [else (or (sijainti=? (first sij) po1) (törmäsikö? (rest sij) po1))]))

;(define (törmääkö-pohjaan? sij po)
;   (cond [(empty? po) #f]
;         [else (or (törmäsikö? sij (first po)) (törmääkö-pohjaan? sij (rest po)))]))

(define (onko-täysi? i po)
 (= PELIN-LEVEYS (length (filter (lambda (f) (equal? #t f)) (map (lambda (f) (= i (sijainti-y (palikka-paikka f)))) po)))))

; etsi täydet rivit
; palauttaa rivien y-koordinaatit listana
(define (etsi-täydet-rivit i rivit po)
  (cond [(= i PELIN-KORKEUS) rivit]
        [(onko-täysi? i po) (etsi-täydet-rivit (add1 i) (cons i rivit) po)]
        [else (etsi-täydet-rivit (add1 i) rivit po)]))

; laske palat alas
; laskee loput palat alaspäin yhden askeleen
(define (laske-alas r pa)
  (define sij (palikka-paikka pa))
  (cond [(< (sijainti-y sij) r) (set-sijainti-y! sij (add1 (sijainti-y sij))) pa]
        [else pa]))

; poista rivi
; r - rivinumero josta poistetaan palat
; palauttaa listan pohjaan jäävistä paloista
(define (poista-rivi r pk)
  (define po (pelikenttä-pohja pk))
  (set-pelikenttä-pohja! pk (filter (lambda (f) (not (= r (sijainti-y (palikka-paikka f))))) po))
  (set-pelikenttä-pohja! pk (map (lambda (f) (laske-alas r f)) (pelikenttä-pohja pk))))
;  (cond [(empty? p1) po]
;        [(cond (= r (sijainti-y (palikka-sijainti (first p1)))) (poista-rivi 

; poista täydet rivit
; poistaa kaikki samalla y-koordinaatilla olevat palikat
(define (poista-täydet-rivit pk)
  (define po (pelikenttä-pohja pk))
  (map (lambda (f) (poista-rivi f pk)) (etsi-täydet-rivit 0 '() (pelikenttä-pohja pk))))

; palan jäädytys pohjaan
(define (jäädytä-pala pk)
  (define pa (pelikenttä-liikkuva-palikka pk))
  (define po (pelikenttä-pohja pk))
  (define ps (anna-koordinaatit (palikka-paikka pa) (anna-muoto pa) (anna-matriisi pa)))
  (set-pelikenttä-pohja! pk (yhdistä-pohjaan pa ps po))
  (poista-täydet-rivit pk)
  (set-pelikenttä-liikkuva-palikka! pk (anna-palikka LÄHTÖ-X LÄHTÖ-Y)))

(define (uusi-tilanne pk)
  (define pa (pelikenttä-liikkuva-palikka pk))
  (define sij (palikka-paikka pa))
  (define uusi-sij (sijainti (sijainti-x sij) (add1 (sijainti-y sij))))
  (define uusi-s (anna-koordinaatit uusi-sij (anna-muoto pa) (anna-matriisi pa)))
  (define po (anna-pohjan-koordinaatit pk))
  (cond [(not (törmääkö-pohjaan? uusi-s po)) (set-sijainti-y! sij (sijainti-y uusi-sij))]
        [else (jäädytä-pala pk)])
  pk)

(define (kuollut? pk)
  (define po (pelikenttä-pohja pk))
  (ormap (lambda (f) (>= (add1 LÄHTÖ-Y) (sijainti-y (palikka-paikka f)))) po)) 

;viimeinen kuva
(define (piirra-loppu pk)
  (overlay (text "Game Over" TEKSTI-KOKO "black") (piirrä-peli pk)))
;---------------------------------------------
; peli
;--------------------------------------------
(define (aloita-tetris)
  (big-bang (pelikenttä (anna-palikka LÄHTÖ-X LÄHTÖ-Y) (vala-pohja 0 '()))
            (on-tick uusi-tilanne NOPEUS)
            (on-key liikuta-pala)
            (to-draw piirrä-peli)
            (stop-when kuollut? piirra-loppu)
))
(aloita-tetris)

; testausfunktioita:
(define kerros (build-list 16 (lambda (f)(sijainti (add1 f) 10))))
(define kerros2 (cons (sijainti 4 -1) kerros))
(define palikka-kerros (map (lambda (f)(palikka tyyppiA f YKSI-PALA)) kerros))
(define palikka-kerros2 (map (lambda (f)(palikka tyyppiA f YKSI-PALA)) kerros2))
(define pituus (length (filter (lambda (f) (equal? #t f)) (map (lambda (f) (= 10 (sijainti-y (palikka-paikka f)))) palikka-kerros))))
(define pituus2 (length (filter (lambda (f) (equal? #t f)) (map (lambda (f) (= 10 (sijainti-y (palikka-paikka f)))) palikka-kerros2))))

(define (testi1)
 ; (trace etsi-täydet-rivit)
  (etsi-täydet-rivit 0 '() palikka-kerros2))

(define kuollut (ormap (lambda (f) (= (sub1 LÄHTÖ-Y) (sijainti-y (palikka-paikka f)))) palikka-kerros2)) 
(define (testi2)
  kuollut)
  