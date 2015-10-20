#lang racket
(require 2htdp/image 2htdp/universe)

;; vakiot
(define LEVEYS 650)
(define KORKEUS 420)
(define KUUSIKON-OFFSET (/ LEVEYS 2))
(define KUUSIKON-VASEN-LAITA (- KUUSIKON-OFFSET))
(define KUUSIKON-KESKIKOHTA KUUSIKON-OFFSET)
(define KUUSIKON-OIKEA-LAITA (+ LEVEYS KUUSIKON-OFFSET))
(define KUUSIKON-ALAREUNA (- KORKEUS 110))
(define NOPEUS 0.1)
(define LINTU-X 250)
(define ASKEL-DX 3)
;; huom! ASKEL-DY on oltava pariton luku, tätä käytetään hyväksi lentoanimaatiossa
(define ASKEL-DY 3)
(define KOKO 1.5)
(define LENTOASKEL 10)
(define LENTORATA-Y (/ KORKEUS 3))
(define LENTOKERROIN 40)
(define SURINAKERROIN 20)
(define KORJAUSKERROIN 5)
  
;; grafiikat
(define LINTU (overlay/xy (overlay/xy (rotate 45 (rectangle 6 30 "solid" "black"))
                                      19 20
                                      (crop 0 30 65 50 (rotate 300 (ellipse 50 70 "solid" "brown"))))
                          79 20
                          (crop 3 0 12 10 (triangle 15 "solid" "black"))))

(define SIIPI-YLÄ (crop 0 0 30 30 (rotate 20 (ellipse 20 50 "solid" "black"))))
(define SIIPI-ALA (flip-vertical SIIPI-YLÄ))
(define LINTU-YLÄ (scale KOKO (underlay/xy LINTU 32 0 SIIPI-YLÄ)))
(define LINTU-ALA (scale KOKO (underlay/xy LINTU 32 30 SIIPI-ALA)))
(define KÄRPÄNEN (scale KOKO (overlay/xy (overlay/xy (rotate 10 (ellipse 4 7 "outline" "black")) 
                                                     3 0 
                                                     (rotate -10 (ellipse 4 7 "outline" "black")))
                                         -1 6
                                         (rectangle 7 3 "solid" "black"))))

(define KUUSIEN-KOKO 3)
(define KUUSEN-VIHREÄ (make-color 53 136 52))
(define KUUSI (overlay/xy (triangle 30 "solid" KUUSEN-VIHREÄ) -10 20  (triangle 50 "solid" KUUSEN-VIHREÄ)))
(define KUUSIKKO-OSA (overlay/xy KUUSI 30 4 (overlay/xy KUUSI 25 -6 (overlay/xy KUUSI 25 4 KUUSI))))
(define KUUSIKKO (scale KUUSIEN-KOKO (overlay/xy KUUSIKKO-OSA 100 0 KUUSIKKO-OSA)))
(define TAIVAAN-SININEN (make-color 240 248 249))

(define TAIVAS (rectangle LEVEYS KORKEUS "solid" TAIVAAN-SININEN))
(define TYHJÄKENTTÄ (overlay/align "middle" "bottom" TAIVAS (empty-scene LEVEYS KORKEUS)))

;; tietorakenteet
(define-struct kenttä (lintu-y kärpänen-x kärpänen-y kuuset1-x kuuset2-x))
 
;; kuusikko saadaan liikkumaan
;; liikuta-kuusia : number -> number
(define (liikuta-kuusia x)
  (if (and (> x KUUSIKON-VASEN-LAITA) (<= x KUUSIKON-OIKEA-LAITA))
      (- x ASKEL-DX)
      KUUSIKON-OIKEA-LAITA))

;; lintu saadaan "lentämään" vuorottelemalla kahden kuvan välillä
(define (anna-lintu k)
  (if (even? (kenttä-lintu-y k))
      LINTU-ALA
      LINTU-YLÄ))

;; pelilogiikka
(define (piirrä-kenttä k)
  (place-image (anna-lintu k) LINTU-X (kenttä-lintu-y k)  
               (place-image KÄRPÄNEN (kenttä-kärpänen-x k) (kenttä-kärpänen-y k)  
                            (place-image KUUSIKKO (kenttä-kuuset1-x k) KUUSIKON-ALAREUNA
                                         (place-image KUUSIKKO (kenttä-kuuset2-x k) KUUSIKON-ALAREUNA TYHJÄKENTTÄ)))))

;; kentän päivittäminen (itsestään liikkuvat elementit)
(define (päivitä-kenttä k)
  (letrec ((x (- (kenttä-kärpänen-x k) ASKEL-DX))
           (kx (/ x KORJAUSKERROIN)))
  (make-kenttä (+ (kenttä-lintu-y k) ASKEL-DY) 
               x 
               (+ (+ (* LENTOKERROIN (sin kx)) (* SURINAKERROIN (sin kx))) LENTORATA-Y)
               (liikuta-kuusia (kenttä-kuuset1-x k))
               (liikuta-kuusia (kenttä-kuuset2-x k)))))

;; linnun lennättäminen
(define (lennätä-lintua k)
  (make-kenttä (- (kenttä-lintu-y k) LENTOASKEL) (kenttä-kärpänen-x k) (kenttä-kärpänen-y k) (kenttä-kuuset1-x k) (kenttä-kuuset2-x k)))
                  
;; näppäimet
(define (hoida-näppäimet k n)
  (cond [(key=? n "up") (lennätä-lintua k)]
        [else k]))

;; pääohjelma
(define ALKU (make-kenttä LENTORATA-Y LEVEYS LENTORATA-Y KUUSIKON-KESKIKOHTA KUUSIKON-OIKEA-LAITA))

(define (aloita-lintu)
  (big-bang ALKU
            (on-draw piirrä-kenttä)
            (on-tick päivitä-kenttä NOPEUS)
            (on-key hoida-näppäimet)
            (record? "animaatio_kuvat"))) 

(aloita-lintu)

                          


