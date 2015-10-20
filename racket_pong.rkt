; ----------------------------------------------------------------------------------------------------
; PONG - peli Racketilla
; Tiina Partanen
; ----------------------------------------------------------------------------------------------------
#lang racket
(require 2htdp/image 2htdp/universe)

; ----------------------------------------------------------------------------------------------------
; peligraafiikat
; - PALLO (vakiot : SÄDE)
; - MAILA (vakiot : MAILAN-KORKEUS, MAILAN-LEVEYS)
; - KENTTÄ (vakiot : LEVEYS, KORKEUS)
; ----------------------------------------------------------------------------------------------------
(define PALLON-SÄDE 15)
(define PALLO (circle PALLON-SÄDE "solid" "white"))
(define MAILAN-KORKEUS 40)
(define MAILAN-LEVEYS 8)
(define MAILA (rectangle MAILAN-LEVEYS MAILAN-KORKEUS "solid" "white"))
(define LEVEYS 800)
(define KORKEUS 400)
(define KENTTÄ (place-image (rectangle LEVEYS KORKEUS "solid" "black") (/ LEVEYS 2) (/ KORKEUS 2) 
                            (empty-scene LEVEYS KORKEUS))) 
  
; ----------------------------------------------------------------------------------------------------
; vakioita pallon ja mailojen sijaintien määrittämiseksi 
; ----------------------------------------------------------------------------------------------------
(define VASEN-LAITA 0)
(define YLÄ-LAITA 0)
  
; mailojen x-koordinaatit eivät koskaan muutu
(define MAILAN-OFFSET 60)
(define VASEN-MAILA-X (+ VASEN-LAITA MAILAN-OFFSET))
(define OIKEA-MAILA-X (- LEVEYS MAILAN-OFFSET))
  
; pallon etäisyys reunoista, kun se on mailan edessä
(define PALLON-OFFSET (+ PALLON-SÄDE MAILAN-OFFSET (/ MAILAN-LEVEYS 2)))
(define PALLO-MAILALLA-VASEN-X (+ VASEN-LAITA PALLON-OFFSET))
(define PALLO-MAILALLA-OIKEA-X (- LEVEYS PALLON-OFFSET))

; -----------------------------------------------------------------------------------------------------
; pelin dynamiikkaan vaikuttavia vakioita
; ----------------------------------------------------------------------------------------------------  
(define NOPEUS 0.02)
(define ALOITUS-NOPEUS-X 3)
(define ALOITUS-NOPEUS-Y 1)
(define MAILA-ASKEL 5)
  
; ----------------------------------------------------------------------------------------------------
; vakioita pallon lähtöpisteeksi
; HUOM! pallo ei saa olla kiinni mailassa kun se lähtee, koska muuten se tulkitaan lyönniksi
; ----------------------------------------------------------------------------------------------------
(define PALLON-LÄHTÖ-VASEN-X (+ PALLO-MAILALLA-VASEN-X 3))
(define PALLON-LÄHTÖ-OIKEA-X (- PALLO-MAILALLA-OIKEA-X 3))
(define PALLON-LÄHTÖ-Y (/ KORKEUS 2))
(define MAILAN-LÄHTÖ-Y (/ KORKEUS 2))

; ----------------------------------------------------------------------------------------------------
; vakioita pisteiden tulostusta varten
; ----------------------------------------------------------------------------------------------------
(define TEKSTI-KOKO 30)
(define MAX-PISTEET 10)
(define PISTEET-X (/ LEVEYS 2))
(define PISTEET-Y 20)

; ----------------------------------------------------------------------------------------------------
; kenttä-struct tallentaa pelin tilan: 
; - pallo-x : pallon x-koordinaatti
; - pallo-y : pallon y-koordinaatti
; - nopeus-x : nopeusvektorin x-komponentti
; - nopeus-y : nopeusvektorin y-komponentti
; - maila-v : vasemman mailan y-koordinaatti
; - maila-o : oikean mailan y-koordinaatti
; - pisteet-v : vasemman pelaajan pisteet
; - pisteet-o : oikean pelaajan pisteet
; ----------------------------------------------------------------------------------------------------
(struct kenttä (pallo-x pallo-y nopeus-x nopeus-y maila-v maila-o pisteet-v pisteet-o) #:mutable)

; ----------------------------------------------------------------------------------------------------
; pelikentän piirtofunktiot
; ----------------------------------------------------------------------------------------------------
(define (piirrä-pisteet k)
  (overlay/xy (text (number->string (kenttä-pisteet-v k)) TEKSTI-KOKO "white") 100 0 
              (text (number->string (kenttä-pisteet-o k)) TEKSTI-KOKO "white")))
  
(define (piirrä-kenttä k)
  (place-image PALLO (kenttä-pallo-x k) (kenttä-pallo-y k) 
               (place-image MAILA OIKEA-MAILA-X (kenttä-maila-o k) 
                         (place-image MAILA VASEN-MAILA-X (kenttä-maila-v k) 
                                      (place-image (piirrä-pisteet k) PISTEET-X PISTEET-Y KENTTÄ)))))
  
; ----------------------------------------------------------------------------------------------------
; funktiot, joilla havaitaan pallon seiniin törmääminen
; ----------------------------------------------------------------------------------------------------
  (define (törmäsi-päätyseinään? k)
  (define x (kenttä-pallo-x k))
  (or (< x VASEN-LAITA) (> x LEVEYS)))

(define (törmäsi-sivuseinään? k)
  (define y (kenttä-pallo-y k))
  (or (> y (- KORKEUS PALLON-SÄDE)) (< y (+ YLÄ-LAITA PALLON-SÄDE))))

; ----------------------------------------------------------------------------------------------------
; funktiot, joilla havaitaan pallon törmäys mailaan ja pallon sijainti mailan takana
; - pallo törmää mailaan, jos pallon keskipisteen y-koordinaatti on mailan ylä- ja alareunan välissä
;   ja x-koordinaatti ylittää mailan reunan
; - törmäystä testataan vain siihen mailaan jota kohti ollaan menossa (muuten pallo voi alkaa jojota
; ----------------------------------------------------------------------------------------------------
(define (törmäsi-mailaan? k)
  (define xp (kenttä-pallo-x k))
  (define yp (kenttä-pallo-y k))
  (define puolimaila (/ MAILAN-KORKEUS 2))
  (define om_ylä (- (kenttä-maila-o k) puolimaila))
  (define om_ala (+ (kenttä-maila-o k) puolimaila))
  (define vm_ylä (- (kenttä-maila-v k) puolimaila))
  (define vm_ala (+ (kenttä-maila-v k) puolimaila))
  (if (< (kenttä-nopeus-x k) 0)
      ; pallo on menossa vasemmalle ja se kohtaa vasemman mailan
      (and (<= xp PALLO-MAILALLA-VASEN-X) (and (<= yp vm_ala) (>= yp vm_ylä)))
      ; pallo on menossa oikealla ja se kohtaa oikean mailan
      (and (>= xp PALLO-MAILALLA-OIKEA-X) (and (<= yp om_ala) (>= yp om_ylä)))))
      
; jos pallo on mailan takana sen ei pitäisi enää "törmätä mailaan"
(define (mailan-takana? k)
  (define x (kenttä-pallo-x k))
    (or (and (< x VASEN-MAILA-X) (> x VASEN-LAITA))
        (and (< x LEVEYS) (> x OIKEA-MAILA-X))))

; ----------------------------------------------------------------------------------------------------
; tässä funktiossa on pääosa pelilogiikasta
; pallo liikkuu aina nopeusvektorin suuntaan yhden askeleen, lisäksi:
; - jos törmätään vasempaan laitaan, nopeusvektorin x-komponentti kerrotaan -1:lla,vastustajalle piste
; - jos törmätään oikeaan laitaan, sama
; - jos törmätään mailaan ja pallo ei ole mailan takana, nopeusvektorin x-komponentti kerrotaan -1:lla
; --> funktio palauttaa uuden kenttä-structin, jossa pelin tila on tallessa
; ----------------------------------------------------------------------------------------------------
(define (uusi-kenttä k)
  (define x (kenttä-pallo-x k))
  (cond [(törmäsi-sivuseinään? k) (kenttä (+ (kenttä-pallo-x k) (kenttä-nopeus-x k)) 
                                          (+ (kenttä-pallo-y k) (* -1 (kenttä-nopeus-y k)))
                                          (kenttä-nopeus-x k) (* -1 (kenttä-nopeus-y k))
                                          (kenttä-maila-v k) (kenttä-maila-o k) 
                                          (kenttä-pisteet-v k) (kenttä-pisteet-o k))]
        [(<= x VASEN-LAITA) (kenttä PALLON-LÄHTÖ-VASEN-X PALLON-LÄHTÖ-Y 
                                          (* -1 (kenttä-nopeus-x k)) (kenttä-nopeus-y k) 
                                          (kenttä-maila-v k) (kenttä-maila-o k)
                                          (kenttä-pisteet-v k) (add1 (kenttä-pisteet-o k)))]  
        [(>= x LEVEYS) (kenttä PALLON-LÄHTÖ-OIKEA-X PALLON-LÄHTÖ-Y 
                                          (* -1 (kenttä-nopeus-x k)) (kenttä-nopeus-y k) 
                                          (kenttä-maila-v k) (kenttä-maila-o k) 
                                          (add1 (kenttä-pisteet-v k)) (kenttä-pisteet-o k))]  
        [(and (törmäsi-mailaan? k) (not (mailan-takana? k))) (kenttä 
                                          (+ (kenttä-pallo-x k) (* -1 (kenttä-nopeus-x k))) 
                                          (+ (kenttä-pallo-y k) (kenttä-nopeus-y k)) 
                                          (* -1 (kenttä-nopeus-x k)) (kenttä-nopeus-y k) 
                                          (kenttä-maila-v k) (kenttä-maila-o k) 
                                          (kenttä-pisteet-v k) (kenttä-pisteet-o k))]
        [else (kenttä (+ (kenttä-pallo-x k) (kenttä-nopeus-x k)) 
                      (+ (kenttä-pallo-y k) (kenttä-nopeus-y k)) 
                      (kenttä-nopeus-x k) (kenttä-nopeus-y k)
                      (kenttä-maila-v k) (kenttä-maila-o k)
                      (kenttä-pisteet-v k) (kenttä-pisteet-o k))]))

; ----------------------------------------------------------------------------------------------------
; pelin ohjausnäppäinten käsittely
; - oikean puoleinen maila liikkuu nuolinäppäimillä ylös ja alas
; - vasemman puoleinen maila liikkuu a:lla ylös ja z:lla alas
; --> funktio palauttaa kenttä-structin (k), jossa pelin tila on tallessa
; ----------------------------------------------------------------------------------------------------
(define (liikuta-mailaa k näppäin)
  (define omaila (kenttä-maila-o k))
  (define vmaila (kenttä-maila-v k))
  (cond [(and (key=? näppäin "up") (> omaila YLÄ-LAITA)) (set-kenttä-maila-o! k (- omaila MAILA-ASKEL))]
        [(and (key=? näppäin "down") (< omaila KORKEUS)) (set-kenttä-maila-o! k (+ omaila MAILA-ASKEL))]
        [(and (key=? näppäin "a") (> vmaila YLÄ-LAITA)) (set-kenttä-maila-v! k (- vmaila MAILA-ASKEL))]
        [(and (key=? näppäin "z") (< vmaila KORKEUS)) (set-kenttä-maila-v! k (+ vmaila MAILA-ASKEL))])
  k)

; ----------------------------------------------------------------------------------------------------
; peli loppuu kun jompi kumpi saa max-pisteet
; ---------------------------------------------------------------------------------------------------- 
(define (loppu? k)
  (define vp (kenttä-pisteet-v k))
  (define op (kenttä-pisteet-o k))
  (or (>= vp MAX-PISTEET) (>= op MAX-PISTEET)))

(define (piirrä-loppu k)
  (overlay (text "Game Over" TEKSTI-KOKO "white") (piirrä-kenttä k)))

; ----------------------------------------------------------------------------------------------------
; pääohjelma:
; luodaan alkutilanteen kenttä-struct, tämä tietorakenne välitetään muille funktioille:
; - uusi-kenttä laskee uuden pelitilanteen
; - liikuta-mailaa hoitaa näppäimet
; - piirrä-kenttä hoitaa ruudulle piirtämisen
; - loppu? toteuttaa lopetusehdon
; ----------------------------------------------------------------------------------------------------
(define (aloita-pong)
  (big-bang (kenttä PALLON-LÄHTÖ-VASEN-X PALLON-LÄHTÖ-Y ALOITUS-NOPEUS-X ALOITUS-NOPEUS-Y 
                    MAILAN-LÄHTÖ-Y MAILAN-LÄHTÖ-Y 0 0)  
            (on-tick uusi-kenttä NOPEUS)
            (on-key liikuta-mailaa)
            (to-draw piirrä-kenttä)
            (stop-when loppu? piirrä-loppu)))

(aloita-pong)

