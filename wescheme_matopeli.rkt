;;----------------------------------------------------------------------------------------------------
;; Matopeli4-mobiiliversio
;;
;; Tehty Realm of Racket - kirjan pohjalta DrRacketilla ja portattu WeScheme - ympäristöön
;; WeScheme - dokumentaatio: http://www.wescheme.org/doc/wescheme.html
;; TILT koodi: https://github.com/bootstrapworld/wescheme-compiler2012/blob/master/src/compiler/..
;;                                     ..mzscheme-vm/collections/bootstrap2014/bootstrap-teachpack.ss~
;;
;; Tähän versioon on lisätty tuki mobiililaitteille
;;
;; Tiina Partanen
;;----------------------------------------------------------------------------------------------------
;; Racket -> WeScheme muutokset:
;; - struct-määrittelyt muutettu (esim. "struct lima" --> "define-struct lima")
;; - konstruktorit muutettu (esim. "lima" --> "make-lima")
;; - kuvatiedosto haetaan URL:n kautta
;; - lokaalien muuttujien määrittelyt muutettu (esim. "define a 3" --> "let ((a 3))"
;; - lokaalien muuttujien määrittelyt muutettu, jos rekursiivisia (esim. "define" --> "letrec")
;; - racketissä kaikki muu paitsi #f on #t mutta schemessä ei (esim. lisätty "(not (false?")
;;----------------------------------------------------------------------------------------------------
;; näitä määritelmiä tarvitaan DrRacketissä (kommentoi pois seuraavat kaksi riviä WeSchemessä):
;#lang racket
;(require 2htdp/universe 2htdp/image picturing-programs)

;; tämä vaaditaan mobiililaitetukeen WeSchemessä:
(require bootstrap2012/bootstrap-tilt-teachpack)

;;----------------------------------------------------------------------------------------------------
;; vakiot:

(define SOLUN-KOKO 40)
(define NOPEUS 0.5)
(define VANHENTUMINEN 20)
(define PELIN-LEVEYS 15)
(define PELIN-KORKEUS 15)
(define LEVEYS (* PELIN-LEVEYS SOLUN-KOKO))
(define KORKEUS (* PELIN-KORKEUS SOLUN-KOKO))
(define TEKSTI-KOKO 30)
(define TEKSTI-KOKO-PIENI (- TEKSTI-KOKO 10))
(define SOLU-IMG (crop 3 3 40 40 (circle 23 255 (make-color 154 178 0))))
(define PÄÄ-OIKEA-IMG (overlay/align "right" 
                                     "middle" 
                                     (rotate 90 (triangle 24.5 "solid" "white")) 
                                     SOLU-IMG))
(define PÄÄ-YLÖS-IMG (rotate 90 PÄÄ-OIKEA-IMG))
(define PÄÄ-ALAS-IMG (rotate 270 PÄÄ-OIKEA-IMG))
(define PÄÄ-VASEN-IMG (rotate 180 PÄÄ-OIKEA-IMG))
(define PINKKI (make-color 254 199 174 ))

;; lisätty koska WeScheme ei tue lopetuskuvan piirtämistä automaattisesti
(define PELI-KÄYNNISSÄ 1)
(define PELI-LOPPUI 0)

;;----------------------------------------------------------------------------------------------------
;; mobiililaitetuki vaatii näitä:

;; TILT-koodin asetukset
(define TOLERANSSI 20)
(define -TOLERANSSI (- TOLERANSSI))
(define LAITE-PYSTYSSÄ 40)

;; lisätty ohjausnuolet kosketusnäytölle
(define NUOLI-YLÖS (triangle 40 "outline" "black"))
(define NUOLI-ALAS (rotate 180 NUOLI-YLÖS))
(define NUOLI-OIKEA (rotate 270 NUOLI-YLÖS))
(define NUOLI-VASEN (rotate 180 NUOLI-OIKEA))
(define NUOLET (overlay/xy 
                (overlay/xy NUOLI-VASEN 70 3 NUOLI-YLÖS) 
                130 0 
                (overlay/xy NUOLI-ALAS 75 -3 NUOLI-OIKEA))) 
(define TYHJÄ-KENTTÄ (overlay/align "middle" "bottom" NUOLET (empty-scene LEVEYS KORKEUS)))
(define NAPIN-LEVEYS 57)
(define NAPIN-KORKEUS 40)

;; napin vasemman reunan rajan x-koordinaatti
(define VASEMMALLE-NAPPI-X 190)
(define YLÖS-NAPPI-X (+ VASEMMALLE-NAPPI-X NAPIN-LEVEYS))
(define ALAS-NAPPI-X (+ YLÖS-NAPPI-X NAPIN-LEVEYS))
(define OIKEALLE-NAPPI-X (+ ALAS-NAPPI-X NAPIN-LEVEYS))
(define NAPIT-Y (- KORKEUS NAPIN-KORKEUS))

;;----------------------------------------------------------------------------------------------------
;; kuvatiedostojen käsittely:

;; kuvan saa suoraan DrRacketiin tiedostosta (kommentoi pois seuraava rivi WeSchemessä):
;(define LIMA-IMG .)

;; kuvan saa WeSchemessä vain URL:n kautta (kommentoi pois seuraava rivi DrRacketissä):
(define LIMA-IMG (image-url "http://goo.gl/FdiIOF"))

;; tällä kierretään WeSchemen bugi (ei näytä "ä" ja "ö" kirjaimia oikein ruudulla
(define OHJETEKSTI (scale 0.6 (image-url "http://goo.gl/7xEIkv")))

;;----------------------------------------------------------------------------------------------------
;; käytetyt tietorakenteet: 

(define-struct sijainti (x y))
(define-struct mato (suunta sijainti))
(define-struct lima (tuoreus sijainti))
;; lisätty status-kenttä, jotta lopetusruudun saa piirrettyä ok
(define-struct kenttä (mato lima status))

;;----------------------------------------------------------------------------------------------------
;; pelilogiikkaa : hahmojen liikkuminen, liman syöminen etc.

;; sijainti=? : sijainti sijainti -> boolean
;; tutkitaan onko kaksi sijaintia samat
(define (sijainti=? s1 s2)
  (and (= (sijainti-x s1)(sijainti-x s2))
       (= (sijainti-y s1)(sijainti-y s2))))
       
;; kohdalla? : sijainti lima -> boolean 
;; tutkitaan onko lima pään kohdalla
(define (kohdalla? pää l)
  (sijainti=? pää (lima-sijainti l)))

;; voiko-syödä : mato lima -> boolean/lima
;; syötävän liman etsiminen
(define (voiko-syödä m limat)
  (cond [(empty? limat) #f]
        [else (if (kohdalla? (first (mato-sijainti m)) (first limat))
                  (first limat)
                  (voiko-syödä m (rest limat)))]))

;; syö-limaa : (listof lima) lima -> (listof lima)
;; liman syöminen ja uuden liman luominen
(define (syö-limaa limat syötävä-lima)
  (cons (uusi-lima) (remove syötävä-lima limat)))

;; siirrä-pää : sijainti number number -> boolean
;; uuden pään sijainti
(define (siirrä-pää pää dx dy)
  (make-sijainti (+ (sijainti-x pää) dx) (+ (sijainti-y pää) dy)))

;; uusi-pää : mato -> sijainti
;; uuden pään luominen 
(define (uusi-pää m)
  (let ((pää (first (mato-sijainti m)))
        (suunta (mato-suunta m)))
  (cond [(string=? suunta "up")(siirrä-pää pää 0 -1)]
        [(string=? suunta "down")(siirrä-pää pää 0 1)]
        [(string=? suunta "left")(siirrä-pää pää -1 0)]
        [(string=? suunta "right")(siirrä-pää pää 1 0)])))

;; poista-vika : (listof sijainti) -> (listof sijainti)
;; poista madon viimeinen solu
(define (poista-vika solut)
  (cond [(empty? (rest solut)) empty]
        [else (cons (first solut)(poista-vika (rest solut)))]))

;; liikuta-matoa : mato -> mato
(define (liikuta-matoa m)
  (make-mato (mato-suunta m) (cons (uusi-pää m)(poista-vika (mato-sijainti m)))))

;; kasvata-matoa : mato -> mato
(define (kasvata-matoa m)
  (make-mato (mato-suunta m) (cons (uusi-pää m) (mato-sijainti m))))

;; mädätä-lima : lima -> lima
(define (mädätä-lima l)
  (make-lima (- (lima-tuoreus l) 1) (lima-sijainti l)))
   
;; vanhenna-limat : (listof lima) -> (listof lima)
(define (vanhenna-limat limat)
  (cond [(empty? limat) empty]
        [else (cons (mädätä-lima (first limat)) (vanhenna-limat (rest limat)))]))

;; huono? : lima -> boolean
(define (huono? l)
  (zero? (lima-tuoreus l)))
 
;; uusi-lima : (void) -> lima
(define (uusi-lima)
  (make-lima VANHENTUMINEN 
             (make-sijainti (add1 (random (sub1 PELIN-LEVEYS))) (add1 (random (sub1 PELIN-KORKEUS))))))

;; tarkista-limat : (listof lima) -> (listof lima)
(define (tarkista-limat limat)
  (cond [(empty? limat) empty]
        [(huono? (first limat)) 
         (cons (uusi-lima) (tarkista-limat (rest limat)))]
        [else (cons (first limat)(tarkista-limat (rest limat)))]))

;; vanhenna-limaa : (listof lima) -> (listof lima)
(define (vanhenna-limaa limat)
  (vanhenna-limat (tarkista-limat limat)))

;; suunta? : key -> boolean
(define (suunta? n)
  (or (key=? n "up")
      (key=? n "down")
      (key=? n "left")
      (key=? n "right")))

;; vastakkainen-suunta? : string string -> boolean
(define (vastakkainen-suunta? s1 s2)
  (cond [(and (string=? s1 "up") (string=? s2 "down")) #t]
        [(and (string=? s1 "down") (string=? s2 "up")) #t]
        [(and (string=? s1 "left") (string=? s2 "right")) #t]
        [(and (string=? s1 "right") (string=? s2 "left")) #t]
        [else #f]))

;; suunta-vaihtuu : kenttä key -> kenttä
(define (suunta-vaihtuu k n)
  (let ((m (kenttä-mato k)))
  (cond [(and (vastakkainen-suunta? (mato-suunta m) n) (cons? (rest (mato-sijainti m)))) 
         (make-kenttä (make-mato n (mato-sijainti m)) (kenttä-lima k) PELI-LOPPUI)]
        [else 
        (make-kenttä (make-mato n (mato-sijainti m)) (kenttä-lima k) (kenttä-status k))])))

;; hoida-näppäimet : kenttä key -> kenttä
(define (hoida-näppäimet k näppäin)
  (let ((k-ok (testaa-kuolema k)))
  (cond [(suunta? näppäin) (suunta-vaihtuu k-ok näppäin)]
        [(and (= (kenttä-status k-ok) PELI-LOPPUI) (string=? näppäin " ")) ALKU]
        [else k-ok])))

;; uusi-kenttä : kenttä -> kenttä 
;; määritellään pelin uusi tilanne
(define (uusi-kenttä k)
  (letrec ((k-ok (testaa-kuolema k))
           (m (kenttä-mato k-ok))
           (limat (kenttä-lima k-ok))
           (syötävää-limaa (voiko-syödä m limat)))
  (if (= (kenttä-status k-ok) PELI-KÄYNNISSÄ) 
      (if (not (false? syötävää-limaa))  
          (make-kenttä (kasvata-matoa m) (vanhenna-limaa (syö-limaa limat syötävää-limaa)) (kenttä-status k-ok))
          (make-kenttä (liikuta-matoa m) (vanhenna-limaa limat) (kenttä-status k-ok)))
      k-ok)))

;;---------------------------------------------------------------------------------------------------
;; mobiililaitetuki vaatii näitä:

;; mobiili-hoida-asento : kenttä number nuumber -> kenttä
;; ruutuun napauttaminen on sama kuin "välilyönti"
(define (mobiili-hoida-kosketus k x y)
  (let  ((k-ok (testaa-kuolema k))) 
  (cond [(= (kenttä-status k-ok) PELI-LOPPUI) (hoida-näppäimet k-ok " ")]
        [(and (> y NAPIT-Y)(< VASEMMALLE-NAPPI-X x YLÖS-NAPPI-X)) (hoida-näppäimet k "left")]
        [(and (> y NAPIT-Y) (< YLÖS-NAPPI-X x ALAS-NAPPI-X)) (hoida-näppäimet k "up")]
        [(and (> y NAPIT-Y) (< ALAS-NAPPI-X x OIKEALLE-NAPPI-X)) (hoida-näppäimet k "down")]
        [(and (> y NAPIT-Y) (< OIKEALLE-NAPPI-X x (+ OIKEALLE-NAPPI-X NAPIN-LEVEYS))) (hoida-näppäimet k "right")]
        [else k-ok])))

;; mobiili-käännä-mato : kenttä number number -> kenttä
;; TILT-koodi:
(define (mobiili-hoida-asento k x y)
    (cond
        [(> x TOLERANSSI) (hoida-näppäimet k "right")]
        [(< x -TOLERANSSI) (hoida-näppäimet k "left")]
        [(> (- y LAITE-PYSTYSSÄ) TOLERANSSI) (hoida-näppäimet k "down")]
        [(< (- y LAITE-PYSTYSSÄ) -TOLERANSSI) (hoida-näppäimet k "up")]
        [else k]))

;;----------------------------------------------------------------------------------------------------
;; piirretään pelikenttä 

;; anna-madon-pää : mato -> img
(define (anna-madon-pää m)
  (let ((suunta (mato-suunta m)))
  (cond [(string=? "up" suunta) PÄÄ-YLÖS-IMG]
        [(string=? "down" suunta) PÄÄ-ALAS-IMG]
        [(string=? "left" suunta) PÄÄ-VASEN-IMG]
        [(string=? "right" suunta) PÄÄ-OIKEA-IMG])))

;; kuva+kenttä : sijainti img img -> img
(define (kuva+kenttä sij kuva-img lima-kenttä)
  (place-image kuva-img 
               (* (sijainti-x sij) SOLUN-KOKO)
               (* (sijainti-y sij) SOLUN-KOKO) 
               lima-kenttä))

;; piirrä-madon-osat : (listof sijainti) img img -> img
(define (piirrä-madon-osat solut solu-img lima-kenttä)
  (cond [(empty? solut) lima-kenttä]
        [else (kuva+kenttä (first solut) 
                           solu-img 
                           (piirrä-madon-osat (rest solut) solu-img lima-kenttä))]))

;; piirrä-limat (listof sijainti) img img -> img
(define (piirrä-limat limat lima-img lima-kenttä)
  (cond [(empty? limat) lima-kenttä]
        [else (kuva+kenttä (lima-sijainti (first limat)) 
                           lima-img 
                           (piirrä-limat (rest limat) lima-img lima-kenttä))]))
  
;; mato+kenttä : mato img -> img
(define (mato+kenttä m lima-kenttä)
  (let ((madon-häntä (piirrä-madon-osat (rest (mato-sijainti m)) 
                                        SOLU-IMG 
                                        lima-kenttä))
        (madon-pää (first (mato-sijainti m))))
  (kuva+kenttä madon-pää (anna-madon-pää m) madon-häntä)))
 
;; limat+kenttä : lima img -> img
(define (limat+kenttä l lima-kenttä)
  (piirrä-limat l LIMA-IMG lima-kenttä))
   
;; piirrä-kenttä : kenttä -> img
(define (piirrä-kenttä k)
  (let ((k-ok (testaa-kuolema k)))
  (cond [(= (kenttä-status k-ok) PELI-KÄYNNISSÄ) 
         (mato+kenttä (kenttä-mato k) (limat+kenttä (kenttä-lima k) TYHJÄ-KENTTÄ))]
        [(= (kenttä-status k-ok) PELI-LOPPUI) 
         (overlay (overlay/xy (text "Game Over" TEKSTI-KOKO "black") 
                              0 30 
                              ; mobiiliversion ohje:
                              (text "Pelaa uudelleen napauttamalla ruutua" TEKSTI-KOKO-PIENI PINKKI))
                              ; normipelin ohje:
                              ;OHJETEKSTI)
                       (mato+kenttä (kenttä-mato k) (limat+kenttä (kenttä-lima k) TYHJÄ-KENTTÄ)))])))

;; piirrä-loppu : kenttä -> img
(define (piirrä-loppu k)
  (overlay (text "Game Over" TEKSTI-KOKO "black") (piirrä-kenttä k)))

;;----------------------------------------------------------------------------------------------------
;; pelilogiikkaa : milloin lopetetaan?

;; törmäsi-itseensä? : mato -> boolean
(define (törmäsi-itseensä? m)
  ; DrRacket vaatii tämän:
  ;(member (first (mato-sijainti m)) (rest (mato-sijainti m)) sijainti=?))
  ; WeScheme vaatii tämän:  
  (member (first (mato-sijainti m)) (rest (mato-sijainti m))))
          
;; törmäsi-seinään? : mato -> boolean
(define (törmäsi-seinään? m)
  (let ((x (sijainti-x (first (mato-sijainti m))))
        (y (sijainti-y (first (mato-sijainti m)))))
  (or (= x 0) (= x PELIN-LEVEYS) (= y 0) (= y PELIN-KORKEUS))))

;; testaa-kuolema : kenttä -> kenttä
(define (testaa-kuolema k)
  (let ((m (kenttä-mato k)))
  (if (or (cons? (törmäsi-itseensä? m)) (törmäsi-seinään? m))
      (make-kenttä (make-mato (mato-suunta m) (mato-sijainti m)) (kenttä-lima k) PELI-LOPPUI)
      (make-kenttä (make-mato (mato-suunta m) (mato-sijainti m)) (kenttä-lima k) (kenttä-status k)))))

;;----------------------------------------------------------------------------------------------------
;; pääohjelma

;; lisätty, että saadaan uusi peli kätevästi käyntiin
(define ALKU (make-kenttä (make-mato "right" (list (make-sijainti 1 1))) 
                         (list (uusi-lima)(uusi-lima)(uusi-lima)(uusi-lima)(uusi-lima))
                         PELI-KÄYNNISSÄ))

;; aloita-matopeli : (void) -> kenttä
(define (aloita-matopeli)
  (big-bang ALKU
            (on-tick uusi-kenttä NOPEUS)
            (on-key hoida-näppäimet)
            (to-draw piirrä-kenttä)
            ; tarvitaan mobiililaitetukeen (vain on-tilt tai on-tap):
            ;(on-tilt mobiili-hoida-asento)
            (on-tap mobiili-hoida-kosketus)))

;; aloitetaan peli
(aloita-matopeli)

