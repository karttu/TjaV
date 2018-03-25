;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ahneet-algoritmit-Kolikot_MR) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;;
;; Malliratkaisut tehtäväsarja 1:een, Ahne algoritmi.
;;
;; Koodasi Racketiksi Antti Karttunen, eka versio 2017-12-12,
;; seuraava versio 2018-03-25.
;;
;; Tämä koodi on julkaistu lisenssillä CC BY-NC-SA 4.0:
;; https://creativecommons.org/licenses/by-nc-sa/4.0/
;; (Creative Commons Nimeä-EiKaupallinen-JaaSamoin 4.0 Kansainvälinen Lisenssi).
;;
;; Katso:
;;   https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5dojaa
;; ja:
;;   https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5dojaa/t1aa
;;

;; WeScheme-versio (joka on täysin samanlainen) löytyy täältä: https://www.wescheme.org/view?publicId=JWRwtUEnGG

(define (vähiten-eurokolikoita summa-senteissä)
 (let ([eurokolikot (list 200 100 50 20 10 5 2 1)]) ;; Eurokolikot suurimmasta pienimpään.
   (vähiten-kolikoita-apu summa-senteissä eurokolikot (list))
 )
)

(define (vähiten-kolikoita-apu summa-senteissä kolikot valitut)
    (cond [(zero? summa-senteissä) ;; Onko rahasumma paloiteltu jo kokonaan kolikoiksi?
                valitut ;; Palautetaan lista valituista kolikoista.
          ]
          [(empty? kolikot)
                "summaa ei voi muodostaa käytettävissä olevilla kolikoilla!"
          ]
          [(<= (first kolikot) summa-senteissä) ;; Voidaanko käyttää (jäljellä olevista) suurinta kolikkoa?
               (vähiten-kolikoita-apu (- summa-senteissä (first kolikot)) ;; vähennä tuo kolikko jäljellä olevasta summasta 
                                      kolikot ;; kolikot pysyvät ennallaan, sillä saatamme käyttää samaa kolikkoa useamman kerran
                                      (cons (first kolikot) valitut) ;; Lisätään tämä kolikko valittuihin.
               )
          ]
          ;; Muuten, jos suurin kolikko ei enää "mahdu" summaan, jatketaan paloittelua, niin
          ;; että siirrytään seuraavaksi pienempään kolikkoon (eli jätetään tällä hetkellä
          ;; suurin kolikko pois kolikot-listasta, rest-funktiota kutsumalla). Lista valitut
          ;; pysyy tässä ennallaan, koska emme lisänneet tässä vaihtorahoihin yhtään uutta
          ;; kolikkoa:
          [else
            (vähiten-kolikoita-apu summa-senteissä
                                   (rest kolikot)
                                   valitut
            )
          ]
    )
)

;; Tehtävä 1, malliratkaisu, kokeile ohjelmaa vaihtorahojen laskemiseksi eri rahasummille:

(check-expect (vähiten-eurokolikoita 135) (list 5 10 20 100))

(check-expect (vähiten-eurokolikoita 0) empty)

(check-expect (vähiten-eurokolikoita 2/3) "summaa ei voi muodostaa käytettävissä olevilla kolikoilla!")

(check-expect (vähiten-eurokolikoita 90) (list 20 20 50))

(check-expect (vähiten-eurokolikoita 85) (list 5 10 20 50))

;; Tehtävä 2, malliratkaisu, muokkaa algoritmia siten että käytetään esimerkiksi seuraavia kolikoita:

;; a) 1, 2, 4, 8, 16 ja 32:

(define (vähiten-1248jne-kolikoita summa-senteissä)
 (let ([kolikot (list 32 16 8 4 2 1)]) ;; Kolikot suurimmasta pienimpään.
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)

(check-expect (vähiten-1248jne-kolikoita 0) empty)

(check-expect (vähiten-1248jne-kolikoita 55) (list 1 2 4 16 32))

(check-expect (vähiten-1248jne-kolikoita 21) (list 1 4 16))

;; b) 1, 3, 9 ja 27:

(define (vähiten-1-3-9-27-kolikoita summa-senteissä)
 (let ([kolikot (list 27 9 3 1)]) ;; Kolikot suurimmasta pienimpään.
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)

(check-expect (vähiten-1-3-9-27-kolikoita 55) (list 1 27 27))


(check-expect (vähiten-1-3-9-27-kolikoita 40) (list 1 3 9 27))

;; c) 1, 5, 50 ja 100:

(define (vähiten-1-5-50-100-kolikoita summa-senteissä)
 (let ([kolikot (list 100 50 5 1)]) ;; Kolikot suurimmasta pienimpään.
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)


(check-expect (vähiten-1-5-50-100-kolikoita 56) (list 1 5 50))

(check-expect (vähiten-1-5-50-100-kolikoita 70) (list 5 5 5 5 50))


;; Tehtävä 3, malliratkaisu. Pystyykö ahne algoritmi muodostamaan optimaalisen
;; vaihtorahasumman jos vaihtorahoina käytetään kolikoita 1, 7 ja 15?

(define (vähiten-1-7-15-kolikoita summa-senteissä)
 (let ([kolikot (list 15 7 1)]) ;; Kolikot suurimmasta pienimpään.
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)

(check-expect (vähiten-1-7-15-kolikoita 14) (list 7 7))

(check-expect (vähiten-1-7-15-kolikoita 21) (list 1 1 1 1 1 1 15)) ;; Ei pysty, sillä lista '(7 7 7) olisi lyhyempi!

(check-expect (vähiten-1-7-15-kolikoita 24) (list 1 1 7 15))


;; Ja bonus-mallivastauksena, "Fibonacci kolikot": 1, 2, 3, 5, 8, 13, 21, 34, 55 ja 89:

(define (vähiten-Fibonacci-kolikoita summa-senteissä)
 (let ([kolikot (list 89 55 34 21 13 8 5 3 2 1)]) ;; Kolikot suurimmasta pienimpään, kymmenen pienintä fibo-kolikkoa
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)

(check-expect (vähiten-Fibonacci-kolikoita 16) (list 3 13))

(check-expect (vähiten-Fibonacci-kolikoita 18) (list 5 13))

(check-expect (vähiten-Fibonacci-kolikoita 54) (list 2 5 13 34))

(check-expect (vähiten-Fibonacci-kolikoita 88) (list 1 3 8 21 55))

(check-expect (vähiten-Fibonacci-kolikoita 100) (list 3 8 89))


(check-expect (vähiten-Fibonacci-kolikoita 143) (list 2 5 13 34 89))

;; Jos näillä Fibonacci-kolikoilla testataan mitä tahansa vaihtosummaa 1:n ja 143:n välillä,
;; niin huomataan, paitsi se että tulos on optimaalinen (pienin mahdollinen määrä vaihtokolikoita),
;; niin myös se että vaihtosummassa ei koskaan esiinny kahta lukuarvoltaan
;; samaa tai "vierettäistä" kolikkoa (esim 1 ja 2, tai 2 ja 3 tai 21 ja 34), vaan aina
;; on vähintään kahden kolikkoarvon verran eroa seuraavaksi suurempaan.
;; (Eli jos esimerkiksi kolikkoa 8 on käytetty, niin seuraavaksi suurempi on
;; vähintäänkin 21 tai sitä suurempi).
;; Lisätietoa: https://fi.wikipedia.org/wiki/Zeckendorfin_lause
;;

;; Seuraava Fibo-kolikko olisi 144, joten sille arvolle yllämainittu ei enää päde kun käytössä on kolikoita vain 89:een asti:
(check-expect (vähiten-Fibonacci-kolikoita 144) (list 55 89))


;; Toisena bonus-mallivastauksena, "kertomakolikot": 1, 2, 6, 24 ja 120:

(define (vähiten-kertomakolikoita summa-senteissä)
 (let ([kolikot (list 120 24 6 2 1)]) ;; Kolikot suurimmasta pienimpään, viisi pienintä kertoma-kolikkoa, n!
   (vähiten-kolikoita-apu summa-senteissä kolikot (list))
 )
)

(check-expect (vähiten-kertomakolikoita 1) (list 1))

(check-expect (vähiten-kertomakolikoita 5) (list 1 2 2))

(check-expect (vähiten-kertomakolikoita 16) (list 2 2 6 6))

(check-expect (vähiten-kertomakolikoita 18) (list 6 6 6))

(check-expect (vähiten-kertomakolikoita 23) (list 1 2 2 6 6 6))

(check-expect (vähiten-kertomakolikoita 25) (list 1 24))

(check-expect (vähiten-kertomakolikoita 50) (list 2 24 24))

(check-expect (vähiten-kertomakolikoita 71) (list 1 2 2 6 6 6 24 24))

(check-expect (vähiten-kertomakolikoita 100) (list 2 2 24 24 24 24))



;; Huomaatko miten vaihtosumma muodostuu mikäli algoritmille annetaan luku, joka
;; on yhtä pienempi kuin jokin kertomaluvuista 2, 6, 24, jne, siis 1, 5, 23 ?
;; Mitä luulet ohjelman antavan vaihtokolikoiksi summalle 119 (= 5! - 1) ?

;; (check-expect (vähiten-kertomakolikoita 119) (list ???))

;;
;; Vielä syvemmälle aiheeseen pureudutaan englanniksi täällä: https://11011110.github.io/blog/2017/12/23/factorial-change-making.html
;;


