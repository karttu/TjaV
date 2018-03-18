;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Listat_rekursiivisesti_TS2_MR) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; Tehtäväsarja 2: Listojen käsittelyä rekursiivisesti - Malliratkaisut sivulla
;; https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5lk/t2ak
;; annettuihin neljään tehtävään.
;;
;;
;; Malliratkaisut kirjoitti Antti Karttunen, 11. maaliskuuta 2018. 
;; Julkaistu lisenssillä CC BY-NC-SA 4.0:
;; https://creativecommons.org/licenses/by-nc-sa/4.0/
;; (Creative Commons Nimeä-EiKaupallinen-JaaSamoin 4.0 Kansainvälinen Lisenssi).
;;


;; Seuraavissa tehtävissä harjoitellaan edelleen häntärekursiota.
;; Kaikille on ominaista, että varsinainen käyttäjän kutsuma funktio
;; kutsuu erillistä apufunktiota, jossa on mukana yksi "ylimääräinen"
;; tulos-argumentti, joka "alustetaan" ensin esimerkiksi nollaksi
;; tai tyhjäksi listaksi.


;; Tehtävä 1: Kirjoita rekursiivinen funktio, joka saa argumenttina
;; kokonaislukuja sisältävän listan, ja palauttaa listan lukujen tulon.

;; Malliratkaisu:

(define (lukujen-tulo luvut) (lukujen-tulo_apu luvut 1))

(define (lukujen-tulo_apu luvut tulos)
  (if (empty? luvut)
      tulos
      (lukujen-tulo_apu (rest luvut) (* (first luvut) tulos))
  )
)


;; Tehtävä 2: Kirjoita rekursiivinen funktio, joka saa argumenttina
;; luvun n, ja palauttaa listan jossa on luvut 1, 2, 3, ..., n.

;; Malliratkaisu:

(define (luvut-yhdestä-ännään n) (luvut-yhdestä-ännään_apu (list) n))

;; Tarvitsee apufunktiota, jossa rakennettava luvut-lista kulkee silmukassa mukana
;; toisena argumenttina. (Ekan kerran kutsuttaessa luvut on alustettu tyhjäksi listaksi).

(define (luvut-yhdestä-ännään_apu tulos n)
  (if (zero? n) ;; Jos n tuli nollaksi...
      tulos     ;; Niin palauta rakennettu tuloslista. Tässä tapauksessa on valmiiksi
                ;; juuri niinpäin kuin halutaan, joten ei tarvita reverse-funktiota.
;; Muussa tapauksessa vähennä n:ää yhdellä ja lisää tämänhetkinen n rakennettavan
;; luvut-listan alkuun, ja palaa silmukan alkuun.
      (luvut-yhdestä-ännään_apu (cons n tulos) (- n 1))
  )
)

(check-expect (luvut-yhdestä-ännään 0) '())

(check-expect (luvut-yhdestä-ännään 1) '(1))

(check-expect (luvut-yhdestä-ännään 2) '(1 2))

(check-expect (luvut-yhdestä-ännään 10) '(1 2 3 4 5 6 7 8 9 10))


(check-expect (length (luvut-yhdestä-ännään 123)) 123)

;; Lisämietintä: Mitä tekee seuraava funktio: ?
(define (mikämikä n) (lukujen-tulo (luvut-yhdestä-ännään n)))


;; Huom: Seuraava EI ole häntärekursiivinen funktio, eikä käytä apufunktiota:

(define (luvut-ännästä-yhteen n)
  (if (zero? n) ;; Jos n tuli nollaksi...
      (list)    ;; Niin palauta tyhjä lista, mikä sopivasti päättää rakennetun listan.
;; Muussa tapauksessa laita (tämänhetkinen) luku n sen listan alkuun,
;; joka saadaan kun tuotetaan lista lukuja n-1 :stä yhteen:
      (cons n (luvut-ännästä-yhteen (- n 1)))
  )
)

(check-expect (luvut-ännästä-yhteen 0) (list))
(check-expect (luvut-ännästä-yhteen 1) (list 1))
(check-expect (luvut-ännästä-yhteen 5) (list 5 4 3 2 1))


;; Harjoitus: Yritä miettiä ensin testaamatta mitä seuraava funktio tekee:

(define (kaskummaa lista) (kaskummaa-apu lista empty))

(define (kaskummaa-apu lista tulos)
   (if (empty? lista) ;; Kun lista on "syöty loppuun"...
       tulos          ;; palautetaan tulos
       ;; Muuten hypätään takaisin alkuun, siten että...
       (kaskummaa-apu (rest lista)  ;;  ... listaa lista "syödään"
                      (cons (first lista) tulos) ;; mutta listaa tulos "rakennetaan".
       )
   )
)

;; Kokeile sen jälkeen tulkki-ikkunassa vaikkapa (kaskummaa (list 11 22 33))
;; Yllätyitkö? Mikä on vastaavan valmisfunktion nimi Racketissa/WeSchemessä?

(check-expect (kaskummaa (list)) (list))


(check-expect (kaskummaa (list 1 2 3)) (list 3 2 1))


;; Tehtävä 3:
;; Kirjoita rekursiivinen funktio, joka saa argumenttina lukuja
;; sisältävän listan ja luvun n, ja palauttaa listan jossa on
;; vain argumenttina annetun listan ne luvut, jotka ovat >= n.

;; Mikäli ehtoon täsmäävissä luvuissa on toistoa, niin tuloksessakin on toistoa.
;; Lisäksi saatujen lukujen pitäisi tulla samassa järjestyksessä kuin ne
;; ovat annetussa listassakin.
;; Vihje: Korjaa tuloslistan järjestys vasta myöhemmin.

(define (kaikki-suuremmat-kuin n luvut) (kaikki-suuremmat-apu n luvut (list)))

(define (kaikki-suuremmat-apu n luvut tulos)
   (cond ((empty? luvut) (reverse tulos))
         ((> (first luvut) n)
            (kaikki-suuremmat-apu n (rest luvut) (cons (first luvut) tulos))
         )
         (else (kaikki-suuremmat-apu n (rest luvut) tulos))
   )
)

(check-expect (kaikki-suuremmat-kuin 2 (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4))
              (list 3 4 4 3 5 7 5 5 4))

(check-expect (kaikki-suuremmat-kuin 20 (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4))
              (list))

(check-expect (kaikki-suuremmat-kuin -5 (list)) (list))


;; Tehtävä 4:
;; Kirjoita rekursiivinen funktio, joka saa argumentteina lukuja
;; sisältävän listan ja luvun n, ja palauttaa luvun n esiintymien
;; lukumäärän listassa.

(define (lukumäärä luvut n) (lukumäärä-apu luvut n 0))

(define (lukumäärä-apu luvut n tulos)
   (cond ((empty? luvut) tulos)
         ((= (first luvut) n)
            (lukumäärä-apu (rest luvut) n (+ 1 tulos))
         )
         (else (lukumäärä-apu (rest luvut) n tulos))
   )
)

(check-expect (lukumäärä (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 1)
              4)

(check-expect (lukumäärä (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 212)
              0)

(check-expect (lukumäärä (list) -555) 0)




