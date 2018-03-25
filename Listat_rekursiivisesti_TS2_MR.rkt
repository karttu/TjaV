;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Listat_rekursiivisesti_TS2_MR) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; Tehtäväsarja 2: Listojen käsittelyä rekursiivisesti - Malliratkaisut sivulla
;; https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5lk/t2ak
;; annettuihin neljään tehtävään.
;;
;;
;; Malliratkaisut kirjoitti Antti Karttunen, 11. & 19. maaliskuuta 2018. 
;; Julkaistu lisenssillä CC BY-NC-SA 4.0:
;; https://creativecommons.org/licenses/by-nc-sa/4.0/
;; (Creative Commons Nimeä-EiKaupallinen-JaaSamoin 4.0 Kansainvälinen Lisenssi).
;;

;; WeScheme-versio löytyy täältä: https://www.wescheme.org/view?publicId=DzxR7QrSz0

;;
;; Seuraavissa tehtävissä harjoitellaan edelleen häntärekursiota.
;; Kaikille on ominaista, että varsinainen käyttäjän kutsuma funktio
;; kutsuu erillistä apufunktiota, jossa on mukana yksi "ylimääräinen"
;; argumentti (näissä esimerkeissä nimeltään tulos), joka tuossa kutsussa
;; "alustetaan" ensin esimerkiksi nollaksi tai tyhjäksi listaksi, ja johon tulos
;; sitten kierros kierrokselta "kasvaa", "kerääntyy" tai "kumuloituu" (engl. accumulates).
;;
;; Jos tulos on tyypiltään luku, niin silloin sen kasvattaminen tapahtuu
;; esimerkiksi plus tai kertolaskufunktion (+ tai *) avulla, ja jos se on
;; tyypiltään lista, silloin sitä useimmiten kasvatetaan cons-funktiolla,
;; joka lisää listan alkuun uuden alkion.
;;

;; Tehtävä 1: Kirjoita rekursiivinen funktio, joka saa argumenttina
;; kokonaislukuja sisältävän listan, ja palauttaa listan lukujen tulon.

;; Malliratkaisu:

;; Huom: tässä tulos alustetaan nollan sijasta ykköseksi, sillä matematiikassa
;; vakiintunut käytäntö on, että vaikka tyhjän joukon summa on nolla,
;; niin tyhjän joukon tulo on yksi, mikä sopimus tekee myös tämän
;; ohjelmoimisesta paljon yksinkertaisempaa, koska ensimmäisessä
;; funktiossa ei silloin erikseen tarvitse tarkistella onko argumenttina
;; annettu luvut-lista tyhjä vai ei.

(define (lukujen-tulo luvut) (lukujen-tulo_apu luvut 1))

(define (lukujen-tulo_apu luvut tulos)
  (if (empty? luvut)     ;; Onko kaikki luvut "syöty" jo?
      tulos              ;; Jos on, niin palauta tulos-apumuuttujaan laskettu lukujen tulo.
      (lukujen-tulo_apu  ;; Muussa tapauksessa, listassa on lukuja jäljellä vielä, joten "kutsu itseä" eli palaa alkuun,
               (rest luvut) ;; siten että luvut-listan alusta poistetaan ensimmäinen luku,
               (* (first luvut) tulos) ;; jolla kerrotaan tähän asti saatu tulos, joka tulee tulos-muuttujan uudeksi arvoksi.
      )
  )
)

(check-expect (lukujen-tulo (list 1 2 3 4 5 6 7)) 5040)

(check-expect (lukujen-tulo (list 77)) 77)

(check-expect (lukujen-tulo (list 3 3 3 3 3 3)) 729)

(check-expect (lukujen-tulo (list -1 -1 -1 -1)) 1)

(check-expect (lukujen-tulo (list)) 1)



;; Tehtävä 2: Kirjoita rekursiivinen funktio, joka saa argumenttina
;; luvun n, ja palauttaa listan jossa on luvut 1, 2, 3, ..., n.

;; Malliratkaisu:

;; Huomaa kuinka tässä apufunktion luvut-yhdestä-ännään_apu apumuuttuja tulos
;; alustetaan tyhjäksi listaksi, mikä saadaan esimerkiksi kutsumalla list-funktiota
;; ilman argumentteja, siis (list):

(define (luvut-yhdestä-ännään n) (luvut-yhdestä-ännään_apu n (list)))


(define (luvut-yhdestä-ännään_apu n tulos)
  (if (zero? n) ;; Jos n tuli nollaksi...
      tulos     ;; Niin palauta rakennettu tuloslista. Tässä tapauksessa on valmiiksi
                ;; juuri niinpäin kuin halutaan, joten ei tarvita reverse-funktiota.
;; Muussa tapauksessa vähennä n:ää yhdellä ja lisää tämänhetkinen n rakennettavan
;; luvut-listan alkuun, ja palaa silmukan alkuun.
      (luvut-yhdestä-ännään_apu (- n 1) (cons n tulos))
  )
)

(check-expect (luvut-yhdestä-ännään 0) '())

(check-expect (luvut-yhdestä-ännään 1) '(1))

(check-expect (luvut-yhdestä-ännään 2) '(1 2))

(check-expect (luvut-yhdestä-ännään 10) '(1 2 3 4 5 6 7 8 9 10))


(check-expect (length (luvut-yhdestä-ännään 123)) 123)



;; Väliharjoitus tehtävään kolme: Yritä miettiä ensin testaamatta mitä seuraava funktio tekee:

(define (kaskummaa lista) (kaskummaa-apu lista empty))

(define (kaskummaa-apu lista tulos)
   (if (empty? lista) ;; Kun lista on "syöty loppuun"...
       tulos          ;; palautetaan tulos.
       ;; Muuten hypätään takaisin alkuun, siten että...
       (kaskummaa-apu (rest lista)  ;;  ... listaa lista "syödään"
                      (cons (first lista) tulos) ;; mutta listaa tulos "rakennetaan",
                      ;; siten että listan lista ensimmäinen alkio siirretään listan tulos alkuun.
       )
   )
)

;; Kokeile sen jälkeen tulkki-ikkunassa vaikkapa (kaskummaa (list 11 22 33))
;; Yllätyitkö? Mikä on vastaavan valmisfunktion nimi Racketissa/WeSchemessä?

;; Vastaus: Kyse on tietenkin reverse-funktiosta, joka kääntää listan alkiot takaperin:

(check-expect (kaskummaa (list)) (list))

(check-expect (kaskummaa (list 1 2 3)) (list 3 2 1))

(check-expect (kaskummaa (list 1 (list 2 3 4) 5)) (list 5 (list 2 3 4) 1))

;; Tämä listan järjestyksen kääntyminen pitää ottaa huomioon silloin kuin uutta tulos-listaa
;; kasvatetaan cons-funktiolla, esimerkiksi kääntämällä tulos-lista aivan lopuksi reverse-funktiolla.
;; (Listan kääntäminen on jonkin verran aikaa vievää, joten se kannattaa tehdä vasta lopuksi.)


;; Tehtävä 3:
;; Kirjoita rekursiivinen funktio, joka saa argumenttina lukuja
;; sisältävän listan ja luvun n, ja palauttaa listan jossa on
;; vain argumenttina annetun listan ne luvut, jotka ovat > n.

;; Mikäli ehtoon täsmäävissä luvuissa on toistoa, niin tuloksessakin on toistoa.
;; Lisäksi saatujen lukujen pitäisi tulla samassa järjestyksessä kuin ne
;; esiintyvät argumenttina annetussa listassa luvut.

;; Malliratkaisu:

(define (kaikki-suuremmat-kuin n luvut) (kaikki-suuremmat-apu n luvut (list)))

(define (kaikki-suuremmat-apu n luvut tulos)
   (cond ((empty? luvut)    ;; Jos lista luvut syötiin jo loppuun?
            (reverse tulos) ;; niin palauta lista tulos, takaisin oikeinpäin käännettynä.
         )
         ((> (first luvut) n) ;; Jos listan ensimmäinen alkio (tässä kohdin!) on suurempi kuin n
            (kaikki-suuremmat-apu  ;; niin "kutsu itseä" eli hyppää funktion alkuun, siten että:
                      n            ;;   n pidetään samana,
                      (rest luvut) ;;   ja listan luvut ensimmäinen alkio
                      (cons (first luvut) tulos) ;; siirretään listan tulos alkuun.
            )
         )
         (else  ;; Muussa tapauksessa palaa alkuun niin että lista luvut lyhenee yhdellä alkiolla,
                ;; ja muut muuttujat pysyvät samoina (nyt alkiota ei oteta mukaan tulos-listaan, koska
                ;; se ei ole suurempi kuin n):
            (kaikki-suuremmat-apu n (rest luvut) tulos)
         )
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

;; Malliratkaisu:

(define (lukumäärä luvut n) (lukumäärä-apu luvut n 0))

(define (lukumäärä-apu luvut n tulos)
   (cond ((empty? luvut) tulos) ;; Jos lista luvut "syöty loppuun", niin palauta tulos.
         ((= (first luvut) n)   ;; Jos tässä kohdin listan eka alkio on n,
            (lukumäärä-apu      ;; niin "kutsu itseä" eli hyppää funktion alkuun, siten että:  
                  (rest luvut)  ;;   lista lyhenee yhdellä (siten että eka alkio jätetään pois)
                  n             ;;   n pidetään samana,
                  (+ 1 tulos)   ;;   ja tulos kasvaa yhdellä.
            )
         )
         (else ;; Muuten palaa alkuun niin että luvut-lista lyhenee yhdellä ja muuttujat n ja tulos pysyvät samoina.
            (lukumäärä-apu (rest luvut) n tulos)
         )
   )
)

(check-expect (lukumäärä (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 1)
              4)

(check-expect (lukumäärä (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 212)
              0)

(check-expect (lukumäärä (list) -555) 0) ;; Tyhjässä listassa ei ole ainuttakaan lukua, tulos aina nolla.

;;
;; Malliratkaisu 4b:
;;

;; Huomataan että ylläolevassa ratkaisussa cond:in toinen ja kolmas ehto tekevät miltei saman asian: ainoa ero on
;; siinä kasvaako muuttuja tulos yhdellä vai pysyykö samana.

;; Vaihtoehtoisessa ratkaisussa tätä seikkaa on käytetty hyväksi siten, että cond on lyhentynyt yhdellä lausekkeella,
;; ja testi (= (first luvut) n) on siirretty uuden if-lauseen ehdoksi, joka palauttaa joko ykkösen tai nollan,
;; mikä sitten lisätään muuttujaan tulos:

(define (lukumäärä2 luvut n) (lukumäärä2-apu luvut n 0))

(define (lukumäärä2-apu luvut n tulos)
   (cond ((empty? luvut) tulos) ;; Jos lista luvut "syöty loppuun", niin palauta tulos.
         (else                  ;; Muuten "kutsu itseä" eli hyppää funktion alkuun, siten että:  
            (lukumäärä2-apu 
                  (rest luvut)  ;;   lista lyhenee yhdellä
                  n             ;;   n pidetään samana
                  (+ tulos      ;;   ja tulos-muuttujaan lisätään,
                     (if        ;;     riippuen siitä,
                         (= (first luvut) n)  ;; onko luvut-listan eka alkio (tässä kohdin listaa) sama kuin n vai ei
                         1                    ;; joko 1
                         0                    ;; tai 0. (Huom: jos lisätään 0 niin tulos pysyy tietenkin samana).
                     )
                  )
            )
         )
   )
)


(check-expect (lukumäärä2 (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 1)
              4)

(check-expect (lukumäärä2 (list 1 2 2 3 4 4 1 3 5 7 5 5 1 1 4) 212)
              0)

(check-expect (lukumäärä2 (list) -555) 0) ;; Tyhjässä listassa ei ole ainuttakaan lukua, tulos aina nolla.



