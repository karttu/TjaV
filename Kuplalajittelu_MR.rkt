;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Kuplalajittelu_MR) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;;
;; Malliratkaisun kuplalajittelusta kirjoitti 2017-12-27 ja 2018-03-25 Antti Karttunen,
;; ja funktioiden nimiä paranteli Timo Poranen.
;; Tämä koodi on julkaistu lisenssillä CC BY-NC-SA 4.0:
;; https://creativecommons.org/licenses/by-nc-sa/4.0/
;; (Creative Commons Nimeä-EiKaupallinen-JaaSamoin 4.0 Kansainvälinen Lisenssi).
;;

;; WeScheme-versio (joka on täysin sama) löytyy osoitteella: https://www.wescheme.org/view?publicId=P9i9He2l5G


;; Katso:
;; https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5v
;; ja
;; https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5v/t1k

;;
;; Huom: tässä ratkaisussa, varsinaisen työn tekevä funktio "lajittelukierros" käyttää
;; sutjakan häntärekursion sijasta raskaampaa täyttä rekursiota, jossa joka kutsusta joudutaan
;; vielä palaamaan edelliselle tasolle, koska palautettavaa arvoa tarvitaan siellä.

;; Mutta koska kupla-lajittelu on jo algoritminakin hidas, niin toteutuksen yksityiskohdilla
;; ei tässä ole niin väliä. Ja kun käytetään vektoreiden sijasta listoja
;; niin välitulokset pitää joka tapauksessa rakentaa ("consata") aina uudestaan,
;; mikä on sekin hidasta. Niille, joita tämä häiritsee, on tämän modulin lopussa annettu "vektoreita"
;; (yksiulotteisia taulukoita) käyttävä häntärekursiivinen ratkaisu.
;;

(define (kuplalajittele lista)
   (let ([lista2 (lajittelukierros lista)])
      (if (equal? lista2 lista) ;; Jos yhden lajittelukierroksen jälkeen ei mikään muuttunut,
          lista ;; niin silloin lista on jo järjestyksessä, pienimmästä suurimpaan.
          (kuplalajittele lista2) ;; Muussa tapauksessa jatka seuraavalle kierrokselle.
      )
   )
)


(define (lajittelukierros lista)
   (cond [(or (empty? lista) (empty? (rest lista))) lista] ;; Tultiin listan loppuun, palauta listan loppu.
         [(> (first lista) (second lista)) ;; Tässä kohdin alkiot väärässä järjestyksessä?
            (cons (second lista)  ;; Vaihda tokana tuleva ekan paikalle, ja jatka lajittelua:
                  (lajittelukierros (cons (first lista) (rest (rest lista))))
            )
         ] ;; Muuten kaksi ekaa alkiota ovat jo valmiiksi oikein päin, jatka lajittelua listan loppua kohti:
         [else (cons (first lista) (lajittelukierros (rest lista)))]
   )
)

;; Vrt. https://en.wikipedia.org/wiki/Bubble_sort#Performance

(check-expect (lajittelukierros '(5 3 1 6 7 2 4 8)) '(3 1 5 6 2 4 7 8))



;; Malliratkaisut, tehtäviin sivulla https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5v/t1k
;; Tehtävä 1: Testaa kuplalajittelua ainakin kolmen erilaisen listan lajitteluun:
;; (tulkki-ikkunassakin voi tietysti testata!)

(check-expect (kuplalajittele '()) '())

(check-expect (kuplalajittele '(-7)) '(-7))

(check-expect (kuplalajittele '(2 1)) '(1 2))

(check-expect (kuplalajittele '(5 3 1 6 7 2 7 4 4 4 8)) '(1 2 3 4 4 4 5 6 7 7 8))

;; Tehtävä 2: a) Millainen lajiteltavan listan pitää olla, että kuplalajittelu toimisi mahdollisimman nopeasti?
;; Vastaus: valmiiksi järjestyksessä:
(check-expect (kuplalajittele '(1 2 3 4 5 6 7 8)) '(1 2 3 4 5 6 7 8))

(check-expect (kuplalajittele '()) '()) ;; ja tietysti lyhyyskin on valttia!


;; Tehtävä 2: b) Millainen lajiteltavan listan pitää olla, että kuplalajittelu toimisi mahdollisimman hitaasti?
;;

;; Malliratkaisu:

;; Asiaa tutkiaksemme kirjoitamme seuraavan funktion, joka toimii muuten kuin kuplalajittele, mutta
;; palauttaakin järjestetyn listan sijasta kuplakierrosten lukumäärän.


(define (kuplakierroksia lista) (kuplakierroksia-apu lista 1)) ;; k alustetaan yhdeksi, koska aina tehdään vähintään yksi lajittelukierros.
  
(define (kuplakierroksia-apu lista k)
   (let ([lista2 (lajittelukierros lista)])
      (if (equal? lista2 lista) ;; Valmis?
          k ;; Palauta kierrosten määrä.
          (kuplakierroksia-apu lista2 (+ 1 k)) ;; Muussa tapauksessa jatka seuraavalle kierrokselle, lisää k:ta yhdellä.
      )
   )
)

;; Kun tutkimme asiaa sillän, niin huomaamme että:

;; Ainakin yksi kierros tarvitaan, jotta huomattaisiin edes se että lista on valmiiksi järjestyksessä:
(check-expect (kuplakierroksia '()) 1)

(check-expect (kuplakierroksia '(100)) 1)

;; Näyttäisi siltä, että suurin määrä kierroksia saadaan kun lista on järjestetty suurimmasta pienimpään eli takaperin:


(check-expect (kuplakierroksia '(2 1)) 2)

(check-expect (kuplakierroksia '(3 2 1)) 3)

(check-expect (kuplakierroksia '(4 3 2 1)) 4)


(check-expect (kuplakierroksia '(8 7 6 5 4 3 2 1)) 8)

;; Toisaalta yhtä paljon kierroksia saadaan kulumaan hiukan toisenlaisillakin järjestyksillä:

(check-expect (kuplakierroksia '(8 6 7 5 4 3 2 1)) 8)

(check-expect (kuplakierroksia '(8 7 5 6 4 3 2 1)) 8)


;; Tehtävä 3: Muokkaa kuplalajittelun algoritmia siten, että lista lajitellaan suurimmasta pienimpään alkioon.

;; Malliratkaisu: vaihda funktiossa lajittelukierros ehdossa (> (first lista) (second lista))
;; suurempi-kuin predikaatti (>) pienempi-kuin predikaatiksi (<).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Extra-malliratkaisu, lisätty 2018-03-25:
;; Näytetään miten sama algoritmi tehtäisiin jos lajittelukierros käyttää listojen sijasta "vektoreita" eli yksiulotteisia
;; taulukoita. 
;;
;; Racket-kielen "vektoreiden" toteutuksesta löytyy englanninkielistä dokumentaatiota:
;; http://docs.racket-lang.org/guide/vectors.html
;; http://docs.racket-lang.org/reference/vectors.html
;;
;; Ja suomen kielellä taulukoista yleisemmin löytyy tietoa: https://fi.wikipedia.org/wiki/Taulukko_(tietorakenne)
;;
;; Huomaa että monella tapaa lista ja yksiulotteinen taulukko ("vector") ovat tietotyyppeinä miltei samanlaisia:
;; niissä on järjestetty kokoelma alkioita, niillä on tietty koko (alkioita on siis äärellinen määrä),
;; ja kuhunkin alkioon voidaan viitata myös erikseen (se voidaan poimia muiden alkioiden seasta, kun tiedetään
;; sen paikka).
;; Erotuksena listoihin, vektoreissa yksittäinen alkio saadaan poimittua aina yhtä nopeasti,
;; riippumatta onko se vektorin alku- vai loppupäässä, kun taas listoissa loppupäässä oleva alkion poimiminen
;; on hidasta, koska siihen ei päästä käsiksi muuten kuin askel askelelta listan alkupäästä lähtien.
;; Toinen tärkeä ero on siinä, että taulukon alkioita voi myös (yleensä) muuttaa taulukon luomisen jälkeenkin,
;; siis tietyssä kohdassa olevan vanhan arvon päälle voi kirjoittaa uuden arvon, niin että uusi arvo
;; näkyy sen jälkeen myös koko vektorin arvossa.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tämäkin versio haluaa edelleen argumenttinaan listan, ja listan se myös palauttaa. Muunnos vektoriksi ja vektorista takaisin
;; listaksi tehdään funktioilla list->vector ja vector->list:

(define (kuplalajittele2 lista) (vector->list (kuplalajittele-v! (list->vector lista) (length lista))))


(define (kuplalajittele-v! v koko)
   (if (zero? (lajittelukierros-vektoreilla! v koko 0 0)) ;; Jos lajittelukierroksen jälkeen ei mikään muuttunut (tehtyjä muutoksia = 0),
       v ;; niin silloin vektorin alkiot ovat jo järjestyksessä, pienimmästä suurimpaan, palauta se tuloksena, kaikkine muutoksineen
       (kuplalajittele-v! v koko) ;; Muussa tapauksessa jatka seuraavalle kierrokselle.
   )
)


;; Aiempaan listaversioon verrattuna funktion lajittelukierros toteutus on muuttunut eniten (uusi nimi: lajittelukierros-vektoreilla!).
;; Argumenteikseen se haluaa vektorin v, johon muutokset tehdään, vektorin (eli alunperin annetun listan) koon (koko),
;; indeksin i (joka alkaa nollasta, ja kasvaa aina yhtä pienemmäksi kuin muuttujan koko arvo),
;; sekä neljäntenä argumenttinaan kullakin kierroksella tehtyjen muutosten määrän (nimeltään muutoksia) joka alkaa nollasta.

;; (begin ... ... ...) muoto kertoo tulkille että sen sisällä tehdään monta asiaa peräkkäin, ja vain viimeisen
;; palauttamaa arvoa käytetään. Toisin sanoen kaikki muut kuin viimeinen lauseke tehdään vain niiden sivuvaikutusten
;; vuoksi. Tässä sivuvaikutuksia aiheuttaa funktio (vector-set! v i arvo) joka vaihtaa vektorin v alkion v[i] uudeksi sisällöksi
;; arvon arvo.
;; (vector-ref v i) taas antaa vektorin i:nnen alkion (jos ensimmäinen alkio on siis "nollas" alkio, samoinhan toimii list-ref).
;;
;; Huomaa että alkioiden vaihtoa ei voi tehdä näin, pelkästään kahdella lauseella (yleinen virhe!):
;;  (vector-set! v       i  (vector-ref v (+ 1 i))) 
;;  (vector-set! v (+ 1 i)  (vector-ref v i)) 
;; koska jälkimmäistä lausetta suoritettaessa v[i]:n päälle on kirjoitettu jo vanha v[i+1]:n arvo, ja v[i]:n vanha arvo on
;; sen jälkeen hukattu ainiaaksi. Tämän vuoksi v[i]:n vanha arvo pitää ensin ottaa väliaikaisesti talteen omaan muuttujaansa,
;; tässä nimeltään tmp (tulee englannin sanasta "temporary").

;; Tärkeänä erona alkuperäiseen listoja käyttävään lajittelukierros -funktioon tässä on myös se,
;; että tämä toteutus on täysin häntärekursiivinen. Kun funktio on kutsunut itseään (joko cond-käskyn toisesta
;; tai kolmannesta haarasta), sillä ei ole mitään varsinaista syytä palata aiemmalle tasolle, vaan
;; sitten kun indeksi i on kasvanut koko-1 :ksi, niin voidaan palauttaa heti tehtyjen muutosten määrä
;; (muutoksia), josta ylemmän tason funktio kuplalajittele-v! näkee onko lajittelu saatu vihdoin päätökseen.
;;



(define (lajittelukierros-vektoreilla! v koko i muutoksia)
   (cond [(>= i (- koko 1)) muutoksia] ;; Tultiin vektorin (melkein) loppuun, palauta tällä kierroksella tehtyjen muutosten määrä.
         [(> (vector-ref v i) (vector-ref v (+ 1 i))) ;; Tässä kohdin alkiot v[i] ja v[i+1] väärässä järjestyksessä?
            (let ([tmp (vector-ref v i)]) ;; Otetaan ensin alkio v[i] talteen "väliaikaismuuttujaan" nimeltä tmp
                (begin
                  (vector-set! v       i  (vector-ref v (+ 1 i))) ;; Koska kirjoitamme sen vanhan arvon "päälle" alkion v[i+1] arvon.
                  (vector-set! v (+ 1 i)  tmp)                    ;; Jonka jälkeen vasta vanha v[i]:n arvo voidaan kirjoittaa alkion v[i+1] "päälle".
                  (lajittelukierros-vektoreilla! v koko (+ 1 i) (+ 1 muutoksia)) ;; Ja sitten jatketaan taulukon loppua kohti (indeksi i kasvaa yhdellä), samoin muutokset
                )
            )
         ] ;; Muuten kaksi alkiota v[i] ja v[i+1] ovat jo valmiiksi oikein päin, jatka lajittelua vektorin (taulukon) loppua kohti:
         [else (lajittelukierros-vektoreilla! v koko (+ 1 i) muutoksia)]
   )
)

;; Määritellään muuttuja testivektori (kahdeksan alkion yksiulotteinen taulukko),
;; johon yllämääritelty funktio seuraavaksi tulee tekemään sivuvaikutuksellisia muutoksia:

(define testivektori (vector 5 3 1 6 7 2 4 8))

;; Huomaa kuinka tässä lajittelukierros-vektoreilla! palauttaa varsinaisena tuloksenaan
;; (paluuarvonaan) 4:n, sillä muutoksia (vaihdoksia) tehdään neljä kappaletta yhden
;; kierroksen aikana: 5 ja 3 vaihtavat paikkaa, sen jälkeen 5 ja 1, jonka jälkeen 5 ja 6
;; eivät vaihda paikkaa, mutta 7 ja 2 vaihtavat, samoin kuin 7 ja 4. Yhteensä neljä vaihdosta:

(check-expect (lajittelukierros-vektoreilla! testivektori (vector-length testivektori) 0 0)
              4)

;; Funktiokutsun varsinainen tarkoitus on kuitenkin ensimmäisenä annettuun vektori-argumenttiin
;; tehdyt muutokset, ja huomataan että tosiaan, muuttuja testivektori on kutsun jälkeen
;; muuttunut sisällöltään:

(check-expect testivektori (vector 3 1 5 6 2 4 7 8))

;; Seuraavalla kierroksella 3 ja 1, 6 ja 2 sekä 6 ja 4 vaihtavat paikkoja (yht. 3 muutosta):
(check-expect (lajittelukierros-vektoreilla! testivektori (vector-length testivektori) 0 0)
              3)

(check-expect testivektori (vector 1 3 5 2 4 6 7 8)) ;; Ja testivektori on taas muuttunut.


;;
;; Funktioiden nimeämisestä: Jos funktio tekee muutoksia annettuihin argumentteihin (siis aiheuttaa sivuvaikutuksia)
;; niin tapana on lisätä sen nimen loppuun huutomerkki (! joka on siis osa nimeä). Huutomerkki varoittaa koodin lukijaa
;; siitä, että funktio ei ole "puhdas", toisin sanoen, paitsi että vaikka se palauttaa (mahdollisesti) jonkin arvon,
;; se saattaa tehdä sellaisia muutoksia argumentteihinsa, joiden kaikkia seurauksia (kutsuvassa funktiossa, tai sitä vielä
;; ylemmällä tasolla) ei aina tule ajatelleeksi.
;;
;; Tästä syystä sellaisten varusfunktioiden määrää on suuresti rajoitettu Racket:in opetuskielissä,
;; vaikka niitä käyttämällä voikin usein kirjoittaa tehokkaampaa koodia.
;;
;; Tässä tapauksessa ylemmän tason funktiossa kuplalajittele2 kutsutut funktiot list->vector ja vector->list, jotka
;; aina rakentavat uuden vektorin tai listan (ja siten siis kopioivat tietoa) estävät mahdollisten sivuvaikutusten
;; leviämisen minnekään kauemmaksi. (Ja siksi emme myöskään lisää kuplalajittele2 funktion nimen perään huutomerkkiä).
;;

(check-expect (kuplalajittele2 '()) '())

(check-expect (kuplalajittele2 '(-7)) '(-7))

(check-expect (kuplalajittele2 '(2 1)) '(1 2))

(check-expect (kuplalajittele2 '(5 3 1 6 7 2 7 4 4 4 8)) '(1 2 3 4 4 4 5 6 7 7 8))

(check-expect (kuplalajittele2 '(1 2 3 4 5 6 7 8)) '(1 2 3 4 5 6 7 8))

(check-expect (kuplalajittele2 '(8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8))


              