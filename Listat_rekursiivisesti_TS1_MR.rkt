;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Listat_rekursiivisesti_TS1_MR) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;;
;; Tehtäväsarja 1: Listojen käsittelyä rekursiivisesti - Malliratkaisut sivulla
;; https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5lk/t1lkr
;; annettuihin neljään tehtävään.
;;
;;
;; Malliratkaisut kirjoitti Antti Karttunen, 11. maaliskuuta 2018. 
;; Julkaistu lisenssillä CC BY-NC-SA 4.0:
;; https://creativecommons.org/licenses/by-nc-sa/4.0/
;; (Creative Commons Nimeä-EiKaupallinen-JaaSamoin 4.0 Kansainvälinen Lisenssi).
;;


;; Tehtävä 1:
;; Esimerkiksi https://peda.net/yhdistykset/maol-ry/materiaalit/kpm/9-luokka/racket-ohjelmointi/4o/5lk
;; mainitusta listassa? funktion määrittelystä mallia ottaen, kirjoita
;; rekursiivinen funktio
;;  (eka-suurempi luku luvut)
;; joka pelkästään lukuja sisältävästä listasta luvut palauttaa ensimmäisen
;; luvun joka on lukua luku suurempi, tai epätoden (#false) mikäli
;; sellaista lukua ei ole (siis mikäli kaikki listan luvut ovat <= luku
;; tai lista on tyhjä).


;; Malliratkaisu:


(define (eka-suurempi luku luvut)
   (cond ((empty? luvut) #false) ;; Jos tultiin jo listan loppuun,
         ;; silloin lukua luku suurempaa lukua ei löytynyt, palauta #false
         ((> (first luvut) luku) (first luvut)) ;; Löytyi suurempi luku: palauta se!
         (else (eka-suurempi luku (rest luvut))) ;; Muuten jatka etsimistä hännästä
   )
)

;; Huomaa että ehto (> (first luvut) luku) suoritetaan, ei ainoastaan funktiolle
;; argumenttina annetun listan aivan alussa, vaan sen jokaisen alkion kohdalla,
;; sitä mukaa kuin etsintä etenee kohti listan loppupäätä, cond-lauseen viimeisessä
;; eli else-haarassa olevan häntärekursiivisen kutsun kautta, aina siihen asti
;; kuin löytyy ensimmäinen lukua luku suurempi alkio, tai tullaan listan loppuun.
;; (Sama huomio pätee myös seuraavien kolmen malliratkaisun kanssa).

(check-expect (eka-suurempi 0 (list)) #false)

(check-expect (eka-suurempi 0 (list 5)) 5)

(check-expect (eka-suurempi 6 (list 5)) #false)

(check-expect (eka-suurempi 10 (list 1 2 5 5 4 33 12)) 33)

(check-expect (eka-suurempi 33 (list 1 2 5 5 4 33 12)) #false)



;; Tehtävä 2:
;; Kirjoita rekursiivinen funktio
;;  (eka-tuplattu lista)
;; joka mitä tahansa alkioita sisältävästä listasta palauttaa ensimmäisen
;; alkion joka esiintyy vähintään kaksi kertaa peräkkäin, tai epätoden (#false)
;; mikäli sellaista alkiota ei ole. (Esimerkiksi jos listan pituus < 2).
;;
;; Malliratkaisu:

(define (eka-tuplattu lista)
   (cond ((empty? lista) #false) ;; Jos tultiin jo listan loppuun, ...
         ((empty? (rest lista)) #false) ;; ... tai enää yksi alkio jäljellä...
         ;; silloin yhtään tuplattua alkiota ei löytynyt, palauta #false
         ((equal? (first lista) (second lista)) ;; Löytyi sama alkio peräkanaa...
                 (first lista)                  ;; palauta se!
         )
         (else (eka-tuplattu (rest lista))) ;; Muuten jatka etsimistä hännästä
   )
)

;; Huomaa kuinka yllä tarvitsemme lisälopetusehdon ((empty? (rest lista)) #false)
;; jolla tarkistetaan onko lista (enää) vain yhden alkion mittainen
;; [Huom: (= 1 (length lista)) ajaisi saman asian, mutta olisi hiukan hitaampi].
;; Ilman kyseistä ehtoa sitä seuraava ehto (equal? (first lista) (second lista))
;; "karahtaisi kiville" yrittäessään tutkia onko yhden alkion pituisessa
;; listassa ensimmäinen ja toinen alkio samanlaiset.

(check-expect (eka-tuplattu (list)) #false)

(check-expect (eka-tuplattu (list 5)) #false)

(check-expect (eka-tuplattu (list 1 2 5 5 4 3)) 5)

(check-expect (eka-tuplattu (list 1 2 5 4 4 5 3 2)) 4)


;; Huomaa vielä tämä:
(check-expect (eka-tuplattu (list 1 2 #false 5)) #false)

(check-expect (eka-tuplattu (list 1 2 #false #false 5 5 6 1)) #false)

;; Toisin sanoen, funktiossamme on on se perustavaa laatua oleva valuvika,
;; että mikäli ensimmäinen toistuva alkio on #false, niin funktion tuloksesta
;; sitä ei voi mitenkään päätellä.
;; Kysymys: Miten asian voisi korjata? Vihje: Muistele mitä funktio member täydessä
;; Racket-kielessä ja Weschemessä palauttaa kun etsitty alkio löytyy? 
;;


;; Harjoitus 3:
;; Ylläolevista esimerkeistä mallia ottaen, kirjoita rekursiivinen funktio
;;  (aidosti-kasvava? luvut)
;; joka mitä tahansa lukuja sisältävästä listasta luvut kertoo ovatko sen luvut
;; kaikki kasvavassa järjestyksessä (eli jokaista lukua seuraa sitä suurempi
;; luku), vai ei, palauttaen siis toden (#true) mikäli lista on aidosti kasvava
;; ja epätoden (#false) mikäli se ei ole aidosti kasvava. Sovimme lisäksi että tyhjä
;; lista ja vain yhden luvun sisältävä lista on aina aidosti kasvava.

;; Malliratkaisu:

;; Huomaa kuinka tässä testit (empty? luvut) ja (empty? (rest luvut)) on pakattu
;; yhden ja saman cond:in ehdon alle or-lausekkeella, toisin kuin harjoituksen 2
;; tai 4 ratkaisuissa, jossa ne on eroteltu kahdeksi perättäiseksi cond:in ehdoksi.
;; Useimmiten on makuasia kumpaa tapaa käyttää, varsinkin jos palautettava tulos
;; on yksinkertainen vakio kuten #false tai #true.

(define (aidosti-kasvava? luvut)
   (cond ((or (empty? luvut) (empty? (rest luvut))) 
;; Jos tultiin jo listan luvut loppuun, tai enää yksi luku jäljellä...
                #true ;; Palauta silloin tosi
         )
         ((<= (second luvut) (first luvut)) #false) ;; Löytyi kohta jossa seuraava
         ;; luku ei olekaan suurempi, palauta false!
         (else (aidosti-kasvava? (rest luvut))) ;; Muuten jatka tarkistamista hännästä
   )
)

(check-expect (aidosti-kasvava? (list)) #true)

(check-expect (aidosti-kasvava? (list -500)) #true)

(check-expect (aidosti-kasvava? (list 1 2)) #true)

(check-expect (aidosti-kasvava? (list 1 0)) #false)

(check-expect (aidosti-kasvava? (list 1 2 2)) #false)

(check-expect (aidosti-kasvava? (list -1 0 1 22 333)) #true)


;; Tehtävä 4:
;; Kirjoita rekursiivinen funktio (onko-duplikaatteja? lista)
;; joka mitä tahansa alkioita sisältävästä listasta kertoo, onko siinä
;; yhtään alkiota joka esiintyy useammin kuin kerran, jolloin palauttaa toden (#true)
;; tai sitten palauttaa epätoden (#false) mikäli kukin sen alkio esiintyy tasan yhden
;; kerran listassa.
;; Tämä funktio eroaa funktiosta eka-tuplattu paitsi paluuarvonsa osalta, niin myös siinä
;; että toistuvien alkioiden ei tarvitse olla peräkkäin.
;; Vihje: funktiota member (tai ylläannettua elekja-funktiota) voi käyttää myös
;; toisen rekursiivisen funktion sisällä.

;; Malliratkaisu:

(define (onko-duplikaatteja? lista)
   (cond ((empty? lista) #false) ;; Jos tultiin jo listan loppuun, ...
         ((empty? (rest lista)) #false) ;; ... tai enää yksi alkio jäljellä...
         ;; silloin yhtään tuplattua alkiota ei löytynyt, palauta #false
         ((not (equal? #false (member (first lista) (rest lista))))
        ;; Jos tässä kohdin listaa on ekana alkiona sellainen alkio joka löytyy
        ;; listan hännästäkin, olemme löytäneet silloin ensimmäisen alkion
        ;; joka esiintyy listassa useasti.
                #true                  ;; Joten palauta tosi
         )
         (else (onko-duplikaatteja? (rest lista))) ;; Muuten jatka etsimistä hännästä
   )
)



(check-expect (onko-duplikaatteja? (list)) #false)

(check-expect (onko-duplikaatteja? (list 5)) #false)

(check-expect (onko-duplikaatteja? (list 1 2 5 5 4 3)) #true)

(check-expect (onko-duplikaatteja? (list 1 2 5 4 7 4 5 3 2)) #true)

(check-expect (onko-duplikaatteja? (list 1 2 #false 5)) #false)

(check-expect (onko-duplikaatteja? (list 1 2 #false #false 5 7)) #true)



