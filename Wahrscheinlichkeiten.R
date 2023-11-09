library(gtools)
library(tidyverse)
library(dslabs)
data(heights)
# die Funktion expand.grid erstellt alle moeglichen Kombinationen 
expand.grid(hose = c("blue", "black"),
            shirt = c("white", "grey", "black"),
            socken = c("white", "black"))

# Permutationen und Kombinationen
# Moeglichkeiten zwei Nummern zwischen 1 und 5 zu waehlen ohne Wiederholung
permutations(5,2)     
# Moeglichkeiten sechs Nummern zwischen 0 und 9 zu waehlen mit Wiederholung
all_phone_numbers <- permutations(10, 6, v = 0:9, repeats.allowed = TRUE)  
all_phone_numbers
n <- nrow(all_phone_numbers)
# Fuenf zufaellige moegliche Nummern erstellen
index <- sample(n, 5)
all_phone_numbers[index,]
# Unterschied und Beispiel Permutation und Kombination
permutations(4,2)    # Reihenfolge wird beruecksichtigt
combinations(4,2)    # Reihenfolge ist egal



# Ein Kartendeck erzeugen
farben <- c("Kreuz", "Pik", "Herz", "Karo")
werte <- c("Zwei", "Drei", "Vier", "Fuenf", "Sechs", "Sieben", "Acht", "Neun",
           "Zehn", "Bube", "Dame", "Koenig", "Ass")


# einen Data-Frame aus zwei Listen erstellen mit allen moeglichen Kombinationen 
deck_df <- expand.grid(Farbe = farben, Kartenwert = werte)
deck_df
class(deck_df)
# alle moeglichen Kombinationen aus dem Data-Frame als character erstellen 
deck_chr <- paste(deck_df$Farbe, deck_df$Kartenwert)
deck_chr
class(deck_chr)

# Die Wahrscheinlichkeit einen Koenig aus einem 52er Kartendeck zu ziehen.

# Erstellen der moeglichen Farbkombinationen mit einem Koenig
koenige <- paste(farben, "Koenig")
koenige
class(koenige)
# Die Wahrscheinlickeit ist die Anzahl der moeglichen Farbkombinationen des Koenigs
# durch die Anzahl der Karten im Deck, was dem arithmetischen Mittel entspricht
mean(deck_chr %in% koenige)
# Zur Kontrolle: Es gibt vier Koenige in 52 Karten
4/52

#------------------------------------------------------------------------------




# Die Wahrscheinlichkeit zwei Karten mit demselben Wert zu erhalten, am Bsp 
# des Koenigs
# mit permutations erhaelt man alle moeglichen Kombinationen, 
# default sind wiederholungen auf false
hands <- permutations(52,2, v = deck_chr)
hands
first_card <- hands[,1]
first_card
second_card <- hands[,2]
second_card
sum(first_card %in% koenige)

# Die Wahrscheinlichkeit zwei Koenige zu erhalten ist die Summe der moeglichen 
# Kombinationen von zwei Koenigen geteilt durch die Anzahle der Moeglichkeiten 
# als erstes einen Koenig zu erhalten Mathematisch: 
# Ereignis A und Ereignis B geteilt durch Ereignis A
sum(first_card %in% koenige & second_card %in% koenige) / sum(first_card %in% koenige)
# Kontrolle
12/204

# Wahrscheinlichkeit 21 mit zwei Karten zu erhalten
asse <- paste(farben, "Ass")
asse
bildkarte <- c("Koenig", "Dame", "Bube", "Zehn")
bildkarte <- expand.grid(Farbe = farben, Kartenwert = bildkarte)
bildkarte <- paste(bildkarte$Farbe, bildkarte$Kartenwert)
bildkarte
hands <- combinations(52, 2, v=deck_chr) # alle moeglichen Haende
hands
# Wahrscheinlichkeit eine 21 zu erhalten mit Ass als erster Karte
mean(hands[,1] %in% asse & hands[,2] %in% bildkarte)

# Wahrscheinlichkeit eine 21 zu erhalten, wobei die Reihenfolge egal ist
mean((hands[,1] %in% asse & hands[,2] %in% bildkarte)|(hands[,2] %in% asse & hands[,1] %in% bildkarte))


# Zufaellige Hand von 2 Karten 
hand <- sample(deck_chr, 2)
hand

# Simulation eine 21 zu erhalten bei 10000 verschiedenen Haenden 
B <- 10000
results <- replicate(B, {
  hand <- sample(deck_chr, 2)
  (hand[1] %in% asse & hand[2] %in% bildkarte) | (hand[2] %in% asse & hand[1] %in% bildkarte)
})
results
# der Durschnitt eine 21 in zu erhalten bei 10000 Haenden/ Spielen
mean(results)


# Das Geburttstags-Problem
# Pruefen ob ein Geburtstag bei 50 Personen mehrfach vorkommt
n <- 50
bdays <- sample(1:365, n, replace = TRUE) # 50 zufaellige Geburtstage generieren
bdays
any(duplicated(bdays))    # Pruefen ob Geburtstage doppelt sind

# Monte Carlo Simulation mit 10000 Wiederholungen
B <- 10000
results <- replicate(B, {    
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # Durchschnitt, dass min ein Geburtstag bei 50 Personen in 10000 Wiederholungen doppelt vorkommt


# Funktion zur Simulierung der Wahrscheinlichkeit, 
# dass bei n Leuten mindestens zwei am gleichen Tag Geburtstag haben;
# der Wert entspricht nicht zu 100% der statistischen Wahrscheinleichkeit, 
# es handelt sich um den Wert der wahrscheinlichkeit nach 10000 zufaelligen Szenarien
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
compute_prob(30)



# einen Vektor von 1 bis 10 erzeugen
x <- 1:10
# sqrt operiert auf jedes einzelne Element des Vektors x
sqrt(x)    
sapply(x, sqrt)    # sapply(x, sqrt) ist equivalent zu sqrt(x)

y <- 1:10
# * operiert Elementenweise auf jeden der beiden Vektoren, d.h.
# erster Vektor x * erster Vektor y, zweiter Vektor x * zweiter Vektor y,...
x*y    

# damit die Funktion auf jedes einzelne Element wiederholt werden kann, 
# wird sapply benoetigt, ohne erhaelt man einen Error
n <- seq(1, 60)
compute_prob(23)
prob <- sapply(n, compute_prob)    # Elementenweises benutzen der Funktion auf n
prob
plot(n, prob)


# Funktion zur Berechnung der exakten Wahrscheinlichkeit von gleichen Geburtstagen bei n Leuten
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   
  1 - prod(prob_unique)    # Die Wahrscheinlichkeit errechnet sich aus 1 minus dem Produkt 
}                            # der einzelnen Gegenwahrscheinlichkeiten(Komplementen) 


# mit sapply lassen wir uns wieder die einzelnen Wahrscheinlichkeiten bei 1 - 60 Leuten berechnen
eprob <- sapply(n, exact_prob)
eprob
# Grafische Darstellung der Wahrscheinlichkeiten 
# Man sieht deutlich, dass die Simulation mit 10000 Durchlaeufen sehr nah an der
# berechneten Wahrscheinlichkeit liegt  
plot(n, prob)    # Simulation der Wahrschienlichkeiten
lines(n, eprob, col = "red")    # als rote Linie darueber die exakten Wahrscheinlichkeiten


# Wir simulieren nun die Geburtstage von 23 Leuten
# Wir beginnen mit der Wahrscheinlichkeit von 10 Simulationen und enden bei 100000
B <- 10^seq(1, 5, len = 100)    
compute_prob <- function(B, n = 23){    
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    
# Graph zur Simulation mit 23 Leuten. Man sieht deutlich wie sich die Wahrscheinlickeit
# bei 1000 Versuchen langsam einpendelt und ab 10000 Versuchen die Wahrscheinlichkeit 
# sich nur noch maginal aendert
plot(log10(B), prob, type = "l")    




#-------------------------------------------------------------------------------



# 10000-fache Simulation des 3 Tore Gewinnspiels ohne Torwechsel
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)                  # Die Tore
  prize <- sample(c("Auto","Zonk","Zonk"))    # Die Preise in zufaelliger Zuordnung
  prize_door <- doors[prize == "Auto"]    # Das Gewinnertor
  my_pick  <- sample(doors, 1)    # Das gewaehlte Tor
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # Der Zonk der geoeffnet wird
  stick <- my_pick    # Wir bleiben beim gewaehlten Tor
  stick == prize_door    # Test ob wir das richtige Tor haben
})
mean(stick)    # Durchschnitt der gewonnen Autos 


# 10000-fache Simulation mit Torwechsel
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("Auto","Zonk","Zonk"))    
  prize_door <- doors[prize == "Auto"]    
  my_pick  <- sample(doors, 1)    
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    
  switch <- doors[!doors%in%c(my_pick, show)]    # Wir wechseln das Tor
  switch == prize_door    # Test ob wir gewonnen haben
})
mean(switch)    # Durchschnitt der gewonnen Autos 



#-------------------------------------------------------------------------------




