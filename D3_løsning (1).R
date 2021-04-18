
# -- LØSNINGSFORSLAG TIL DATAØVING 3 -- #
# ------           MET4          ------ #


# OPPGAVE 2.2 ---------
library(readxl)
visits <- read_excel("C16-01.xlsx")

# OPPGAVE 2.3 ---------
plot(visits$Week, visits$Museum, type = "l")
lines(visits$Week, visits$`A-Park`)

# OPPGAVE 2.4 ---------
plot(visits$Week, 
     visits$Museum, 
     type = "l",
     col = "darkred",
     xlab = "Ukenummer",
     ylab = "Besøkstall",
     main = "Besøkstall på museet og fornøyelsesparken")
lines(visits$Week, visits$`A-Park`)

# OPPGAAVE 2.5 ---------
reg1 <- lm(Museum ~ `A-Park`, data = visits, subset = 1:32)

# OPPGAVE 2.6 ----------
library(stargazer)
stargazer(reg1, type = "text")

# Oppgave 2.7 ----------
plot(reg1$residuals,
     xalb = "",
     ylab = "",
     main = "Residualer mot indeks",
     pch = 20,
     bty = "l")
abline(h = 0, lty = 2)

qqnorm(reg1$residuals,
       main = "QQ-plot",
       pch = 20,
       bty = "l")
qqline(reg1$residuals, lty = 2)

hist(reg1$residuals,
     col = "grey30",
     border = "white",
     main = "Histogram over residualer",
     xlab = "",
     ylab = "")

# Det ser ikke ut til å være dramatiske avvik fra antakelsene i dette tilfellet.
# Ingen spesielle mønster i  residualplotter, og heller ingen påfallende avvik
# fra normalitet.

# OPPGAVE 2.8 ----------
uker_stengt <- visits$Museum == 0
visits_pred <- visits[uker_stengt, "A-Park"]
predicted_visits1 <- predict(reg1, newdata = visits_pred)

# OPPGAVE 2.9 ----------
plot(visits$Week, 
     visits$Museum, 
     type = "l",
     col = "darkred",
     xlab = "Ukenummer",
     ylab = "Besøkstall",
     main = "Besøkstall på museet og fornøyelsesparken")
lines(visits$Week, visits$`A-Park`)
lines(visits$Week[uker_stengt], 
      predicted_visits1, 
      lty = 2, 
      col = "darkblue")

# OPPGAVE 2.10 ---------

# Vi ser fra figuren at besøkstallet på museet før brannen lå litt under
# besøkstallet i fornøyelsesparken. Det ser vi igjen i de estimerte
# regresjonskoeffisientene, der det kommer frem at vi skal sette det hypotetiske
# besøkstallet til museet i gjenoppbyggingsperioden til drøyt 70% av
# besøkstallet i fornøyelsesparken. Konstantleddet er forholdsvis lite og ikke
# signifikant forskjellig fra null. Dette ser vi igjen i figuren vi lagde i
# forrige oppgave.

# OPPGAVE 2.11 ---------
reg2 <- lm(Museum ~ `A-Park`, data = visits, subset = 180:205)
summary(reg2)

# OPPGAVE 2.12 ---------
predicted_visits2 <- predict(reg2, newdata = visits_pred)
plot(visits$Week, 
     visits$Museum, 
     type = "l",
     col = "darkred",
     xlab = "Ukenummer",
     ylab = "Besøkstall",
     main = "Besøkstall på museet og fornøyelsesparken")
lines(visits$Week, visits$`A-Park`)
lines(visits$Week[uker_stengt], 
      predicted_visits1, 
      lty = 2, 
      col = "darkblue")
lines(visits$Week[uker_stengt], 
      predicted_visits2, 
      lty = 2, 
      col = "forestgreen")

# OPPGAVE 2.13 --------
sum(6.99*(predicted_visits2 - predcted_visits1))

# OPPGAVE 2.14 -------- 

# Det er ingenting i veien for å bruke *både* perioden før brannen og perioden
# etter brannen til å estimere regresjonsmodellen, da blir begge syn tatt hensyn
# til, og vektet noenlunde likt siden det er omtrent like mange observasjoner i
# de to gruppene. Bruk f.eks "subset = c(1:32, 180:205)" for å få det til.
# Alternativt gjør vi det enkelt og bare møtes på midten. Begge har litt feil,
# og begge har litt rett.
#
# Et annet moment som kan komme inn her (takk til stud.ass. Håvard for
# innspill!) er at det nyåpnede museet er betraktelig større enn det som brant
# ned, og at det kan ha vært med på å øke populariteten (og kanskje forklare den
# helt og fullt). Forsikringsselskapet kan da argumentere med at de har
# forsikret museet slik det stod da det brant, og ikke kan ta hensyn til en
# eventuell oppgradering som museet *kanskje* ville gjort uansett.
