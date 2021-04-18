
# -- LØSNINGSFORSLAG TIL DATAØVING 2 -- #
# ------          MET4           ------ #


# OPPGAVE 1 ---------

# Spørsmål 1.1: Leser inn datasettet
library(readxl)                                
data <- read_excel("ovelsesdata.xlsx")    

# Spørsmål 1.2: Er forventet avling større ved bruk av nye sprøytemidler?
mu0 <- 100    # Forventet avling ved bruk av gammel metode
t.test(x = data$X2, alternative = "greater", mu = mu0)

# Vi får en p-verdi på 0.038, dvs at vi forkaster nullhypotesen om at forventet
# avling er lik 100 med den nye metoden.

# Spørsmål 1.3: Har variansen forandret seg?
sigma_0   <- 10                  # Nullhypotesen
st.avvik  <- sd(data$X2)         # Det empiriske standardavviket
n         <- length(data$X2)     # Antall observasjoner

T <- (n-1)*st.avvik^2/sigma_0^2  # Testobservatoren

# Kritiske verdier
L <- qchisq(0.025, df = n - 1)
U <- qchisq(0.975, df = n - 1)

# Testobservatoren ligger over nedre forkastningsgrense og under øvre
# forkastningsgrense, så vi forkaster ikke nullhypotesen om at den sanne
# variansen fortsatt er lik 10.

# Spørsmål 1.4: To to-utvalgs t-tester, med og uten paring. Antar lik varians,
# ref forrige oppgave.
t.test(x = data$X1, y = data$X2, var.equal = TRUE)
t.test(x = data$X1, y = data$X2, var.equal = TRUE, paired = TRUE)

# Vi får ikke forkastning i den første testen, men når vi parer observasjonene
# tar vi høyde for at det kan være variasjoner mellom jordlappene (f.eks pga
# jordsmonn eller lysforhold), og da ser vi en klar forskjell mellom avlingen
# med de to sprøytemetodene. Det kan dog hende at noe av forskjellen kan oppstå
# på grunn av forskjeller i vær/temparatur mellom de to vekstsesongene, men det
# kan vi ikke undersøke ved hjelp av dette datasettet.

# OPPGAVE 2 --------

# Spørsmål 2.1: Les inn datasettet
violence <- read_excel("violence.xlsx")

# Spørsmål 2.2: Henter ut vektorer
voldelig      <- violence$aggression_level[violence$violent_treatment == "Violent"]
ikke_voldelig <- violence$aggression_level[violence$violent_treatment == "Less Violent"]

# Spørsmål 2.3: Kjører en t-test for to populasjoner. Ingen spesiell grunn til å
# velge ensidig test, og standardavvikene er så like at vi antar lik varians i
# testen. Dette kan vi selvsagt også teste for mer formelt ved hjelp av en
# varianstest.
t.test(voldelig, ikke_voldelig, var.equal = TRUE)

# Ingen forkastning, ikke overrskende med tanke på boksplottet vi lagde i
# forrige øving.

# Oppgave 2.4: Lager et redusert datasett og krysstabell
violence_redusert <- violence[c("violent_treatment", "experienced_violence")]
krysstabell <- table(violence_redusert)

chisq.test(krysstabell)

# Mega-soleklar forkastning, som på ingen måte er overraskende, og det skulle da
# bare mangle.

# OPPGAVE 3 ------

# Spørsmål 3.1: Leser inn datasettet
yield <- read_excel("roubik_2002_coffe_yield.xlsx")

# Spørsmål 3.2: Trekker ut de fire vektorene
new_p1 <- yield$yield_61_to_80[yield$world == "new"]
new_p2 <- yield$yield_81_to_01[yield$world == "new"]
old_p1 <- yield$yield_61_to_80[yield$world == "old"]
old_p2 <- yield$yield_81_to_01[yield$world == "old"]

# Spørsmål 3.3: En paret t-test for å undersøke om avlingen har endret seg i den
# gamle verden:
t.test(old_p1, old_p2, paired = TRUE)

# Spørsmål 3.4: En paret t-test for å undersøke om avlingen har endret seg i den
# nye verden:
t.test(new_p1, new_p2, paired = TRUE)

# Noen kommentarer til diskusjonsspørsmålet følger under, scroll ned, men bruk
# gjerne litt tid på dette! Diskuter! Sov på det!
#
#
#      ||
#      ||
#      \/




































#      ||
#      ||
#      \/
































#      ||
#      ||
#      \/























































#    ____  _ _    _            ___ 
#   / ___|(_) | _| | _____ _ _|__ \
#   \___ \| | |/ / |/ / _ \ '__|/ /
#    ___) | |   <|   <  __/ |  |_| 
#   |____/|_|_|\_\_|\_\___|_|  (_) 
#






























































#      ||
#      ||
#      \/





























































#      ||
#      ||
#      \/





















































# Spørsmål 3.5.
#
# For det første er det langt igjen til vi kan gjennomføre en kausal fortolkning
# av resultatet (er flere bier er *årsaken* til større avling?). Til det trenger
# vi helst et kontrollert og randomisert eksperiment der vi lar bier pollinere
# et sett med planter, og dekker til en kontrollgruppe slik at eneste
# forskjellen er at insekter ikke kommer til. Forfatteren av den aktuelle
# artikkelen er forsåvidt inne på dette tankesporet, og viser til at slike
# eksperimenter har blitt gjort.
#
# Den andre innvendingen er litt mer subtil, men desto viktigere å få øye på.
# Det at økningen i den gamle verden ikke er signifikant, men økningen i den nye
# verden er signifikant, betyr *IKKE* at forskjellen mellom den gamle og den nye
# verden er signifikant!!! Det må testes for seg selv, og lar seg enklest gjøre
# ved å teste om gjennomsnittet av differansene mellom de to periodene i den nye
# verden er signifikant forskjellig fra gjennomsnittet av differansene mellom de
# to periodene i den gamle verden.
#
# Bruk gjerne litt tid på å akseptere dette argumentet. Testen gjør vi enkelt
# som følger:

t.test(new_p2 - new_p1, old_p2 - old_p1)

# Og den testen forkaster ikke nullhypotesen om lik utvikling over tid! Følgende
# bloggpost diskuterer dette eksempelet, men i lys av en litt annen statistisk
# estimeringsteknikk:
#
# http://www.sumsar.net/blog/2014/02/bayesian-first-aid-two-sample-t-test/
#


