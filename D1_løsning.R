# -- LØSNINGSFORSLAG TIL DATAØVING 1 -- #
# ------          MET4           ------ #


# les inn data
library(readxl)
violence <- read_excel("violence.xlsx")

# utforsking av datastruktur
class(violence)
dim(violence)
names(violence)
str(violence)
head(violence)

# Vektorer med aggresjonsnivå til gruppen som har spilt voldelig/ikke-voldelig spill 
voldelig      <- violence$aggression_level[violence$violent_treatment == "Violent"]      
ikke_voldelig <- violence$aggression_level[violence$violent_treatment == "Less Violent"]

# Deskriptiv statistikk
min(voldelig)
mean(voldelig)
sd(voldelig)
length(voldelig)
summary(voldelig) 
#... osv

# Vi ser at vi har litt færre observasjoner i vårt datasett enn det som
# er oppgitt i oppgaven. Derfor blir f.eks. gjennomsnittet litt annerledes.
# Dette skyldes for øvrig at noen forsøkspersoner mangler data på en annen
# variabel som vi skal bruke senere i oppgaven, så de er tatt ut her.


# Skalert histogram
hist(voldelig, freq = TRUE, breaks = 10, main = "Voldelig")
hist(ikke_voldelig, freq = TRUE, breaks = 10, main = "Ikke voldelig")

# boxplot
boxplot(voldelig, ikke_voldelig, names = c("voldelig", "ikke voldelig"))

# Fordelingen av aggresjonsscore ser nesten helt identisk ut i de to gruppene.
# Det er vanskelig å tenke at de skal være trukket fra to fordelinger som er
# veldig ulike.


