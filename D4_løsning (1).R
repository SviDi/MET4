# -- LØSNINGSFORSLAG TIL DATAØVING 4 -- #
# ------          MET4           ------ #


# Dataene får vi tak i via pakken ISLR
library(ISLR)       
head(Default)       # Ta en titt på dataene

# Boksplott som indikerer sammenhengen mellom å ikke betale kreditkortgjeld og størrelsen på gjelden
boxplot(balance ~ default, data = Default,
        ylab = "balance", xlab = "default")


# Vi deler dataene inn i et treningsett og et testsett
library(dplyr)
my_data <- Default %>%
  mutate(id = row_number()) # først legger vi til en unik id variabel til hvert individ

set.seed(123)               # Fiks "tilfeldig frø" sikrer at du kan trekke det samme treningssettet om igjen
train <- my_data %>%        # Så trekker vi et utvalg bestående av 70 % av dataene
  sample_frac(.70)

test <- my_data %>%         # Treningssettet er da de resterende 30 % av dataene
  anti_join(train, by = 'id')

# model1: Logistisk regresjon med  'balance' (gjeld) som forklaringsvariabel
model1 <- glm(default ~ balance, data = train,
              family = "binomial")
summary(model1)
exp(coef(model1))

# Prediksjon av sannsynlighet for mislighold for to personer med 1000 og 2000 kr i 'balance' (gjeld)
pred <- predict(model1, data.frame(balance = c(1000, 2000)), type = "response")
ifelse(pred > 0.5, "Yes", "No")


# model2: logistisk regresjon med 'student' som forklaringsvariabel
model2 <- glm(default ~ student, family = "binomial", data = train)
summary(model2)
exp(coef(model2))

# Prediksjon av sannsynlighet for mislighold for en student og en ikke-student
predict(model2, data.frame(student = factor(c("Yes", "No"))), type = "response")
# En student har større sannsynlighet for mislighold

# model3: multippel logistisk regresjon med alle forklaringsvariablene
model3 <- glm(default ~ balance + income + student, family = "binomial", data = train)
summary(model3) 

# # Prediksjon av sannsynlighet for mislighold for en student og en ikke-student med lik gjeld og inntekt
to_kunder <- data.frame(balance = 1500, income = 10000, student = c("Yes", "No"))
predict(model3, newdata = to_kunder, type = "response") 

# Ikke-studenten har dobbelt så stor sannsynlighet for mislighold. Dette samsvarer jo ikke med model2! 
# Når vi har justert for gjeld og inntekt så viser det seg at studenter håndterer lånet sitt bedre 
# enn ikke-studenter. Dette er et eksempel på kolineæritet mellom forklaringsvariablene. Det viser 
# seg at det å være student og gjeld er avhengige variabler. Vi ser det ut fra følgende boksplott

# boksplot
boxplot(balance ~ student, data = train, 
        names = c("ikke-student", "student"),
        ylab = "gjeld")

# Moral: Er du banken og skal vurdere om en person skal få lån eller ikke tenker du som følger:
# 1) Hvis ikke du vet hvilken gjeld personen har er en student mer risikabel enn en ikke-student.
#    Dette er fordi studenter i gjennomsnitt har større gjeld.
# 2) Hvis du vet hvilken gjeld personen har, vil en student være mindre risikabel enn en ikke-student 
#    med en tilsvarende gjeld.



#---------k nærmeste naboer klassifisering
library(caret)

# R-kode dersom vi vil velge k automatisk
set.seed(200)
trControl <- trainControl(method  = "cv", # 5-fold kryssvalidering
                          number  = 5)

# Tilpasser modellen
model4 <- train(default ~ balance + income + student,
                 data = train,
                 method = "knn",
                 trControl  = trControl,
                 metric     = "Accuracy")

# Hvilken k valgte kryssvalideringen?
k <- model4$finalModel$k
k
  
# prediksjon av to kunder 

predict(model4, newdata = to_kunder)

#--------- test av klassifiseringsevne

# logistisk regresjonsmodell
sann <- test$default                                              # Den sanne verdien i testdataene

pred_logreg <- predict(model3, newdata = test, type = "response") # Predikert sannsynlighet
klass_logreg <- ifelse(pred_logreg > 0.5, "Yes", "No")            # Klassifisering av kundene
logreg_tab <- table(sann, klass_logreg)                           # Kontigenstabell
logreg_tab_norm <- logreg_tab %>% 
  prop.table %>%
  round(3)

logreg_tot <- sum(diag(logreg_tab_norm))               # Total andel korrekt klassfisering

# knn 
knn_klass <- predict(model4, newdata = test)   # Prediksjon av test data
knn_tab <- table(sann, knn_klass)               # kontigenstabell
knn_tab_norm <- knn_tab %>%                     # Normalisert kontigenstabell
  prop.table %>%
  round(3)
knn_tab_norm
knn_tot <- sum(diag(knn_tab_norm))               # Total andel korrekt klassfisering

knn_tot
logreg_tot

# Vi ser at den logistiske regresjonsmodellen totalt sett gjør det bedre og spesielt når det kommer til å
# predikere de som misligholder. Dermed gjør den mindre såkalte falske positiv feil noe som er viktig for banken
# dersom den ønsker å luke ut dårlige kunder som misligholder. 
# knn gjør det litt bedre i å spå hvem som ikke misligholder og gjør dermed ferre "falske positive" feil
# men dette er mindre alvorlig for banken hvis den er konservativ (dog vil de akseptere flere "gode" kunder)





#----- Ekstra stilige figurer med ggplot2 (ikke del av øvingen, men illuster litt bedre hva som skjer)----#
library(ggplot2)

# Showcasing why ordinary linear regression does not work
my_data %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "lm") +
  ggtitle("Linear regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

# Plot of model
my_data %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

# Figure illustrating whats going on
ggplot(default, aes(x = student, y = balance)) + 
  geom_boxplot(aes(color = student))

# Regression curve adjusted for students and non-students:
my_data %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob, color = student)) +
  geom_point(alpha = .15) +
  geom_smooth(aes(color = student), method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")
