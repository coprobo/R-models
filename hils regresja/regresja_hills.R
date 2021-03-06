###############################################
#           Model Regresji Liniowej           #
###############################################

#zaczytanie danych (tutaj skorzystam z wbudowanego zestawu "hills")
install.packages("MASS")
library("MASS")
hills = hills
attach(hills)

#konstruujemy prosty model regresji liniowej
model.regresji = lm(dist ~ time, data = hills)

#dokladniejsze wypisanie uzyskanych wynikow
model.opis = summary(model.regresji)

#Wyraz wolny Bo
coef(model.regresji)[1]

#Wsp�czynnik kierunkowy B1
coef(model.regresji)[2]

#Warto�ci prognozowane: Yi
fitted(model.regresji)

#typowy wykres regresji || jedna zmienna objasniaj�ca
plot(time, dist, main="Wykres regresji liniowej")
abline(model.regresji)
grid()