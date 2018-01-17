###############################################
#           Model Drzewa Decyzyjnego          #
###############################################
install.packages("party")
library("party")

#skorzystam z dobrze znanego data setu do nauki
#a mianowicie z danych dotycz¹cych irysów
#jest to data frame ze 150 przypadkami i 5 zmiennymi

#zaczytanie danych
irysy = as.data.frame(iris)

#podstawowe informacje o danych
str(iris)
#Sepal length and width = d³ugoœæ i szerokoœæ kielicha
#Petal length and width = d³ugoœæ i szerokoœæ p³atków
#Species = gatunek


#Zamieniam nazwy kolumn na polskie
install.packages("plyr")
library(plyr)
irysy = rename(irysy, c("Sepal.Length"="Kielich.D³ugoœæ", "Sepal.Width"="Kielich.Szerokoœæ",
                "Petal.Length"="P³atek.D³ugoœæ", "Petal.Width"="P³atek.Szerokoœæ", 
                "Species"="Gatunek"))

#Dzielê dane na treningowe i testowe w skali 70% i 30%
set.seed(1234)
temp = sample(2, nrow(irysy), replace=TRUE, prob=c(0.7,0.3))
trainData = irysy[temp==1,]
testData = irysy[temp==2,]


#drzewo decyzyjne, przewidywanie gatunku s¹dz¹c po wymaiarach
irysy.drzewo1 = ctree(Gatunek ~ Kielich.D³ugoœæ + Kielich.Szerokoœæ +
                       P³atek.D³ugoœæ + P³atek.Szerokoœæ, data = trainData)
#Otrzymane drzewo decyzyjne:
plot(irysy.drzewo1)
#barplot, wykres dla ka¿dego liœcia wêz³a ukazuje prawdopodobieñstwo
#danego przypadku, ze wzglêdu na jeden z trzech gatunków

plot(irysy.drzewo1, type="simple")
#tutaj na przyk³ad node2 mówi ¿e mamy 40 przypadków, z czego
#wszystkie nale¿¹ do pierwszego gatunku czyli Setosa

#Przewidywania na danych treningowych
przewidywaniaTrain = predict(irysy.drzewo1)
przewidywania1 = table(przewidywaniaTrain,trainData$Gatunek)

#Przewidywania na danych testowych
przewidywaniaTest = predict(irysy.drzewo1, newdata = testData)
przewidywania2 = table(przewidywaniaTest, testData$Gatunek)

