###############################################
#           Model Drzewa Decyzyjnego          #
###############################################
install.packages("party")
library("party")

#skorzystam z dobrze znanego data setu do nauki
#a mianowicie z danych dotycz�cych irys�w
#jest to data frame ze 150 przypadkami i 5 zmiennymi

#zaczytanie danych
irysy = as.data.frame(iris)

#podstawowe informacje o danych
str(iris)
#Sepal length and width = d�ugo�� i szeroko�� kielicha
#Petal length and width = d�ugo�� i szeroko�� p�atk�w
#Species = gatunek


#Zamieniam nazwy kolumn na polskie
install.packages("plyr")
library(plyr)
irysy = rename(irysy, c("Sepal.Length"="Kielich.D�ugo��", "Sepal.Width"="Kielich.Szeroko��",
                "Petal.Length"="P�atek.D�ugo��", "Petal.Width"="P�atek.Szeroko��", 
                "Species"="Gatunek"))

#Dziel� dane na treningowe i testowe w skali 70% i 30%
set.seed(1234)
temp = sample(2, nrow(irysy), replace=TRUE, prob=c(0.7,0.3))
trainData = irysy[temp==1,]
testData = irysy[temp==2,]


#drzewo decyzyjne, przewidywanie gatunku s�dz�c po wymaiarach
irysy.drzewo1 = ctree(Gatunek ~ Kielich.D�ugo�� + Kielich.Szeroko�� +
                       P�atek.D�ugo�� + P�atek.Szeroko��, data = trainData)
#Otrzymane drzewo decyzyjne:
plot(irysy.drzewo1)
#barplot, wykres dla ka�dego li�cia w�z�a ukazuje prawdopodobie�stwo
#danego przypadku, ze wzgl�du na jeden z trzech gatunk�w

plot(irysy.drzewo1, type="simple")
#tutaj na przyk�ad node2 m�wi �e mamy 40 przypadk�w, z czego
#wszystkie nale�� do pierwszego gatunku czyli Setosa

#Przewidywania na danych treningowych
przewidywaniaTrain = predict(irysy.drzewo1)
przewidywania1 = table(przewidywaniaTrain,trainData$Gatunek)

#Przewidywania na danych testowych
przewidywaniaTest = predict(irysy.drzewo1, newdata = testData)
przewidywania2 = table(przewidywaniaTest, testData$Gatunek)

