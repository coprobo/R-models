#Zaczytanie wstêpnych danych

test = read.csv("test.csv", header = TRUE)

#Dane treningowe, do nauki, kto przetrwa
train = read.csv("train.csv", header = TRUE)

#Dodanie zmiennej na przetrwanie do zbioru testowego
test.survived = data.frame(Survived = rep("None",nrow(test)), test[,])

#Po³¹czenie danych
data.combined = rbind(train, test.survived)

#Typy danych
str(data.combined) ##pokazuje typy danych w data.combined

data.combined$Pclass = as.factor(data.combined$Pclass)
data.combined$Survived = as.factor(data.combined$Survived)


#Sprawdzam dane nieobrobione
#ile przetrwa³o otd.
table(data.combined$Survived)

table(data.combined$Pclass)

#pakiet do wykresów
library(ggplot2)

#Za³o¿enie -> bogaci klienci czêœciej uchodzili z ¿yciem
train$Pclass = as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  #geom_bar(width=0.5) +
  stat_count(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total count") + 
  labs(fill="Survived")


data.combined$Survived = factor(data.combined$Survived)

#head(as.character(train$Name))

#Sprawdzam ile uniklanych wartosci w kolumnie Name
length(unique(as.character(data.combined$Name)))

dup.names = as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
#z data.combined biorê kolumnê Name i sprawdzam, które rzeczy
#powtarzaj¹ siê


library(stringr)
misses = data.combined[which(str_detect(data.combined$Name, "Miss.")), ]
misses[1:5,]


#funkcja pozwalaj¹ca wy³uskanie tytu³u
extractTitle = function(Name)
{
  Name = as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0)
    return ("Miss.")
  else if (length(grep("Master.",Name)) > 0)
    return ("Master.")
  else if (length(grep("Mrs.",Name)) > 0)
    return ("Mrs.")
  else if (length(grep("Mr.",Name)) > 0)
    return ("Mr.")
  else 
    return ("Other")
}

titles = NULL

for (i in 1:nrow(data.combined))
{
  titles = c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$title = as.factor(titles)



#Pierwsze zobrazowanie danych treningowych,czyli pierwszych
# 891 wierszy (tyle zawiera training set)
#Wp³yw tytu³u(Title) na ryzyko œmierci/przetrwania

ggplot(data.combined[1:891,], aes(x=title, fill=Survived))+
  stat_count(width = 0.8)+
  facet_wrap(~Pclass)+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survied")

#iloœæ kobiet i mê¿czyzn (females and males)
table (data.combined$Sex)


#Zobrazowanie zwi¹zku miêdzy kolumnami Sex, Pclass i Survival
ggplot (data.combined[1:891,], aes(x = Sex, fill = Survived))+
  geom_bar(width=0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs (fill = "Survived")


#Info o kolumnie Age
summary(data.combined$Age)


#Du¿o brakuj¹cych danych w kolumnie Age,
#Sprawdzam czy dane pojawiaj¹ sie w treningowych

summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,], aes(x=Age, fill=Survived))+
  #geom_bar(width=0.5) +
  geom_histogram(binwidth=10)+
  facet_wrap(~Sex+Pclass)+
  xlab("Age")+
  ylab("Total Count")




#Feature engineering. Creating a family size feature
#temp.sibsp = c(train$SibSp,test$SibSp)
#temp.parch = c(train$Parch,test$Parch)
#data.combined$family.size = as.factor(temp.sibsp+temp.parch+1)




#Info o kolumnie Ticket
str(data.combined$Ticket)

#zmiana typów danych

data.combined$Ticket = as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

data.combined$Ticket.first.char = c(1:1309)
Ticket.first.char = ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

data.combined$Ticket.first.char = as.factor(Ticket.first.char)

#Wykres dla Ticket i Survival

ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by Ticket.first.char")+
  xlab("Ticket.first.char")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

#Wykres dla pclass, ticket number i survival
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")


#Op³aty pasa¿erów (Fare)

summary(data.combined$Fare)
length(data.combined$Fare)

#Wartoœci NA
na.number=which(is.na(data.combined$Fare)=="TRUE")


ggplot(data.combined, aes(x = Fare))+
  geom_bar(width = 2)+
  ggtitle("Combined fare distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,100)

#Cabins
data.combined$Cabin = as.character(data.combined$Cabin)
data.combined[which(data.combined$Cabin==""), "cabin"] <- "U"
data.combined$Cabin[1:100]

Cabin.first.char = as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)



##### Random Forest ####



library(randomForest)

#rf - random forest

rf.train.1 = data.combined[1:891, c("Pclass", "title")]
rf.label = as.factor(train$Survived)

#Ziarno
set.seed(1234)

rf.1 = randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


#####
rf.train.2 = data.combined[1:891, c("Pclass", "title", "SibSp")]
set.seed(1234)


rf.2 = randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


######

rf.train.3 = data.combined[1:891, c("Pclass", "title", "Parch", "SibSp")]
set.seed(1234)


rf.3 = randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

########

rf.train.5 = data.combined[1:891, c("Pclass", "title", "family.size")]
set.seed(1234)


rf.5 = randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


###
#
#Kolumny u¿yte to pclass, parch, 
#title oraz family.size do trenowania random forest
#
###

rf.train.7 = data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]
set.seed(1234)


rf.7 = randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)
#OOB estimate of error rate to jest wspolczynnik czy nasza metoda 
#przewiduje dobrze
# 100%-19.42%=80.58%
#nasz model przewiduje ze skutecznoœci¹ 80.58%
#U¿ywaj¹æ Random Forest

