#Association Rules
install.packages("arules")
library(arules)
Titanic<-read.csv("C:/Lekshmi/Excelr/Data Science/Data Set/Titanic.csv")

Titanic<-Titanic[,-c(1)]
rules <- apriori(Titanic)
arules::inspect(rules) # print the rules
rules.sorted <- sort(rules, by="lift")
arules::inspect(rules.sorted)
# rules with rhs containing "Survived" only
rules <- apriori(Titanic,parameter = list(supp=0.5, conf=0.7)
                 ,appearance = list(rhs=c("Survived=No", "Survived=Yes")
                 ),control = list(verbose=F))
arules::inspect(rules)
