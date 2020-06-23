#Reading file
library(readxl)
library(car)
toyota_r<-read_excel("C:/Lekshmi/Excelr/Data Science/Data Set/Toyota.xlsx")

#Checking for missing values in dataset
colSums(is.na(toyota_r))
toyota<-na.omit(toyota_r)

#Extracting required columns
colnames(toyota)
toyota<-toyota[,c(3,4,7,9,16,13,14,17,18)]

#toyota1<-subset(toyota,select = c("Price","cc") )

#Scatter Plot Matrix:
pairs(toyota)

#Correlation Matrix:
cor(toyota)

#Regression Model and Summary
model.car<-lm(Price~.,data = toyota)
summary(model.car)

#Multi-colinearity
install.packages("car")
library(car)
car::vif(model.car)

#########Model Validation
#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.car) 

#Residuals vs Regressors
residualPlots(model.car)

#Added Variable Plots
avPlots(model.car)

#QQ plots of studentized residuals
qqPlot(model.car)

#Deletion Diagnostics
influenceIndexPlot(model.car) # Index Plots of the influence measures

####Iteration 1 
#Remove 79th observation and apply tranformation on Age
toyota['Age2']<-toyota$Age_08_04*toyota$Age_08_04
toyota1<-toyota[-79,]
model.car1<-lm(Price~.,data = toyota1)
summary(model.car1)


plot(model.car1) 
residualPlots(model.car1)
qqPlot(model.car1)
influenceIndexPlot(model.car1)

toyota2<-toyota[-c(79,219),]

model.car2<-lm(Price~.,data = toyota2)
summary(model.car2)
qqPlot(model.car2)
influenceIndexPlot(model.car2)

###Final
model.car2<-lm(Price~.,data = toyota[-c(950,79,600,220,219,218,520,957,217,518),-c(7)])
summary(model.car2)
qqPlot(model.car2)
influenceIndexPlot(model.car2)
##Predict for new data
testdata<-data.frame(Age_08_04=20,KM =2000,HP =90,Gears =5,cc=1500,
                     Quarterly_Tax=200,Weight=1500,Age2=400)

predict(model.car2,testdata)
