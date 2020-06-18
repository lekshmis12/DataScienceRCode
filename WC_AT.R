#Checking if Adipose Tissue Area depends on Waist size using regression model.
#Reading data set.
WC_AT<-read.csv("C:/Lekshmi/Excelr/Data Science/WC_AT.csv")
attach(WC_AT)
#Plotting scatter Plot
plot(Waist,AT)
#Creating regression model
model<-lm(AT~Waist,data=WC_AT)
summary(model)
#Checking Adipose Tissue values for given Waist size.
new_Waist=data.frame("Waist"=c(40,75,200))
pd<-predict(model,new_Waist)
pd
#Comparing the predicted value and given value 
pd1<-predict(model)
new_table<-data.frame(WC_AT,pd1)
detach(WC_AT)
