library(dplyr)


setwd("C:\\Users\\91841\\Downloads")

###Reading both the files
Real_Estate_train=read.csv("housing_train.csv")
Real_Estate_test=read.csv("housing_test.csv")


glimpse(Real_Estate_test)
glimpse(Real_Estate_train)
glimpse(Real_estate)

####adding missing columns
Real_Estate_test$data="test"
Real_Estate_train$data="train"
Real_Estate_test$Price=NA


####binding two dataset
Real_estate=rbind(Real_Estate_train,Real_Estate_test)

table(Real_estate$Type)

table(Real_estate$Rooms)


#Replacing NA Values
Real_estate <- Real_estate %>% 
  mutate(Bedroom2 = ifelse(is.na(Bedroom2),median(Bedroom2,na.rm = T),Bedroom2),
         Car = ifelse(is.na(Car),median(Car,na.rm=T),Car),
         BuildingArea = ifelse(is.na(BuildingArea), mean(BuildingArea,na.rm=T),BuildingArea),
         Landsize = ifelse(is.na(Landsize),mean(Landsize,na.rm = T),Landsize),
         YearBuilt = ifelse(is.na(YearBuilt),median(YearBuilt,na.rm = T),YearBuilt),
         CouncilArea = ifelse(CouncilArea == "","NONE",CouncilArea),
         Bathroom = ifelse(is.na(Bathroom),median(Bathroom,na.rm = T),Bathroom)
  ) 



######Creating Dummies of categorical columns
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


Real_estate=CreateDummies(Real_estate,"Type")
Real_estate=CreateDummies(Real_estate,"CouncilArea",50)
Real_estate=CreateDummies(Real_estate,"SellerG",100)
Real_estate=CreateDummies(Real_estate,"Method")
Real_estate=CreateDummies(Real_estate,"Suburb",100)



#Making a new column for Encoded Address
Real_estate$Encoded_Address <- word(Real_estate$Address, start = 3)
Real_estate = CreateDummies(Real_estate,"Encoded_Address",50)

#Removing unwanted variables
Real_estate$Address = NULL
glimpse(Real_estate)

#Seperating train and test dataset

real_train=Real_estate %>% filter(data=='train') %>% select(-data)
real_test=Real_estate%>%filter(data=='test')%>%select(-data,-Price)

#Spliting dataset into train and validation datatest

set.seed(10)
s=sample(1:nrow(real_train),0.8*nrow(real_train))
real_train1=real_train[s,]
real_train2=real_train[-s,]

#Running model

fit=lm(Price~.,data=real_train1) 


#Checking for MultiCollinearity

library(car)
sort(vif(fit),decreasing = T)

alias(lm(Price~.,data=real_train1))


#Improving model

fit=step(fit)
summary(fit)
formula(fit)


#####Final Model

#Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
#  Landsize + BuildingArea + YearBuilt + Type_u + Type_h + CouncilArea_14 + 
#CouncilArea_19 + CouncilArea_11 + CouncilArea_5 + CouncilArea_8 + 
# CouncilArea_13 + CouncilArea_20 + CouncilArea_12 + CouncilArea_18 + 
#CouncilArea_7 + CouncilArea_6 + CouncilArea_15 + CouncilArea_4 + 
#CouncilArea_NONE + SellerG_Kay + SellerG_Hodges + SellerG_Gary + 
#SellerG_Miles + SellerG_Greg + SellerG_RT + SellerG_Biggin + 
#SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
#SellerG_Jellis + Method_VB + Method_SP + Method_PI + Suburb_Doncaster + 
#Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
#Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
#Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + 
#Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
#Suburb_Glenroy + Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + 
#Suburb_Preston + Suburb_Richmond + Suburb_Reservoir


#Using the final formula to built the model

model.fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
               Landsize + BuildingArea + YearBuilt + Type_u + Type_h + CouncilArea_14 + 
               CouncilArea_19 + CouncilArea_11 + CouncilArea_5 + CouncilArea_8 + 
               CouncilArea_13 + CouncilArea_20 + CouncilArea_12 + CouncilArea_18 + 
               CouncilArea_7 + CouncilArea_6 + CouncilArea_15 + CouncilArea_4 + 
               CouncilArea_NONE + SellerG_Kay + SellerG_Hodges + SellerG_Gary + 
               SellerG_Miles + SellerG_Greg + SellerG_RT + SellerG_Biggin + 
               SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
               SellerG_Jellis + Method_VB + Method_SP + Method_PI + Suburb_Doncaster + 
               Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
               Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
               Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_Glenroy + Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + 
               Suburb_Preston + Suburb_Richmond + Suburb_Reservoir,data=real_train1)

summary(model.fit)

#Making Predictions on validation data 

pred <- predict(model.fit,newdata = real_train2)

#Checking for errors

errors=real_train2$Price - pred

#RMSE calculation

rmse = errors**2 %>% mean() %>% sqrt()

#Evaluating Score

212467/rmse 

#Running the model on the entire training data using the same formula


fit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                 Landsize + BuildingArea + YearBuilt + Type_u + Type_h + CouncilArea_14 + 
                 CouncilArea_19 + CouncilArea_11 + CouncilArea_5 + CouncilArea_8 + 
                 CouncilArea_13 + CouncilArea_20 + CouncilArea_12 + CouncilArea_18 + 
                 CouncilArea_7 + CouncilArea_6 + CouncilArea_15 + CouncilArea_4 + 
                 CouncilArea_NONE + SellerG_Kay + SellerG_Hodges + SellerG_Gary + 
                 SellerG_Miles + SellerG_Greg + SellerG_RT + SellerG_Biggin + 
                 SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
                 SellerG_Jellis + Method_VB + Method_SP + Method_PI + Suburb_Doncaster + 
                 Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
                 Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
                 Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + 
                 Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
                 Suburb_Glenroy + Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + 
                 Suburb_Preston + Suburb_Richmond + Suburb_Reservoir, data = real_train)


###Running Model on test data 

test.pred = predict(fit.final,newdata = real_test)


write.csv(test.pred,"Kiran_Limbachiya_P1_part2.csv",row.names = F)



######THIS IS THE CHANGE



