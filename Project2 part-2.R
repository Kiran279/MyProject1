##### Installing Library
library(dplyr)
library(car)

##### Reading Files

setwd("C:/Users/91841/OneDrive/Desktop/R class")
store_train=read.csv("C:/Users/91841/OneDrive/Desktop/R class/store_train.csv")
store_test=read.csv("C:/Users/91841/OneDrive/Desktop/R class/store_test.csv")


store_test$data="Test"
store_train$data="Train"
store_test$store=NA

###### Binding two dataset

Store_data=rbind(store_train,store_test)

glimpse(Store_data)
#### Checking count NA.s in each column

lapply(Store_data, function(x) sum(is.na(x)))

5######  Removing NA.s From data 

Store_data <- Store_data %>% mutate(
  population = ifelse(is.na(population),mean(population,na.rm = T),population),
  country = ifelse(is.na(country),mean(country,na.rm = T),country),
  sales_sum = rowSums(Store_data[,2:6]))


####### Creating Dummy values

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
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


Store_data=CreateDummies(Store_data,"state_alpha",5)
Store_data=CreateDummies(Store_data,"store_Type")
Store_data$storecode <- substr(Store_data$storecode, 1,2)
Store_data=CreateDummies(Store_data,"storecode")

table(Store_data$state_alpha)

##### Removing unwanted variables

Store_data$sales0=NULL
Store_data$sales1=NULL
Store_data$sales2=NULL
Store_data$sales3=NULL
Store_data$sales4=NULL
Store_data$Id=NULL
Store_data$countytownname=NULL
Store_data$countyname=NULL
Store_data$State=NULL
Store_data$Areaname=NULL
glimpse(Store_data)


#Seperating train and test dataset

store_train=Store_data %>% filter(data=='Train') %>% select(-data)
store_test=Store_data%>%filter(data=='Test')%>%select(-data,-store)

#Spliting dataset into train and test

set.seed(10)
s=sample(1:nrow(store_train),0.8*nrow(store_train))
store_train1=store_train[s,]
store_train2=store_train[-s,]

###### Linear regression model

fit=lm(store~.,data=store_train1) 

sort(vif(fit),decreasing = T)[1:3]


#####REmoved the Alias from the data 
fit=lm(store~.-state_alpha_ME,data=store_train1)
sort(vif(fit),decreasing = T)[1:3]


### Logistic Regression 

log.fit = glm(store~.,data = store_train1, family = "binomial")
step(log.fit)
summary(log.fit)
formula(log.fit)
log.fit.new=glm(store ~ country + CouSub + population + sales_sum + state_alpha_NV + 
                  state_alpha_NJ + state_alpha_WY + state_alpha_MD + state_alpha_AK + 
                  state_alpha_UT + state_alpha_NM + state_alpha_OR + state_alpha_RI + 
                  state_alpha_WA + state_alpha_ID + state_alpha_SC + state_alpha_ND + 
                  state_alpha_WV + state_alpha_MT + state_alpha_CA + state_alpha_NY + 
                  state_alpha_CO + state_alpha_LA + state_alpha_SD + state_alpha_AL + 
                  state_alpha_FL + state_alpha_PA + state_alpha_WI + state_alpha_AR + 
                  state_alpha_OK + state_alpha_PR + state_alpha_MS + state_alpha_MI + 
                  state_alpha_MN + state_alpha_OH + state_alpha_IN + state_alpha_NE + 
                  state_alpha_TN + state_alpha_IA + state_alpha_NC + state_alpha_IL + 
                  state_alpha_KS + state_alpha_MO + state_alpha_KY + state_alpha_VA + 
                  state_alpha_GA + state_alpha_CT + state_alpha_TX + state_alpha_VT + 
                  state_alpha_NH + state_alpha_MA + state_alpha_ME + store_Type_SupermarketType3 + 
                  store_Type_GroceryStore + store_Type_SupermarketType1 + storecode_NC,data =store_train1 )
summary(log.fit.new)

#### performance of score model on validation data
library(pROC)

predicted.values <- predict(log.fit,newdata =store_train2,type="response")

predicted.values


auc(roc(store_train2$store,predicted.values))



### Building the model on entire train data
for_vif=lm(store~.,data = store_train)



sort(vif(for_vif),decreasing = T)

##### Removed the high VIF

for_vif=lm(store~.-state_alpha_ME,data=store_train)

log.fit.final=glm(store~.,data=store_train)

sort(vif(log.fit.final),decreasing = T)

log.fit.final=glm(store~.-state_alpha_ME,data=store_train)
step(log.fit.final)
summary(log.fit.final)
formula(log.fit.final)

log.fit.final=glm(store ~ (country + CouSub + population + sales_sum + state_alpha_NV + 
                             state_alpha_NJ + state_alpha_WY + state_alpha_MD + state_alpha_AK + 
                             state_alpha_UT + state_alpha_NM + state_alpha_OR + state_alpha_RI + 
                             state_alpha_WA + state_alpha_ID + state_alpha_SC + state_alpha_ND + 
                             state_alpha_WV + state_alpha_MT + state_alpha_CA + state_alpha_NY + 
                             state_alpha_CO + state_alpha_LA + state_alpha_SD + state_alpha_AL + 
                             state_alpha_FL + state_alpha_PA + state_alpha_WI + state_alpha_AR + 
                             state_alpha_OK + state_alpha_PR + state_alpha_MS + state_alpha_MI + 
                             state_alpha_MN + state_alpha_OH + state_alpha_IN + state_alpha_NE + 
                             state_alpha_TN + state_alpha_IA + state_alpha_NC + state_alpha_IL + 
                             state_alpha_KS + state_alpha_MO + state_alpha_KY + state_alpha_VA + 
                             state_alpha_GA + state_alpha_CT + state_alpha_TX + state_alpha_VT + 
                             state_alpha_NH + state_alpha_MA + state_alpha_ME + store_Type_SupermarketType3 + 
                             store_Type_GroceryStore + store_Type_SupermarketType1 + storecode_NC) - 
                    state_alpha_ME,data = store_train)

summary(log.fit.final)


val.score=predict(log.fit.final,newdata = store_test,type='response')


write.csv(val.score,"Kiran_Limbachiya_p2_part2.csv",row.names = F)
