#IST 687 - Applied Data Science
#Final Project : Hyatt NPS Data
#Akshay K., Rohit A., Rohit J., Santhosh N. 

############################################################################
                                  #Part 1#
###########################################################################

#----------------------Libraries invoked--------------------------------#
library(data.table)
library(ggplot2)
library(arules)
library(arulesViz)
library(ggmap)
library(maptools)
library(maps)
library(zipcode)
#----------------------------------------------------------------------#

#---------------------Loading Data------------------------------------#
setwd("C:/Users/rohit/Desktop/IST687/Project")
dummy<-c(169,111,110,71,145)                                      
dummy<-dummy-1
SurveyData<-fread("out-201412.csv", sep = ",",select = c(dummy))
variables<-SurveyData
#Sorting out data just for state of California
SurveyData<-SurveyData[which(SurveyData$State_PL=="California"),]

#-----------------------Age Range Bar Graph---------------------------------#
SurveyData[,5]<-ifelse(SurveyData[,5] =="Y",1,0)
SurveyData$Age_Range_H <- as.factor(SurveyData$Age_Range_H)
Cleandata<- SurveyData[-c(which(SurveyData$Age_Range_H=="")),]

# counts
p1 <- ggplot(data.frame(Cleandata$Age_Range_H), aes(x=Cleandata$Age_Range_H)) +
      geom_bar(fill="Cyan3") +ggtitle ("Age Range of Visitors")+
      labs( x="Age Range", y="Count")+theme(plot.title = element_text(size = rel(2)),
      axis.text = element_text(size =20),axis.title.y = element_text(size = rel(1.5))
     ,axis.title.x = element_text(size = rel(1.5)))

#-------------------------------------------------------------------------#

#------------------------Plot for Age V/S Purpose of Visit---------------#

p2<- ggplot(new, aes(new$Age_Range_H, fill=new$POV_H)) + geom_bar( position="stack")+ggtitle ("Age V/S Purpose of Visit")+
     labs( x="Age Range (Years)", y="Count")+theme(legend.title = element_text(size=15),legend.box.background = element_rect(),
     legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),plot.title = element_text(size = rel(2)),
     axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),axis.title.x = element_text(size = rel(1.5)))
     +guides(fill=guide_legend(title="Purpose of Visit"))
#------------------------------------------------------------------------#

#--------------------------Heat Map for Age, POV, Internet Saisfaction----------#

Cleandata1$POV_H <- gsub("Combination of both business and leisure", "Both", Cleandata1$POV_H)

p3 <- ggplot(Cleandata1, aes(Cleandata1$POV_H,Cleandata1$Age_Range_H)) + geom_tile(aes(fill=Cleandata1$Internet_Sat_H), color = "Blue")+
      gtitle ("Relation Between Age Range, Purpose of Visit & Internet Satisfaction")+
      labs( x="Purpose of Visit",y="Age Range (Years)")+theme(legend.title = element_text(size=15),
      legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6), legend.text = element_text(size = 15),
      plot.title = element_text(size = rel(2)), axis.text = element_text(size =15), axis.title.y = element_text(size = rel(1.5)),
      axis.title.x = element_text(size = rel(1.5)))+guides(fill=guide_legend(title="Internet Rating"))


#------------------------------------------------------------------------------------#

#-------------------Linear Modeling on All the Facilities--------------------------# 

dummy1<-c(200:228,169,168,138,111,110,71,145)
dummy1<-dummy1-1
SurveyData1<-fread("out-201412.csv", sep = ",",select = c(dummy1), na.strings = "")
variables1<-SurveyData1
variables1<-variables1[which(variables1$State_PL=="California"),]
variables1$Likelihood_Recommend_H[is.na(variables1$Likelihood_Recommend_H)] <- "0"
variables1<- variables1[-c(which(variables1$Likelihood_Recommend_H==0)),]
variables1[is.na(variables1)] <- "N"
variables2<-data.frame((variables1))
for(i in 1:6){
    variables2[,i]<-ifelse(variables2[,i] =="Y",1,0)
}
for(i in 14:35){
  variables2[,i]<-ifelse(variables2[,i] =="Y",1,0)
}
new$POV_H <- gsub("Combination of both business and leisure", "Both", new$POV_H)
variables2$POV_H <- gsub("Combination of both business and leisure", "Both", variables2$POV_H)

#Doing step wise regression to determine best possible variables which would affect the Likelihood to recommend

lm1 <- lm (variables2$Likelihood_Recommend_H ~ variables2[,2]+variables2[,3]+variables2[,14]+variables2[,16]+variables2[,19]+
             variables2[,23]+variables2[,25]+variables2[,26]+variables2[,27]+variables2[,28]+variables2[,33]+variables2[,34])
summary(lm1)
plot(lm1)

#moving the best variables to another data frame
Temp <- variables2 [,c(2,33,14,16,19,23,25,26,27,28,33,34)]
summary(Temp)

p3 <- ggplot(variables2, aes(variables2$POV_H,variables2$Spa_PL)) + geom_bar(stat = "identity", fill="orange")+ggtitle ("Number of Spa Users")+
      labs( x="Purpose of Visit", y="Spa Users")+theme(legend.title = element_text(size=15),legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),plot.title = element_text(size = rel(2)),
       axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),axis.title.x = element_text(size = rel(1.5)))
      +guides(fill=guide_legend(title="Purpose of Visit"))


p4 <- ggplot(variables2, aes(variables2$POV_H,variables2$Likelihood_Recommend_H)) + geom_bar(stat = "identity")
      +ggtitle ("Ratings V/S POV")+ labs( x="Purpose of Visit", y="Ratings")+theme(legend.title = element_text(size=15),
      legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),plot.title = element_text(size = rel(2)),
      axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),axis.title.x = element_text(size = rel(1.5)))
     

p5 <- ggplot(data.frame(variables2$POV_H), aes(x=variables2$POV_H)) + geom_bar()
      +ggtitle ("POV Count")+ labs( x="Purpose of Visit", y="Count")+theme(legend.title = element_text(size=15),
      legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),
      plot.title = element_text(size = rel(2)), axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),
      axis.title.x = element_text(size = rel(1.5)))


p6 <- ggplot(data.frame(variables2$Likelihood_Recommend_H), aes(x=variables2$Likelihood_Recommend_H)) + geom_bar(fill="skyblue", color="Blue") +ggtitle ("Ratings given by Customers")+
      labs( x="Rating", y="Count")+theme(legend.title = element_text(size=15),legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),plot.title = element_text(size = rel(2)),
      axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5))
      ,axis.title.x = element_text(size = rel(1.5)))

#------------------------------------------------------------------------------------#

#-----------------------Association Rule Modeling------------------------------------#

dummy1<-c(233,138,109:111,168,169)
dummy1<-dummy1-1
SurveyData1<-fread("out-201412.csv", sep = ",",select = c(dummy1), na.strings = "")
variables1<-SurveyData1
variables1<-variables1[which(variables1$State_PL=="California"),]
variables1$Likelihood_Recommend_H[is.na(variables1$Likelihood_Recommend_H)] <- "0"
variables1<- variables1[-c(which(variables1$Likelihood_Recommend_H==0)),]
variables2 <- variables1[,-5]
variables2 <- variables1[,-2]
variables2 <- data.frame(variables2) 
variables2[] <- lapply(variables2, factor)

ruleSetA <- apriori(variables2[,1:5], parameter = list(support=0.05, confidence=0.6))
summary(ruleSetA)
inspect( subset( ruleSetA, subset = rhs %pin% "NPS_Type=Promoter" ) )
plot(ruleSetA)

#------------------------------------------------------------------------------------#

#-----------------------------NPS distribution on Map--------------------------------#

data("zipcode")
zip <- zipcode
zip <- zip[which(zip$state=="CA"),]
new <- merge(variables2, zip , by.x="City_PL", by.y="city")

#loading the map for California
CA = geocode("CA")
CAMap = get_map(location = CA, zoom = 6)   #Zooming and adjusting the map
CA <- ggmap(CAMap)
CA

new$Likelihood_Recommend_H <-as.numeric(as.character(new$Likelihood_Recommend_H))

p7 <- CA + geom_point(data = new, aes(x = as.numeric(new$longitude),
          y = as.numeric(new$latitude), color = new$Likelihood_Recommend_H), size=5)+ theme(legend.key.size =unit(1,"cm"))+
          ggtitle ("Hotel Ratings in California")+labs( x="Latitude",y="Longitude")+theme(legend.title = element_text(size=15),
          legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),
          plot.title = element_text(size = rel(2)), axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)))+guides(fill=guide_legend(title="Ratings"))

#-----------------------------------------------------------------------------------------------#

#------------------------------COmparision POV VS Age for LA and SF----------------------------_#

new1 <- new
new1<-new[which(new$City_PL=="Los Angeles" | new$City_PL=="San Diego"),]

p8<- ggplot(new1, aes(new1$Age_Range_H, fill=new1$POV_H)) + geom_bar( position="stack")+ggtitle ("Age V/S Purpose of Visit")+
     labs( x="Age Range (Years)", y="Count")+theme(legend.title = element_text(size=15),legend.box.background = element_rect(),
     legend.box.margin = margin(6, 6, 6, 6), legend.text = element_text(size = 15),plot.title = element_text(size = rel(2)),
     axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5))
     ,axis.title.x = element_text(size = rel(1.5)))+guides(fill=guide_legend(title="Purpose of Visit"))

#Trying some combination to check if relation can be established
lm2 <- lm (new1$Likelihood_Recommend_H~new1$Fitness.Trainer_PL+new1$Fitness.Center_PL+new1$Spa_PL)
summary(lm2)

#-----------------------------------------------------------------------------------------------#

#---------------------------------Linear Modeling on Customer Feedback Ratings------------------#

dummy2<-c(139:148,169,168,138,111,110,233)
dummy2<-dummy2-1
SurveyData3<-fread("out-201412.csv", sep = ",",select = c(dummy2), na.strings = "")
variables3<-SurveyData3
variables3<-variables3[which(variables3$State_PL=="California"),]
variables3$Likelihood_Recommend_H[is.na(variables3$Likelihood_Recommend_H)] <- "0"
variables3<- variables3[-c(which(variables3$Likelihood_Recommend_H==0)),]
variables3[is.na(variables3)] <- "0"
myVars<-variables3

lm3<- lm(variables3$Likelihood_Recommend_H ~ variables3$'F&B_Overall_Experience_H'+variables3$Guest_Room_H+variables3$Tranquility_H+variables3$Condition_Hotel_H+variables3$Customer_SVC_H+variables3$Staff_Cared_H+variables3$Internet_Sat_H+variables3$Check_In_H)
summary(lm3)

#-----------------------------------------------------------------------------------------------#

#---------------------------Creating a Pie Chart for least related variables -------------------#

mytable1 <- table(variables3$Tranquility_H)
p9 <- pie(mytable,main="Pie Chart of Tranquility")


mytable2 <- table(variables3$Internet_Sat_H)
p10 <- pie(mytable2,main="Pie Chart of Internet")

mytable3 <- table(variables3$Staff_Cared_H)
p11 <- pie(mytable3,main="Pie Chart of Internet")

#-----------------------------------------------------------------------------------------------#

#----------------------------------Histogram for Checkins & Checkouts---------------------------#
dummy<-c(13,14,18,19,22,23,43,49,53,54,56,57,60,65,71,74,89,90,92,111,127,128,138
         ,139,140,141,142,143,144,145,146,147,148,168,169,200:228,233)
dummy<-dummy-1
SurveyData<-fread("out-201412.csv", sep = ",",select = c(dummy))
variables<-SurveyData
#Substitute No with N, Yes with Y, I don't know with N, ^$ (blank) with N for Clublounge_Used_H (126,21)& Spa_Used_H(127,22)
variables$Clublounge_Used_H <- sub("No", "N", variables$Clublounge_Used_H)
variables$Clublounge_Used_H <- sub("Yes", "Y", variables$Clublounge_Used_H)
variables$Clublounge_Used_H <- sub("^$", "N", variables$Clublounge_Used_H)
variables$Clublounge_Used_H <- sub("I don't know", "N", variables$Clublounge_Used_H)
variables$Spa_Used_H <- sub("^$", "N", variables$Spa_Used_H)
variables$Spa_Used_H <- sub("No", "N", variables$Spa_Used_H)
variables$Spa_Used_H <- sub("Yes", "Y", variables$Spa_Used_H)
variables[is.na(variables)] <- "N"
clean <- variables
clean[is.na(clean)]<-0

#put the cleaned data in a data frame
clean<-data.frame((clean))

#replace all the Y values with 1 & rest to 0 for Clublounge_Used_H (126,21)& Spa_Used_H(127,22)
for( i in 21:22){
  clean[,i]<-ifelse(clean[,i] =="Y",1,0)
}

#replace all the Y values with 1 & rest to 0 for all the facility variables
for( i in 36:64){
  clean[,i]<-ifelse(clean[,i] =="Y",1,0)
}

#sum the Clublounge_Used_H & Spa_Used_H values
clean$sum <- rowSums(clean[,(21:22)])
#sum all the facility variables
clean$sum2 <- rowSums(clean[,(36:64)])
#sum all the facility variables with the Clublounge_Used_H & Spa_Used_H values
clean$sum3 <- clean$sum+clean$sum2
#get rid of Clublounge_Used_H, Spa_Used_H, facilities, clean$sum & clean$sum2
Cleandata <- clean[,-c(21,22,36:64,66,67)]
#get only california survey data
Cleandata<-Cleandata[which(Cleandata$State_PL=="California"),]
#str(Cleandata)
#remove the State_PL data
Cleandata<- Cleandata[ , -c(33)]
#rename the sum3 cloumn as Sum Factors
colnames(Cleandata)[34]<-"Sum Factors"
for( i in 21:31){
  if(Cleandata[,1] =="N"){
    Cleandata[i,]<-"0"
  }
}
for( i in 21:31 ){
  Cleandata[which(Cleandata[,i]=="N"),][i]<-0
}

#creating a backup of cleandata   
CleandataBackUp<-Cleandata
cleandata2 <- CleandataBackUp
Cleandata2<-Cleandata[,-c(11)]
Cleandata2[which(Cleandata2[11]==""),][11]<-"NA"
Cleandata2[which(Cleandata2[16]==""),][16]<-"NA"
Cleandata2[which(Cleandata2[17]==""),][17]<-"NA"
Cleandata3<- Cleandata2[ , -c(17,18,19)]
Cleandata3[which(Cleandata3[29]==""),][29]<-"NA"
Cleandata3<- Cleandata3[, -c(11)]

as.Date()
class(Cleandata3$CHECK_IN_DATE_C)

Check_in_Date<-as.Date(Cleandata3$CHECK_IN_DATE_C)
Checkin_Dates<-format(as.Date(Check_in_Date, format= "&d"), "%d")
Checkin_Dates<-as.integer(Checkin_Dates)
hist(Checkin_Dates, col ='pink', breaks=30)

Check_out_Date<-as.Date(Cleandata3$CHECK_OUT_DATE_C)
Checkout_Dates<-format(as.Date(Check_out_Date, format= "&d"), "%d")
Cleandata4<-Cleandata3[,-c(1,9,10)]
Checkout_Dates<-as.integer(Checkout_Dates)
hist(Checkout_Dates, col ='slateblue', breaks=30)

#-----------------------------------------------------------------------------------------------#

#----------------------------------Calculating Percentages--------------------------------------#

dummy<-c(138,233,169,110)
dummy<-dummy-1
Hyatt_Dec<-fread("out-201412.csv", sep = ",",select = c(dummy))
Dec_Total <- nrow(Hyatt_Dec)
Hyatt_Dec <- Hyatt_Dec[which(!is.na(Hyatt_Dec$Likelihood_Recommend_H)),]

Dec_Total_New <- nrow(Hyatt_Dec)
Dec_Pro <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Promoter"),])/Dec_Total_New * 100
Dec_Pas <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Passive"),])/Dec_Total_New * 100
Dec_Det <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Detractor"),])/Dec_Total_New * 100

#########################################################################

dummy<-c(138,233,169,110)
dummy<-dummy-1
Hyatt_Dec<-fread("out-201412.csv", sep = ",",select = c(dummy))
Hyatt_Dec<-Hyatt_Dec[which(Hyatt_Dec$State_PL=="California" & Hyatt_Dec$Age_Range_H=="46-55"),]
Dec_Total <- nrow(Hyatt_Dec)
Hyatt_Dec <- Hyatt_Dec[which(!is.na(Hyatt_Dec$Likelihood_Recommend_H)),]
Dec_Total_New <- Hyatt_Dec[which (Hyatt_Dec$Age_Range_H=="46-55"),]
Dec_Total_New <- nrow(Hyatt_Dec)

Dec_Pro <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Promoter"),])/Dec_Total_New * 100
Dec_Pas <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Passive"),])/Dec_Total_New * 100
Dec_Det <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Detractor"),])/Dec_Total_New * 100

#-----------------------------------------------------------------------------------------------#

#-----------------------------Plotting NPS pattern throughoout USA------------------------------#

dummy<-c(172, 171, 138, 169,233)
dummy<-dummy-1
Hyatt_Dec<-fread("out-201412.csv", sep = ",",select = c(dummy))
Hyatt_Dec<-Hyatt_Dec[which(Hyatt_Dec$Country_PL=="United States" ),]
Hyatt_Dec <- Hyatt_Dec[which(!is.na(Hyatt_Dec$Likelihood_Recommend_H)),]
Dec_Total_New <- nrow(Hyatt_Dec)
Dec_Pro <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Promoter"),])/Dec_Total_New * 100
Dec_Pas <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Passive"),])/Dec_Total_New * 100
Dec_Det <- nrow(Hyatt_Dec[which(Hyatt_Dec$NPS_Type=="Detractor"),])/Dec_Total_New * 100

zip <- zipcode
Hyatt_Dec$`Postal Code_PL`<- as.character(as.integer(Hyatt_Dec$`Postal Code_PL`))
new_hy <- merge(Hyatt_Dec, zip , by.x="Postal Code_PL", by.y="zip")
USA = geocode("usa")
usmap = get_map(location = USA, zoom = 4)   #Zooming and adjusting the map
usmap <- ggmap(usmap)
usmap

p12 <- usmap + geom_point(data = new_hy, aes(x = as.numeric(new_hy$longitude), y = as.numeric(new_hy$latitude  ), 
       color = new_hy$Likelihood_Recommend_H), size=5)+ theme(legend.key.size =unit(1,"cm"))+
       ggtitle ("Hotel Ratings in USA")labs( x="Latitude",y="Longitude")+theme(legend.title = element_text(size=15),
       legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),legend.text = element_text(size = 15),
       plot.title = element_text(size = rel(2)),axis.text = element_text(size =15),axis.title.y = element_text(size = rel(1.5)),
       axis.title.x = element_text(size = rel(1.5)))+guides(fill=guide_legend(title="Ratings"))

#-----------------------------------------------------------------------------------------------#

#-----------------------------Association Rules for different Facilities -----------------------#

dummy1<-c(200:228,233,169,168,138,111,110,71,145)
dummy1<-dummy1-1
SurveyData1<-fread("out-201412.csv", sep = ",",select = c(dummy1), na.strings = "")
variables1<-SurveyData1
variables1<-variables1[which(variables1$State_PL=="California"),]
variables1$Likelihood_Recommend_H[is.na(variables1$Likelihood_Recommend_H)] <- "0"
variables1<- variables1[-c(which(variables1$Likelihood_Recommend_H==0)),]

variables1[is.na(variables1)] <- "N"

variables2 <- variables1

variables2$POV_H <- gsub("Combination of both business and leisure", "Both", variables2$POV_H)
variables2$POV_H <- gsub("Combination of both business and leisure", "Both", variables2$POV_H)

variables3 <- variables2 [,c(1:5,10, 15:37)]
variables3 <- data.frame(variables3) 
variable4 <- variables3[,c(5,6,3,25,8,11,15,17,18,19,20,26)]
variable4[] <- lapply(variable4, factor)


ruleSetA <- apriori(variable4[,1:12], parameter = list(support=0.3, confidence=0.65))
x <- inspect( subset( ruleSetA, subset = rhs %pin% "NPS_Type=" ) )
summary(ruleSetA)
inspect(ruleSetA)
plot(ruleSetA)

#-----------------------------------------------------------------------------------------------#

############################################################################
                                    #Part 2#
############################################################################

#-------------------------#Yearly Analysis Code#--------------------------#

#-----------------------------Libraries-----------------------------#
library(openintro)
library(ggplot2)
library(tm)
library(stringr)
library(wordcloud)
library(treemap)
library(Hmisc)
library(corrgram)
library(sqldf)
library(maps)
library(ggmap)
library(rworldmap)
library(data.table)
library(plotrix)
library(corrplot)
library(cowplot)

#Read all the survey data for all the datasets. Remove all the survey data apart from surveys received for California state hotels. Also remove the NA values   
Data201402 <- fread("out-201402.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201402 <- Data201402[Data201402$State_PL=="California"]
Data201402 <- na.omit(Data201402)
Data201402 <- Data201402[,-"State_PL"]
Data201403 <- fread("out-201403.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201403 <- Data201403[Data201403$State_PL=="California"]
Data201403 <- na.omit(Data201403)
Data201403 <- Data201403[,-"State_PL"]
Data201404 <- fread("out-201404.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201404 <- Data201404[Data201404$State_PL=="California"]
Data201404 <- na.omit(Data201404)
Data201404 <- Data201404[,-"State_PL"]
Data201405 <- fread("out-201405.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201405 <- Data201405[Data201405$State_PL=="California"]
Data201405 <- na.omit(Data201405)
Data201405 <- Data201405[,-"State_PL"]
Data201406 <- fread("out-201406.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201406 <- Data201406[Data201406$State_PL=="California"]
Data201406 <- na.omit(Data201406)
Data201406 <- Data201406[,-"State_PL"]
Data201407 <- fread("out-201407.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201407 <- Data201407[Data201407$State_PL=="California"]
Data201407 <- na.omit(Data201407)
Data201407 <- Data201407[,-"State_PL"]
Data201408 <- fread("out-201408.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201408 <- Data201408[Data201408$State_PL=="California"]
Data201408 <- na.omit(Data201408)
Data201408 <- Data201408[,-"State_PL"]
Data201409 <- fread("out-201409.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201409 <- Data201409[Data201409$State_PL=="California"]
Data201409 <- na.omit(Data201409)
Data201409 <- Data201409[,-"State_PL"]
Data201410 <- fread("out-201410.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201410 <- Data201410[Data201410$State_PL=="California"]
Data201410 <- na.omit(Data201410)
Data201410 <- Data201410[,-"State_PL"]
Data201411 <- fread("out-201411.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201411 <- Data201411[Data201411$State_PL=="California"]
Data201411 <- na.omit(Data201411)
Data201411 <- Data201411[,-"State_PL"]
Data201412 <- fread("out-201412.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201412 <- Data201412[Data201412$State_PL=="California"]
Data201412 <- na.omit(Data201412)
Data201412 <- Data201412[,-"State_PL"]
Data201501 <- fread("out-201501.csv",sep = ",", select=c("State_PL","POV_CODE_C","STATE_R","COUNTRY_CODE_R","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F&B_Overall_Experience_H","City_PL","Property Latitude_PL","Property Longitude_PL","All Suites_PL","Boutique_PL","Dry-Cleaning_PL","Elevators_PL","Fitness Center_PL","Laundry_PL","Restaurant_PL","Spa_PL","Shuttle Service_PL","Mini-Bar_PL","Pool-Indoor_PL","NPS_Type"))
Data201501 <- Data201501[Data201501$State_PL=="California"]
Data201501 <- na.omit(Data201501)
Data201501 <- Data201501[,-"State_PL"]

#Combine all the datasets into Data201402 dataset
Data201402 <- rbind(Data201402,Data201403,Data201404,Data201405,Data201406,Data201407,Data201408,Data201409,Data201410,Data201411,Data201412,Data201501)
colnames(Data201402)
#Convert NPS_Type column to character vector
Data201402$NPS_Type <-as.character(Data201402$NPS_Type)
#Create dataset for HotelFacility
HotelFacility <- subset(Data201402,select = c(17:27))
#Create dataset for SurveyFeedback
SurveyFeedback <- subset(Data201402,select = c(15,16,4:13))
#Create dataset for HotelLocation
HotelLocation <- subset(Data201402,select =c(14:16))
#Create dataset for StayData
StayData <- subset(Data201402,select = c(14,15,16,1:3,28))
StayData$GuestStateFullName <- abbr2state(StayData$STATE_R)
HotelFacility <- as.data.frame(ifelse(HotelFacility=="Y",1,0))
StayData$GuestStateFullName <- as.character(StayData$GuestStateFullName)
StayData$STATE_R <- as.character(StayData$STATE_R)
StayData$COUNTRY_CODE_R <- as.character(StayData$COUNTRY_CODE_R)
HotelFacility$FacilityTotal <- rowSums(HotelFacility==1)
HotelFacility$`Property Latitude_PL` <- HotelLocation$`Property Latitude_PL`
HotelFacility$`Property Longitude_PL` <- HotelLocation$`Property Longitude_PL`
StayData$COUNTRY_CODE_R <- str_replace(StayData$COUNTRY_CODE_R," ","")
StayData$GuestStateFullName <- str_replace_all(StayData$GuestStateFullName," ","")
HotelLocation$GuestStateFullName <- StayData$GuestStateFullName
SurveyFeedback$GuestStateFullName <- StayData$GuestStateFullName
mapDevice('x11')
library(tm)
FreqMatrix <- function(x){
  vecSource <- VectorSource(x)
  wordCorpus <- Corpus(vecSource)
  tdm <- TermDocumentMatrix(wordCorpus)
  wordMatrix <- as.matrix(tdm)
  wordCount <- rowSums(wordMatrix)
  wordCount <- sort(wordCount,decreasing = TRUE)
  wordFrame <- data.frame(Name=names(wordCount),freq=wordCount)
  return(wordFrame)
}
#------------------------------------------------------------------------------#

#Guests from around the world Analysis
reqGuestCountry <- FreqMatrix(StayData$COUNTRY_CODE_R)
PercUSGuest <- round((reqGuestCountry$freq[reqGuestCountry$Name=="unitedstates"]/sum(reqGuestCountry$freq))*100,digits = 0)
PercUSGuest
# 3D Exploded Pie Chart
slices <- c(PercUSGuest, 100-PercUSGuest) 
lbls <- c(paste("US",toString(PercUSGuest),"%",sep=" "),paste("Rest of the world",toString(100-PercUSGuest),"%",sep=" "))
pie3D(slices,labels=lbls,explode=0.1,main="Guest from around the world")
#Guests from different state within United States Analysis
FreqGuestState <- FreqMatrix(StayData$GuestStateFullName)
PercStateGuest <- round((FreqGuestState$freq[FreqGuestState$Name=="california"]/sum(FreqGuestState$freq))*100,digits = 0)
PercStateGuest
#TreeMap plot
FreqGuestStateTreeMap<- treemap(FreqGuestState,index = c("Name"),vSize="freq",type="index",palette = "Dark2",title = "US Guests from different states",fontsize.title = 14,fontsize.labels = 12,border.col = "white")
#USMap using Geocode for Guests from different state within United States Analysis
USMap <- map_data("state")
StateGeoCode <- geocode(as.character(FreqGuestState$Name))
FreqGuestState$Latitude <- StateGeoCode$lat
FreqGuestState$Longitude <- StateGeoCode$lon
FreqGuestState
FreqGuestStatePlot <- ggplot(FreqGuestState,aes(map_id=Name))+geom_map(map=USMap,aes(fill=freq),col="white")+expand_limits(x=USMap$long,y=USMap$lat)+scale_fill_gradient(low = "red", high = "green", guide = "colorbar")+coord_map()+ggtitle("US Guests from different states")
FreqGuestStatePlot
#City Wise Guest Stay Analysis
StayData$City_PL <- as.character(StayData$City_PL)
StayData$City_PL <- str_replace_all(StayData$City_PL," ","")
FreqCity <- FreqMatrix(StayData$City_PL)
FreqCityWC <- wordcloud(FreqCity$Name,FreqCity$freq,rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
FreqCityWC

#------------------------------------------------------------------------------#

#Linear Modeling Analysis
SurveyFeedback$FacilityTotal <- HotelFacility$FacilityTotal
corMatrix <- subset(SurveyFeedback,select=c(3:12,14))
corData <- cor(corMatrix)
corrplot(corData, method = "pie")
SurveyFeedbackLM <- lm(Likelihood_Recommend_H~.,data = corMatrix)
summary(SurveyFeedbackLM)

#City Wise Net Promoter Scope (NPS) Analysis
PromoterCount <- sqldf("select count(NPS_Type) PromoterCount, City_PL from StayData where NPS_Type= 'Promoter' group by City_PL order by count(NPS_Type) DESC")
PassiveCount <- sqldf("select count(NPS_Type) PassiveCount, City_PL from StayData where NPS_Type= 'Passive' group by City_PL order by count(NPS_Type) DESC")
DetractorCount <- sqldf("select count(NPS_Type) DetractorCount, City_PL from StayData where NPS_Type= 'Detractor' group by City_PL order by count(NPS_Type) DESC")
MergedNPSCountCityWise <- merge(PromoterCount,PassiveCount,by ="City_PL")
MergedNPSCountCityWise <- merge(MergedNPSCountCityWise,DetractorCount,by="City_PL")
NPSTotalCount <- aggregate(MergedNPSCountCityWise$PromoterCount+MergedNPSCountCityWise$PassiveCount+MergedNPSCountCityWise$DetractorCount,FUN=mean,data=MergedNPSCountCityWise,by=list(MergedNPSCountCityWise$City_PL))
MergedNPSCountCityWise$TotalCount <- NPSTotalCount$x
MergedNPSCountCityWise

#Promotors across California cities
PromoterPlot <- ggplot(MergedNPSCountCityWise,aes(x=City_PL,y=PromoterCount),fill="red")+geom_bar(stat ="identity",col="white",fill="green")+theme(axis.text.x =element_text(angle=90,hjust=1)) + coord_flip()
PromoterPlot
#Passive Guests across California cities
PassivePlot <- ggplot(MergedNPSCountCityWise,aes(x=City_PL,y=PassiveCount),fill="red")+geom_bar(stat ="identity",col="white",fill="yellow")+theme(axis.text.x =element_text(angle=90,hjust=1)) + coord_flip()
PassivePlot
#Detractors across California cities
DetractorPlot <- ggplot(MergedNPSCountCityWise,aes(x=City_PL,y=DetractorCount),fill="red")+geom_bar(stat ="identity",col="white",fill="red")+theme(axis.text.x =element_text(angle=90,hjust=1)) + coord_flip()
DetractorPlot
#Total surveys considered for Hyatt hotels across California State
TotalSurveyPlot <- ggplot(MergedNPSCountCityWise,aes(x=City_PL,y=TotalCount),fill="red")+geom_bar(stat ="identity",col="white",fill="blue")+theme(axis.text.x =element_text(angle=90,hjust=1)) + coord_flip()
TotalSurveyPlot

#City wise NPS for Hyatt Hotels across California  
MergedNPSCountCityWise$PercPromoter <- round((MergedNPSCountCityWise$PromoterCount/ MergedNPSCountCityWise$TotalCount * 100),digits = 0)
MergedNPSCountCityWise$PercDetractor <- round((MergedNPSCountCityWise$DetractorCount/ MergedNPSCountCityWise$TotalCount * 100),digits = 0)
MergedNPSCountCityWise$CityNPS <- MergedNPSCountCityWise$PercPromoter - MergedNPSCountCityWise$PercDetractor 
CityNPSPlot <- ggplot(MergedNPSCountCityWise,aes(x=City_PL,y=CityNPS),fill="red")+geom_bar(stat ="identity",col="white",fill="Orange")+theme(axis.text.x =element_text(angle=90,hjust=1)) + coord_flip()
#Likelihood to recommend vs Facility Total Analysis
Data201402$FacilityTotal <- HotelFacility$FacilityTotal
FacilityPlot <- ggplot(data=Data201402,aes(x=FacilityTotal,y=Likelihood_Recommend_H))
FacilityPlot <- FacilityPlot+geom_point(aes(colour=NPS_Type),shape=19,alpha=0.1,position = position_jitter(w=0.3,h=0.3))
FacilityPlot

#------------------------------------------------------------------------------#

#Function for Linear Modeling with Multiple variables
#linear modeling function to find the combination of independents variables with
#highest r.squared value against a dependent variable. Note that the data shouldn't
#have any "NA" and also the variables are continues

lmBestFit <- function (CleanedData, dependentVar){
  colNames <- colnames(CleanedData)
  if (!is.na(match(dependentVar,colNames))){
    colNames <- colNames[-match(dependentVar,colNames)]
  }
  indivRValue <- data.frame()
  for (i in 1:length(colNames)){
    colNamesCombo <- t(combn(colNames,i))
    indiVarVec <- c()
    for (r in 1:nrow(colNamesCombo)){
      indiVar <- ""
      for (c in 1:ncol(colNamesCombo)){
        ifelse((c == 1),indiVar <- colNamesCombo[r,c],indiVar <- paste(indiVar, colNamesCombo[r,c], sep =" + "))
      }
      indiVarVec <- c(indiVarVec,indiVar)
    }
    for (j in 1:length(indiVarVec)){
      Formula <- paste(dependentVar,"~",indiVarVec[j])
      lmModel <- lm(Formula, data=CleanedData)
      indivRValue <- rbind(indivRValue,c(indiVarVec[j],summary(lmModel)$r.squared),stringsAsFactors=FALSE)
    }
  }
  colnames(indivRValue) <- c("varCombo", "rValue")
  return(indivRValue)
}

######################################################################################
######################################################################################
