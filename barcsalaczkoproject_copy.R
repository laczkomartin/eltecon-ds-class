#project made by: Barcsa Zoltán (CIVA57) and Laczkó Martin (YCURXU)
library(data.table)
library(ggplot2)
library(fasttime)

# data can be downloaded from: https://www.kaggle.com/sobhanmoosavi/us-accidents
# info  about each column  can also be found on this website: https://smoosavi.org/datasets/us_accidents

usaccident<-fread("data/US_Accidents_May19.csv")
summary(usaccident)

#let's delete a few columns we won't need

usaccident[,c("Source","TMC","Start_Lat","Start_Lng","End_Lat","End_Lng","Description","Number","Street",
              "Side","City","County","Zipcode","Country","Airport_Code","Weather_Timestamp","Wind_Chill(F)",
              "Precipitation(in)","Amenity","Bump","Give_Way","Junction","No_Exit","Railway","Roundabout",
              "Station","Stop","Traffic_Calming","Traffic_Signal","Turning_Loop","Civil_Twilight",
              "Nautical_Twilight","Astronomical_Twilight"):=NULL]

#let's customize a few columns

usaccident[,ID:=1:nrow(usaccident)]
usaccident[,Start_Time:=as.Date(fastPOSIXct(Start_Time))]
usaccident[,End_Time:=as.Date(fastPOSIXct(End_Time))]
usaccident[,State:=as.factor(State)]
usaccident[,Crossing:=as.factor(Crossing)]
usaccident[Timezone=="",Timezone:=NA]
usaccident[,Timezone:=as.factor(Timezone)]
usaccident[Wind_Direction=="",Wind_Direction:=NA]
usaccident[,Wind_Direction:=as.factor(Wind_Direction)]
usaccident[Sunrise_Sunset=="",Sunrise_Sunset:=NA]
usaccident[,Sunrise_Sunset:=as.factor(Sunrise_Sunset)]
usaccident[Weather_Condition=="",Weather_Condition:=NA]
usaccident[,Weather_Condition:=as.factor(Weather_Condition)]

summary(usaccident)

# we realized that we won't need Start_Time and End_Time in our analysis (1. we don't want to perform time-series analysis
# 2. data seems a bit unreliable and we might get confused using them(maximum of End_time is 2020-10-31))

usaccident[,c("Start_Time","End_Time"):=NULL]


# the highest ever measured windspeed is 302mph https://en.wikipedia.org/wiki/Wind_speed#Highest_speed
usaccident[`Wind_Speed(mph)`>302,`Wind_Speed(mph)`:=NA]

# a maximum distance affected is 333.63 miles which seem unbelievable, but we can't rule out this 
# observation without any proof

# the highest temperature ever measured in the US is 134 Fahrenheit : 
# https://www.livescience.com/30582-highest-hottest-temperature-recorded-us-world.html
usaccident[`Temperature(F)`>134,`Temperature(F)`:=NA]
# lowest temperature ever measured in the US was -80 Fahrenheit, so we cannot surely rule out the minimum

# all time high and low air pressures: https://sciencing.com/high-low-reading-barometric-pressure-5814364.html
# max and min are from 13 March 2018, but we assume that the max and min have not changed after this period
usaccident[`Pressure(in)`<25.9|`Pressure(in)`>32.01,`Pressure(in)`:=NA]

table(usaccident$`Visibility(mi)`)
# we don't have any additional info of Visibility(mi), so we leave it for now.

ggplot(usaccident,mapping=aes(`Humidity(%)`))+geom_histogram()

# we don't see any outliers to work with (no data <0 or >100)

ggplot(usaccident,mapping = aes(Wind_Direction))+geom_bar()

ConvertMitoKm<-function(usaccident){
  usaccident[,`Distance(mi)`:=`Distance(mi)`*1.60934]
  usaccident[,`Visibility(mi)`:=`Visibility(mi)`*1.60934]
}

ConvertFahrenheittoCelsius<-function(usaccident){
  usaccident[,`Temperature(F)`:=(`Temperature(F)`-32)*5/9]
}

ConvertMphtoKph<-function(usaccident){
  usaccident[,`Wind_Speed(mph)`:=`Wind_Speed(mph)`*1.60934]
}

ConvertMitoKm(usaccident)
ConvertFahrenheittoCelsius(usaccident)
ConvertMphtoKph(usaccident)

names(usaccident)[which(names(usaccident)=="Distance(mi)")]<-"Distance(km)"
names(usaccident)[which(names(usaccident)=="Visibility(mi)")]<-"Visibility(km)"
names(usaccident)[which(names(usaccident)=="Temperature(F)")]<-"Temperature(C)"
names(usaccident)[which(names(usaccident)=="Wind_Speed(mph)")]<-"Wind_Speed(kph)"

summary(usaccident)
