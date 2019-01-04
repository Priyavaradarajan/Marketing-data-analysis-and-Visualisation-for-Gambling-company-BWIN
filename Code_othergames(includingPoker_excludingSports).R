#############--------------------R group assignment on gambling dataset(Group 9)-------------------###############
####---------subsetting the other games including poker into seperate CSV files----------####

#Install the packages:
install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages('stringr')
library(stringr)
install.packages("tidyverse")
library(tidyverse)

####Subsetting Poker data from the basetable

Gambling_basetable_final <- read.csv("Gambling_basetable_final.csv")

#Construct final poker table
Poker <- Gambling_basetable_final[, -c(1, 4:28, 33:38)] 

#Merge sports table with additional table to add user continent
Continent <- read.csv('country.csv')

Poker2 = merge(x = Poker, y = Continent, by = "UserID", all.x = TRUE)
Poker2 <- Poker2[, -7]

PokerFinal <- Poker2[-(which(Poker$Poker_Buy == 0 & Poker$Poker_Sell == 0)),]

#Replace gender values of 0 and 1 with F and M
PokerFinal$Gender[which(PokerFinal$Gender == 1)] = 'Male'
PokerFinal$Gender[which(PokerFinal$Gender == 0)] = 'Female'

summary(PokerFinal)

write.csv(PokerFinal, "Poker.csv")

##########Subsetting for the other tables like Casino,Games and Supertoto - Reading the basetable######
Overalldata = read.csv('Gambling_basetable_final.csv')
country = read.csv('country.csv')
head(Overalldata)

Overalldata$X = NULL

otherproduct = Overalldata[,c(1,2,13:27,32:37)]

str(otherproduct)

#########Casino#########

Casino = otherproduct[,c(1,2,8:12,18:23)]
which(Casino$Casino_max_stakes == 0 & Casino$Casino_mean_bets == 0 & Casino$Casino_mean_Stakes == 0 & Casino$Casino_mean_Winnings == 0)
Casino = Casino[-which(Casino$Casino_max_stakes == 0 & Casino$Casino_mean_bets == 0 & Casino$Casino_mean_Stakes == 0 & Casino$Casino_mean_Winnings == 0),]
Casino = merge(x=Casino,y=country,by = "UserID", all.x = TRUE)
Casino$X = NULL
Casino = Casino[,c(1:7,14)]
str(Casino)
Casino$Continent = as.character(Casino$Continent)

Casino$Continent[which(is.na(Casino$Continent))] = 'Others'
Casino$Gender[which(Casino$Gender == 1)] = 'Male'
Casino$Gender[which(Casino$Gender == 0)] = 'Female'

summary(Casino)

write.csv(Casino,file='Casino.csv')


#########Games#########


Games = otherproduct[,c(1,2,13:23)]
which(Games$Games_mean_Winnings == 0 & Games$Games_max_stakes == 0 & Games$Games_mean_bets == 0 & Games$Games_mean_Stakes == 0)
Games = Games[-which(Games$Games_mean_Winnings == 0 & Games$Games_max_stakes == 0 & Games$Games_mean_bets == 0 & Games$Games_mean_Stakes == 0),]
Games = merge(x=Games,y=country,by="UserID",all.x = TRUE)
Games$X = NULL
Games = Games[,c(1:7,14)]
str(Games)
Games$Continent = as.character(Games$Continent)
Games$Continent[which(is.na(Games$Continent))] = 'Others'
Games$Gender[which(Games$Gender == 1)] = 'Male'
Games$Gender[which(Games$Gender == 0)] = 'Female'

summary(Games)
write.csv(Games,file='Games.csv')

########Supertoto######

Supertoto = otherproduct[,c(1:7,18:23)]
which(Supertoto$Supertoto_max_stakes == 0 & Supertoto$Supertoto_mean_Bets == 0 & Supertoto$Supertoto_mean_Stakes == 0 & Supertoto$Supertoto_mean_Winnings == 0)
Supertoto = Supertoto[-which(Supertoto$Supertoto_max_stakes == 0 & Supertoto$Supertoto_mean_Bets == 0 & Supertoto$Supertoto_mean_Stakes == 0 & Supertoto$Supertoto_mean_Winnings == 0),]
Supertoto = merge(x=Supertoto,y=country,by="UserID",all.x = TRUE)
Supertoto$X = NULL
Supertoto = Supertoto[,c(1:7,14)]
str(Supertoto)
Supertoto$Continent = as.character(Supertoto$Continent)
Supertoto$Continent[which(is.na(Supertoto$Continent))] = 'Others'
Supertoto$Gender[which(Supertoto$Gender == 1)] = 'Male'
Supertoto$Gender[which(Supertoto$Gender == 0)] = 'Female'

summary(Supertoto)

write.csv(Supertoto,file='Supertoto.csv')

###Extracting data from the base table to see the overall users played across countries, gender etc

Demographic = Overalldata[,c(1,2,32:37)]
Demographic = merge(x=Demographic,y=country,by="UserID",all.x = TRUE)
Demographic$X = NULL
Demographic= Demographic[,c(1,2,9)]
str(Demographic)
Demographic$Continent = as.character(Demographic$Continent)
Demographic$Continent[which(is.na(Demographic$Continent))] = 'Others'
Demographic$Gender[which(Demographic$Gender == 1)] = 'Male'
Demographic$Gender[which(Demographic$Gender == 0)] = 'Female'

write.csv(Demographic,file='Demographics.csv')

hist(Casino$Casino_LOS)

