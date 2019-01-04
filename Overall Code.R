#############--------------------R group assignment on gambling dataset(Group 9)-------------------###############

#Install the packages:
install.packages("haven")
library(haven)
install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages('stringr')
library(stringr)
install.packages("dummies")
library(dummies)
install.packages('readxl')
library(readxl)
install.packages('countrycode')
library(countrycode)


#Read all the files from the gambling dataset

Gambdemo = read_sas("RawDataIDemographics.sas7bdat")
head(Gambdemo)
dim(Gambdemo)
Gambpok = read_sas("RawDataIIIPokerChipConversions.sas7bdat")
head(Gambpok)
Gambagg = read_sas("RawDataIIUserDailyAggregation.sas7bdat")
head(Gambagg)
Gambintnt = read_sas("AnalyticDataInternetGambling.sas7bdat")
head(Gambintnt)


###Manipulating the aggregation table

#Coverting the data into readable format

Gambagg1 = Gambagg %>% separate(Date, into = c('Year', 'Date'), sep = 4, convert = TRUE)
Gambagg1 = Gambagg1 %>% separate(Date, into = c('Month', 'Day'), sep = -2, convert = TRUE)
head(Gambagg1)

Gambagg1$Day = str_pad(Gambagg1$Day,2,pad = "0")
Gambagg1$Month = str_pad(Gambagg1$Month,2,pad = "0")
Gambagg1 = Gambagg1 %>% unite("Date",c("Day","Month","Year"),sep="/")
Gambagg1$Date = as.Date(Gambagg1$Date, format = '%d/%m/%Y')

#Creating new variables like max,mean,recency for all the products grouping by userid and productid

Gambagg2 <- Gambagg1 %>% group_by(UserID,ProductID) %>% summarise(mean_Stakes = round(mean(Stakes),2), 
                                                     mean_Winnings = round(mean(Winnings),2),
                                                     mean_Bets = round(mean(Bets),2),mean_12Stakes = round(max(Stakes),2),mean_Recency_bet = as.Date("01/10/2005",'%d/%m/%Y') - max(Date))

#Viewing the top5 rows of the newdataset created                                              
head(Gambagg2)

#Checking the rows and columns
dim(Gambagg2)

#Checking the number of unique products
unique(Gambagg2$ProductID)
length(unique(Gambagg$UserID))

#Creating different rows for every product for different values of max, min, recency for stakes, winnings and bets
Gambagg3 = Gambagg2 %>%
  gather(Var, val, starts_with("mean")) %>% 
  unite(Var1,Var, ProductID) %>% 
  spread(Var1, val)

#Checking the rows, columns and top rows of the new dataset created
dim(Gambagg3)
head(Gambagg3)
length(unique(Gambagg3$UserID))



###Manipulating the poker chips table

#Coverting the date to readable format
Gambpok$TransDateTime = as.POSIXct(Gambpok$TransDateTime,'%Y-%m-%d %H:%M:%S')
Gambpok$TransDateTime = format(as.POSIXct(Gambpok$TransDateTime,'%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d')
Gambpok$TransDateTime = as.Date(Gambpok$TransDateTime, format = '%Y-%m-%d')

#Creating new variables from the poker dataset like mean amount, recency etc.
Gambpok1 <- Gambpok %>% group_by(UserID,TransType) %>% summarise(mean_TransAmount = round(sum(TransAmount),2),
                                                                 mean_Recency_poker = as.Date("01/10/2005",'%d/%m/%Y') - max(TransDateTime))
                                                            

#Checking the top rows of the new table created
head(Gambpok1)

#Checking number of unique users in poker dataset
length(unique(Gambpok$UserID))


#Dividing the rows based on the poker sell or buy for values like recency and mean trans amount

Gambpok2 = Gambpok1 %>%
  gather(Var, val, starts_with("mean")) %>% 
  unite(Var1,Var, TransType) %>% 
  spread(Var1, val)


#Viewing the top rows after the creation of new rows
head(Gambpok2)

#Renaming some of the rows
names(Gambpok2)[4] = "Poker_Sell"
names(Gambpok2)[5] = "Poker_Buy"
names(Gambpok2)[3] = "Recency_Poker"

#Finding the profit value
Gambpok2['Profit_Poker'] = Gambpok2$Poker_Sell - Gambpok2$Poker_Buy

Gambpok2$mean_Recency_poker_124 = NULL

###Merging of the Aggregation and Poker Table:
Gambdata = merge(x=Gambagg3,y=Gambpok2,by = "UserID",all.x = TRUE)
head(Gambdata)


#Renaming the columns
colnames(Gambdata)[2:8]
colnames(Gambdata)[c(2:8)] = c("Max_Stakes_1","Max_Stakes_2","Max_Stakes_4","Max_Stakes_5","Max_Stakes_6",
                         "Max_Stakes_7","Max_Stakes_8")


###Manipulating the demographics table

#Read the data with country code and country name
countrydata2 <- read_excel("Country_data.xls", col_names = FALSE)

#Rename column names of country data file
colnames(countrydata2)[1] <- "Country"
colnames(countrydata2)[2] <- "Names"

#Merging country data with original demographics table
Gambdemo1 = merge(x = Gambdemo, y = countrydata2, by = "Country", all.x = TRUE)

#Retriving the continent information based on the country names
Gambdemo1$Continent <- factor(countrycode(sourcevar = Gambdemo1[, "Names"],
                                          origin = "country.name",
                                          destination = "continent"))

#R countrycode package didn't recognize 2 countries (David Garcia ane Serbia and Montenegro), so we manually added continent
Gambdemo1[42643, 3] = "Europe"
Gambdemo1[42644, 3] = "Europe"
Gambdemo1[42645, 3] = "Europe"
Gambdemo1[42646, 3] = "Europe"
Gambdemo1[42647, 3] = "Europe"
Gambdemo1[42648, 3] = "Europe"
Gambdemo1[42649, 3] = "Asia"

#Removing uneccessary columns
Gambdemo1$Names <- Gambdemo1$Language <- Gambdemo1$RegDate <- Gambdemo1$FirstSp <- Gambdemo1$FirstCa <- Gambdemo1$FirstGa <-
  Gambdemo1$FirstPo <- Gambdemo1$ApplicationID <- Gambdemo1$Country <- Gambdemo1$FirstAct <- NULL

#Converting date columns into separate columns for year, month, day
Gambdemo1 <- Gambdemo1 %>% separate(FirstPay, into = c('Year', 'FirstPay'), sep = 4, convert = TRUE)
Gambdemo1 <- Gambdemo1 %>% separate(FirstPay, into = c('Month', 'FirstPay'), sep = -2, convert = TRUE)
colnames(Gambdemo1)[2] <- "Year_FirstPay"
colnames(Gambdemo1)[3] <- "Month_FirstPay"
colnames(Gambdemo1)[4] <- "Day_FirstPay"

#Find and delete users from after cut-off date (Sept 30, 2005)
which(Gambdemo1$Month_FirstPay == "10")
Gambdemo1 <- Gambdemo1[-which(Gambdemo1$Month_FirstPay == "10"), ]

head(Gambdemo1)

dim(Gambdemo1)

#Concatenate date back together

Gambdemo1$Day_FirstPay = str_pad(Gambdemo1$Day_FirstPay,2,pad = "0")
Gambdemo1$Month_FirstPay = str_pad(Gambdemo1$Month_FirstPay,2,pad = "0")

Gambdemo1 = Gambdemo1 %>% unite("Date",c("Day_FirstPay","Month_FirstPay","Year_FirstPay"),sep="/")

Gambdemo1$Date = as.Date(Gambdemo1$Date, format = '%d/%m/%Y')

#Rename some columns
names(Gambdemo1)[2] = "First_pay_Date"
names(Gambdemo1)[1] = "UserID"


##Merge the data 'Gambdata' which was the one already aggregated with aggregation and poker table with the manipulated demographic data
Gambdataoveralldata = merge(x=Gambdata,y=Gambdemo1,by = "UserID", all.x = TRUE)

#Viewing the data to get some insights
head(Gambdataoveralldata)

#Extracting userid with country details
countrydetails = Gambdataoveralldata[,c(1,43)]

write.csv(countrydetails,file='country.csv')

#####Data cleaning of the overall merged data

#Deleting the 'Firstpaydate' column
Gambdataoveralldata1 = Gambdataoveralldata[,-41]

dim(Gambdataoveralldata1)

#Creating dummy variables for the column continent 
for(v in "Continent"){
  d = dummy(v,data=Gambdataoveralldata1)
  Gambdataoveralldata1 = cbind(Gambdataoveralldata1,d)
  Gambdataoveralldata1[v] = NULL
}

#checking the summary of the data

summary(Gambdataoveralldata1)


#Replace the values in Recency_poker which are less than 0 to 0
Gambdataoveralldata1$Recency_Poker[which(Gambdataoveralldata1$Recency_Poker < 0)] = 0

#Delete the observation where the gender value is NA
Gambdataoveralldata1 = Gambdataoveralldata1[-which(is.na(Gambdataoveralldata1$Gender)),]


#Find the NA values on other numeric columns and replace them with 0 which shows they have not participated in some games
is.na.dataframe <- function(x)
   do.call(cbind,lapply(x,is.na))

Gambdataoveralldata1[is.na(Gambdataoveralldata1)] <- 0

summary(Gambdataoveralldata1)

#Rename the columns of recency
names(Gambdataoveralldata1)[16] = 'Recency_bet_sportsFO'
names(Gambdataoveralldata1)[17] = 'Recency_bet_sportsLA'
names(Gambdataoveralldata1)[18] = 'Recency_bet_Casinoboss'
names(Gambdataoveralldata1)[19] = 'Recency_bet_supertoto'
names(Gambdataoveralldata1)[20] = 'Recency_bet_GamesVS'
names(Gambdataoveralldata1)[21] = 'Recency_bet_Gamesbwin'
names(Gambdataoveralldata1)[22] = 'Recency_bet_casinochartwell'

#Merging the max stakes of products 4 and 8 which belong to Casino
Gambdataoveralldata1['Casino_max_stakes'] = Gambdataoveralldata1['Max_Stakes_4'] + Gambdataoveralldata1['Max_Stakes_8']
#Merging the max stakes of products 6 and 7 which belong to Games
Gambdataoveralldata1['Games_max_stakes'] = Gambdataoveralldata1['Max_Stakes_6'] + Gambdataoveralldata1['Max_Stakes_7']

#Deleting the individual columns of max stakes for the products 4,6,7 and 8
Gambdataoveralldata1$Max_Stakes_4 = Gambdataoveralldata1$Max_Stakes_8 = Gambdataoveralldata1$Max_Stakes_6 = Gambdataoveralldata1$Max_Stakes_7 = NULL

#Merging the mean bets of products 4 and 8 which belong to Casino
Gambdataoveralldata1['Casino_mean_bets'] = Gambdataoveralldata1['mean_Bets_4'] + Gambdataoveralldata1['mean_Bets_8']
#Merging the mean bets of products 4 and 8 which belong to Games
Gambdataoveralldata1['Games_mean_bets'] = Gambdataoveralldata1['mean_Bets_6'] + Gambdataoveralldata1['mean_Bets_7']

#Deleting the individual columns of mean for the products 4,6,7 and 8
Gambdataoveralldata1$mean_Bets_4 = Gambdataoveralldata1$mean_Bets_8 = Gambdataoveralldata1$mean_Bets_6 = Gambdataoveralldata1$mean_Bets_7 = NULL

#Merging the mean stakes of products 4 and 8 which belong to Casino
Gambdataoveralldata1['Casino_mean_Stakes'] = Gambdataoveralldata1['mean_Stakes_4'] + Gambdataoveralldata1['mean_Stakes_8']
#Merging the mean stakes of products 6 and 7 which belong to Games
Gambdataoveralldata1['Games_mean_Stakes'] = Gambdataoveralldata1['mean_Stakes_6'] + Gambdataoveralldata1['mean_Stakes_7']


##Deleting the individual columns of mean stakes for the products 4,6,7 and 8
Gambdataoveralldata1$mean_Stakes_4 = Gambdataoveralldata1$mean_Stakes_8 = Gambdataoveralldata1$mean_Stakes_6 = Gambdataoveralldata1$mean_Stakes_7 = NULL

#Merging the mean winnings of products 4 and 8 which belong to Casino
Gambdataoveralldata1['Casino_mean_Winnings'] = Gambdataoveralldata1['mean_Winnings_4'] + Gambdataoveralldata1['mean_Winnings_8']
#Merging the mean winnings of products 6 and 7 which belong to Games
Gambdataoveralldata1['Games_mean_Winnings'] = Gambdataoveralldata1['mean_Winnings_6'] + Gambdataoveralldata1['mean_Winnings_7']

##Deleting the individual columns of mean winnings for the products 4,6,7 and 8
Gambdataoveralldata1$mean_Winnings_4 = Gambdataoveralldata1$mean_Winnings_8 = Gambdataoveralldata1$mean_Winnings_6 = Gambdataoveralldata1$mean_Winnings_7 = NULL

#Renaming the Columns
names(Gambdataoveralldata1)[2] = 'sportsFO_max_stakes'
names(Gambdataoveralldata1)[3] = 'sportsLA_max_stakes'
names(Gambdataoveralldata1)[4] = 'Supertoto_max_stakes'
names(Gambdataoveralldata1)[5] = 'sportsFO_mean_Bets'
names(Gambdataoveralldata1)[6] = 'sportsLA_mean_Bets'
names(Gambdataoveralldata1)[7] = 'Supertoto_mean_Bets'
names(Gambdataoveralldata1)[15] = 'sportsFO_mean_Stakes'
names(Gambdataoveralldata1)[16] = 'sportsLA_mean_Stakes'
names(Gambdataoveralldata1)[17] = 'Supertoto_mean_Stakes'
names(Gambdataoveralldata1)[18] = 'sportsFO_mean_Winnings'
names(Gambdataoveralldata1)[19] = 'sportsLA_mean_Winnings'
names(Gambdataoveralldata1)[20] = 'Supertoto_mean_Winnings'
names(Gambdataoveralldata1)[26] = 'Africa'
names(Gambdataoveralldata1)[27] = 'Americas'
names(Gambdataoveralldata1)[28] = 'Asia'
names(Gambdataoveralldata1)[29] = 'Europe'
names(Gambdataoveralldata1)[30] = 'Oceania'
names(Gambdataoveralldata1)[31] = 'Others_Country'

#Renaming a column
names(Gambdataoveralldata1)[10] = 'Recency_bet_Casinoboss'

Gambdataoveralldata1 = Gambdataoveralldata1[,-c(40:42)]
  
#Creatinga new column for recency for casino and games , by finding the minimun value between them
Gambdataoveralldata2 = mutate(Gambdataoveralldata1, Casino_LOS = pmax(Recency_bet_Casinoboss,Recency_bet_casinochartwell))
Gambdataoveralldata2 = mutate(Gambdataoveralldata2, Games_LOS = pmax(Recency_bet_GamesVS,Recency_bet_Gamesbwin))

#Deleting the original columns of recency for the products 4,6,7,8
Gambdataoveralldata2$Recency_bet_Casinoboss = Gambdataoveralldata2$Recency_bet_casinochartwell = Gambdataoveralldata2$Recency_bet_GamesVS = Gambdataoveralldata2$Recency_bet_Gamesbwin = NULL

#Rearraging the variable names:

Gambdataoveralldata3 = Gambdataoveralldata2[,c(1,21,2,3,5,6,11,12,14,15,8,9,10,4,7,13,16,10,28,30,32,34,36,29,
                                               31,33,35,37,18,19,20,17,22,23,24,25,26,27)]
Gambdataoveralldata3$Recency_bet_supertoto = NULL

#Renaming a column
names(Gambdataoveralldata3)[17] = 'Supertoto_LOS'

#Writing the basetable to CSV
write.csv(Gambdataoveralldata3,file='Gambling_basetable_final.csv')

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


#Read in Sports Betting data (fixed odds and live action)
SportsBetting <- read.sas7bdat("AnalyticDataInternetGambling.sas7bdat")

#Overview of raw dataset
head(SportsBetting)

install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)

#Deriving new variables from the aggregation table
#Add columns for average FO and LA stake (via FO/LA total stakes and total bets columns)
SportsBetting$FOAvgStake <- SportsBetting[,'FOTotalStakes']/SportsBetting[,'FOTotalBets']
SportsBetting$LAAvgStake <- SportsBetting[,'LATotalStakes']/SportsBetting[,'LATotalBets']
SportsBetting$FOAvgTotalWinnings <- SportsBetting[,'FOTotalWinnings']/SportsBetting[,'FOTotalBets']
SportsBetting$LAAvgTotalWinnings <- SportsBetting[,'FOTotalWinnings']/SportsBetting[,'LATotalBets']

#Add columns for FO and LA active days (First Active Date - Last Active Date)
SportsBetting$FOTotalActiveDays <- SportsBetting[,11]-SportsBetting[,10]
SportsBetting$LATotalActiveDays <- SportsBetting[,17]-SportsBetting[,16]

#Removing of uneccessary columns
SportsBetting$FOTotalStakes <- SportsBetting$FOTotalBets <-SportsBetting$FOTotalDaysActive <- SportsBetting$RegistrationDate <- NULL
SportsBetting$LATotalDaysActive <- NULL
SportsBetting$LAWinningsPerActiveDays <- SportsBetting$FOWinningsPerActiveDays <- SportsBetting$FORecency <- SportsBetting$LARecency <- NULL
SportsBetting$LATotalStakes <- NULL
SportsBetting$LATotalBets <- NULL
SportsBetting$FOTotalWinnings <- NULL
SportsBetting$LATotalWinnings <- NULL
SportsBetting$FirstSportsActiveDate <- NULL
SportsBetting$LANGUAGE <- NULL
SportsBetting$RegistrationDate <- NULL
SportsBetting$COUNTRY <- NULL
SportsBetting$FOFirstActiveDate <- NULL
SportsBetting$LAFirstActiveDate <- NULL
SportsBetting$FOLastActiveDate <- NULL
SportsBetting$LALastActiveDate <- NULL
SportsBetting$GENDER <- NULL

#Check for any outliers
summary(SportsBetting)

#Replace all Nan values with 0, so as to be able to manipulate data later on 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

SportsBetting[is.nan(SportsBetting)] <- 0

#View summary
summary(SportsBetting)

colnames(SportsBetting)[1] <- "UserID"

#Merge basetable and sports table and then remove irrelevant rows
Gambling_basetable_final <- read.csv('Gambling_basetable_final.csv')

SportsBetting2 = merge(x = SportsBetting, y = Gambling_basetable_final, by = "UserID", all.x = TRUE)
SportsBetting3 <- SportsBetting2[, -c(9, 21:45)]
SportsBetting3 <- SportsBetting3[-35840, ]
summary(SportsBetting3)

#Merge sports table with additional table to add user continent
Continent <- read.csv('country.csv')
SportsBetting4 = merge(x = SportsBetting3, y = Continent, by = "UserID", all.x = TRUE)
SportsBettingFinal <- SportsBetting4[, -20]

#Replace gender values of 0 and 1 with F and M
SportsBettingFinal$Gender[which(SportsBettingFinal$Gender == 1)] = 'Male'
SportsBettingFinal$Gender[which(SportsBettingFinal$Gender == 0)] = 'Female'

Sports = SportsBettingFinal[,c(1:9,18:20)]

summary(Sports)

Sports$Continent = as.character(Sports$Continent)

Sports$Continent[which(is.na(Sports$Continent))] = 'Others'

write.csv(Sports, "Sports.csv")

save(Poker,Sports,Games,Casino,Supertoto,Demographic, file= "bastetable.Rdata")





