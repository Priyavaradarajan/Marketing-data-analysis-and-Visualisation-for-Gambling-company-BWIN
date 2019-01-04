####Final Sports Analytics table code

##Install the package
install.packages("sas7bdat")
library("sas7bdat")

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

