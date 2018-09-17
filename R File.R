install.packages("bitops")
install.packages("yaml")
library(installr)
updateR()

install.packages("Rcpp")
devtools::install_github("BillPetti/baseballr")

library(yaml)
library(devtools)
library(XML)
library(XML2R)
library(stringi)
library(Rcpp)
library(bitops)

library(baseballr)
library(dplyr)
library(tidyr)
library(zoo)
library(RcppRoll)
library(lubridate)

df_hitters <- data.frame()
i <- as.Date("2018-03-29")
while (i < "2018-07-16") {
dat<- data.frame(daily_batter_bref(t1= i, t2= i))
dat$dt <- i
df_hitters <- rbind(df_hitters, dat)
i <- as.Date(i+1)
}

df_hitters2 <- data.frame()
i <- as.Date("2018-07-19")
while (i < as.Date("2018-09-15")) {
  dat<- data.frame(daily_batter_bref(t1= i, t2= i))
  dat$dt <- as.Date(i)
  df_hitters2 <- rbind(df_hitters2, dat)
  i <- as.Date(i+1)
}

#Calculate fanduel and draft kings points scored based on points here:
#https://www.fanduel.com/mlb-guide
#https://www.draftkings.com/help/rules/mlb
df_hitters_all <- rbind(df_hitters,df_hitters2)
df_hitters_all[is.na(df_hitters_all)] <- 0

df_hitters_all <- df_hitters_all %>% 
  mutate(FD_Points = 3*`X1B` + 6*`X2B` + 9*`X3B` + 12*HR + 3.5*RBI + 3.2*R + 
           3*BB + 6*SB + 3*HBP,
         DK_Points = 3*`X1B` + 5*`X2B` + 8*`X3B` + 10*HR + 2*RBI + 2*R + 
           2*BB + 5*SB + 2*HBP)

df_hitters_all$

df_pitchers <- data.frame()
i <- as.Date("2018-03-29")
while (i < "2018-07-16") {
  dat<- data.frame(daily_pitcher_bref(t1= i, t2= i))
  dat$dt <- i
  df_pitchers <- rbind(df_pitchers, dat)
  i <- as.Date(i+1)
}


df_pitchers2 <- data.frame()
i <- as.Date("2018-07-19")
while (i < "2018-09-15") {
  dat<- data.frame(daily_pitcher_bref(t1= i, t2= i))
  dat$dt <- i
  df_pitchers2 <- rbind(df_pitchers2, dat)
  i <- as.Date(i+1)
}

df_pitchers_all <- rbind(df_pitchers,df_pitchers2)
df_pitchers_all[is.na(df_pitchers_all)] <- 0

#Calculate fanduel and draft kings points scored based on points here:
#https://www.fanduel.com/mlb-guide
#https://www.draftkings.com/help/rules/mlb
df_pitchers_all <- df_pitchers_all %>% 
  mutate(QS = ifelse(ER<=3 & IP>=6, 1, 0),
         NOHT = ifelse(H==0 & IP==9,1,0),
         SHO = ifelse(ER==0 & IP==9,1,0),
         CG = ifelse(IP==9,1,0),
         FD_Points = 3*IP + 3*SO + 6*W + -3*ER + 4*QS,
         DK_Points = 2.25*IP + 2*SO + 4*W + -2*ER+-0.6*H+-0.6*BB + 2.5*CG + 2.5*SHO + 5*NOHT,
         BIP = BF - SO - BB - HBP,
         GB = BIP * GB.FB,
         FB = BIP * (1 - GB.FB))


#write to csv so can be loaded faster
setwd("C:/Users/bkrei/Desktop/Bk's Stuff Desktop/School/Github NEW PATH/Capstone---Baseball")
write.csv(df_hitters_all, file = "daily hitters.csv")
write.csv(df_pitchers_all, file = "daily pitchers.csv")

last7games_hit <- data.frame()
i <- as.Date("2018-03-29")
i2 <- as.Date(i) + 10
while (i2 < "2018-09-15") {
  dat<- data.frame(daily_batter_bref(t1= i, t2= i2))
  dat$dt <- i
  dat$dt2 <- i2
  last7games_hit <- rbind(last7games_hit, dat)
  i <- as.Date(i+1)
  i2 <- as.Date(i2+1)
}

df_hitters_all <- read.csv("https://raw.githubusercontent.com/bkreis84/Capstone---Baseball/master/daily%20hitters.csv")


#colnames(df_pitchers_all)[colnames(df_pitchers_all) == 'i'] <- 'dt'

#Rolling hitter performance, past 7
df <- df_hitters_all %>% 
  arrange(Name, dt) %>%
  group_by(Name) %>% 
  mutate(X1B_7 = rollapplyr(X1B, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(x1B_7 = lag(X1B_7,1)) %>% 
  mutate(X2B_7 = rollapplyr(X2B, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(x2B_7 = lag(X2B_7,1)) %>% 
  mutate(X3B_7 = rollapplyr(X3B, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(x3B_7 = lag(X3B_7,1)) %>% 
  mutate(HR_7 = rollapplyr(HR, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(HR_7 = lag(HR_7,1)) %>% 
  mutate(uBB_7 = rollapplyr(uBB, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(uBB_7 = lag(uBB_7,1)) %>% 
  mutate(HBP_7 = rollapplyr(HBP, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(HBP_7 = lag(HBP_7,1)) %>% 
  mutate(AB_7 = rollapplyr(AB, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(AB_7 = lag(AB_7,1)) %>% 
  mutate(BB_7 = rollapplyr(BB, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(BB_7 = lag(BB_7,1)) %>% 
  mutate(IBB_7 = rollapplyr(IBB, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(IBB_7 = lag(IBB_7,1)) %>% 
  mutate(SF_7 = rollapplyr(SF, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(SF_7 = lag(SF_7,1)) %>% 
  mutate(PA_7 = rollapplyr(PA, width = 7, FUN = sum, partial = TRUE)) %>% 
  mutate(PA_7 = lag(PA_7,1))

df <- df %>% 
  arrange(Name, dt) %>%
  group_by(Name) %>% 
  mutate(X1B_YTD = cumsum(X1B)) %>% 
  mutate(X1B_YTD = lag(X1B_YTD,1)) %>% 
  mutate(X2B_YTD = cumsum(X2B)) %>% 
  mutate(X2B_YTD = lag(X2B_YTD,1)) %>% 
  mutate(X3B_YTD = cumsum(X3B)) %>% 
  mutate(X3B_YTD = lag(X3B_YTD,1)) %>% 
  mutate(HR_YTD = cumsum(HR)) %>% 
  mutate(HR_YTD = lag(HR_YTD,1)) %>% 
  mutate(uBB_YTD = cumsum(uBB)) %>% 
  mutate(uBB_YTD = lag(uBB_YTD,1)) %>% 
  mutate(HBP_YTD = cumsum(HBP)) %>% 
  mutate(HBP_YTD = lag(HBP_YTD,1)) %>% 
  mutate(AB_YTD = cumsum(AB)) %>% 
  mutate(AB_YTD = lag(AB_YTD,1)) %>% 
  mutate(BB_YTD = cumsum(BB)) %>% 
  mutate(BB_YTD = lag(BB_YTD,1)) %>% 
  mutate(IBB_YTD = cumsum(IBB)) %>% 
  mutate(IBB_YTD = lag(IBB_YTD,1)) %>% 
  mutate(SF_YTD = cumsum(SF)) %>% 
  mutate(SF_YTD = lag(SF_YTD,1)) %>% 
  mutate(PA_YTD = cumsum(PA)) %>% 
  mutate(PA_YTD = lag(PA_YTD,1))




#Teams table with commonly used abbreviations
teams <- read.csv("https://raw.githubusercontent.com/bkreis84/Capstone---Baseball/master/teams.csv")
abb <- as.character(teams$abbrev)
abb

#Read in results of games (helps align which team played which team)
matchups <- data.frame()
i=1
while (i <= length(abb)){
  dat<- data.frame(team_results_bref(abb[[i]], 2018))
  dat$team <- abb[[i]]
  matchups <- rbind(matchups, dat)
  i = i+1
}

library(stringr)
#Pull out the league from main hitter dataframe do not include dash, find any characters after
lg <- "(?<=\\-).*"

df$Level <- df$Level %>% 
  str_match(lg)

colnames(df)[colnames(df) == 'Team'] <- 'city'
colnames(df)[colnames(df) == 'Level'] <- 'league'
df$league <- df$league[,1]


dfm <- merge(df, teams, by = c("city", "league"), all.x = TRUE)


datef <- "(?<=[, ]).*"

matchups$newdt <- matchups$Date %>% 
  str_match(datef)  

matchups$newdt <- paste0(matchups$newdt,"-","2018")  
matchups$newdt <- sub(" ","", matchups$newdt)
matchups$newdt <- sub(" ","-", matchups$newdt)

matchups$newdt2 <- strptime(matchups$newdt, "%b-%d-%Y")

class(dfm$dt)
class(matchups$Date)
as.Date(matchups$Date)
as.date
library(RCurl)
library(xml2)
library(rvest)
library(stringr)
library(XML)
library(dplyr)
# NO LONGER NEEDED
#Team abbreviations
url <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Baseball/Team_abbreviations"

tms <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul') 

prse <- xmlParse(tms[[1]])
lst <- xmlToList(prse)
df <- do.call(rbind.data.frame, lst)


#Return 2 or 3 values (non-greedy-?) ending at look-ahead assertion of equal sign
abbrev_pos <- ".{2,3}?(?=\\=)"

abbrev <- df[,1] %>% 
  str_match(abbrev_pos)

name_pos <- ".{2,3}?(?=\\=)"

abbrev <- df[,1] %>% 
  str_match(abbrev_pos)


