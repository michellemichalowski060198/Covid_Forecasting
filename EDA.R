# ADVANCED R - GROUP PROJECT 
################################
# EDA
################################


# Importing Libraries
library(data.table)
library(ggplot2) 
library(dplyr)
library(DataCombine)
library(TTR)
library(lubridate)
library(rgdal)

# DATA INPUT

# Load initial data
df <- read.csv("/Users/michellemichalowski/Desktop/R/Covid19_Forecasting-/data/train.csv", 
               header = TRUE,
               na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
               sep = ",")
# Transform to date type
df$Date <- as.Date(df$Date, "%Y-%m-%d")

# Load and merge health system data
# https://www.kaggle.com/danevans/world-bank-wdi-212-health-systems
df_health_system <- read.csv("/Users/michellemichalowski/Desktop/R/Covid19_Forecasting-/data/health_system.csv", 
                             header = TRUE,
                             na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                             sep = ",")
df_health_system <- subset(df_health_system, select = -c(Province_State))
df <- merge(df, df_health_system, by="Country_Region", all.y=TRUE)

# Interesting data but time span online from 22.01-30.03 (approx) TODO: Shall we use it?
# Load and merge misc data

# Load and merge weather data
# https://www.kaggle.com/davidbnn92/weather-data/data?scriptVersionId=30695168
df_weather <- read.csv("./data/weather_info.csv", 
                             header = TRUE,
                             na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                             sep = ",")
df_weather$Date <- as.Date(df_weather$Date, "%Y-%m-%d")
df_weather <- subset(df_weather, select = c("Province.State","Country.Region","Date","temp","min","max","stp","wdsp", "prcp", "fog"))
df <- merge(df, df_weather, by.x = c("Country_Region","Province_State", "Date"), by.y=c("Country.Region","Province.State","Date"), all.x=TRUE)

# in the end we decided to not use those external data sets for our modelling 
# What columns do we have in our data set?
head(df)

# Dimensions?
dim(df)

# Summary of columns
summary(df)

# Check the data types for each column 
sapply(df, class)

# Searching for missing values
sapply(df, function(x) sum(is.na(x)))

# Checking minimum, maximum and mean
max(df$ConfirmedCases,  na.rm = TRUE)
min(df$ConfirmedCases,  na.rm = TRUE)
mean(df$ConfirmedCases, na.rm = TRUE)

max(df$Fatalities ,  na.rm = TRUE)
min(df$Fatalities ,  na.rm = TRUE)
mean(df$Fatalities , na.rm = TRUE)

################################
#### Data Exploration ##########
################################

# Checking how many countries there are in the dataset
length(unique(df$Country_Region))
#184 countries

# Aggregated Results per country 

# cases 
cases_per_country <- df %>% 
  group_by(Country_Region) %>% 
  summarise(cases_max  = max(ConfirmedCases, na.rm = T),
            cases_min  = min(ConfirmedCases, na.rm = T),
            cases_sum  = sum(ConfirmedCases, na.rm = T),
            cases_mean = mean(ConfirmedCases,  na.rm = T),
            count   = n()) 
cases_per_country

# fatality 
fatality_per_country <- df %>% 
  group_by(Country_Region) %>% 
  summarise(deaths_max  = max(Fatalities, na.rm = T),
            deaths_min  = min(Fatalities, na.rm = T),
            deaths_sum  = sum(Fatalities, na.rm = T),
            deaths_mean = mean(Fatalities,  na.rm = T),
            count   = n()) %>%
            arrange(desc(deaths_max))
fatality_per_country

#plots per country

ggplot(data=fatality_per_country[c(1:50),], aes(x=deaths_sum, y=Country_Region)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()

ggplot(data=fatality_per_country[c(51:100),], aes(x=deaths_sum, y=Country_Region)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()

# Aggregated Results per date 
# cases 
cases_per_date <- df %>% 
  group_by(Date) %>% 
  summarise(cases_max  = max(ConfirmedCases, na.rm = T),
            cases_min  = min(ConfirmedCases, na.rm = T),
            cases_sum  = sum(ConfirmedCases, na.rm = T),
            cases_mean = mean(ConfirmedCases,  na.rm = T),
            count   = n()) 
cases_per_date

#for the first 50 days
ggplot(data=cases_per_date[c(1:50),], aes(x=Date, y=cases_sum)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()

#for the second 50 days
ggplot(data=cases_per_date[c(50:100),], aes(x=Date, y=cases_sum)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()


# fatality
fatality_per_date <- df %>% 
  group_by(Date) %>% 
  summarise(deaths_max  = max(Fatalities, na.rm = T),
            deaths_min  = min(Fatalities, na.rm = T),
            deaths_sum  = sum(Fatalities, na.rm = T),
            deaths_mean = mean(Fatalities,  na.rm = T),
            count   = n()) 
fatality_per_date

#for the first 50 days
ggplot(data=fatality_per_date[c(1:50),], aes(x=deaths_sum, y=Date)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()

# how did the number increase and decrease %-wise? 

Pct_change_Date <- df %>%
  group_by(Date) %>% 
  mutate(pct_change_cases = (ConfirmedCases/lead(ConfirmedCases) - 1),
         pct_change_deaths = (Fatalities/lead(Fatalities) - 1))

# since we have a lot of NA lets calculate average per country

Pct_change_Aggregated <- Pct_change_Date %>%
  group_by(Country_Region) %>%
  summarise(pct_change_cases_avg = mean(pct_change_cases, na.rm= TRUE),
            pct_change_deaths_avg = mean(pct_change_deaths, na.rm= TRUE))


#plot the developement

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
 # TODO: Pick countries via Shiny
countries <- c("Spain", "Germany", "China", "US")
fig(20,20)
par(mfrow = c(ceiling(length(countries) / 2),2))

# Plot case development 
for (country in countries) {
  # intersect(): Returns rows which are in both dataframes
  # which(): Returns index where statement is true
  country_this <- intersect( which( df$Country_Region == country ), which( df$Date < as.Date("2020-05-16")))
  # f$Date[country_this]: List of 
  plot(df$Date[country_this], df$ConfirmedCases[country_this],  main=country, ylim=c(0,max(df$ConfirmedCases, na.rm=TRUE)), xlab="Date", ylab="Confirmed Cases", cex.main=2, cex.lab=1.5, cex.axis=2, col=4)
}

# Plot fatality development 
for (country in countries) {
  # intersect(): Returns rows which are in both dataframes
  # which(): Returns index where statement is true
  country_this <- intersect( which( df$Country_Region == country ), which( df$Date < as.Date("2020-05-16")))
  # f$Date[country_this]: List of 
  plot(df$Date[country_this], df$Fatalities[country_this],  main=country, ylim=c(0,max(df$Fatalities, na.rm=TRUE)), xlab="Date", ylab="Fatalities", cex.main=2, cex.lab=1.5, cex.axis=2, col="red")
}



# Check for seasonality 

#sum of cases per day in each region 
ConfirmedCases_perDate <- df %>%
  group_by(Date) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases, na.rm= TRUE))

#convert to ts data 
ts_cases <- ts(ConfirmedCases_perDate$ConfirmedCases, start = decimal_date(as.Date("2020-01-22")), frequency = 365)

plot.ts(ts_cases)

log_cases <- log(ts_cases)

plot.ts(log_cases)

#smoothing time series 
#moving average of order 3
ts_casesSMA3 <- SMA(ts_cases,n=3)
plot.ts(ts_casesSMA3)

#check for weekly seasonality 
library(ggplot2)

total_per_date <- df %>%
  mutate(Date = as.Date(Date),
         weekday = weekdays(Date)) %>%
  group_by(weekday) %>%
  summarise(Cases = sum(ConfirmedCases),
            Deaths = sum(Fatalities))
na.omit(total_per_date)
# Plotting the evolution of cases and deaths through time
total_per_date$weekday <- factor(total_per_date$weekday, levels= c("Sunday", "Monday", 
                                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

total_per_date[order(total_per_date$weekday), ]
na.omit(total_per_date)
#Cases 
ggplot(data=total_per_date, aes(x=weekday, y=Cases)) +
  geom_bar(stat="identity", color = "darkgreen", fill = "darkgreen")
#Deaths per Date
ggplot(data=total_per_date, aes(x=weekday, y=Deaths)) +
  geom_bar(stat="identity", color = "darkgreen", fill = "darkgreen")

