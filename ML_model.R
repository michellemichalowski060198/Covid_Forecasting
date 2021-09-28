#############################
######ML model ##############
#############################

#load libraries
library(tidyverse)

#load data sets
train <- read.csv("/Users/michellemichalowski/Downloads/covid19-global-forecasting-week-4/train.csv",
                  stringsAsFactors = F,
                  na.strings = "") %>%
  mutate(Province_State = replace_na(Province_State, "none")) %>%
  mutate(Date=as.Date(Date))

test <- read.csv("/Users/michellemichalowski/Downloads/covid19-global-forecasting-week-4/test.csv",
                 stringsAsFactors = F,
                 na.strings = "") %>%
  mutate(Province_State = replace_na(Province_State, "none")) %>%
  mutate(Date=as.Date(Date))

submission <- read.csv("/Users/michellemichalowski/Downloads/covid19-global-forecasting-week-4/submission.csv",
                       stringsAsFactors = F,
                       na.strings = "")

#create unique identifier per area for train

infected_per_day <- train %>%
  group_by(Country_Region, Province_State) %>%filter (Date < "2020-04-02") %>%ungroup()%>%
  unite(Area, Country_Region, Province_State, remove = FALSE, na.rm = TRUE) %>%
  mutate(weekday = weekdays(Date),
         week = strftime(Date, format = "%V"))

#mutate also the test set 

test_grouped <-test %>%
  group_by(Country_Region, Province_State) %>%
  mutate(days = difftime(max(train[["Date"]], na.rm = TRUE),
                         min(train[["Date"]], na.rm = TRUE),
                         units = "days")) %>% ungroup() %>%
  select(ForecastId, Date, Country_Region, Province_State) %>%
  unite(Area, Country_Region, Province_State, remove = FALSE, na.rm = TRUE) %>%
  mutate(weekday = weekdays(Date),
         week = strftime(Date, format = "%V"))

#convert date 
test_grouped$Date <- as.Date(test_grouped$Date, "%Y-%m-%d")


#merge with external data 

#misc data 
df_weather <- read.csv("/Users/michellemichalowski/Desktop/R/Covid19_Forecasting-/data/weather_info.csv", 
                       header = TRUE,
                       na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                       sep = ",")

df_weather <- df_weather %>%
  mutate(Date=as.Date(Date))

colnames(df_weather)[colnames(df_weather) == 'Country.Region'] <- 'Country_Region'

train <- merge(x = infected_per_day, y = df_weather, by = c("Country_Region", "Date"), all.x = TRUE)
test <- merge(x = test_grouped, y = df_weather, by = c("Country_Region", "Date"), all.x = TRUE)
train <- left_join(infected_per_day,df_weather)
test <- left_join(test_grouped,df_weather)


lapply(train,class)
lapply(test,class)

library(randomForest)

train$week <- as.numeric(train$week)
test$week <- as.numeric(test$week)

colSums(is.na(train))

train$temp[is.na(train$temp)] <- mean(train$temp, na.rm = TRUE)
train$day_from_jan_first[is.na(train$day_from_jan_first)] <- mean(train$day_from_jan_first, na.rm = TRUE)

test$temp[is.na(test$temp)] <- mean(test$temp, na.rm = TRUE)
test$day_from_jan_first[is.na(test$day_from_jan_first)] <- mean(test$day_from_jan_first, na.rm = TRUE)

predicitions_Cases <- train %>%
  group_nest(Area) %>% 
  mutate(model = map(data,
                     ~randomForest(ConfirmedCases ~ weekday + week,
                                   data = .x))
  ) %>% 
  left_join(test %>% group_nest(Area), by = "Area") %>% 
  mutate(fitted = map2(model,
                       data.y,
                       ~predict(.x, newdata = .y ))) %>% 
  unnest_longer("fitted") %>% 
  select(Area, fitted)

predicitions_Fatalities <- train %>%
  group_nest(Area) %>% 
  mutate(model = map(data,
                     ~randomForest(Fatalities ~ weekday + week,
                                   data = .x))
  ) %>% 
  left_join(test %>% group_nest(Area), by = "Area") %>% 
  mutate(fitted = map2(model,
                       data.y,
                       ~predict(.x, newdata = .y))) %>% 
  unnest_longer("fitted") %>% 
  select(Area, fitted)

head(submission)

submission$ConfirmedCases <- predicitions_Cases$fitted
submission$Fatalities <- predicitions_Fatalities$fitted

head(submission)

