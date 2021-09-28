# Importing Libraries
library(data.table)
library(ggplot2) 
library(dplyr)
library(DataCombine)
library(TTR)
library(lubridate)
library(rgdal)
library(dplyr)
library("tidyverse")
library("forecast")
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(plotly)
library(readxl)
library(zoo)
library(Metrics)
library(purrr)
library(tibble)
library(forecast) #library with ARIMA functions
library(htmlwidgets)
library(IRdisplay)


# Load initial data
train <- read.csv("Desktop/Covid19_Forecasting--feature-implement_graph_map/data/train.csv", 
                  header = TRUE,
                  na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                  sep = ",")

train$Date <- as.Date(df$Date, "%Y-%m-%d")
test <- read.csv("Desktop/Covid19_Forecasting--feature-implement_graph_map/data/test.csv", 
                 header = TRUE,
                 na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                 sep = ",")
test$Date <- as.Date(df$Date, "%Y-%m-%d")


submission <- read.csv("Desktop/Covid19_Forecasting--feature-implement_graph_map/data/submission.csv", 
                       header = TRUE,
                       na.strings=c("","NA"), # na.strings: make sure NAs are not read as empty strings
                       sep = ",")

#creating unique identifier Area

infected_per_day <- train %>%
  group_by(Country_Region, Province_State) %>%filter (Date < "2020-04-02") %>%ungroup()%>%
unite(Area, Country_Region, Province_State, remove = FALSE, na.rm = TRUE)

test_grouped <-test %>%
  group_by(Country_Region, Province_State) %>%
  unite(Area, Country_Region, Province_State, remove = FALSE, na.rm = TRUE)

test_grouped$Date <- as.Date(test_grouped$Date, "%Y-%m-%d")

forecast_period <- difftime(max(test[["Date"]], na.rm = TRUE),
                            min(test[["Date"]], na.rm = TRUE),
                            units = "days")
models <- infected_per_day %>%
  group_by(Area) %>%
  nest() %>%
  mutate(
    model_cases =
      map(data,
          ~auto.arima(
            y = .x$ConfirmedCases
          )
      ),
    model_fatalities =
      map(data,
          ~auto.arima(
            y = .x$Fatalities
            
          )
      )
    
    
  )


forcast_cases <- models %>%
  ungroup() %>%
  mutate(ConfirmedCases  = map(model_cases,
                               ~forecast(.x, h = forecast_period+1)$mean)) %>%
  select(Area, ConfirmedCases) %>%
  unnest(ConfirmedCases) %>%
  group_by(Area)%>% mutate(Date = seq.Date(as.Date("2020-04-02"), as.Date("2020-05-14"), by = "days"))%>%
  left_join(test_grouped, by = c("Area" = "Area", "Date" = "Date"))


forecast_fatalities <- models %>%
  ungroup() %>%
  mutate(Fatalities  = map(model_fatalities,
                           ~forecast(.x, h = forecast_period+1)$mean)) %>%
  select(Area, Fatalities) %>%
  unnest(Fatalities)%>%
  group_by(Area)%>% mutate(Date = seq.Date(as.Date("2020-04-02"), as.Date("2020-05-14"), by = "days"))%>%
  left_join(test_grouped, by = c("Area" = "Area", "Date" = "Date")) %>% transmute(Fatalities = round(Fatalities)) 

all_forecast <- forcast_cases  %>% left_join(forecast_fatalities) %>% select("ForecastId","Country_Region","Province_State",
                                                                             "ConfirmedCases","Fatalities")

all_forecast_sub <- all_forecast %>%
                      ungroup() %>%
                      select(ForecastId, ConfirmedCases, Fatalities) 

write.csv(all_forecast_sub,"Desktop/sub1.csv", row.names = FALSE)


