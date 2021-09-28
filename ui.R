#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    setBackgroundColor(
        color = "ghostwhite",
        gradient = c("linear", "radial"),
        direction = c("bottom", "top", "right", "left"),
        shinydashboard = FALSE
    ),

    # Application title
    HTML("<br>"),
    titlePanel("Covid Development Shiny App"),
    HTML("<br><br>"),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Data Table ", h3("Data overview"),
                         HTML("<br>"),
                         sidebarPanel(h3("Please filter"),
                                          dateInput("date_input",
                                                    "Select Date:",
                                                    value = max(df$Date),
                                                    min =  min(df$Date),
                                                    max = max(df$Date)),
                                          selectInput("country_input",
                                                      "Select Country:",
                                                      choices = unique(df$Country_Region),
                                                      selected  = "Germany",
                                                      multiple = TRUE),
                                          sliderInput("cases_input",
                                                      "Select range of cases per day:",
                                                      min = min(df$ConfirmedCases),
                                                      max = max(df$ConfirmedCases),
                                                      value = c(min(df$ConfirmedCases), max(df$ConfirmedCases)))),
                         mainPanel(
                         h3("The underlying data"),
                         DT::dataTableOutput('filteredTable', height = "500px"))),
                tabPanel("Cases Aggregated", h3("Cases/Fatalities aggregated per Date"), 
                         
                         HTML("<br>"),
                         sidebarPanel(h3("Please filter"),
                                        selectInput("country_input_2",
                                                   "Select Country:",
                                                   choices = unique(df$Country_Region),
                                                   selected  = "Germany",
                                                   multiple = FALSE),
                                        dateRangeInput("date_range_input",
                                                       "Select date range input:",
                                                       start = min(df$Date),
                                                       end = max(df$Date),
                                                       min = min(df$Date),
                                                       max = max(df$Date)
                                                       )),
                         mainPanel(fluidRow(
                             shinydashboard::valueBoxOutput("vbox1"),
                             shinydashboard::valueBoxOutput("vbox2")),
                             plotlyOutput("plot_1"))
            ),
            tabPanel("Country Comparison", h3("Cases Comparison"),
                     HTML("<br>"),
                     sidebarPanel(h3("Please filter"),
                         selectInput("country_comp",
                                     "Select countries:",
                                     choices = unique(df$Country_Region),
                                     selected  = "Germany",
                                     multiple = TRUE
                         ),
                         dateRangeInput("date_range_input_comp",
                                        "Select date range input:",
                                        start = min(df$Date),
                                        end = max(df$Date),
                                        min = min(df$Date),
                                        max = max(df$Date)
                         )
                     ),
                     mainPanel(fluidRow(
                         h3("Cases"),
                         plotlyOutput("cases_comp"),
                         h3("Fatalities"),
                         plotlyOutput("fatalities_comp")
                     ))
            ),
            tabPanel("World Health Expenditures", h3("Percentage Health System Expenditures on GDP"),
                     HTML("<br>"),
                     mainPanel(fluidRow(
                         h3("World Map"),
                         "This world map describes the evel of current health expenditure expressed as a percentage of GDP. Estimates of current health expenditures include healthcare goods and services consumed during each year. This indicator does not include capital health expenditures such as buildings, machinery, IT and stocks of vaccines for emergency or outbreaks.",
                         leafletOutput("health_system_map")
                     ))
            ),
            tabPanel("Hot Spot Development",h3("Hot Spot Development"),
            mainPanel(fluidRow(
                "This map shows the hot spot development over time.",
                HTML("<br>"),
                sliderInput("integer", 
                            "Days since first reported case as of January 22, 2020:",
                            ticks = FALSE, 
                            min = min(df_map_cases$day), 
                            max = max(df_map_cases$day), 
                            value = 2, 
                            step = 1,
                            animate = animationOptions(interval = 1000, loop = TRUE)
                ),
                leafletOutput("health_system_map_2")
            ))
            )
        )
    )
)
)

