#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)

fig <- function(width, heigth){
    options(repr.plot.width = width, repr.plot.height = heigth)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    df_filtered<-reactive({
        df %>% filter(Date == input$date_input,
                      Country_Region %in% input$country_input)
    })
    
    df_cases_date<-reactive({ 
        cases_per_date <- df %>% 
            filter(Country_Region %in% input$country_input_2,
                   Date >= input$date_range_input[1],
                   Date <= input$date_range_input[2]) %>%
            mutate(Date = as.Date(Date)) %>%
            group_by(Date) %>% 
            summarise(cases_mean = mean(ConfirmedCases,  na.rm = T),
                      fatalities_mean = mean(Fatalities,  na.rm = T))
    })
    
    df_cases_comp<-reactive({
        cases_comp <- df %>% 
            filter(Country_Region %in% input$country_comp,
                   Date >= input$date_range_input_comp[1],
                   Date <= input$date_range_input_comp[2]) %>%
            mutate(Date = as.Date(Date)) %>%
            group_by(Date) %>% 
            group_by(Date, Country_Region) %>%
            summarize(cases_total = sum(ConfirmedCases))
    })
    
    
    df_fatalities_comp<-reactive({
        fatalities_comp <- df %>% 
            filter(Country_Region %in% input$country_comp,
                   Date >= input$date_range_input_comp[1],
                   Date <= input$date_range_input_comp[2]) %>%
            mutate(Date = as.Date(Date)) %>%
            group_by(Date) %>% 
            group_by(Date, Country_Region) %>%
            summarize(fatalities_total = sum(Fatalities))
    })
    
    df_map_cases_func <-reactive({
        df_map_cases %>% filter(day == input$integer)
    })
    
    
    output$filteredTable <- DT::renderDataTable({
        data.table(
            df_filtered()
        )
    })
    
    output$plot_1<-renderPlotly({
            ggplot(df_cases_date()) +
                geom_line(aes(x=Date,y=cases_mean), colour='darkblue') + 
                geom_line(aes(x=Date,y=fatalities_mean), colour='red') +
                scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                         date_labels = "%B") +
                theme_minimal()
    })
    
    

    output$cases_comp<-renderPlotly({
        ggplot(df_cases_comp(), aes(x=Date, y=cases_total, group=Country_Region, color=Country_Region)) + geom_line() + theme_minimal()
    })
    
    
    output$fatalities_comp<-renderPlotly({
        ggplot(df_fatalities_comp(), aes(x=Date, y=fatalities_total, group=Country_Region, color=Country_Region)) + geom_line() + theme_minimal()
    })
    
    
    output$health_system_map <- renderLeaflet({
        leaflet(data = df_map_health) %>%
            addTiles() %>%
            addPolygons(color = "black",
                        fillColor = ~pal_gdp(exp_gdp),
                        smoothFactor = 0.2,
                        fillOpacity = 0.8,
                        popup =paste("Country:", df_map_health$ADMIN, "<br>", 
                                     "Expenditure in %:", df_map_health$exp_gdp)
            )
    })
    
    output$health_system_map_2 <- renderLeaflet({
        leaflet(data = df_map_health) %>%
            addTiles()

    })
    
    observe({
        leafletProxy("health_system_map_2", data = df_map_cases_func()) %>%
            clearMarkers() %>%
            addCircleMarkers(lng = ~long,
                             lat = ~lat,
                             radius = ~cases_total / 10000)
        
    })
    
    df_vbox<-reactive({
        df_vbox_2 <- df %>% 
            filter(Country_Region %in% input$country_input_2,
                   Date >= input$date_range_input[1],
                   Date <= input$date_range_input[2])

    })
    

    output$vbox1 <- renderValueBox({
        valueBox(
            value = paste(format(sum(df_vbox()$ConfirmedCases), big.mark = ","), "", sep = " "),
            subtitle = "Total Cases", icon("cog", lib = "glyphicon"),
            color = "purple")
    })
    
    output$vbox2 <- renderValueBox({
        valueBox(
           value = paste(format(sum(df_vbox()$Fatalities, na.rm = TRUE), big.mark = ","), " (",
                         round(100 * sum(df_vbox()$Fatalities, na.rm = TRUE) / sum(df_vbox()$ConfirmedCases), 1),
                          "%)",
                         sep = ""
            ),
            subtitle = "deaths in total vs %", icon("stats",lib='glyphicon')
       )
    })
    
})


