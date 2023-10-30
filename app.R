###############################################################################
# Shiny App - Allison Jacobson
# October 30, 2023
#
# Key information of vehicle accidents in 2021
#
# Deployed at 
# GitHub
###############################################################################
#libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(dplyr)

#cleaning and sorting data 
#read in data 
accident <- read.csv("accident.csv")

drimpair <- read.csv("drimpair.csv")

vehicle <- read.csv("vehicle.csv")

weather <- read.csv("weather.csv")

#remove unneccesary columns 
accident <- accident %>% 
  select(STATE, STATENAME, ST_CASE, LATITUDE, LONGITUD, FATALS, COUNTYNAME, MONTHNAME)
vehicle <- vehicle %>%
  select(STATE, STATENAME, ST_CASE, VEH_NO, MAKENAME, MOD_YEAR)

#merge data by ST_CASE column
drimpair <- merge(accident, drimpair, by = "ST_CASE")
vehicle <- merge(accident, vehicle, by = "ST_CASE")
weather <- merge(accident, weather, by = "ST_CASE")

#select only certain car makes for sake of space/looks
keep_makes <- c("Toyota", "Ford", "Chevrolet", "Honda", "Jeep / Kaiser-Jeep / Willys- Jeep")
vehicle <- vehicle[vehicle$MAKENAME %in% keep_makes, ]

#Rename Jeep variable 
vehicle$MAKENAME <- ifelse(vehicle$MAKENAME == "Jeep / Kaiser-Jeep / Willys- Jeep", "Jeep", vehicle$MAKENAME)

#Remove not reported values 
drimpair <- subset(drimpair, DRIMPAIR != "99")
drimpair <- subset(drimpair, DRIMPAIR != "95")
drimpair <- subset(drimpair, DRIMPAIR != "98")

#shiny app
# Define UI for the dashboard application
ui <- dashboardPage(
  dashboardHeader(title = "Assignment 10"),
  dashboardSidebar(
    #set tabs for sidebar
    sidebarMenu(
      menuItem("Main", tabName = "tab1"),
      menuItem("Weather", tabName = "tab2"),
      menuItem("Make/Model", tabName = "tab3"),
      menuItem("Impaired", tabName = "tab4")
    )
  ),
  #body of tabs
  dashboardBody(
    tabItems(
      #Tab 1 - welcome page 
      tabItem(tabName = "tab1",
              h3("Vehicle Accident Data in the United States (2021)"),
              p("Please select a tab to begin.")
      ),
      #Tab 2 - weather
      tabItem(tabName = "tab2",
              h3("Vehicle Accident Data By Weather Type"),
              p("Please select a state and month(s) to begin."),
              sidebarLayout(
                sidebarPanel(
                  selectInput("State2", "Select State", choices = unique(weather$STATENAME.x)),
                  checkboxGroupInput("months", "Select Month(s)", choices = unique(weather$MONTHNAME))
                ), 
                mainPanel(
                  plotOutput("weatherPlot"),
              )
            )
      ),
      #Tab 3 - fatalities by car make
      tabItem(tabName = "tab3",
              h3("Vehicle Fatalities By Car Make"),
              p("Please select a state to begin."),
              sidebarLayout(
                sidebarPanel(
                  selectInput("State3", "Select State", choices = unique(vehicle$STATENAME.x))
                ),
                mainPanel(
                  plotOutput("barPlot")
                )
              )
      ), 
      #Tab 4 - impaired or not
      tabItem(tabName = "tab4",
              h3("Vehicle Accident Data By Impairment"),
              p("Please select a state to begin."),
              sidebarLayout(
                sidebarPanel(
                  selectInput("State4", "Select State", choices = unique(drimpair$STATENAME.x))
                ),
                mainPanel(
                  plotlyOutput("pieChart")
                
                )
          )
      )
    )
  )
)

server <- function(input, output) {
  #construct weather plot 
  output$weatherPlot <- renderPlot({
    filtered_data <- weather %>%
      filter(STATENAME.x == input$State2, MONTHNAME %in% input$months)
    
    ggplot(filtered_data, aes(x = WEATHERNAME, fill = WEATHERNAME)) +
      geom_bar() +
      labs(title = paste(input$State2),
           x = "Weather Type",
           y = "Number of Accidents")+
          theme_fivethirtyeight()+
      theme(axis.title = element_text())+
      scale_fill_brewer(palette = "Blues")
  })
  
  #construct car make plot 
  output$barPlot <- renderPlot({
  filtered_data <- vehicle[vehicle$STATENAME.x == input$State3,]
  
  if (nrow(filtered_data) == 0) {
    return(NULL)
  }
  summary_data <- filtered_data %>%
    group_by(MAKENAME) %>%
    summarise(Total_Fatalities = sum(FATALS))
  
  ggplot(summary_data, aes(x = MAKENAME, y = Total_Fatalities, fill = MAKENAME)) +
    geom_bar(stat = "identity", position = "identity") +
    labs(title = paste(input$State3),
         x = "Car Make",
         y = "Number of Fatalities") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    scale_fill_brewer(palette = "Blues")
  })
  #construct impaired plot 
  filtered_data <- reactive({
    drimpair[drimpair$STATENAME.x == input$State4, ]
  })
  
  output$pieChart <- renderPlotly({
    if (is.null(input$State4)) {
      return(NULL)
    }
    
    summary_data <- as.data.frame(table(filtered_data()$DRIMPAIRNAME))

    reversed_palette <- rev(RColorBrewer::brewer.pal(9, "Blues"))
    
    pie_chart <- plot_ly(data = summary_data, labels = ~Var1, values = ~Freq, type = 'pie') %>%
      layout(
        paper_bgcolor = '#f9f9f9',
        plot_bgcolor = "#f9f9f9", 
        colorway = reversed_palette 
      )
    
    pie_chart
  })
}

# Run the application
shinyApp(ui = ui, server = server)


