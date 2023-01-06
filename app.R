# load packages
library(markdown)
library(shiny)
library(tidyverse)
library(nycflights13)

#load calc_average_delay func
source("R/function.R")

# Define UI for application that draws a histogram
ui = navbarPage(
  title="Flight Delay",
  tabPanel(
    title="Input/Visualization",
    titlePanel(title="NYC Flight 2013 Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId= "month",
          label="Month",
          choices=sort(unique(flights$month)),
          selected=12),
        selectInput(
          inputId= "origin",
          label="Origin",
          choices=sort(unique(flights$origin)),
          selected = "JFK"),
        selectInput(
          inputId= "dest",
          label="Destination",
          choices=sort(unique(flights$dest)),
          selected="ORD"),
        checkboxInput(
          inputId= "airports",
          label="Filter Table to all NYC origin airports",
          value=FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  tabPanel(title="Table",  dataTableOutput("table")),
  tabPanel(title="About", includeMarkdown("about.Rmd"))
)

# Define server logic required to draw a histogram
server  = function(input, output) {
  
  flights_month = reactive({
    flights |>
      filter(month == input$month)
  })
  
  observeEvent(
    eventExpr = input$month,
    handlerExpr = {
      updateSelectInput(inputId = "origin",
                        choices = sort(unique(flights_month()$origin)),
                        selected = "JFK")
      updateSelectInput(inputId = "dest",
                        choices = sort(unique(flights_month()$dest)),
                        selected = "ORD")
    }
  )
  
  output$plot <- renderPlot({
    flights |>
      filter(month == input$month) |>
      filter(origin == input$origin) |>
      filter(dest== input$dest) |>
      group_by(day) |>
      calc_delay() |>
      ggplot() +
      aes(x = day, y = delay, fill=delay) |>
      geom_bar(stat="identity") +
      theme_bw()
  })
  
  output$table <- renderDataTable({
    tab = flights_month() |>
      filter(dest== input$dest)
    if (!(input$airports)){
      tab = tab |>
        filter(origin == input$origin)
    }
    
    tab
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
