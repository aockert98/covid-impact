## Objective: Create Shiny web app for interactive datatable and plot of UI Claims
## Note: Experimental mode--currently redundant, just playing
##       around with some options

# Load libraries
library(shiny)
library(shinythemes)

# Note: Using df_industry4 data from UI_claims_2.R

ui <- fluidPage(
  titlePanel("UI Claims 2005-2020"),
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year","Select Year", min = 2005, max = 2020,
                  value = 2020),
      selectInput("industry","Pick Industry",
                  choices = c("Construction", "Real Estate")),
      textOutput("choice")),
      
    mainPanel(DT::DTOutput("UI_table"))))
  
  #sliderInput("year","Select Year", min = 2005, max = 2020,
   #           value = 2020),
  #selectInput("industry","Pick Industry",
   #           choices = c("Construction", "Real Estate")),
  #textOutput("choice"),
  #DT::DTOutput("UI_table")


server <- function(input, output) {
  output$choice <- renderText({
    paste("I picked", input$year)
  })
  output$UI_table <- DT::renderDT({
    df_industry4 %>%
      filter(Industry == input$industry)
  })
  
}

shinyApp(ui = ui, server = server)
