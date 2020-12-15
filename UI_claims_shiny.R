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
      
    mainPanel(tabsetPanel(
              tabPanel("table",DT::DTOutput("UI_table")),
              tabPanel("plot", plotOutput("plot1"))))))
              
  
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
  output$plot1 <- renderPlot({
    df_industry2 %>%
      dplyr::filter(year == input$year) %>%
      ggplot2::ggplot(aes(new_claim_date, claims, color = industry)) +
      ggplot2::geom_path() +
      labs(title = "COVID-19 and Connecticut's Economy", subtitle = "UI Claims by Month, 2005-2020",
           fill = "Total Claims") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 16),
            axis.text.x = element_text(angle = 45))
  })
  
}

shinyApp(ui = ui, server = server)
