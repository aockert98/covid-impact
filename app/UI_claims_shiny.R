## Objective: Create Shiny web app for interactive datatable and plot of UI Claims
## Note: Experimental mode--currently redundant, just playing
##       around with some options

# Load libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Note: Using df_industry4 data from UI_claims_2.R

ui <- fluidPage(
  titlePanel("UI Claims 2005-2020"),
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year","Select Year", min = 2005, max = 2020,
                  value = 2020, sep = ""), # sep takes the comma out
      ## different selecting choices--still playing around with these
      selectInput("industry","Pick Industry",
                  choices = c("construction", "manufacturing",
                              "information",
                              multiple = TRUE,
                              names(ui_industry$industry))),
      multiInput("industry2", "Select Industries:",
                 choices = c("construction","manufacturing",
                             "information"),
                 selected = "construction"),
      awesomeCheckboxGroup(
        inputId = "industry3",
        label = "Select Industries:", 
        choices = c("construction", "manufacturing", "information"),
        selected = "construction"
      ),
      textOutput("choice")),
    #Trying out input for dumbbell plot
   # selectInput("xstart", "From:", choices = c("year2006","year2008")),
    #selectInput("xend", "To:", choices = c("year2018","year2019")),
    # Date range input
    #dateRangeInput("dates", "Date Range:",
     #              start = "2005-01-01",
      #             end   = "2020-12-20"),
      
    mainPanel(tabsetPanel(
              tabPanel("table",DT::DTOutput("UI_table")),
              tabPanel("plot", plotOutput("plot1")),
              tabPanel("Other plot", plotOutput("plot2"))
              #tabPanel("Dumbbell plot", plotOutput("plotd"))
              ))))
              
  
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
    ui_industry %>%
      filter(Industry == input$industry)
  })
  output$plot1 <- renderPlot({
    ui_industry %>%
      dplyr::filter(year == input$year,
                    industry == input$industry3) %>%
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
  output$plot2 <- renderPlot({
    ui_industry %>%
      dplyr::filter(year > input$year,
                    industry == input$industry3) %>%
      ggplot(aes(new_claim_date, claims, color = industry)) + 
      geom_path() +
      theme_light()
  })
  output$plotd <- renderPlot({
    ui_dumb3 %>%
      #filter(industry != "Self Employed") %>%
      ggplot(aes(x = input$xstart, xend = input$xend, y = industry, group = industry)) +
      geom_dumbbell(color = "darkgray",
                    colour_x = "darkgray",
                    colour_xend="black") +
      labs(x = "UI Claims Filed",
           y = "Industry",
           title = "The Economic Impact of the Coronavirus Pandemic in Connecticut",
           subtitle = "Comparing Unemployment Insurance Claims Filed in 2013 vs. 2020", 
           caption = "Source: (fill in)") +
      theme_minimal() +
      theme(plot.margin = unit(c(1.5,3,1,0), "cm"))
  })
  
}

shinyApp(ui = ui, server = server)
