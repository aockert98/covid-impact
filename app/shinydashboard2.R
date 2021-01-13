library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinydashboardPlus)

# Import Data ----

# UI Claims Data
source(
  here::here("transformation/UI_claims_data.R")
)

# Covid Data by CT Town
source(
  here::here("transformation/covid_town.R")
)

# Set a color palette
cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                    "#ea4b24","#eb411f","#eb371a","#eb220f",
                                    "#c11108","#960000"))

# Source custom helper functions
source(
  here::here("app/helpers.R")
)


# Build UI
header <- dashboardHeader(
  title = "COVID in CT"
)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Welcome", 
             tabName = "Welcome",
             icon = icon("dashboard")),
    menuItem("Data",
             tabName = "data", 
             icon = icon("filter")),
    menuItem("Economic Impact",
             tabName = "economic",
             icon = icon("chart-line")),
    menuItem("Case Explorer",
             tabName = "cases",
             icon = icon("hospital"))
  )
)

body <- dashboardBody(

  tabItems(
    ## Welcome tab for project background info
    tabItem(tabName = "Welcome",
            "Welcome to our project!",
              shinydashboardPlus::descriptionBlock(
                header = "About our project",
                text = "Welcome to our COVID-19 Explore page.\n
                We are seeking to explore the economic and medical impact\n
                of the coronavirus pandemic on the state of Connecticut."
                ),
            box(
              title = "About our Project",
              status = "primary",
              width = NULL,
              shinydashboardPlus::userPost(
                "Welcome to our COVID-19 explore page!
                We are seeking to explore the economic and
                medical impact the coronavirus pandemic has had
                on the state of Connecticut."
              )
            ),
            
            ## Create "About Author" type section
            shinydashboardPlus::widgetUserBox(
              title = "Amelia Ockert",
              subtitle = "Student",
              type = NULL,
              src = "https://image.isu.pub/171215165947-27b595478b4ba69b06b7a7d35d3a40c7/jpg/page_1_thumb_large.jpg",
              color = "blue",
              closable = TRUE,
              footer = "Student at Dartmouth, interested in Data Viz"
            )
            ),
    
    ## Data tab to explore interactive data table
    tabItem(tabName = "data",
            "Explore our data!",
            fluidRow(
              box(width = 12,
                  "Explore our Data")
            ),
            fluidRow(
              box(
                width = 12,
                title = "Interactive Data Table",
                DT::DTOutput("UI_table")
              ))
            ),
    
    ## Tab to explore the economic-related plots
    tabItem(tabName = "economic",
            fluidRow(
              "Explore the Economic Impact of COVID-19 in CT"
            ),
            fluidRow(
            #"Explore the Economic Impact of COVID",
            tabBox( width = 12,
              title = "Plots", 
              
              tabPanel(
                "Dumbbell Plot", 
                
                shiny::fluidRow(
                  
                  # Set static dumbbell ggplot inputs
                  shiny::column(
                    width = 3,
                    shiny::sliderInput(
                      inputId = "year",
                      label = "Select Year",
                      min = 2005,
                      max = 2020, 
                      value = 2008,
                      sep = "")
                  ), 
                  
                  shiny::column(
                    width = 9, 
                    plotOutput("plot")
                  ),
                )
              ),
              
              tabPanel("Interactive Dumbbell Plot", plotlyOutput("plotlyplot")),
              tabPanel("Line Graph")
            ))),
    tabItem(tabName = "cases",
            "Explore COVID Cases throughout CT",
            plotOutput("covid_town"))))


ui <- dashboardPage(header, sidebar, body, skin = "black")


# Build Server
server <- function(input, output) {
  
  ## Static dumbbell plot, makes call to our "helpers.R" functions file
  output$plot <- renderPlot({

    generate_dumbbell_ggplot2(data = ui_dumb2, year = input$year)
    
  })
  
  
  
  output$UI_table <- DT::renderDT({
    ui_dt %>% 
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        #colnames = c("Total" = "total_month"),
        extensions = "Buttons",
        options = list(dom = "Bfrtip",
                       buttons = c("csv","excel","pdf")))
  })
  
  output$plotlyplot <- renderPlotly({
    
    generate_plotly(data = ui_dumb)
    
    })
  
  output$covid_town <- renderPlot( {
    
    generate_map(data = mid_dec)
    
  })
}
shiny::shinyApp(ui, server)