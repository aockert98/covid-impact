library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)

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
             tabName = "economic"),
    menuItem("Case Explorer",
             tabName = "cases")
  )
)

body <- dashboardBody(

  tabItems(
    tabItem(tabName = "Welcome",
            "Welcome to our project!",
            #fluidRow(
              box(
                width = 12,
                title = "About our project"
              )),
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
                    width = 4, 
                    # shinyWidgets::multiInput(
                    shinyWidgets::pickerInput(
                      inputId = "dumbbell_industries", 
                      label = "Select Industries:",
                      choices = c(ui_dumb2$industry),
                      selected = c(ui_dumb2$industry), 
                      multiple = TRUE
                    )
                  ), 
                  
                  shiny::column(
                    width = 8, 
                    plotOutput("plot")
                  )
                  
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
  
  output$plot <- renderPlot({
    
    generate_dumbbell_ggplot(data = ui_dumb2, industry = input$dumbbell_industries)
    
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
  
  output$plotlyplot <- renderPlotly( {
  ui_int <- plot_ly(ui_dumb, color = I("gray80")) %>% 
    add_segments(x = ~`2013`, xend = ~`2020`,
                 y = ~industry, yend = ~industry, 
                 showlegend = FALSE) %>%
    add_markers(x = ~`2013`, y = ~industry, 
                name = "2013", color = I("gray80")) %>%
    add_markers(x = ~`2020`, y = ~industry, 
                name = "2020", color = I("black")) %>%
    layout(
      title = "UI Claims Filed in 2013 versus 2020",
      xaxis = list(title = "UI Claims Filed (in thousands)"), 
      yaxis = list(title = ""), 
      margin = list(l = 65))
  
  ui_int })
  
  output$covid_town <- renderPlot( {
    mid_dec %>%
      ggplot() +
      geom_sf() +
      geom_sf(aes(fill = per100k)) +
      labs(title = "The Coronavirus in Connecticut Towns",
           subtitle = "Cases per 100,000",
           fill = "") +
      scale_fill_gradientn(colors = cols(10)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
}
shiny::shinyApp(ui, server)