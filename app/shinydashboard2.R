library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
# Grab the UI claims data 
#source("covid-impact/transformation/UI_claims_data.R")

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
              tabPanel("Dumbbell Plot", plotOutput("plot")),
              tabPanel("Interactive Dumbbell Plot", plotlyOutput("plotlyplot")),
              tabPanel("Line Graph")
            ))),
    tabItem(tabName = "cases",
            "Explore COVID Cases throughout CT",
            plotOutput("covid_town"))))


ui <- dashboardPage(header, sidebar, body, skin = "black")


server <- function(input, output) {
  output$plot <- renderPlot({
    ui_dumb2 %>%
      filter(industry != "Self Employed") %>%
      ggplot2::ggplot(aes(x = year2013/1000, xend = year2020/1000, y = industry, group = industry)) +
      ggalt::geom_dumbbell(color = "darkgray",
                           colour_x = "darkgray",
                           colour_xend="black") +
      ggplot2::labs(x = "UI Claims Filed (in thousands)",
                    y = "",
                    title = "The Economic Impact of the Coronavirus Pandemic in Connecticut",
                    subtitle = "Comparing Unemployment Insurance Claims Filed in 2013 vs. 2020", 
                    caption = "Source: (fill in)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.margin = unit(c(1.5,3,1,0), "cm")) +
      #geom_rect(aes(xmin = 100, xmax = 120, ymin = -Inf, ymax = Inf), fill = "grey") +
      geom_text(aes(label=paste0(ifelse(delta > 0, "+" , ""), round(delta), "%"), y = industry, x = year2020/1000, color = ifelse(delta > 0, "green", "red")), hjust = -0.5, 
                size = 3) +
      # geom_text(data = filter(ui_dumb, industry=="Wholesale Trade"),
      #           aes(x = 90, y = industry, label = "Percent Change"), vjust = -1, hjust = 0.5) +
      scale_y_discrete(expand=c(0.15,0)) + 
      ggplot2::expand_limits(x = c(-2, 90)) + 
      ggplot2::theme(legend.position = "none")
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
  library(plotly)
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