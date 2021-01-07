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
            fluidRow(
              box(
                width = 12,
                title = "About our project"
              )
            )),
    tabItem(tabName = "data",
            "Learn about our data!",
            box(
              width = 12,
              title = "Interactive Data Table",
              DT::DTOutput("UI_table")
            )),
    tabItem(tabName = "economic",
            "Explore the Economic Impact of COVID",
            plotOutput("plot")),
    tabItem(tabName = "cases",
            "Explore COVID Cases throughout CT")))
  
  #tabBox(
   # title = "Options",
    #tabPanel("Plot1", "Bar"),
    #tabPanel("Plot2", "Second"),
    #tabPanel("Plot3", plotOutput("plot"))))


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
}
shiny::shinyApp(ui, server)