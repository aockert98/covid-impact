library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
# Grab the UI claims data 
#source("transformation/UI_claims_data.R")

header <- dashboardHeader(
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Amelia",
                 message = "Hello there!"
               )
)
)
 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", 
             tabName = "Welcome",
             icon = icon("dashboard")),
    menuItem("Data",
             tabName = "data", 
             icon = icon("filter"))
  )
)

body <- dashboardBody(
  plotOutput("plot"),
  tabItems(
    tabItem(tabName = "Welcome",
           "Welcome to our project!"),
    tabItem(tabName = "data",
            "Learn about our data!")),
   tabBox(
      title = "Options",
      tabPanel("Plot1", "Bar"),
      tabPanel("Plot2", "Second")))


ui <- dashboardPage(header, sidebar, body, skin = "black")
server <- function(input, output) {
  output$plot <- renderPlot({
    ui_dumb %>%
      filter(industry != "Self Employed") %>%
      ggplot2::ggplot(aes(x = `2013`/1000, xend = `2020`/1000, y = industry, group = industry)) +
      ggalt::geom_dumbbell(color = "darkgray",
                           colour_x = "darkgray",
                           colour_xend="black") +
      ggplot2::labs(x = "UI Claims Filed",
                    title = "The Economic Impact of the Coronavirus Pandemic in Connecticut",
                    subtitle = "Comparing Unemployment Insurance Claims Filed in 2013 vs. 2020", 
                    caption = "Source: (fill in)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.margin = unit(c(1.5,5,1,1), "cm")) +
      #geom_rect(aes(xmin = 100, xmax = 120, ymin = -Inf, ymax = Inf), fill = "grey") +
      geom_text(aes(label=paste0(round(delta), "%"), y = industry, x = `2020`/1000), hjust = -0.5, 
                size = 3) +
      ggplot2::geom_text(data = filter(ui_dumb, industry=="Wholesale Trade"),
                aes(x = 70, y = industry, label = "Percent Change"), vjust = -1, hjust = 0.5) +
      ggplot2::scale_y_discrete(expand=c(0.15,0)) +
      ggplot2::theme(axis.title.y = element_blank())
  })
}
shiny::shinyApp(ui, server)
