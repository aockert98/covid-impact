## Objective: Create Shiny web app for interactive datatable and plot of UI Claims
## Note: Experimental mode--currently redundant, just playing
##       around with some options

# Load libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Grab the UI claims data 
#source("transformation/UI_claims_data.R")

ui <- fluidPage(
  titlePanel("UI Claims 2005-2020"),
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year","Select Year", min = 2005, max = 2020,
                  value = 2020, sep = ""), # sep takes the comma out
      ## different selecting choices--still playing around with these
      multiInput("industry2", "Select Industries:",
                 choices = c("construction","manufacturing",
                             "information"),
                 selected = "construction"),
      varSelectInput("vars","Comparing 2020 and:", ui_dumb2,
                     selected = "year2013"),
      awesomeCheckboxGroup(
        inputId = "industry3",
        label = "Select Industries:", 
        choices = c("construction", "manufacturing", "information"),
        selected = "construction"
      )),
    
    mainPanel(tabsetPanel(
      tabPanel("table",DT::DTOutput("UI_table")),
      tabPanel("plot", plotOutput("plot1")),
      tabPanel("Other plot", plotOutput("plot2")),
      tabPanel("Dumbbell plot", plotOutput("plotd"))
    ))))



server <- function(input, output) {
 
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
    ui_dumb2 %>%
      filter(industry != "Self Employed") %>%
      #ggplot2::ggplot(aes(x = year2013/1000, xend = year2020/1000, y = industry, group = industry)) +
      ggplot2::ggplot(aes(x = input$vars, xend = year2020/1000, y = industry, group = industry)) +
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
  
}

shinyApp(ui = ui, server = server)