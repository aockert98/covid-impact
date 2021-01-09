## Create functions to generate plots in shinydashboard server

generate_dumbbell_ggplot <- function(data, industry) {
  
  data %>%
    filter(industry %in% industry) %>%
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
}

generate_plotly <- function(data){
  plot_ly(data, color = I("gray80")) %>% 
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
}

