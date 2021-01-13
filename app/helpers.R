## Create functions to generate plots in shinydashboard server

myFunc1 <- function(x) {
  year <- paste0("year",x)
  
  ui_dumb3 <- ui_dumb2 %>%
    select(year2 = year, year2020, industry) %>%
    mutate(d = year2 - year2020)
  
  ui_dumb3 %>%
    ggplot(aes(industry, year2)) +
    geom_col()
}

myFunc1(2012)

generate_dumbbell_ggplot2 <- function(data, year) {
  
  year <- paste0("year",year)
  ui_dumb3 <- ui_dumb2 %>%
    select(year2 = year, year2020, industry) %>%
    mutate(delta = (year2020-year2)/year2 * 100)
  
  ui_dumb3 %>%
    ggplot2::ggplot(aes(x = year2/1000, xend = year2020/1000, y = industry, group = industry)) +
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
  plot_ly(data, color = I("gray80"),
          hoverinfo = 'text',
          text = ~paste('</br><b>Industry:</b> ', industry,
                        '</br><b>2013 claims:</b> ', `2013`,
                        '<br><b>2020 claims:</b> ', `2020`,
                        '<br><b>Percent Change:</b> ', paste0(round(delta,1),"%"))) %>% 
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

generate_map <- function(data){
  cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                      "#ea4b24","#eb411f","#eb371a","#eb220f",
                                      "#c11108","#960000"))
  
  data %>%
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
  
}
generate_map(mid_dec)