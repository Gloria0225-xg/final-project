library("dplyr")
library("ggplot2")
library("shiny")
library("plotly")
library("bslib")
#####
#sky
adi_df <- read.csv("adidas_usa.csv", stringsAsFactors = FALSE)
count_data <- adi_df %>% 
  count(category, color) %>% 
  rename(Category = category, Color = color, Count = n)

#Steven
new_df <- adi_df %>% 
  group_by(category, color) %>% 
  summarise(Ave_price = mean(selling_price))


#Frank
count_price_data <- adi_df %>% 
  count(category, selling_price) %>% 
  rename(Category = category, Price = selling_price, Count = n)
#sky
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "selectCategory", choices = c("All", unique(count_data$Category)), selected = "All")
    updateSliderInput(session, "countRange", min = 0, max = 300, value = c(0, 300))
  })
  
  output$barPlot <- renderPlotly({
    if(input$selectCategory == "All") {
      filtered_data <- count_data %>% filter(Count >= input$countRange[1] & Count <= input$countRange[2])
    } else {
      filtered_data <- count_data %>%
        filter(Category == input$selectCategory, Count >= input$countRange[1] & Count <= input$countRange[2])
    }
    
    p <- ggplot(filtered_data, aes(x = Color, y = Count, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Distribution of Colors for", input$selectCategory),
           x = "Color", y = "Number",
           fill = "Category")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  #Frank
  output$pricePlot <- renderPlotly({
    
    filtered_df <- count_price_data %>%
      filter(Category %in% input$CategorySelection) %>%
      filter(Price >= input$PriceSelection[1] & Price <= input$PriceSelection[2])
    
    # Line plot
    price_plot <- ggplot(data = filtered_df) +
                  geom_line(mapping =
                              aes(x = Price,
                                  y = Count,
                                  color = Category))
                          
    
    
    price_plotly <- ggplotly(price_plot)
    
    return(price_plotly)
    
  })
  
  #Steven
  output$colorPlot <- renderPlotly({
    
    steven_df <- new_df %>% 
      filter(category == input$CategorySelection2) %>%
      filter(Ave_price >= input$PriceSelection2[1] & Ave_price <= input$PriceSelection2[2])
    
    color_plot <- ggplot(data = steven_df) +
                  geom_col(mapping = 
                             aes(x = color,
                                 y = Ave_price)) +
                  labs(title = paste("Average Price for different color in: ", input$CategorySelection2),
                       x = "Colors",
                       y = "Average Price",
                       fill = "blue")
          
    
    color_plotly <- ggplotly(color_plot)
    
    return(color_plotly)
    
  })
  
}
#####


