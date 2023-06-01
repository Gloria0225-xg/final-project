library("dplyr")
library("plotly")
library("ggplot2")
library("shiny")
library("bslib")
adi_df <- read.csv("adidas_usa.csv", stringsAsFactors = FALSE)
my_theme <- bs_theme(
  bg = "#550FEC",
  fg = "white",
  primary = "#0dcaf0")

####
#sky
ui <- navbarPage(
  theme = my_theme,
  "Adidas USA Analysis",
  tabPanel("Introduction",
           h1("How Different Factors Influence the Sales", align="center"),
           h3("Skyyang, FrankChen, StevenXu, GloriaXu", align="center"),
           includeHTML("intro.html")
  ),
  tabPanel("Category and Color",
           sidebarLayout(
             sidebarPanel(
               selectInput("selectCategory", "Select Category:", choices = NULL),
               sliderInput("countRange", "Number:", min = 0, max = 300, value = c(0, 300))
             ),
             mainPanel(
               plotlyOutput("barPlot"),
               includeHTML("custom.html")
             )
           )
  ),
  
  #frank
  tabPanel("Category and Price",
           sidebarLayout(
             sidebarPanel(
               h2("Options"),
               selectInput(inputId = "CategorySelection", 
                           label = "Select Category:", 
                           choices = unique(adi_df$category),
                           selectize = TRUE,
                           multiple = TRUE,
                           selected = "Clothing"),
               sliderInput(inputId = "PriceSelection", 
                           label = " Price range", 
                           min = min(0), 
                           max = max(250), 
                           value = c(0, 250))
             ),
             mainPanel(
               plotlyOutput("pricePlot"),
               includeHTML("custom3.html")
             )
           )
  ),
  #Steven
  tabPanel("Category & Color with Price",
           sidebarLayout(
             sidebarPanel(
               h2("Options"),
               selectInput(inputId = "CategorySelection2", 
                           label = "Select Category:", 
                           choices = unique(adi_df$category),
                           selectize = TRUE,
                           multiple = FALSE,
                           selected = "Clothing"),
               sliderInput(inputId = "PriceSelection2", 
                           label = " Price range", 
                           min = min(0), 
                           max = max(120), 
                           value = c(0, 120))
             ),
             mainPanel(
               plotlyOutput("colorPlot"),
               includeHTML("custom2.html")
             )
           )
  ),
)
#####

