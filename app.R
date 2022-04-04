# latest
library(shiny)
library(hash)
library(ggplot2)
library(leaflet)
library(maps)
library(quantmod)
library(gtrendsR)
library(plotly)
library(tidyverse)
library(shinyTime)
library(jsonlite)
library(curl)
library(XML)
library(rvest)
library(newsanchor)
library(htmlwidgets)
library(lubridate)
library(dplyr)
library(geckor)
library(httr)
library(Dict)
library(tidytext)
library(shinybusy)
library(shinyWidgets)


#--------------------------------------------------------------------------------------------------------

#Get functions

#--------------------------------------------------------------------------------------------------------
source("Final_Project_Functions.R")

#--------------------------------------------------------------------------------------------------------

api_key <- "455f5389e3ad44ae8d7a6e6c1eaa80e6"

results <- get_everything(query="Cryptocurrency", api_key = api_key, language="en", sort_by="popularity") #free plan only until last month

cryptonews.df <- as.data.frame(results$results_df)

cryptonews.df$url_to_image <- paste0("<img src='",cryptonews.df$url_to_image,"'height='110'></img>")
cryptonews.df$title <- paste0("<a href='",cryptonews.df$url,"' style='color:rgb(141,198,71,1)'><font size='+1'><b>",cryptonews.df$title,"</b></font></a>")
cryptonews.df$subtitle <- paste0("<b>",cryptonews.df$name,"</b> (",cryptonews.df$published_at,")")
cryptonews.df$description <- paste0(substr(cryptonews.df$description, 1, 300)," (","<a href='",cryptonews.df$url,"'>Read more...</a>",")")

cryptonews.df$combinedtext <- paste0(cryptonews.df$title,"<br>",cryptonews.df$subtitle,"<br>",cryptonews.df$description)

cryptonews.df <- subset(cryptonews.df, select=c(url_to_image, combinedtext))
#--------------------------------------------------------------------------------------------------------


#Define possible inputs
n <- c(1:100)
coin_metrics <- c("Price", "Market Cap", "Low 24h", "High 24h", "Price Change 24h", 
                  "Price Percentage Change 24h", "Fully Diluted Valuation", "Circulating Suppy",
                  "Maximum Supply", "Market Cap Rank", "Total Volume", "Market Cap Percentage Change 24h", "ROI", "Total Supply")

category_metrics <- c("Market Cap", "Market Cap Change 24h", "Volume")

not_sel <- "Not Selected"

#--------------------------------------------------------------------------------------------------------


market_page <- tabPanel(
  title = "Market Analysis",
  titlePanel("Market Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      selectInput("cat", "Would you like to receive info about companies or coins?",choices=c("Not Selected","Coin", "Company")),
      
      conditionalPanel(
        condition = "input.cat == 'Coin'",
        selectInput("metric","Variable",choices=c("Market Data", "Category")),
        
        conditionalPanel(
          condition = "input.metric == 'Market Data'",
          helpText("Please select the relevant metric"),
          selectInput("coin_metric","Metric",choices=c(not_sel,coin_metrics)),
          sliderInput("coin_num", "Number of Coins", 0, 10,3, dragRange = F),
          actionButton("market_button","Run Analysis",icon=icon("play")),
          
        ),
        
        conditionalPanel(
          condition = "input.metric == 'Category'",
          helpText("Please select the relevant metric"),
          selectInput("cat_metric","Metric",choices=c(not_sel,category_metrics)),
          sliderInput("cat_num", "Number of Categories", 0, 10 ,2, dragRange = F),
          actionButton("cat_button","Run Analysis",icon=icon("play")),
          
        )
      ),
      conditionalPanel(
        condition = "input.cat == 'Company'",
        helpText("Click to view distribution of coin companies"),
        actionButton("company_button","Show Distribution",icon=icon("play")),
      )),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("mainplot"),
        ),
        
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 12, strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 12, dataTableOutput("cointable")),
            column(width = 12, dataTableOutput("cat_table"))
          )
          
        )
      )
    )
  )
)

#--------------------------------------------------------------------------------------------------------

gao <- tabPanel(
  title="Coin",
  titlePanel("Coin"),
  sidebarLayout(
    
    headerPanel("Price of selected crypto"),
    
    sidebarPanel(
      selectInput(inputId = "crypto1",
                  label = "Selected Crypto",
                  choices = c("BTC-USD","ETH-USD", "BNB-USD", "ADA-USD", "SOL1-USD", "XRP-USD", "DOT1-USD", "HEX-USD")),
      radioButtons(inputId = "time",
                   label = "Time Frame",
                   choices = c( "last 7 day", "last 30 day", "last 3 months", "last 6 months", "last 1 year", "last 5 years", "last 10 years")),
    actionButton("price_trend","Run Analysis",icon=icon("play"))),
  ),
  
  mainPanel(plotOutput("custPlot"))
)

#--------------------------------------------------------------------------------------------------------


liu <- tabPanel(
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("Sentiment Analysis"),
  title = 'Sentiment',
  sidebarLayout(
    
    sidebarPanel(
      helpText("Check out the latest notices of various Cryptocurrencies on Google Search."),
      selectInput('coin_type', "Which coins are you interested in?", choices=c("Highest Market Cap", "Trending")),
      airDatepickerInput(
        inputId = "sentiment_date",
        label = "Your Time Range:",
        placeholder = "Please pick one month",
        clearButton = TRUE,
        maxDate = as.Date(Sys.Date() - 30,"%Y-%mm"),
        value = as.Date(Sys.Date() - 30,"%Y-%mm"),
        minDate = "2016-Jan",
        view = "months", #editing what the popup calendar shows when it opens
        minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
        dateFormat = "yyyy-mm"
      ),
      
      conditionalPanel(
        condition = "input.coin_type == 'Highest Market Cap'",
        checkboxGroupInput('select_coin1', "Select Coins:", choices=get_top_10())
      ),
      conditionalPanel(
        condition = "input.coin_type == 'Trending'",
        checkboxGroupInput('select_coin2', "Select Coins:", choices=trending_coins()$coin_id)),
      
      actionButton('action', 'Analyze')
    ),
    
    mainPanel(
      plotlyOutput(outputId = "plotsentiment")
    )
  )
)



#--------------------------------------------------------------------------------------------------------

joseph <- tabPanel(
  title="Popular Cryptocurrency News",
  titlePanel("News"),
  fluidPage(
    DT::dataTableOutput("cryptonewstable")
  )
)
#--------------------------------------------------------------------------------------------------------

ui <- navbarPage(
  
  title = "CrAnalyser", 
  gao,
  market_page,
  liu,
  joseph
  
  
)

#--------------------------------------------------------------------------------------------------------


server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  #When user clicks the market button we trigger the respective api
  observeEvent(input$market_button,{
    metric <- input_maps[input$coin_metric,]
    n <- as.numeric(input$coin_num)
    data <- find_coin_metric(as.numeric(n), metric)
    
    output$cointable <- renderDataTable({data})
    output$mainplot <- renderPlot({draw_mainplot(data, metric, n, input$coin_metric)})
    
  })
  
  #--------------------------------------------------------------------------------------------------------
  
  # when user clicks on category button
  observeEvent(input$cat_button,{
    category_metric <- input_cat_maps[input$cat_metric,]
    category_number <- as.numeric(input$cat_num)
    result1 <- cat_metric(category_number, category_metric)
    
    output$cat_table <- renderDataTable({result1})
    output$mainplot <- renderPlot({cat_mainplot(result1, category_number, input$cat_metric)})
  })

  #--------------------------------------------------------------------------------------------------------
  
  # display map for bitcoin & ethereum
  observeEvent(input$company_button, {
    updated_df <- get_crypto_countries()
    
    output$mainplot <- renderPlot({
      ggplot() +
        geom_map(
          data = map_data("world"), map = map_data("world"),
          aes(long, lat, map_id = region),
          fill = "lightgray", size = 0.05
        ) +
        geom_point(
          data = updated_df %>% select(Longtitude, Latitude, Country, coin),
          aes(Longtitude, Latitude, color = coin, size = updated_df$companies.country), alpha = 1) +
        scale_size_manual(values=c(3,10,3,6,6,3,3,3,20)) +
        geom_text(data=updated_df, aes(Longtitude, Latitude, label = Country), size=2.5) +
        ggtitle(label = "Worldwide distribution of Bitcoin and Ethereum companies")+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
      
    })
  })
  
  observeEvent(input$price_trend, {
    output$custPlot <- renderPlot(
      {
        
        symbol <- loadSymbols(input$crypto1) # Get crypto symbol from Yahoo Finance
        
        
        if(input$crypto1 == "BTC-USD") 
          
        {chartSeries(`BTC-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "in Tens ($)"))}
        
        
        else if (input$crypto1 == "ETH-USD") 
        {chartSeries(`ETH-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        else if (input$crypto1 == "BNB-USD") 
        {chartSeries(`BNB-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        
        else if(input$crypto1 == "ADA-USD") 
        {chartSeries(`ADA-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        else if(input$crypto1 == "SOL1-USD") 
        {chartSeries(`SOL1-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        
        else if(input$crypto1 == "XRP-USD") 
        {chartSeries(`XRP-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        
        else if(input$crypto1 == "HEX-USD") 
        {chartSeries(`HEX-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))}
        
        
        else if(input$crypto1 == "DOT1-USD") 
        {chartSeries(`DOT1-USD`, subset= input$time , theme='white', TA=NULL, type='candles', name = paste("  ", symbol, "($)"))} 
        
      }
      
    )
  })
  
  #--------------------------------------------------------------------------------------------------------
  observeEvent(input$action,{
    
      date <- format(as.Date(input$sentiment_date,format="%Y-%m-%d"),"%Y-%m")
      start = as.Date(paste0(date,"-01"))
      end = as.Date(paste0(date,"-28"))

      if(input$coin_type == "Highest Market Cap"){
        output$plotsentiment <- renderPlotly(gsearch(input$select_coin1, start, end))
      }
      if(input$coin_type == "Trending"){
        output$plotsentiment <- renderPlotly(gsearch(input$select_coin2, start, end))
      }
  })
  
  #--------------------------------------------------------------------------------------------------------
  
  headerCallback <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  # customize the length drop-down menu; display 15 rows per page by default
  output$cryptonewstable <- DT::renderDataTable({
    DT::datatable(cryptonews.df, 
                  options = list(lengthMenu = c(15, 30, 50),
                                 pageLength = 15,
                                 headerCallback = JS(headerCallback)),
                  escape=FALSE,
                  rownames=FALSE,
    )
  })
  
  
}

#--------------------------------------------------------------------------------------------------------


shinyApp(ui = ui, server = server)



