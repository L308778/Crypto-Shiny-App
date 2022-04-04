# latest
library(ggplot2)
library(shiny)
library(shinyTime)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(leaflet)
library(ggmap)
library(geckor)
library(httr)
library(jsonlite)
library(Dict)
library(curl)
library(XML)
library(rvest)
library(plotly)
library(shinyTime)
library(tidytext)
library(tidytext)
library(shinybusy)
library(lubridate)

"
1st 
Get the data from the api

We want the api to pull the best 20 coins with respect to the metric the user chooses

Depending on that the user can choose which currencies to compare with
"

input_maps <- data.frame(row.names=c("Price", "Market Cap", "Low 24h", "High 24h", "Price Change 24h", 
                        "Price Percentage Change 24h", "Fully Diluted Valuation", "Circulating Suppy",
                        "Maximum Supply", "Market Cap Rank", "Total Volume", "Market Cap Percentage Change 24h", "ROI", "Total Supply"),
                        val=c("current_price", "market_cap", "low_24h", "high_24h", "price_change_24h",
                         "price_change_percentage_24h", "fully_diluted_valuation", "circulating_supply", "max_supply",
                        "market_cap_rank", "total_volume", "market_cap_change_percentage_24h", "roi","total_supply"))

input_cat_maps <- data.frame(row.names= c("Market Cap", "Market Cap Change 24h", "Volume 24h"), val=c("market_cap", "market_cap_change_24h/100", "volume_24h"))



#Top-7 trending coins on CoinGecko as searched by users in the last 24 hours (Ordered by most popular first)
find_trending <- function(){
  res <- GET("https://api.coingecko.com/api/v3/search/trending")
  trending <- data.frame(fromJSON(rawToChar(res$content)))
  trending <- as.data.frame(trending$coins$item)
  print(trending)
  return(trending)
}

#Get tokens based on categories. Category is defined as which use case is relevant to the specific crypto
find_cats <- function(n, metric){
  
  res <- GET("https://api.coingecko.com/api/v3/coins/categories")
  data <- data.frame(fromJSON(rawToChar(res$content)))
  data <- data %>% arrange(desc(metric)) %>% top_n(n)
  return(data)
}

#Use this to obtain all the coins market data
find_coin_metric <- function(n, metric){
  res <- GET("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=gecko_desc&per_page=250&page=1&sparkline=false")
  coin_data <- data.frame(fromJSON(rawToChar(res$content))) %>% arrange(desc(metric))
  result <- coin_data[!colnames(coin_data)%in%c("id", "image", "symbol")][1:n,]
  return(result)
}

# obtain categories
cat_metric <- function(n, metric){
  res <- GET("https://api.coingecko.com/api/v3/coins/categories")
  cat_data <- data.frame(fromJSON(rawToChar(res$content)))
  result1 <- cat_data %>% arrange(desc(metric))
  result1 <- result1[1:n,]
  return(result1)
}


# obtain countries & company holdings of bitcoin companies
get_crypto_countries <- function(){
  data_bitcoin <- fromJSON("https://api.coingecko.com/api/v3/companies/public_treasury/bitcoin")
  df_bitcoin <- as.data.frame(data_bitcoin)
  
  # rename countrycode 
  
  df_bitcoin$companies.country <- gsub("Japan", "JP", df_bitcoin$companies.country)
  df_bitcoin$companies.country <- gsub("Canada", "CA", df_bitcoin$companies.country)
  table(df_bitcoin$companies.country)
  
  # obtain countries & company holdings for ethereum companies
  
  data_ethereum <- fromJSON("https://api.coingecko.com/api/v3/companies/public_treasury/ethereum")
  df_ethereum <- as.data.frame(data_ethereum)
  table(df_ethereum$companies.country)
  
  
  # download data of countries with their longtitude and latitude 
  
  webpage <- read_html("https://www.mapsofworld.com/world-maps/world-map-with-latitude-and-longitude.html")
  table <- webpage %>% html_table(fill= TRUE)
  country_lat_lon <- table[[2]]
  colnames(country_lat_lon) <- c("Country", "Latitude", "Longtitude", "ISO")
  country_lat_lon
  
  
  # merge country_lat_lon with df_bitcoin & df_ethereum
  
  df_bitcoin$coin <- "Bitcoin"
  df_ethereum$coin <- "Ethereum"
  
  updated_df <- rbind(df_bitcoin, df_ethereum)
  updated_df
  
  updated_df$companies.country <- gsub("UK", "GB", updated_df$companies.country)
  
  updated_df <- merge(updated_df, country_lat_lon,  by.x = "companies.country", by.y = "ISO", all.x = TRUE)
  
  return(updated_df)
}


"
df is data received from API
met is the metric called
n is the number of cryptos
read_met is the user input metric in readable format
"
draw_mainplot <- function(df, met, n, read_met){
  
  options(scipen = 999)
  
  if(read_met == "Trending"){
    market_plot <- ggplot(data = df, mapping = aes_string(x = "name", y = "price_btc")) + geom_bar(stat = "identity") + 
      labs(title="Top 7 Trending (Most searched) Crypto Coins ranked by Price in Bitcoin", x="Name", y="Price (Bitcoin)")
  }
  else{
    market_plot <- ggplot(data = df, mapping = aes_string(x = "name", y = met))+ geom_bar(stat = "identity") + 
      labs(title=paste0("Top ", n, " Crypto Coins ranked by ", read_met), x="Name", y=read_met)
  }
  return(market_plot)
}


cat_mainplot <- function(df, n, read_met){
  
  options(scipen = 999)
  if (read_met == "Market Cap"){
  market_cap_plot <- ggplot(data = df, mapping = aes_string(x = "name", y = "market_cap")) + 
   geom_bar(stat = "identity") + 
    labs(title = paste0("Top ", n, " by ", read_met), x="Categories", y=read_met) 
  }
  
  else if (read_met == "Market Cap Change 24h"){
    market_cap_plot <- ggplot(data = df, mapping = aes_string(x = "name", y = "market_cap_change_24h/100")) + 
      geom_bar(stat = "identity") + 
      labs(title = paste0("Top ", n, " by ", read_met, " (in %)"), x="Categories", y=read_met) 
  }
  
  else {
    market_cap_plot <- ggplot(data = df, mapping = aes_string(x = "name", y = "volume_24h")) + 
      geom_bar(stat = "identity") + 
      labs(title = paste0("Top ", n, " by ", read_met), x="Categories", y=read_met) 
  }

    return(market_cap_plot)
}


get_top_10 <- function()
{
  res <- GET("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=gecko_desc&per_page=250&page=1&sparkline=false")
  top_coins_df <- data.frame(fromJSON(rawToChar(res$content))) %>% arrange(desc("market_cap"))
  top_10 <- top_coins_df[!colnames(top_coins_df)%in%c("id", "image", "symbol")][1:10,] %>% .$name
  return(top_10)
}



gsearch <- function(name_of_crypto, start_date, end_date){
  res <- gtrends(name_of_crypto)
  iot <- res$interest_over_time
  iot_req <- subset(iot, date > start_date & date < end_date)
  iot_req$hits <- as.integer(iot_req$hits)
  p1 <- iot_req %>% ggplot() + geom_line(aes(x = date,y = hits, color = keyword)) +
    theme_minimal() + geom_point(aes(x=date, y = hits, fill=keyword), shape=23, size=2) + labs(title = "Google Trends Report",
                                                                                               caption = "Courtesy: gtrendsR package")
  return(ggplotly(p1))
}
  

  
  