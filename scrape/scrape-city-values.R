city = "Beijing"
getMainValues <- function(city = city_search_bar) {
  
  # Scrape city data in numbeo.com
  mainURL <- paste0("https://www.numbeo.com/cost-of-living/in/", city, "?displayCurrency=PLN")
  cat("\n"); cat(mainURL)
  mainSource <- getURL(mainURL)
  mainSource <- read_html(mainSource, verbose = TRUE)
  
  # `Find tag based on class .emp_number. Found manually how to systematically search for such values?
  empNumberNodes <- html_nodes(mainSource, ".emp_number" )
  
  ### 1. A single person monthly costs without rent.
  # Raw form
  monthly_value <- empNumberNodes[[2]] %>% html_text()
  
  # String cleaning from raw form 
  loc_char      <- str_locate(monthly_value, "³")
  monthly_value <- substr(monthly_value, 1, loc_char)
  monthly_value <- str_replace(monthly_value, ",", "") %>% str_replace("z³", "") %>% as.numeric()
  #monthly_value <- paste0(monthly_value, " z³")
  
  ### 2. Cost of living rank nth out of N cities in the world.
  # Raw form
  rank_index_value <- empNumberNodes[[4]] %>% html_text()
  
  return(list=c("Miesiêczne wydatki" = monthly_value, "Index"= rank_index_value))
}

#res <- getMainValues()
#res[1]






