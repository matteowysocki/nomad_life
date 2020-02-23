#city = "Reno"
#city = "The-Hague-Den-Haag"
#currency = "PLN"
#data_serv <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = ",", stringsAsFactors = FALSE)

getMainValues <- function(city = city_search_bar, currency = "PLN") {
  
  # Scrape city data in numbeo.com
  mainURL <- paste0("https://www.numbeo.com/cost-of-living/in/", city, "?displayCurrency=", currency)
  cat("\n"); cat(mainURL);cat("\n")
  mainSource <- getURL(mainURL)
  mainSource <- read_html(mainSource, verbose = TRUE)
  
  # `Find tag based on class .emp_number. Found manually how to systematically search for such values?
  SuplNodes   <- html_nodes(mainSource, ".emp_number" )
  suplVector <- sapply(SuplNodes, html_text, simplify = TRUE)
  
  ### 1. A single person monthly costs without rent.
  # Raw form A single person monthly costs
  LiNodes <- html_nodes(mainSource, "li" )
  nodesVector        <- sapply(LiNodes, html_text, simplify = TRUE)
  is_single_cost     <- sapply(nodesVector, str_detect, "A single person monthly costs")
  if (sum(is_single_cost) == 0) {
    monthly_value_single <- NA
  } else {
    monthly_value_single <- nodesVector[which(is_single_cost == TRUE)]
    monthly_value_single <- str_remove(monthly_value_single, "A single person monthly costs: ")
    # String cleaning from raw form 
    monthly_value_single <- substr(monthly_value_single, 1, str_locate(monthly_value_single, "³"))
    monthly_value_single <- str_replace(monthly_value_single, ",", "") %>% str_replace("z³", "") %>% as.numeric()
    }
  
  ### 2. Cost of living rank nth out of N cities in the world.
  # Raw form
  is_index    <- sapply(suplVector, str_detect, "out of")
  if (sum(is_index) == 0) {
    city_rank_index <- NA
  } else city_rank_index <- suplVector[which(is_index == TRUE)]
  
  ### 3. Get and clean big table
  TableNode <- html_nodes(mainSource, ".data_wide_table" )
  mainTable <- TableNode[[1]] %>% html_table()
  colnames(mainTable) <- c("variable", "value", "range")
  mainTable <- mainTable %>% dplyr::filter(value != "[ Edit ]")
  mainTable$value <- mainTable$value %>% str_replace_all(c("," = "","z³" = "")) 
  mainTable$range <- mainTable$range %>% str_replace_all(",", "")
  mainTable <- suppressWarnings(tidyr::separate(mainTable, range, c("min", "max"), sep = "-", remove = TRUE))
  mainTable <- mainTable %>% mutate("currency" = currency)
  
  ### 4. Add previous values to data.frame
  mainTable[nrow(mainTable) + 1, ] <- c("Miesiêczne Wydatki Dla Singla", monthly_value_single, NA, NA, "PLN")
  mainTable[nrow(mainTable) + 1, ] <- c("Index Miasta", city_rank_index, NA, NA, "PLN")
  mainTable <- mainTable %>% mutate("city" = city)
  
  rent_value_outside_city <- mainTable %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(2)
  rent_value_outside_city <- rent_value_outside_city %>% str_replace_all(c("," = "", "z³" = "")) %>% str_trim() %>% as.numeric()
  #return(mainTable)
  return(list=c("Miesiêczne wydatki" = monthly_value_single, "Index"= city_rank_index, "Miesieczny czynsz" = rent_value_outside_city))
}


# # Define table
# tableDefinition <- function(variable = NULL, value = NULL, min = NULL, max = NULL, currency = NULL, city = NULL) {
#    thisTable <- data.frame(
#    variable = as.character(variable),
#    value = as.character(value),
#    min = as.character(min),
#    max = as.character(max),
#    currency = as.character(currency),
#    city = as.character(city)
#  )
#    return(thisTable)
#  }
# # Structure res table
# res <- tableDefinition()
# 
# tictoc::tic("WEBSCRAPE ALL CITIES")
# for (city in data_serv$City_Search_Bar) {
#    cat("\n\n")
#    cat(city)
#    unit_data <- try(getMainValues(city, "PLN"))
#    if(inherits(unit_data, "try-error"))
#    {
#      # NEXT STATEMENT IF ERROR ENCOUNTERED
#      unit_data <- tableDefinition(variable = "unknown", value = "unknown", min = "unknown", max = "unknown", currency = "unknown", city = city)
#      #next
#    }
#    res <- plyr::rbind.fill(unit_data, res)
#    print(as_data_frame(head(unit_data,1)))
#    cat("\n")
#    print(as_data_frame(head(res,1)))
#    cat("\n\n")
#  }
# tictoc::toc()
# # Writing file
# xx <- res
# write.table(x    = res,
#             file = "D:/analytics/shiny/nomad_life/data/city_data.txt",
#             col.names = TRUE,
#             row.names = FALSE,
#             quote = FALSE,
#             sep = "\t",
#             #fileEncoding  = "UTF-8"
# )
# 
# 
# 
