# city = "Reno"
#data_serv <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = ",", stringsAsFactors = FALSE)
#city_url_full = "https://www.numbeo.com/cost-of-living/city_result.jsp?country=Afghanistan&city=Herat"



getMainValues <- function(city_url_full = city_url_full, currency = "PLN") {
  
  # Get country
  city_id   = cities_df[which(cities_df$city_url_full == city_url_full), "city_id"]  %>% unlist()
  city_name = cities_df[which(cities_df$city_url_full == city_url_full), "city_name"] %>% unlist()
  country_id   = cities_df[which(cities_df$city_url_full == city_url_full), "country_id"]  %>% unlist()
  country_name = cities_df[which(cities_df$city_url_full == city_url_full), "country_name"] %>% unlist()
  # Scrape city data in numbeo.com
  mainURL <- paste0(city_url_full, "&displayCurrency=", currency)
  #mainURL <- paste0(city_search_bar, "&displayCurrency=", currency)
  #cat("\n"); cat(mainURL);cat("\n")
  mainSource <- getURL(mainURL, .opts=curlOptions(followlocation = TRUE)) # https://stackoverflow.com/questions/25452896/r-geturl-returning-empty-string
  mainSource <- read_html(mainSource, verbose = TRUE)
  is_city_not_found <- mainSource %>% html_text() %>% str_detect("Our system cannot find city with named with")
  if (is_city_not_found) {
    print("The given URL wasn't found on Numbeo")
  }
  
  # `Find tag based on class .emp_number with summary data
  SuplNodes   <- html_nodes(mainSource, ".emp_number" )
  suplVector <-  sapply(SuplNodes, html_text, simplify = TRUE) %>% unlist()
  
  ### 1. A single person monthly costs without rent.
  # Raw form A single person monthly costs
  LiNodes <- html_nodes(mainSource, "li" )
  nodesVector        <- sapply(LiNodes, html_text, simplify = TRUE)
  is_single_cost     <- sapply(nodesVector, str_detect, "A single person monthly costs")
  if (class(is_single_cost) == "list") {
    monthly_value_single <- NA
  } else if (sum(is_single_cost) == 0) {
    monthly_value_single <- NA
  } else {
    monthly_value_single <- nodesVector[which(is_single_cost == TRUE)]
    monthly_value_single <- str_remove(monthly_value_single, "A single person monthly costs: ")
    # String cleaning from raw form 
    monthly_value_single <- substr(monthly_value_single, 1, str_locate(monthly_value_single, "�"))
    monthly_value_single <- str_replace(monthly_value_single, ",", "") %>% str_replace("z�", "") %>% as.numeric()
  }
  
  ### 2. Cost of living rank nth out of N cities in the world.
  # Raw form
  is_index    <- sapply(suplVector, str_detect, "out of", simplify = TRUE)

  if (class(is_index) == "list") {
    city_rank_index <- NA
  } else if (sum(is_index) == 0) {
    city_rank_index <- NA
  } else city_rank_index <- suplVector[which(is_index == TRUE)]
  
  ### 3. Get information on how many people took a survey
  surveyInfoNodes <- html_nodes(mainSource, ".align_like_price_table" ) 
  survey_info_text <- surveyInfoNodes %>% html_text() %>%
    str_split("\\n") %>% unlist() 
  survey_info_text <- survey_info_text[sapply(survey_info_text, function(x) str_length(x) > 0)]
  survey_respondents_info <- survey_info_text[1]
  survey_update_info      <- survey_info_text[2]
  
  ### 4. Get and clean big table
  TableNode <- html_nodes(mainSource, ".data_wide_table" )
  mainTable <- TableNode[[1]] %>% html_table()
  colnames(mainTable) <- c("variable", "value", "range")
  mainTable <- mainTable %>% dplyr::filter(value != "[ Edit ]")
  mainTable$value <- mainTable$value %>% str_replace_all(c("," = "", "z�" = "", "\\?" = "")) 
  mainTable$value[mainTable$value == ""] <- NA
  mainTable$range <- mainTable$range %>% str_replace_all(",", "")
  # Distinct between duplicated for Restaurant and Market item
  # Assumption is that Restaurant comes first (as on the website)
  mainTable$variable[which(mainTable$variable == "Imported Beer (0.33 liter bottle)")[1]] <- paste0("Imported Beer (0.33 liter bottle)" , " at restaurant")
  mainTable$variable[which(mainTable$variable == "Imported Beer (0.33 liter bottle)")[2]] <- paste0("Imported Beer (0.33 liter bottle)" , " at market")
  mainTable <- suppressWarnings(tidyr::separate(mainTable, range, c("min", "max"), sep = "-", remove = TRUE))
  mainTable <- mainTable %>% mutate("currency" = currency)
  
  ### 4. Add previous values to data.frame
  mainTable[nrow(mainTable) + 1, ] <- c("A single person monthly costs", monthly_value_single, NA, NA, "PLN")
  mainTable[nrow(mainTable) + 1, ] <- c("Cost of living index", city_rank_index, NA, NA, "PLN")
  mainTable[nrow(mainTable) + 1, ] <- c("Survey respondents info", survey_respondents_info, NA, NA, "PLN")
  mainTable[nrow(mainTable) + 1, ] <- c("Survey update info", survey_update_info, NA, NA, "PLN")
  mainTable <- mainTable %>% mutate("country_id"    = country_id,
                                    "country_name"  = country_name,
                                    "city_id"       = city_id,
                                    "city_name"     = city_name
                                    )
  mainTable <- mainTable %>% select(country_id, country_name, city_id, city_name, everything())
  
  
  rent_value_outside_city <- mainTable %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(2)
  rent_value_outside_city <- suppressMessages(rent_value_outside_city %>% str_replace_all(c("," = "", "z�" = "")) %>% str_trim() %>% as.numeric())
  
  ### Whitespace cleaning for entire table
  mainTable <- mainTable %>%
    mutate_if(is.character, str_trim)
  
  return(mainTable)
  #return(list=c("Miesi�czne wydatki" = monthly_value_single, "Index"= city_rank_index, "Miesieczny czynsz" = rent_value_outside_city))
}

cities_df <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = "\t", stringsAsFactors = FALSE)
#data_serv <- data_serv %>% mutate(city_url = paste0(city_search_bar, "&displayCurrency=", currency))
# Define table
tableDefinition <- function(country_id = NULL,
                            country_name = NULL,
                            city_id = NULL,
                            city_name = NULL,
                            variable = NULL,
                            value = NULL,
                            min = NULL,
                            max = NULL,
                            currency = NULL) {
  
   thisTable <- data.frame(
     country_id = as.character(country_id),
     country_name = as.character(country_name),
     city_id = as.character(city_id),
     city_name = as.character(city_name),
     variable = as.character(variable),
     value = as.character(value),
     min = as.character(min),
     max = as.character(max),
     currency = as.character(currency)
 )
   return(thisTable)
 }
# Structure res table
res <- tableDefinition()

tictoc::tic("WEBSCRAPE ALL CITIES")
print(Sys.time())
for (city_url_full in cities_df$city_url_full) {
#for (city_search_bar in data_serv$city_search_bar) {
  city_id   = cities_df[which(cities_df$city_url_full == city_url_full), "city_id"]  %>% unlist()
  city_name = cities_df[which(cities_df$city_url_full == city_url_full), "city_name"] %>% unlist()
  country_id   = cities_df[which(cities_df$city_url_full == city_url_full), "country_id"]  %>% unlist()
  country_name = cities_df[which(cities_df$city_url_full == city_url_full), "country_name"] %>% unlist()
  cat("\n\n", city_name, "\n", city_url_full, "\n")
  unit_data <- try(getMainValues(city_url_full, "PLN"))
  if(inherits(unit_data, "try-error"))
   {
     # NEXT STATEMENT IF ERROR ENCOUNTERED
     unit_data <- tableDefinition(
       country_id = country_id,
       country_name = country_name,
       city_id = city_id,
       city_name = city_name,
       variable = "unknown",
       value = "unknown",
       min = "unknown",
       max = "unknown",
       currency = "unknown")
   }
  res <- plyr::rbind.fill(unit_data, res)
  print(as_data_frame(head(unit_data,1)))
  cat("\n\n")
  Sys.sleep(2)
 }
tictoc::toc()
print(Sys.time())
# Writing file
backup <- res 
res <- res %>% 
  mutate_at(vars(min, max), as.numeric)
write.table(x    = res,
            file = "D:/analytics/shiny/nomad_life/data/city_data.txt",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t",
            #fileEncoding  = "UTF-8"
)

df <- plyr::rbind.fill(city_values1, city_values2)
df <- plyr::rbind.fill(df, city_values4)
df <- df %>% arrange(country_id, city_id)

city_values <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_data_updated.txt", sep = "\t", stringsAsFactors = FALSE)
#backup_city_values <- read.csv(file = "D:/analytics/shiny/nomad_life/data/backup_city_data.txt", sep = "\t", stringsAsFactors = FALSE)

