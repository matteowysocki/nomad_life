INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
source(glue::glue(INPUT_DIR, "R/function.R"))

### COUNTRIES LIST FROM NUMBEO
# Get mainSource for countries
mainSource <- getURL("https://www.numbeo.com/cost-of-living/") %>% read_html(mainSource, verbose = TRUE)
# Get xpath for div with country names and attrs
nodes <- html_nodes(mainSource, xpath = "//*[@id='content_and_logo']/div")
countries_node <- nodes[[9]] %>% html_nodes("a") # my data appears to be in ninth box
# Get names and hrefs
#vec_countries_urls              <- countries_node %>% html_attrs() %>% unlist()
#vec_countries_urls_full         <- paste0("https://www.numbeo.com/cost-of-living/", vec_countries_urls)
vec_countries_names             <- countries_node %>% html_text()
vec_countries_names_url_encoded <- sapply(vec_countries_names, URLencode, reserved = TRUE)
vec_countries_urls              <- paste0("country_result.jsp?country=", vec_countries_names_url_encoded)
vec_countries_urls_full         <- paste0("https://www.numbeo.com/cost-of-living/", vec_countries_urls)

countries_df   <- tibble("country_id"               = seq.int(length(vec_countries_names)),
                         "country_name"             = vec_countries_names,
                         "country_name_url_encoded" = vec_countries_names_url_encoded,
                         "country_url_full"         = vec_countries_urls_full,
                         "country_url"              = vec_countries_urls)

### CITIES LIST FROM NUMBEO
tic("Getting list of cities from all countries")

# Empty data frame
cities_df = tibble(
  "country_id"            = character(),
  "city_name"             = character(),
  "city_name_url_encoded" = character(),
  "city_url_full"         = character(),
  "city_url"              = character()
)

#url_main <- "https://www.numbeo.com/cost-of-living/city_result.jsp?country="
#url_main <- "https://www.numbeo.com/cost-of-living/"
# countries_df %>% filter(country_name == "United States") %>% select(country_url_full)
# iter_country_url_full = "https://www.numbeo.com/cost-of-living/country_result.jsp?country=United%20States"
# iter_country_url_full = "https://www.numbeo.com/cost-of-living/country_result.jsp?country=Poland"
for (iter_country_url_full in countries_df$country_url_full) {
  # Get webdata
  mainURL <- iter_country_url_full
  cat("\n"); cat(mainURL);cat("\n")
  mainSource <- getURL(mainURL) %>% read_html(mainSource, verbose = TRUE)
  nodes <- html_nodes(mainSource, xpath = "//*[@id='city']/option")
  
  # Get needed country data per list of cities
  country_name_url_encoded <- countries_df %>%
    filter(country_url_full == iter_country_url_full) %>% select(country_name_url_encoded) %>% unlist()
  country_id <- countries_df %>%
    filter(country_url_full == iter_country_url_full) %>% select(country_id) %>% unlist()
  
  # Scrape list of cities per country  full url
  vec_cities_names <- nodes %>% html_attrs() %>% unlist()
  vec_cities_names_url_encoded  <- sapply(vec_cities_names, URLencode, reserved = TRUE)
  vec_cities_urls <- paste0("city_result.jsp?country=", country_name_url_encoded, "&city=",  vec_cities_names_url_encoded)
  vec_cities_urls_full <- paste0("https://www.numbeo.com/cost-of-living/", vec_cities_urls)
  vec_country_id  <- rep(country_id, length(vec_cities_names_url_encoded))

  cat(paste("\nDla", URLdecode(country_name_url_encoded), "odnaleziono", length(vec_cities_urls), "miast\n",
            "Druga pozycja w wektorze miast:\n     ", vec_cities_names[2]))
  
  # Create final dataframe for a country iteration
  cities_df_tmp = tibble(
    "country_id"            = vec_country_id,
    "city_name"             = vec_cities_names,
    "city_name_url_encoded" = vec_cities_names_url_encoded,
    "city_url_full"         = vec_cities_urls_full,
    "city_url"              = vec_cities_urls
  )
  cities_df_tmp <- cities_df_tmp %>% filter(str_count(city_name) > 0)
  cities_df <- plyr::rbind.fill(cities_df, cities_df_tmp)
  
  Sys.sleep(5) # zzz...
}
cities_df <- cities_df %>% mutate("city_id" = seq.int(nrow(cities_df)))
cities_df <- cities_df %>% select(country_id, city_id, everything())
toc()

cities_df_bck <- cities_df

write.table(x    = countries_df,
            file = "D:/analytics/shiny/nomad_life/data/country_list.txt",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t",
            fileEncoding  = "UTF-8"
)


write.table(x    = cities_df,
            file = "D:/analytics/shiny/nomad_life/data/city_list.txt",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t",
            fileEncoding  = "UTF-8"
)
cities <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = "\t", stringsAsFactors = FALSE)
countries <- read.csv(file = "D:/analytics/shiny/nomad_life/data/country_list.txt", sep = "\t", stringsAsFactors = FALSE)







########### testing
# Scrape city data in numbeo.com
city = "Zary-¯ary"
city_search_bar
currency = "PLN"
mainURL <- paste0("https://www.numbeo.com/cost-of-living/in/", city, "?displayCurrency=", currency)
cat("\n"); cat(mainURL);cat("\n")
mainSource <- getURL(mainURL)
mainSource <- read_html(mainSource, verbose = TRUE)
is_city_not_found <- mainSource %>% html_text() %>% str_detect("Our system cannot find city with named with")
print(is_city_not_found)
if (is_city_not_found) {
  mainURL <- paste0("https://www.numbeo.com/cost-of-living/in/", city, "-Poland", "?displayCurrency=", currency)
  cat("\n"); cat(mainURL);cat("\n")
  mainSource <- getURL(mainURL)
  mainSource <- read_html(mainSource, verbose = TRUE)
  is_city_not_found <- mainSource %>% html_text() %>% str_detect("Our system cannot find city with named with")
}
is_city_not_found

# `Find tag based on class .emp_number. Found manually how to systematically search for such values?
SuplNodes   <- html_nodes(mainSource, ".emp_number" )
suplVector <-  sapply(SuplNodes, html_text, simplify = TRUE)
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



