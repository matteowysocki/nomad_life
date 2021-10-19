#!/usr/bin/env Rscript
#####################################################################
############### CITY LIST FROM NUMBEO RANKING #######################
#####################################################################

# Load libraries
INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
source(glue::glue(INPUT_DIR, "R/function.R"))

# UTL to HTML
pURL <- paste0("https://www.numbeo.com/cost-of-living/rankings.jsp")
cat("\n"); cat(pURL)
mainSource <- getURL(pURL)
mainSource <- read_html(mainSource, verbose = TRUE)

# Find tag based on class .cityOrCountryInIndicesTable.
tableNodes <- html_nodes(mainSource, ".cityOrCountryInIndicesTable" )

# Get raw strings in list 
lPlace   <- lapply(tableNodes, html_text)
vPlace   <- lPlace %>% unlist() 

# Get all comas location. Important assumption that only two can occur in the position
# Get comas, unlist that object and at the end select first two (not sure why 4 objects are returned by the function)
loc_char   <- lapply(lapply(lapply(lPlace,  str_locate_all, ","), unlist), "[", c(1:2))
first_coma <- sapply(loc_char, "[[", 1, simplify = TRUE) - 1
last_coma  <- sapply(loc_char, "[[", 2, simplify = TRUE) + 1

# Get country and city from vector
vec_countries_names_ranking             <- substr(vPlace, last_coma, sapply(vPlace, nchar)) %>% trimws(which = "left")
vec_countries_names_ranking_url_encoded <- sapply(vec_countries_names_ranking, URLencode, reserved = TRUE)
vec_cities_names_ranking                <- substr(vPlace, 1, last_coma - 2)
vec_cities_names_ranking_url_encoded    <- sapply(vec_cities_names_ranking, URLencode, reserved = TRUE)

# Create final dataframe for a country iteration
cities_ranking_df_tmp = tibble(
  "country_name"            = vec_countries_names_ranking,
  "city_name"               = vec_cities_names_ranking,
  "city_name_url_encoded"   = vec_cities_names_ranking_url_encoded,
  "country_name_url_encoded"= vec_cities_names_ranking_url_encoded
)

# Import data for join
cities <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Add columns from main cities table. Watch that this match is done only on strings so it be sometimes incorrect (?) not sure..
cities_ranking_df <- cities_ranking_df_tmp %>%
  left_join(cities %>% select(-c("city_name_url_encoded", "country_name_url_encoded")),
            by = c("country_name", "city_name")) %>% 
  relocate(country_id, city_id,)

#####################################################################
######################## WRITE TABLE ################################
#####################################################################
write.table(x    = cities_ranking_df,
            file = "D:/analytics/shiny/nomad_life/data/country_list_ranking.txt",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t",
            fileEncoding  = "UTF-8")
