INPUT_DIR <- "D:/analytics/shiny/nomad_life/"
#INPUT_DIR <- here::here()
#INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
#source(glue::glue(INPUT_DIR, "/R/function.R"))

### DATA PREP
# Import UI Data
city_list <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"),
                        sep = "\t", stringsAsFactors = FALSE) #, encoding = "UTF-8"
city_list <- city_list %>% arrange(country_name, city_name)
city_list <- city_list %>% mutate(city_name_to_lower    = tolower(city_name),
                                 country_name_to_lower = tolower(country_name))
city_geo  <- read.csv(file = glue::glue(INPUT_DIR, "data/worldcities.csv"), sep = ",",
                        stringsAsFactors = FALSE, encoding = "UTF-8") #, encoding = "UTF-8" "windows-1252"
# [1] "city"       "city_ascii" "lat"        "lng"        "country"    "iso2"       "iso3"       "admin_name" "capital"  "population" "id" 
city_geo <- city_geo %>% arrange(country, city)
city_geo <- city_geo %>% mutate(city_name_to_lower   = tolower(city_ascii),
                               country_name_to_lower = tolower(country))
city_geo <- city_geo %>% select(country_name_to_lower, city_name_to_lower, country, iso2)
country_geo_dict <- city_geo %>% group_by(country) %>% summarise(country_code = max(iso2))
colnames(country_geo_dict) <- c("country_name", "country_code")

# Make columns to be manually updated due to city and countries name conflicts
city_geo2    <- read.csv(file = glue::glue(INPUT_DIR, "data/cities1000.txt"), 
                         sep = "\t",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         header = FALSE) #, encoding = "UTF-8" "windows-1252"
colnames(city_geo2) <- c("geonameid", "name", "asciiname", "alternatenames", "lat", "lng", "feature_class",
                         "feature_code", "country_code", "cc2", "admin1_code", "admin2_code", "admin3_code", "admin4_code",
                         "population", "elevation", "dem", "timezone", "modification_date")
city_geo2 <- city_geo2 %>% select(name, asciiname, lat, lng, country_code, admin1_code, admin2_code) 
city_geo2 <- city_geo2 %>% arrange(country_code, asciiname)
city_geo2 <- city_geo2 %>% mutate(city_name_to_lower   = tolower(asciiname))
city_geo2 <- city_geo2 %>% mutate(city_name_to_lower   = case_when(
  country_code == "US" ~ paste0(city_name_to_lower, ", ", tolower(admin1_code)),
  TRUE ~ city_name_to_lower
))
#
city_list_processed <- city_list %>% left_join(country_geo_dict, by = c("country_name"))
city_list_processed <- city_list_processed %>% mutate(country_code = case_when(
  country_name == "Aland Islands" ~ "AX",
  #country_name == "Alderney" ~ NA,
  country_name == "Anguilla" ~ "AI",
  country_name == "Bahamas" ~ "BS",
  #country_name == "Bonaire" ~ NA,
  country_name == "British Virgin Islands" ~ "VG",
  country_name == "Cape Verde" ~ "CV",
  country_name == "Congo" ~ "CG",
  country_name == "Curacao" ~ "CW",
  country_name == "Czech Republic" ~ "CZ",
  country_name == "Falkland Islands" ~ "FK",
  country_name == "French Southern Territories" ~ "TF",
  country_name == "Gambia" ~ "GM",
  country_name == "Guernsey" ~ "GG",
  country_name == "Ivory Coast" ~ "CI",
  country_name == "Jersey" ~ "JE",
  country_name == "Kosovo (Disputed Territory)" ~ "XK",
  country_name == "Macao" ~ "MO",
  country_name == "Micronesia" ~ "FM",
  country_name == "Montserrat" ~ "MS",
  country_name == "Myanmar" ~ "MM",
  country_name == "Namibia" ~ "NA",
  country_name == "North Korea" ~ "KP",
  country_name == "North Macedonia" ~ "MK",
  country_name == "Palestine" ~ "PS",
  country_name == "Republic Of Congo" ~ "CD",
  country_name == "Saint Helena" ~ "SH",
  country_name == "South Korea" ~ "KR",
  country_name == "Us Virgin Islands" ~ "VI",
  country_name == "Vatican City" ~ "VA",
  country_name == "Western Sahara" ~ "EH",
  TRUE ~ country_code))


#city_list_processed %>% filter(is.na(country_code)) %>% distinct(country_name)

city_list_processed <- city_list_processed %>% left_join(city_geo2, by = c("city_name_to_lower", "country_code"))
city_list_processed %>% filter(country_name == "United States") %>% head()


city_list_processed %>% filter(country_name == "United States") %>% head()
city_geo2 %>% filter(country_code == "US") %>% tail()
dups <- city_list_processed %>% group_by(city_id) %>%
  summarize(n = n(), city1 = min(city_name), city2 = max(city_name), country = min(country_name)) %>%
  filter(n > 1) %>% arrange(desc(n))







#city_geo <- city_geo[!duplicated(city_geo$City),]
#city_geo_cols <- city_geo_list %>% select(country_name, city_name, country, city_ascii, lat, lng, population)

#city_list %>% anti_join(city_geo, by = "City")
#city_geo %>% anti_join(city_list, by = "City")


# write.table(x    = city_geo,
#             file = "D:/analytics/shiny/nomad_life/data/city_geo.txt",
#             col.names = TRUE,
#             row.names = FALSE,
#             quote = FALSE,
#             sep = "\t")
#   


