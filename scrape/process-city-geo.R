
INPUT_DIR <- "D:/analytics/shiny/nomad_life/"
#INPUT_DIR <- here::here()
#INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
#source(glue::glue(INPUT_DIR, "/R/function.R"))

### DATA PREP
# Import UI Data
city_list   <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = ",", stringsAsFactors = FALSE) #, encoding = "UTF-8"
city_data   <- read.csv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), sep = "\t", stringsAsFactors = FALSE) #, encoding = "UTF-8"
city_geo    <- read.csv(file = glue::glue(INPUT_DIR, "data/worldcities.csv"), sep = ",", stringsAsFactors = FALSE, encoding = "windows-1252") #, encoding = "UTF-8"

# Make columns to be manually updated due to city and countries name conflicts
city_geo = city_geo %>% mutate(city_ascii_numbeo    = city_ascii,
                               country_numbeo       = country)

# Write file to be corrected
write.table(x    = city_geo,
            file = "D:/analytics/shiny/nomad_life/data/worldcities_corrections.csv",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")

city_geo    <- read.csv(file = glue::glue(INPUT_DIR, "data/worldcities_corrections.csv"), sep = ",", stringsAsFactors = FALSE, encoding = "windows-1252") #, encoding = "UTF-8"




city_geo <- city_list %>% left_join(city_geo, by = c("City" = "city_ascii", "Country" = "country" ))
city_geo <- city_geo[!duplicated(city_geo$City),]


#city_list %>% anti_join(city_geo, by = "City")
#city_geo %>% anti_join(city_list, by = "City")


# write.table(x    = city_geo,
#             file = "D:/analytics/shiny/nomad_life/data/city_geo.txt",
#             col.names = TRUE,
#             row.names = FALSE,
#             quote = FALSE,
#             sep = "\t")
#   


