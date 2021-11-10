INPUT_DIR <- ""
### UI datasets
data_values  <- read_tsv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), col_types = cols()) #, encoding = "UTF-8"
data_values  <- data_values %>% mutate(variable_styled_name = variable %>%
                                         str_replace_all("[[:punct:]]", "") %>%
                                         str_replace_all(" ", "_"))
data_values$variable_styled_name <- ifelse(substr(data_values$variable_styled_name, 1, 1) == "1",
                                           sub("^.", "One", data_values$variable_styled_name), data_values$variable_styled_name)
# TODO UNCOMMENT BELOW TO HAVE JUST RANKING CITIES IN FIRST PANEL
# data_ranking  <- read_tsv(file = glue::glue(INPUT_DIR, "data/country_list_ranking.txt"), col_types = cols())
# data_ranking <- data_values

# Filter data values for cities that have included a A single person monthly costs statistic available
# data_values_filtered <- data_values %>%
#   filter(variable == "A single person monthly costs" & !is.na(value)) %>%
#   select(country_id, city_id) %>%
#   left_join(data_values, by = c("country_id", "city_id"))

# TODO UNCOMMENT BELOW TO HAVE JUST RANKING CITIES IN FIRST PANEL
# data_values_filtered <- data_ranking %>%
#   select(country_id, city_id) %>%
#   left_join(data_values, by = c("country_id", "city_id"))
# data_values_filtered <- data_values

# Make dictionary of all available cities
data_dict_ui <- data_values %>% distinct(country_name, city_name) #data_values_filtered

# Make a list of choices where each choice is of type list. Care should be take for one element list therefore second line
choices <- lapply(data_dict_ui %>% split(data_dict_ui$country_name), select, city_name)
choices <- lapply(choices, function(x) unlist(x) %>% as.vector() %>% as.list())

# Make a list of countries choices
choices_countries <- data_dict_ui["country_name"] %>% unique() %>% unlist() %>% unname() %>% stringr::str_sort()
# Make a list of cities choices
choices_cities <- data_dict_ui %>% split(data_dict_ui$country_name)

# Make a vector of variables from numbeo.com to choose from
price_positions <- unique(data_values$variable)
data_map_dict   <- unique(data_values[c("variable", "variable_styled_name")])

### Server side datasets
# Maps data
data_dict   <- read_tsv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), col_types = cols())
data_geo    <- read_tsv(file = glue::glue(INPUT_DIR, "data/city_geo.txt"), col_types = cols())
#data        <- data_values %>% inner_join(data_dict, by = c("city_id" = "city_id"))

# Cost analysis datasets
data_wide   <- read_tsv(file = glue::glue(INPUT_DIR, "data/data_wide_all.csv"), col_types = cols(
  .default = col_double(),
  country_name = col_character(),
  city_name = col_character(),
  Cost_of_living_index = col_character(),
  Survey_respondents_info = col_character(),
  Survey_update_info = col_character(),
  Buffalo_Round_1kg_or_Equivalent_Back_Leg_Red_Meat = col_double()
))
data_wide   <- data_wide %>% left_join(data_geo %>% select(country_name, city_name, lat, lng), by = c("country_name", "city_name"))
data_wide_map <- data_wide %>% filter(!is.na(lat) & !is.na(lng))