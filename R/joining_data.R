INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
source(glue::glue(INPUT_DIR, "R/function.R"))
# Import server Data
# data_dict   <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = "\t", stringsAsFactors = FALSE)
# data_values <- read.csv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), sep = "\t", stringsAsFactors = FALSE)
# data_geo    <- read.csv(file = glue::glue(INPUT_DIR, "data/city_geo.txt"), sep = "\t",  stringsAsFactors = FALSE)
# data        <- data_values %>% inner_join(data_dict, by = c("city" = "City_Search_Bar"))
# data        <- data        %>% inner_join(data_geo %>% select(City_Search_Bar, lat, lng),  by = c("city" = "City_Search_Bar"))
# 
# data <- data %>% select(variable, value, min, max, Country, City, lat, lng)
# data$variable <- data$variable %>% str_replace_all("[[:punct:]]", "")
# data$variable <- data$variable %>% str_replace_all(" ", "_")
# data$variable %>% unique()
# 
# df <- data %>% select(Country, City, variable, value)
# df <- df[!duplicated(df), ]
# df <- df %>% group_by(City)
# df <- df[!duplicated(df),]
# df <- df %>% filter(variable != "Imported_NonAlcoholic_Beer_033_liter_bottle")
# data_all <- df %>% spread(variable, value) %>% left_join(data_geo %>% select(Country, City, lat, lng), by = c("Country", "City"))
# tmp <- data.frame(sapply(data_all[, 3:ncol(data_all)], as.numeric))
# data_all_numeric <- data.frame(data_all[, 1:2], tmp)
# xxxx <- data_all_numeric %>% select(Country, City, lng, lat)
df  <- read_tsv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), col_types = cols()) #, encoding = "UTF-8"
df$variable %>% unique()
df$variable <- df$variable %>% str_replace_all("[[:punct:]]", "")
df$variable <- df$variable %>% str_replace_all(" ", "_")
df$variable <- ifelse(substr(df$variable, 1, 1) == "1", sub("^.", "One", df$variable), df$variable)
df$variable %>% unique()
df <- df %>% filter(variable != "Imported_NonAlcoholic_Beer_033_liter_bottle")
data_all <- df %>% select(country_name, country_id, city_name, city_id, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)
tmp <- data.frame(sapply(data_all[, 5:ncol(data_all)], as.numeric))
data_all_numeric <- data.frame(data_all[, c("country_id" ,"city_id", "country_name", "city_name")], tmp)

# Write file to be corrected
write.table(x    = data_all_numeric,
            file = "D:/analytics/shiny/nomad_life/data/data_wide_all.csv",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")
