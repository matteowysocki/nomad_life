# UTL to HTML
pURL <- paste0("https://www.numbeo.com/cost-of-living/rankings.jsp")
cat("\n"); cat(pURL)
pSource <- getURL(pURL)
pSource <- read_html(pSource, verbose = TRUE)

# Find tag based on class .cityOrCountryInIndicesTable. Found manually how to systematically search for such values?
myNodes <- html_nodes(pSource, ".cityOrCountryInIndicesTable" )

# Get raw strings in list 
lPlace   <- lapply(myNodes, html_text)
vPlace   <- lPlace %>% unlist() 

# Get all comas location
loc_char   <- lapply(lapply(lPlace,  str_locate_all, ","), unlist)
first_coma <- sapply(loc_char, "[[", 1, simplify = TRUE) - 1
last_coma  <- sapply(loc_char, "[[", 2, simplify = TRUE) + 1

# Get country and city from vector
vCountry <- substr(vPlace, last_coma, sapply(vPlace, nchar)) %>% trimws(which = "left")
vCountry %>% sort()
vCity <- substr(vPlace, 1, first_coma) 
vCity %>% sort()

# Locate space and 
vCitySearchBar <- sapply(vCity, str_replace_all, c(" " = "-", "\\(" = "", "\\)" = ""))

# Final data.frame
df <- data.frame("Country"         = vCountry,
                 "City"            = vCity,
                 "City_Search_Bar" = vCitySearchBar)


                   
write.table(x    = df, 
          file = "D:/analytics/shiny/nomad_life/data/city_list.txt",
          col.names = TRUE,
          row.names = FALSE,
          quote = FALSE,
          sep = ",",
          fileEncoding  = "UTF-8"
          ) 

#data <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = ",")

  


