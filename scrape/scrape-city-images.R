library(plyr)
library(reshape2)
require(rvest)


scrapeJSSite <- function(searchTerm){
  url <- paste0("https://www.google.com/search?as_st=y&tbm=isch&hl=en-GB&as_q=", searchTerm, "&as_epq=&as_oq=&as_eq=&cr=&as_sitesearch=&safe=images&tbs=isz:lt,islt:4mp")
  
  lines <- readLines("D:/analytics/shiny/nomad_life/www/google_get_photo.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "D:/analytics/shiny/nomad_life/www/google_get_photo.js")
  
  ## Download website
  system("phantomjs D:/analytics/shiny/nomad_life/www/google_get_photo.js")
  
  pg <<- read_html("1.html")
  files <<- pg %>% html_nodes("img") %>% html_attr("src")
  df <- data.frame(images=files, search=searchTerm) %>% filter(str_detect(images, "http:"))
  
  return(df)
}

data_serv <- read.csv(file = "D:/analytics/shiny/nomad_life/data/city_list.txt", sep = ",", stringsAsFactors = FALSE)
for (i in 1:nrow(data_serv)) {
  cat("\n\n")
  cat(as.character(data_serv[i, 2]), as.character(data_serv[i, 1]))
  searchTerm = paste0(as.character(data_serv[i, 2]), "+", as.character(data_serv[i, 1]), "+City")
  gg <- scrapeJSSite(searchTerm = searchTerm)
  download.file(as.character(gg[1, 1]), destfile = paste0("D:/analytics/shiny/nomad_life/www/img3/", searchTerm,".jpg"), mode = "wb")
}



### exchange the search terms here!
#gg <- scrapeJSSite(searchTerm = "warsaw+poland")
download.file(as.character(gg[1, 1]), destfile = "D:/analytics/shiny/nomad_life/www/img3/test.jpg", mode = "wb")
#searchTerm = "warsaw+poland"

xx  <- html_nodes(pg, xpath = '/html/body')

lapply(xx, html_text)

xx <- html_text(pg)
