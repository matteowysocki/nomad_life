# Scrape city data in numbeo.com
mainURL <- "https://www.timeanddate.com/weather/poland/warsaw/climate"
cat("\n"); cat(mainURL);cat("\n")
mainSource <- GET(mainURL, ssl.verifypeer = FALSE)
mainSource <- read_html(mainURL)

# Check content
#nodes <- html_nodes(cont, "div")
#mainSource %>% html_nodes(".climategraph")# %>% html_nodes("div")
#mainSource %>% html_nodes(".climategraph") %>% html_text()
#scripts <- mainSource %>% html_nodes("script") %>% html_text()
#mainSource %>% html_nodes(".four columns")# %>% html_text() climate-month climate-month--january
#x <- mainSource %>% html_nodes("div section div div div div div div div div") #%>% html_text()

system("D:/programs/phantomjs-2.1.1-windows/bin/phantomjs.exe D:/analytics/shiny/nomad_life/scraper_timeanddate.js")

sourceJS <- xml2::read_html("D:/analytics/shiny/nomad_life/data/weather.html")

tMaxNodes <- sourceJS %>% html_nodes(".climategraph__high-label")
sapply(tMaxNodes, html_text)


