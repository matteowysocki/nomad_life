# Scrape city data in numbeo.com
mainURL <- paste0("https://www.numbeo.com/cost-of-living/")
cat("\n"); cat(mainURL);cat("\n")
mainSource <- getURL(mainURL)
mainSource <- read_html(mainSource, verbose = TRUE)
related_links
# `Find tag based on class .emp_number. Found manually how to systematically search for such values?
### 4. Get and clean big table
nodes <- html_nodes(mainSource, xpath = "/html/body/div/div[9]/table/tbody/tr/td[1]/a[1]")
nodes <- html_nodes(mainSource, xpath = "//*[@id='content_and_logo']/div")
my_node <- nodes[[9]]
hrefs <- my_node %>% html_nodes("a")
hrefs <- hrefs %>% html_attrs() 
vec_links <- hrefs %>% unlist()


nodes[[2]] %>% html_text()
mainTable <- TableNode[[1]] %>% html_table()
mainAttrs <- TableNode[[1]] %>% html_attrs()

/html/body/div/div[9]/table/tbody/tr/td[1]/a[1]
//*[@id="content_and_logo"]/div[9]/table/tbody/tr/td[1]/a[1]


TableNode <- html_nodes(mainSource, ".related_links")
