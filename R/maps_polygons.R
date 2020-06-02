library(jsonlite)
library(broom)
library(geojsonio)
library(maptools)
library(ggplot2)
library(ggmap)
library(leaflet)
library(rgdal)
library(tibble)
INPUT_DIR = ""
data_map_raw <- geojson_read(glue::glue(INPUT_DIR, "data/worldmap.json"), what = "sp")
data_map <- suppressWarnings(tidy(data_map_raw, region = "iso_a2"))
df <- data_map_raw %>% as_tibble()

#https://stackoverflow.com/questions/44678039/how-to-use-the-google-satellite-view-as-tile-in-leaflet-with-r
pal <- colorFactor("YlOrRd", domain = data_map_raw$iso_a2)

leaflet(data_map_raw) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(
    fillColor = ~pal(data_map_raw$iso_a2),
    #opacity = 0.3,
    color = "white",
    fillOpacity = 0.3
  ) 

#testing
# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,5,10,50,100,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (cumulative)", "2019-COVID (active)", "2019-COVID (new)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (active)", "2019-COVID (new)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deathsper100k,
            title = "<small>Deaths per 100,000</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names
