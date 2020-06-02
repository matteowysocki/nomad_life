#####################################################################
######################### UI ########################################
#####################################################################
#Sys.setlocale("LC_CTYPE", 'Polish')
#INPUT_DIR <- "D:/analytics/shiny/nomad_life/"
#INPUT_DIR <- here::here()
INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
source(glue::glue(INPUT_DIR, "R/function.R"))

### DATA PREP
# Import UI Data
data <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = "\t", stringsAsFactors = FALSE) #, encoding = "UTF-8"

# Make a list of choices where each choice is of type list. Care should be take for one element list therefore second line
choices <- lapply(data %>% split(data$Country), select, City)
choices <- lapply(choices, function(x) unlist(x) %>% as.vector() %>% as.list())

# Other dfs
data_values <- read.csv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), sep = "\t", stringsAsFactors = FALSE)
data_values <- data_values %>% mutate(variable_styled_name = variable %>% str_replace_all("[[:punct:]]", "") %>% str_replace_all(" ", "_"))
#price_positions <- unique(data_values$variable_styled_name)
price_positions <- unique(data_values$variable)
data_wide   <- read.csv(file = glue::glue(INPUT_DIR, "data/data_wide_all.csv"), sep = "\t", stringsAsFactors = TRUE)
data_map_dict <- unique(data_values[c("variable", "variable_styled_name")])

### UI DEFINITON
ui <- fluidPage(
  
  # list(tags$style(HTML("
  # 
  #     .navbar.navbar-default.navbar-static-top{
  #                 color: #ff3368;
  #                                     }
  #     .navbar .navbar-header {float: left;
  #                 font-family: 'Lobster', cursive;
  #                 line-height: 1.1;
  #                 color: #DC143C;
  #                 text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;
  #                                     } 
  #     .navbar-default .navbar-brand { 
  #                 color: #ff3368; 
  #                 font-size: 20px;
  #                                     } 
  # 
  # "))),
  
  navbarPage("Znajdz swoje Bieszczady!",
             #headerPanel("Znajdz swoje Bieszczady!"),
             tabPanel("Analiza kosztow", icon = icon("bar-chart-o"),
                      # Background style
                      setBackgroundColor(
                        color = c("orange", "pink"), #skyblue "#DCDCDC", "skyblue"
                        gradient = "linear", #c("linear", "radial"),
                        shinydashboard = FALSE,
                        direction = "bottom" #left right top bottom
                      ),
                      
                      # CSS file
                      includeCSS(glue::glue(INPUT_DIR, "www/header_style.css")),
                      # TRUEre sidebar layout  
                      sidebarLayout(
                        
                        # Sidebar panel for all user inputs
                        sidebarPanel(
                          # Help panel
                          helpText("Symulacje finansowe wyjazdow z pomoca www.numbeo.com."),
                          
                          # Select panel (drop-down list)
                          selectizeInput(inputId = "destination_city",
                                         label = "Wybierz miasto:",
                                         choices = choices, 
                                         options = list(create = TRUE,
                                                        #optgroups = 
                                                        placeholder = "wpisz nazwe miasta...")),
                          # Slider 1
                          sliderInput("user_value_to_spend", "Wskaz kwote:",
                                      min = 0, max = 100000, step = 1000, value = 40000, post = " zl", ticks = FALSE),
                          # Slider 2
                          sliderInput("trip_duration_months", "Wskaz liczbe miesiecy wyjazdu:",
                                      min = 0, max = 36, step = 1, value = 12, ticks = TRUE),
                          # Launch button - works ever since then
                          actionButton("do", "Uruchom"),
                          
                          # Line break for aeasthetics
                          br(),
                          br(),
                          
                          # Map
                          # TODO Ge all cities lat lon throug ggmap package. Needs Google Maps API Key 
                          leafletOutput("map"),
                          #br(),
                          htmlOutput("small_text")
                        ),
                        mainPanel(
                          # Main fluid row 1
                          fluidRow(
                            column(12, htmlOutput("summary_ibox"))),
                          # Main fluid row 2
                          fluidRow(
                            column(3, htmlOutput("population_ibox")),
                            column(3, htmlOutput("population_ibox2")),
                            column(3, htmlOutput("population_ibox3")),
                            column(3, htmlOutput("population_ibox4"))),
                          # Main fluid row 3
                          fluidRow(
                            column(6, plotlyOutput("plot")),
                            column(6, plotlyOutput("piechart")))
                        ) # Main panel 1
                      ) # Sidebar layout 1 icon = icon("chart-bar") icon = icon("globe-americas"),
             ), # Tab panel 1
             tabPanel("Mapa swiata", icon = icon("globe"),
                      sidebarPanel(
                       #  column(2, 
                       #        wellPanel(
                       #          helpText("Wybierz interesujace cie pozycje kosztowe."))),
                       #  column(4, 
                       #         wellPanel(selectizeInput(inputId = "price_positions_selected", multiple = T,
                       #                 label = "Wybierz pozycje informacji:",
                       #                 choices = price_positions,
                       #                 options = list(create = TRUE,
                       #                 placeholder = "wpisz nazwe...")))), #color_positions_selected
                       #  column(4, 
                       #         wellPanel(selectizeInput(inputId = "color_positions_selected", multiple = FALSE,
                       #                 label = "Wybierz pozycje koloru:",
                       #                 choices = price_positions,
                       #                 options = list(create = TRUE,
                       #                 placeholder = "wpisz nazwe...")))), #color_positions_selected
                       # # Launch button - works ever since then
                       # column(2,
                       #        wellPanel(actionButton("do_map", "Uruchom")))
                                 helpText("Wybierz interesujace cie pozycje kosztowe."),
                                 selectizeInput(inputId = "price_positions_selected",
                                        multiple = TRUE,
                                        label = "Wybierz pozycje informacji:",
                                        choices = price_positions,
                                        options = list(create = TRUE,
                                        placeholder = "wpisz nazwe...")), #color_positions_selected
                                 selectizeInput(inputId = "color_positions_selected",
                                        multiple = FALSE,
                                        label = "Wybierz pozycje koloru:",
                                        selected = NULL,
                                        choices = price_positions,
                                        options = list(create = TRUE,
                                                  placeholder = "wpisz nazwe...",
                                                  onInitialize = I('function() { this.setValue(""); }'))), #color_positions_selected
                                 actionButton("do_map", "Uruchom")
                      ),
                      mainPanel(
                      #mainPanel(
                        tags$head(includeCSS("www/styles.css")),
                        tags$style(type = "text/css", "#map_world {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("map_world")
                        # absolutePanel(id = "controls", class = "panel panel-default",
                        #               top = 120, left = 20, width = 250, fixed=TRUE,
                        #               draggable = TRUE, height = "auto",
                        #               
                        #               selectizeInput(inputId = "price_positions_selected", multiple = T,
                        #                              label = "Wybierz pozycje info:",
                        #                              choices = price_positions, 
                        #                              options = list(create = TRUE,
                        #                                             #optgroups = 
                        #                                             placeholder = "wpisz nazwe...")), #color_positions_selected
                        #               selectizeInput(inputId = "color_positions_selected", multiple = FALSE,
                        #                              label = "Wybierz pozycje koloru:",
                        #                              choices = price_positions, 
                        #                              options = list(create = TRUE,
                        #                                             #optgroups = 
                        #                                             placeholder = "wpisz nazwe...")), #color_positions_selected
                        #               # Launch button - works ever since then
                        #               actionButton("do_map", "Uruchom")
                                      # ) # absolutePanel
                      ) #fluidROw
                      
                      #) # Main panel 2
             ), # Tab panel 2
             tabPanel("O aplikacji", icon = icon("info-circle"),
                      fluidRow(
                        column(12,
                               #br(),
                               h3(p("Informacje o aplikacji")),
                               h5(p("Aplikacja ma na celu ulatwienie planowania wyjazdow do roznych miast na swiecie oraz wskazanie i prezentacje kosztow utrzymania w kazdym z nich (w ujeciu miesiecznym).
                         ")),
                               h5(p("Dane prezentowane w aplikacji pochodza ze strony www.numbeo.com. Nalezy miec na uwadze pewne ograniczenia w ich jakosci oraz ponadto dla niektorych miast
                         moga wystepowac braki danych jesli jakies pozycje nie wystepowaly w zrodle strony na numbeo.com. To samo tyczy sie map.
                         W najblizszym czasie planowane jest uzupelnienie tych brakow oraz dodawanie kolejnych miast. W zwiazku z tym stay tuned :)")),
                               h5(p("Aby uruchomic aplikacje przejdz do zakladni Analiza kosztow, wybierz kwote przeznaczona na wyjazd [w tys. PLN] oraz liczbe miesiecy planowanego wyjazdu.
                         Nastepnie nacisnij przycisk Uruchom. Zmiany wybieranych parametrow lub zmiana miasta musi byc zawsze zatwierdzona tym przyciskiem. 
                         W przeciwnym wypadku zadne dane nie beda wyswietlane. Po kliknieciu przycisku Uruchom pojawia sie takze dane w zakladce Mapa swiata")),
                               h3(p("Informacje o autorze")),
                               h5(p("Autorem aplikacji jest Mateusz Wysocki, ktory pracuje zawodowo jako konsultant Data Science oraz posiada kilkuletnie doswiadczenie w tym zakresie.
                         Wszelkie pytania lub chec kontaktu/wspolpracy moga byc kierowane pod adres: matteo.wysocki@gmail")
                               )
                               #HTML('<img src="GregPicCrop.png", height="200px"'),
                               
                        ) 
                      )
             ) # Tab panel 2
  ) # Navbar page
)
#####################################################################
####################### SERVER ######################################
#####################################################################
# Now city is input manually, how to fetch all options to  list in shiny?

server <- function(input, output, session) {

  observeEvent(input$do_map, {
    data_wide   <- read.csv(file = glue::glue(INPUT_DIR, "data/data_wide_all.csv"), sep = "\t", stringsAsFactors = TRUE) 
    #print(head(data_map_dict))
    # INPUT_DIR = ""
    # color_chosen <- "Apples (1kg)"
    # positions_chosen <- c("Meal, Inexpensive Restaurant", "Domestic Beer (0.5 liter draught)", "Cappuccino (regular)")
    positions_chosen  <- input$price_positions_selected
    print(paste("Initial arg", positions_chosen))
    color_chosen <- input$color_positions_selected
    print(color_chosen) 
    print((exists("color_chosen")))
    if (color_chosen != "") {
      col1 <- data_map_dict %>% filter(variable == color_chosen) %>% select(variable_styled_name) %>% unlist() %>% as.character() 
      beatCol <- colorNumeric(palette = 'RdYlGn', data_wide[[col1]], reverse = TRUE)
    } else {
      color_chosen <- "blue"
      #beatCol <- colorNumeric("skyblue", domain = NULL)
      }
    print(color_chosen)  
    positions_chosen_1 <- positions_chosen[1]
    positions_chosen_2 <- positions_chosen[2]
    positions_chosen_3 <- positions_chosen[3]
    if (!is.null(positions_chosen_1)) {
      positions_chosen_1 <- positions_chosen_1
      var1 <- data_map_dict %>% filter(variable == positions_chosen_1) %>% select(variable_styled_name) %>% unlist() %>% as.character()} else {
        positions_chosen_1 <- NA}
    if (!is.null(positions_chosen_2)) {
      positions_chosen_2 <- positions_chosen_2
      var2 <- data_map_dict %>% filter(variable == positions_chosen_2) %>% select(variable_styled_name) %>% unlist() %>% as.character()} else {
        positions_chosen_2 <- NA}
    if (!is.null(positions_chosen_3)) {
      positions_chosen_3 <- positions_chosen_3
      var3 <- data_map_dict %>% filter(variable == positions_chosen_3) %>% select(variable_styled_name) %>% unlist() %>% as.character()} else {
        positions_chosen_3 <- NA}
     
  output$map_world <- renderLeaflet({
    if (color_chosen == "blue") { 
    leaflet(data_wide) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(lng = ~lng,
                         lat = ~lat, 
                         popup  = ~paste0(" ",
                           if (!is.na(positions_chosen_1)) {paste0(positions_chosen_1, ": ", as.character(data_wide[, var1]))
                             } else {"Nie wybrano zadnej pozycji"}, " <br> ",
                           if (!is.na(positions_chosen_2)) {paste0(positions_chosen_2, ": ", as.character(data_wide[, var2]))}, " <br> ",
                           if (!is.na(positions_chosen_3)) {paste0(positions_chosen_3, ": ", as.character(data_wide[, var3]))}, " "
                          ),
                         label  = ~City,
                         radius = ~11,
                         color  = ~"skyblue",
                         stroke = FALSE,
                         fillOpacity = 0.6
        )
    } else {
      leaflet(data_wide) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(lng = ~lng,
                         lat = ~lat, 
                         popup  = ~paste0(" ",
                            if (!is.na(positions_chosen_1)) {paste0(positions_chosen_1, ": ", as.character(data_wide[, var1]))
                             } else {"Nie wybrano zadnej pozycji"}, " <br> ",
                            if (!is.na(positions_chosen_2)) {paste0(positions_chosen_2, ": ", as.character(data_wide[, var2]))}, " <br> ",
                            if (!is.na(positions_chosen_3)) {paste0(positions_chosen_3, ": ", as.character(data_wide[, var3]))}, " "
                         ),
                         label  = ~City,
                         radius = ~11,
                         #color  = ~"skyblue",
                         color = ~beatCol(data_wide[[col1]]),
                         stroke = FALSE,
                         fillOpacity = 0.6
        ) %>% addLegend("bottomright", pal = beatCol, values = data_wide[[col1]])     
    }
    })
  })

  
  observeEvent(input$do, {
    
    ### DATA PREP
    # Import server Data
    data_dict   <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = "\t", stringsAsFactors = FALSE)
    data_values <- read.csv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), sep = "\t", stringsAsFactors = FALSE)
    data_geo    <- read.csv(file = glue::glue(INPUT_DIR, "data/city_geo.txt"), sep = "\t", stringsAsFactors = FALSE)
    data        <- data_values %>% inner_join(data_dict, by = c("city" = "City_Search_Bar"))
    data_wide   <- read.csv(file = glue::glue(INPUT_DIR, "data/data_wide_all.csv"), sep = "\t", stringsAsFactors = TRUE)
    # Get UI input$city value
    city_chosen       <- input$destination_city
    pln_total_value   <- input$user_value_to_spend
    months            <- input$trip_duration_months
    number_of_months  <- months

    
    # Filter data.frame with UI input value - take into account search format in column city_search_bar
    city_data <- data     %>% filter(City == city_chosen)
    city_geo  <- data_geo %>% filter(City == city_chosen)
    
    # Get column values
    monthly_value    <- city_data %>% filter(variable == "A single person monthly costs")           %>% select(value) %>% as.numeric()
    monthly_rent     <- city_data %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(value) %>% as.numeric()
    rank_index_value <- city_data %>% filter(variable == "Cost of living index") %>% select(value) %>% str_replace(" out of ", "/") %>% str_remove_all("[stndrdth]") %>% as.character()
    surveys          <- city_data %>% filter(variable == "Survey respondents info") %>% select(value) %>% as.character()
    surveys          <- strsplit(surveys, " ")[[1]][6]
    
    # Calculate other values
    pln_monthly_value  <- pln_total_value / months
    city_monthly_value <- (monthly_value + monthly_rent) %>% as.numeric()
    
    # Dynamic dataset of money spent for the plot
    available_value <- pln_total_value
    for (i in 1:months) {
      temp_value      <- available_value[length(available_value)] - city_monthly_value 
      available_value <- append(available_value, temp_value)
    }
    available_value            <- available_value[-1] %>% round(2)
    available_value_at_the_end <- tail(available_value, 1)
    if (available_value_at_the_end > 0) {
      summary_messages   <- c("Brzmi obiecujaco!", "Super!", "Powodzenia w podrozy!", "Jedziesz?", "Sztos!", "Czas na zostanie Nomadem!")
    } else {
      summary_messages   <- c("Zdecydujesz sie?", "Przemysl to!", "Warto?", "Poczekaj i zaoszczedz wiecej!", "Do przemyslenia!")
    }
    summary_emotion_message <- as.character(sample(summary_messages, 1)[1])
    ## Define and move dates by month
    date_next   <- as.Date("2020-06-01")
    date_seq    <- seq(date_next, by = "month", length.out = months)
    
    df <- data.frame("Kwota"      = rep(city_monthly_value, months),
                     "Data"       = date_seq,
                     "Dostepne"   = available_value)
    df <- df %>% mutate(PositiveNumber = ifelse(Dostepne > 0, "Yes", "No"))
    
    output$data <- renderTable({
      # Old data frame
      df = data.frame(
        "Zycie"   = paste0(monthly_value),
        "Mieszkanie"  = paste0(monthly_rent),
        "PLN"     = paste0((monthly_value + monthly_rent)),
        "Index"   = rank_index_value)
    })
    
    output$population_ibox <- renderUI({
      info_box_tip_icon(title = "Populacja miasta", value = city_geo[1, "population"], with_currency_shown = FALSE)
    })
    
    output$population_ibox2 <- renderUI({
      info_box_tip_icon(title = "Suma wydatkow", value = monthly_value + monthly_rent, with_currency_shown = TRUE)
    })
    
    output$population_ibox3 <- renderUI({
      info_box_tip_icon(title = "Wydatki na zycie", value = monthly_value, with_currency_shown = TRUE)
    })
    
    output$population_ibox4 <- renderUI({
      info_box_tip_icon(title = "Wydatki na mieszkanie", value = monthly_rent, with_currency_shown = TRUE)
    })
    
    output$summary_ibox <- renderUI({
      info_box_summary(available_value_at_the_end, number_of_months, city_chosen, summary_emotion_message)
    })
    
    
    output$small_text <- renderUI({
      html_query <- 
        paste0("<p> <span style='font-size: 10px'> Liczba zrealizowanych ankiet: ", surveys,
               "<br>",
               "Ranking miasta: ", rank_index_value, " </span> <p>")
      HTML(html_query,  sep = '<br/>')
      #<p>My mother has <span style="color:blue">blue</span> eyes.</p>
    })
    
    output$map <- renderLeaflet({
      lat <- city_geo[1, "lat"] 
      lon <- city_geo[1, "lng"] 
      leaflet() %>% addTiles() %>% setView(lon, lat, zoom = 9)
    })
    
    output$piechart <- renderPlotly({ 
      df <- data.frame("variables" = c("Wydatki na zycie", "Mieszkanie"),
                       "values" = c(monthly_value, monthly_rent))
      print(plot_ly(df,
                    labels = ~variables,
                    values = ~values,
                    type = "pie", 
                    textfont = list(family = "Helvetica", size = 15)) %>% 
              #layout(fig_bgcolor = "rgb(255, 255, 255)") %>% 
              layout(plot_bgcolor = "rgba(0, 0, 0, 0)") %>% 
              layout(paper_bgcolor = "rgba(0, 0, 0, 0)")) %>% layout(title = "Proporcje wydatkow")
    })
    
    ### Plot on main panel
    output$plot <- renderPlotly({
      
      # Minimal theme + blue fill color
      print(
        # ggplotly(
        #   ggplot(data=df, aes(x=Data, y=Dostepne)) +
        #     geom_bar(stat="identity", fill="skyblue") + #, colour = "blue"
        #     geom_bar(data=df, aes(x=Data, y=Kwota),
        #              stat="identity", fill="red", alpha = 0.4) +
        #     theme_minimal()
        hide_legend(ggplotly(
          ggplot(data=df, aes(x=Data, y=Dostepne, fill = PositiveNumber)) +
            geom_bar(stat="identity", colour = "grey30") +
            scale_fill_manual(values = c("Yes" = "skyblue", "No" = "#CC6666")) +
            #geom_bar(data=df, aes(x = Data, y=Kwota),
            #         stat="identity", fill="red", alpha = 0.5) +
            #geom_point(data=df, aes(x=Data, y=Dostepne), color = "red") +
            scale_y_continuous(name="Dostepne srodki", labels = scales::comma) +
            theme(
              plot.title = element_text(family = "Helvetica", size = (15)),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_line(color = "grey"),
              panel.grid.minor = element_line(color = "transparent"),
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
            ) + labs(title = "     Stan budzetu w kazdym z miesiecy")
          #theme_gdocs()
        )) # Plot end
        
      )
    })
  })
  #################
  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(stopApp)
}

#####################################################################
####################### APP RUN #####################################
#####################################################################
#library(shiny)
shinyApp(ui, server)
#library(rsconnect)
#INPUT_DIR <- "D:/analytics/shiny/nomad_life/R"
#rsconnect::deployApp(INPUT_DIR)

