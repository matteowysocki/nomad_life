#####################################################################
######################### UI ########################################
#####################################################################
ui <- fluidPage(
  
  list(tags$style(HTML("

      .navbar.navbar-default.navbar-static-top{
                  color: #ff3368;
                                      }
      .navbar .navbar-header {float: left;
                  font-family: 'Lobster', cursive;
                  line-height: 1.1;
                  color: #DC143C;
                  text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;
                                      }
      .navbar-default .navbar-brand {
                  color: grey;
                  font-size: 20px;
                                      }

  "))),
  
  navbarPage("Znajdz swoje Bieszczady!",
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
                      
                      # Left panel layout  
                      sidebarLayout(
                        
                        # Sidebar panel for all user inputs
                        sidebarPanel(
                          # Help panel
                          helpText("Symulacje finansowe wyjazdow z pomoca www.numbeo.com."),
                          
                          # Select panel (drop-down list)
                          selectizeInput(inputId  = "destination_country",
                                         label    = "Wybierz kraj:",
                                         choices  = choices_countries, 
                                         selected = "Poland",
                                         options = list(create = TRUE,
                                                        placeholder = "wybierz kraj...")),
                          # Select panel (drop-down list)
                          selectizeInput(inputId  = "destination_city",
                                         label    = "Wybierz miasto:",
                                         choices  = choices_cities,
                                         selected = NULL,
                                         options = list(create = TRUE,
                                                        placeholder = "wybierz miasto...")),
                          # Slider 1
                          sliderInput("user_value_to_spend", "Wskaz kwote:",
                                      min = 0, max = 100000, step = 1000, value = 40000, post = " zl", ticks = FALSE),
                          # Slider 2
                          sliderInput("trip_duration_months", "Wskaz liczbe miesiecy wyjazdu:",
                                      min = 0, max = 36, step = 1, value = 12, ticks = TRUE),
                          # Launch button - works ever since then
                          actionButton("do", "Uruchom"),
                          
                          # Line break for aesthetics
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
                         Wszelkie pytania lub chec kontaktu moga byc kierowane pod adres: matteo.wysocki@gmail")
                               )
                               #HTML('<img src="pic.png", height="200px"'),
                               
                        ) 
                      )
             ) # Tab panel 2
  ) # Navbar page
)