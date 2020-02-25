#####################################################################
######################### UI ########################################
#####################################################################
INPUT_DIR <- "D:/analytics/shiny/nomad_life"
source(glue::glue(INPUT_DIR, "/R/library.R"))
source(glue::glue(INPUT_DIR, "/R/function.R"))

# Import UI Data
data <- read.csv(file = glue::glue(INPUT_DIR, "/data/city_list.txt", sep = ",", stringsAsFactors = FALSE))

# Make a list of choices where each choice is of type list. Care should be take for one element list therefore second line
choices <- lapply(data %>% split(data$Country), select, City)
choices <- lapply(choices, function(x) unlist(x) %>% as.vector() %>% as.list())

ui <- fluidPage(#theme = "bootstrap_dark_violet.css",
  includeCSS(glue::glue(INPUT_DIR, "/www/header_style.css")),
  #Style
  setBackgroundColor(
    color = c("skyblue", "skyblue"),
    gradient = "radial",
    direction = "right",
    shinydashboard = FALSE
    ),
  # 
  # App title ----
  headerPanel("\nZnajdź swoje Bieszczady!"),
    
  # TRUEre sidebar layout  
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Help panel
      helpText("Symulacje finansowe wyjazdów z pomocą www.numbeo.com."),
      
      # Select panel (drop-down list)
      selectizeInput(inputId = "destination_city",
                     label = "Wybierz miasto:",
                     choices = choices,
                     options = list(create = TRUE,
                                    #optgroups = 
                                    placeholder = "wpisz nazwę miasta...")),
      sliderInput("user_value_to_spend", "Wskaż kwotę:",
                  min = 0, max = 100000, step = 1000, value = 24000, post = " zł", ticks = FALSE),
      sliderInput("trip_duration_months", "Wskaż liczbę miesięcy wyjazdu:",
                  min = 0, max = 36, step = 1, value = 12, ticks = TRUE),
      actionButton("do", "Uruchom"),
      tableOutput("data")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )  
)

#####################################################################
####################### SERVER ######################################
#####################################################################
# `Now city is input manually, how to fetch all options to  list in shiny?

server <- function(input, output, session) {

  observeEvent(input$do, {
    
    # Import server Data
    data_dict   <- read.csv(file = glue::glue(INPUT_DIR, "/data/city_list.txt", sep = ",", stringsAsFactors = FALSE))
    data_values <- read.csv(file = glue::glue(INPUT_DIR, "/data/city_data.txt", sep = "\t", stringsAsFactors = FALSE))
    data_server <- data_values %>% inner_join(data_dict, by = c("city" = "City_Search_Bar"))
    
    output$data <- renderTable({
      
      # Get UI input$city value
      city_chosen <- input$destination_city
      
      # Filter data.frame with UI input value - take into account search format in column city_search_bar
      city_data <- data_server %>% filter(City == city_chosen)
      
      # Scrape values from numbeo.com NOT ANYMORE IS FULLY SCRAPED INTO FILE
      monthly_value    <- city_data %>% filter(variable == "Miesięczne Wydatki Dla Singla") %>% select(value) %>% as.numeric()
      rank_index_value <- city_data %>% filter(variable == "Index Miasta") %>% select(value) %>% as.character()
      monthly_rent     <- city_data %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(value) %>% as.numeric()
      
      df = data.frame(
                      "Życie"   = paste0(monthly_value, "zł"),
                      "Czynsz"  = paste0(monthly_rent, "zł"),
                      "PLN"     = paste0((monthly_value + monthly_rent), "zł"),
                      "Index"   = rank_index_value)
      })
    output$plot <- renderPlotly({
      
      # Get UI input$city value
      city_chosen <- input$destination_city
      
      # Filter data.frame with UI input value - take into account search format in column city_search_bar
      city_data <- data_server %>% filter(City == city_chosen)
      
      # Scrape values from numbeo.com
      #main_values <- getMainValues(city = city_search_bar)
      monthly_value     <- city_data %>% filter(variable == "Miesięczne Wydatki Dla Singla") %>% select(value) %>% as.numeric()
      monthly_rent      <- city_data %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(value) %>% as.numeric()
      pln_total_value   <- input$user_value_to_spend
      months            <- input$trip_duration_months
      
  
      pln_monthly_value  <- pln_total_value / months
      city_monthly_value <- (monthly_value + monthly_rent) %>% as.numeric()
      
      
      available_value <- pln_total_value
      for (i in 1:months) {
        temp_value      <- available_value[length(available_value)] - city_monthly_value 
        available_value <- append(available_value, temp_value)
      }
      available_value <- available_value[-1] %>% round(2)
      #available_value[length(available_value)] <- 800
      
      date_next   <- as.Date("2020-03-01")
      ## by month
      date_seq    <- seq(date_next, by = "month", length.out = months)
      date_seq
      
      df <- data.frame("Kwota"      = rep(city_monthly_value, months),
                       "Data"       = date_seq,
                       "Dostepne"   = available_value)
      df <- df %>% mutate(PositiveNumber = ifelse(Dostepne > 0, "Yes", "No"))
      
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
            geom_bar(stat="identity") +
            scale_fill_manual(values = c("Yes" = "skyblue", "No" = "#CC6666")) +
            #geom_bar(data=df, aes(x = Data, y=Kwota),
            #         stat="identity", fill="red", alpha = 0.5) +
            #geom_point(data=df, aes(x=Data, y=Dostepne), color = "red") +
            scale_y_continuous(name="Dostępne środki", labels = scales::comma) +
            theme_few() 
            #theme_gdocs()
        )) # Plot end
        
        
      )
    })
})
  #################
}

#####################################################################
####################### APP RUN #####################################
#####################################################################
library(shiny)
shinyApp(ui, server)


