#####################################################################
######################### UI ########################################
#####################################################################
Sys.setlocale(category = "LC_ALL", locale = "Polish")
#INPUT_DIR <- "D:/analytics/shiny/nomad_life/"
#INPUT_DIR <- here::here()
INPUT_DIR <- ""
source(glue::glue(INPUT_DIR, "R/library.R"))
#source(glue::glue(INPUT_DIR, "/R/function.R"))

### DATA PREP
# Import UI Data
data <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = ",", stringsAsFactors = FALSE)

# Make a list of choices where each choice is of type list. Care should be take for one element list therefore second line
choices <- lapply(data %>% split(data$Country), select, City)
choices <- lapply(choices, function(x) unlist(x) %>% as.vector() %>% as.list())


### UI DEFINITON
ui <- fluidPage(#theme = "bootstrap_dark_violet.css",
  
  # Background style
  setBackgroundColor(
    color = c("#DCDCDC", "#4169E1"), #skyblue
    gradient = "radial",
    direction = "right",
    shinydashboard = FALSE
    ),
  
  # App title
  includeCSS(glue::glue(INPUT_DIR, "www/header_style.css")),
  headerPanel("Znajdz swoje Bieszczady!"),
    
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
                  min = 0, max = 100000, step = 1000, value = 24000, post = " zl", ticks = FALSE),
      # Slider 2
      sliderInput("trip_duration_months", "Wskaz liczbe miesiecy wyjazdu:",
                  min = 0, max = 36, step = 1, value = 12, ticks = TRUE),
      # Launch button - works ever since then
      actionButton("do", "Uruchom"),
      
      # Table seen by user
      #tableOutput("data"),
      
      # Value table
      htmlOutput("summary_values_html"),
      
      #TESTING SIDEBAR PLOT
      plotlyOutput("piechart")
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )  
)

#####################################################################
####################### SERVER ######################################
#####################################################################
# Now city is input manually, how to fetch all options to  list in shiny?

server <- function(input, output, session) {

  observeEvent(input$do, {
    
    ### DATA PREP
    # Import server Data
    data_dict   <- read.csv(file = glue::glue(INPUT_DIR, "data/city_list.txt"), sep = ",", stringsAsFactors = FALSE)
    data_values <- read.csv(file = glue::glue(INPUT_DIR, "data/city_data.txt"), sep = "\t", stringsAsFactors = FALSE)
    data        <- data_values %>% inner_join(data_dict, by = c("city" = "City_Search_Bar"))
    # Get UI input$city value
    city_chosen       <- input$destination_city
    pln_total_value   <- input$user_value_to_spend
    months            <- input$trip_duration_months
    # Filter data.frame with UI input value - take into account search format in column city_search_bar
    city_data <- data %>% filter(City == city_chosen)
    
    # Get column values
    monthly_value    <- city_data %>% filter(variable == "A single person monthly costs") %>% select(value) %>% as.numeric()
    monthly_rent     <- city_data %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(value) %>% as.numeric()
    rank_index_value <- city_data %>% filter(variable == "Cost of living index") %>% select(value) %>% str_replace(" out of ", "/") %>% str_remove_all("[stndrdth]") %>% as.character()
    
    output$data <- renderTable({
      # Old data frame
      df = data.frame(
                      "Zycie"   = paste0(monthly_value),
                      "Mieszkanie"  = paste0(monthly_rent),
                      "PLN"     = paste0((monthly_value + monthly_rent)),
                      "Index"   = rank_index_value)
      })
    
    output$summary_values_html <- renderUI({
      html_query <- paste0(
        # "<span style='font-weight:bold'>Sredni koszt utrzymania (bez mieszkania): </span> ",
        # monthly_value, " PLN", "<br/>", 
        # "<span style='font-weight:bold'>Kawalerka poza centrum miasta: </span> ", 
        # monthly_rent, " PLN", "<br/>", 
        "<br/>", 
        "<span style='font-weight:bold'>Szacunkowy miesieczny laczny koszt utrzymania: </span> ",
        (monthly_value + monthly_rent), " PLN", "<br/>", 
        "<span style='font-weight:bold'>Indeks miasta: </span> ",
        rank_index_value
       )
      HTML(html_query,  sep = '<br/>')
      })
    
    
    output$piechart <- renderPlotly({ 
      df <- data.frame("variables" = c("Wydatki na zycie", "Mieszkanie"),
                       "values" = c(monthly_value, monthly_rent))
      print(plot_ly(df, labels = ~variables, values = ~values, type = "pie") %>% 
              #layout(fig_bgcolor = "rgb(255, 255, 255)") %>% 
              layout(plot_bgcolor = "rgba(0, 0, 0, 0)") %>% 
              layout(paper_bgcolor = "rgba(0, 0, 0, 0)"))
    })
    
    ### Plot on main panel
    output$plot <- renderPlotly({
      
      # Filter data.frame with UI input value - take into account search format in column city_search_bar
      city_data <- data %>% filter(City == city_chosen)
      # Scrape values from numbeo.com
      #main_values <- getMainValues(city = city_search_bar)
      monthly_value      <- city_data %>% filter(variable == "A single person monthly costs")           %>% select(value) %>% as.numeric()
      monthly_rent       <- city_data %>% filter(variable == "Apartment (1 bedroom) Outside of Centre") %>% select(value) %>% as.numeric()
      pln_monthly_value  <- pln_total_value / months
      city_monthly_value <- (monthly_value + monthly_rent) %>% as.numeric()
      
      # Dynamic dataset of money spent for the plot
      available_value <- pln_total_value
      for (i in 1:months) {
        temp_value      <- available_value[length(available_value)] - city_monthly_value 
        available_value <- append(available_value, temp_value)
      }
      available_value   <- available_value[-1] %>% round(2)

      ## Define and move dates by month
      date_next   <- as.Date("2020-04-01")
      date_seq    <- seq(date_next, by = "month", length.out = months)
      
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
            scale_y_continuous(name="Dostepne srodki", labels = scales::comma) +
            theme_few() 
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

