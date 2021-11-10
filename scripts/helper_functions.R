info_box_tip_icon <- function(title, value, with_currency_shown = FALSE) {
  html_query <- 
    paste0(
      '
      <link rel="stylesheet" href="https://www.webnots.com/resources/font-awesome/css/font-awesome.min.css"> ',
      "<div class='webnots-tip webnots-notification-box'>",
      title,
      " <br> ",
      if (with_currency_shown == TRUE) {
        paste(format(value, nsmall=1, big.mark=","), "PLN")
      } else {
        format(value, nsmall=1, big.mark=",") 
      },
      " </div> ")
  return(HTML(html_query,  sep = '<br/>'))
}

info_box_summary <- function(available_value_at_the_end, number_of_months, city, message) {
  html_query <- 
    paste0(
      '<link rel="stylesheet" href="https://www.webnots.com/resources/font-awesome/css/font-awesome.min.css"> ',
      "<div class='webnots-information webnots-notification-box'> ",
      "Wybrales miasto ", city, ". <br> ",
      if (available_value_at_the_end > 0) {
        paste0("Po ", number_of_months, " miesiacach pozostanie Ci jeszcze ", format(available_value_at_the_end, nsmall=1, big.mark=","), " PLN z Twojego budzetu. ",
               message)  
      } else {
        paste0("Po ", number_of_months, " miesiacach braknie Ci pieniedzy. Potrzebowalbys ", format(available_value_at_the_end, nsmall=1, big.mark=","), " PLN kredytu. ",
               message)
      },
      " </div> ")
  return(HTML(html_query,  sep = '<br/>'))
}