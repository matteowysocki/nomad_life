# UTL to HTML
pURL <- paste0("https://www.analizy.pl/fundusze/fundusze-inwestycyjne/notowania/firma/--/produkt/--/grupa/--/nazwa/--/typ-jednostki/--/typ/--/data/--/limit/100/strona/",
               1,
               "/sort/7/sort_dir/DESC/")
cat("\n"); cat(pURL)
pSource <- getURL(pURL)
pSource <- read_html(pSource, verbose = TRUE)
myNodes <- html_nodes(pSource, "#noteTable" )

main_table <- pSource %>% html_table(trim = TRUE)
main_table <- main_table[[2]]
colnames(main_table) <- main_table[1, ] %>% unlist() %>% as.vector()
main_table %>% head()
main_table <- main_table[-1, ]
main_table %>% head()

for (i in 1:6) {
  pURL <- paste0("https://www.analizy.pl/fundusze/fundusze-inwestycyjne/notowania/firma/--/produkt/",
                 "--/grupa/--/nazwa/--/typ-jednostki/--/typ/--/data/--/limit/100/strona/",
                 i,
                 "/sort/7/sort_dir/DESC/")
  cat("\n"); cat(pURL)
  pSource <- getURL(pURL)
  pSource <- read_html(pSource, verbose = TRUE)
  myNodes <- html_nodes(pSource, "#noteTable" )
  
  if (i == 1) {
    main_table <- pSource %>% html_table(trim = TRUE)
    main_table <- main_table[[2]]
    colnames(main_table) <- main_table[1, ] %>% unlist() %>% as.vector()
    main_table <- main_table[-1, ]
    main_table %>% head()  
  } else {
    temp_table <- pSource %>% html_table(trim = TRUE)
    temp_table <- temp_table[[2]]
    colnames(temp_table) <- temp_table[1, ] %>% unlist() %>% as.vector()
    temp_table <- temp_table[-1, ]
    main_table <- bind_rows(main_table, temp_table)
    main_table %>% head()     
  }
}

funds_table <- main_table[2:13]
#funds_table %>% str_replace_all(vars(contains("m"), "%", ""))
#x <- funds_table %>% map(contains("m"), str_replace_all, "%", "")
#x <- funds_table %>% map(vars(contains("m"), select))
funds_table %>% select(contains("m")) %>% map_df(str_replace, "%", "")

f_values <- apply(funds_table[4:10], 2, str_replace, "%", "")
f_values <- apply(f_values,          2, str_replace, ",", ".")
#f_values <- apply(f_values,          2, str_replace, "-", "")
f_values <- apply(f_values,          2, function(x) ifelse(str_count(x) == 1, str_replace(x, "-", ""), x))
f_values <- apply(f_values,          2, as.numeric)
final <- cbind(funds_table[1:3], f_values, funds_table[11:12])
#funds_table %>% map_df(contains("m"), str_replace, "%", "")
#funds_table %>% select_if(contains("m")) %>% map_df(function(x) str_replace, x, "%", "")
final <- final %>% arrange(desc(ytd)) %>% as_tibble()
colnames(final) <- c("Fundusz", "Data", "Jednostka Netto", "D1", "M1", "M3", "M12", "M36", "M60", "YTD", "Grupa", "SSRI")
               
write.table(x    = final,
            file = "D:/analytics/analizy_fundusze_inwestycyjne_yields_20200519",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")


# Graph 1 
coll = "M12"
p <- function(coll) {
  coll <- sym(coll)
  print(coll)
  #coll_ <- as.name(coll)
  ggplot(data = final, aes(x = !!coll)) +
    geom_histogram(bins = 100, color = "blue", fill = "lightblue") + 
    theme_minimal()
}
p(coll)


# Graph 2 
#mean = mean(final$`12m`, na.rm = TRUE)
coll = "M12"
n    = nrow(final)
mean = 0
sd   = sd(final[, coll] %>% na.omit() %>% unlist() %>% as.numeric())
binwidth = 0.3 # passed to geom_histogram and stat_function
p <- function(coll) {
  coll <- sym(coll)
  ggplot(final, aes(x = !!coll, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(bins = 100, colour = "blue", fill = "lightblue") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * 1.5 *n ,
                color = "darkred", size = 1) +
  ggtitle("Investment Funds returns", subtitle = "12 months")
  }
p(coll)
p("M1")
ggplotly(p(coll))


# Graph 3
final_ <- na.omit(final)
n_     <- nrow(final_)
plot_ly(data = final_,
          x    = ~M12,
          name = "Histogram",
          type = "histogram",
          xbins = n_,
          nbinsx = n_,
          text = ~n_,
          hoverinfo = "text",
          hovertext = paste(final_$Fundusz, "<br>", final_$Grupa)
  ) %>% layout(title = "Investment Funds returns")



# fit <- density(final_$`12m`)
# p %>% 
#   add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
#   layout(yaxis2 = list(overlaying = "y", side = "right"))

# Graph 4
final_ <- final %>% filter(str_detect(Fundusz, "Millennium") == TRUE)
final_ <- final
final_ <- final_ %>% arrange(desc(M1))
plot_ly(data = final_,
        x    = ~M1,
        name = "Returns",
        type = "bar",
        color= ~M1,
        text = ~n_,
        hoverinfo = "text",
        hovertext = paste(final_$Fundusz, "<br>", final_$Grupa, "<br>", final_$M1)) %>% layout(title = "Investment Funds returns")  


#######
coll = "ytd"
ggplot(final, aes_string(x = coll, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(bins = 100, colour = "blue", fill = "lightblue") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * 1.5 *n ,
                color = "darkred", size = 1) +
  ggtitle("Investment Funds returns", subtitle = "12 months")
