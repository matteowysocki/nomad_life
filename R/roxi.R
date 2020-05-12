pURL <- paste0("https://www.roksa.pl/pl/anonse/wszystkie/Warszawa")
cat("\n"); cat(pURL)
pSource <- getURL(pURL)
pSource <- read_html(pSource, verbose = TRUE)

myNodes1 <- html_nodes(pSource,  ".tooltip_header") 
nazwa <- myNodes1 %>% html_text() %>% trimws(which = "both")

myNodes2 <- html_nodes(pSource,  ".tooltip_content") 
text2 <- myNodes2 %>% html_text() %>% trimws(which = "both")
loc_zl <- text2 %>% str_locate(" z³")
cena <- text2 %>% str_sub(loc_zl-3, loc_zl)
cena[cena == "000"] <- "1000"
cena = as.numeric(cena)
cena2 = as.numeric(cena) + (runif(length(cena)) * 10)
#cena = cena + runif(1)

loc_wiek <- text2 %>% str_locate("wiek: ")
wiek <- text2 %>% str_sub(loc_wiek[, 2], loc_wiek[, 2] + 3) %>% str_remove_all(" ")
wiek = as.numeric(wiek)

loc_wzrost <- text2 %>% str_locate("wzrost: ")
wzrost <- text2 %>% str_sub(loc_wzrost[, 2], loc_wzrost[, 2] + 3) %>% str_remove_all(" ")
wzrost = as.numeric(wzrost)

df <- data.frame("nazwa" = nazwa, "cena" = cena, "wiek" = wiek, "wzrost" = wzrost)
df <- df %>% mutate("cena2" = cena2 )
df2 <- df %>% arrange(cena)
#df <- df %>% arrange(desc(cena))
df %>% head()
#df <- na.omit(df)
n_ = nrow(df)
plot_ly(data = df,
        x    = ~cena,
        name = "Cennik kurew w Warszawie",
        type = "histogram",
        xbins = 50,
        nbinsx = n_) %>% layout(title = paste0("Cennik kurew [kurew w Warszawie jest: ", n_, "]"))

###

plot_ly(data = df2,
        x    = ~cena,
        name = "Kurwy",
        type = "bar",
        color= ~cena,
        text = ~n_,
        hoverinfo = "text",
        hovertext = paste("nazwa:", df2$nazwa, "cena:", df2$cena)) %>% layout(title = "Cennik w Wwa")

###
fig <- plot_ly(data = df2,
               type = "scatter",
               x = ~cena2,
               y = ~wiek,
               marker = list(size = 13,
                             color = ~cena2,#'rgba(30,144,255)',
                             line = list(color = 'rgba(0,0,205)', width = 1)
                             ),
               #text = ~n_,
               hoverinfo = "text",
               hovertext = paste("nazwa:", df2$nazwa, "<br>", "cena:", df2$cena, "<br>", "wiek:", df2$wiek)
               )

fig <- fig %>% layout(title = 'Wiek vs Cena',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))
fig

###
# fig <- plot_ly(data = df,
#                type = "scatter",
#                x = ~wzrost,
#                y = ~wiek,
#                marker = list(size = 11,
#                              color='rgba(30,144,255)',
#                              line = list(color = 'rgba(0,0,205)', width = 1)
#                ),
#                text = ~n_,
#                hoverinfo = "text",
#                hovertext = paste("Kurwa o nazwie:", df$nazwa, "<br>", "cena:", df$cena, "<br>", "wiek:", df$wiek)
# )
# 
# fig <- fig %>% layout(title = 'Kurwy: Wiek vs Cena',
#                       yaxis = list(zeroline = FALSE),
#                       xaxis = list(zeroline = FALSE))
# fig
