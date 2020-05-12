df <- data_wide %>% select(-c(Country, City, Survey_respondents_info, Survey_update_info)) 
df <- df[, which(colMeans(!is.na(df)) > 0.4)]
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
df <- df %>% as.matrix()
knns <- get.knn(df, k=5)
colnames(knns[[1]]) <- c("knn1", "knn2", "knn3", "knn4", "knn5")

dims <- data_wide %>% select(Country, City) %>% cbind(knns[[1]])


dims$fst <- dims[dims$knn1, "City"]
dims$snd <- dims[dims$knn2, "City"]
dims$thr <- dims[dims$knn3, "City"]
dims$fou <- dims[dims$knn4, "City"]
dims$fiv <- dims[dims$knn5, "City"]
