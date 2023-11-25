# Assumes that all necessary libraries have been installed
library(tidyverse);library(dbscan);library(factoextra)
df <- read.csv("https://raw.githubusercontent.com/czhd-create/CET182_Data_Mining_Assignment/main/df_processed_44927.csv", header=TRUE)

# remove variables (justified by pre-processing)
df_x <- dplyr::select(df,-target,-fnlwgt,-capital.gain,-capital.loss,-index)
# use scaled variables
df_x <- dplyr::select(df_x,-age,-hours.per.week,-education.num)
colnames(df_x)
# remove variables  (one-hot encoded may not be necessary for dbscan)
df_train <- df_x[-c(8:88)]

kNNdistplot(df_train, k=3)
abline(h = 1.6, col = "red")  # red horizontal line highlights knee location 
# default - euclidean measure
df_euclid_minpts11 <- dbscan(df_train, eps = 1.6, minPts = 11) 
df_euclid_minpts11 ## 54 clusters identified (1947 noise points)
df_euclid_minpts150 <- dbscan(df_train, eps = 1.6, minPts = 150)
df_euclid_minpts150 ## 15 clusters identified (10400 noise points)
df_train$clus_minpts11 <- df_euclid_minpts11$clus
df_train$clus_minpts150 <- df_euclid_minpts150$clus

# Intepret the clusters (top 5 frequency) by using summary statistics (median)
top5_clus_minpts11 <-sort(table(df_train$clus_minpts11),decreasing=TRUE)[1:5]
top5_clus_label_minpts11 <- as.numeric(names(top5_clus_minpts11))
top5_clus_minpts150 <-sort(table(df_train$clus_minpts150),decreasing=TRUE)[1:5]
top5_clus_label_minpts150 <- as.numeric(names(top5_clus_minpts150))
df_train_minpts11 <- df_train[df_train$clus_minpts11 %in% top5_clus_label_minpts11,]
df_train_minpts150 <- df_train[df_train$clus_minpts150 %in% top5_clus_label_minpts150,]
profiles_mp11 <- df_train_minpts11 %>% group_by(clus_minpts11)%>% summarize_all(median)
profiles_mp11[,c(1:11)] %>% arrange(order(top5_clus_label_minpts11))
profiles_mp150 <- df_train_minpts150 %>% group_by(clus_minpts150)%>% summarize_all(median)
profiles_mp150[,c(1:11)] %>% arrange(order(top5_clus_label_minpts150))

sessionInfo()