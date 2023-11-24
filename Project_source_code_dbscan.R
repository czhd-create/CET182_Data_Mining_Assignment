# Assumes that all necesary libraries have been installed
# DBScan
library(tidyverse)
library(dbscan)
library(factoextra)
#library(StatMatch) 
df <- read.csv("https://raw.githubusercontent.com/czhd-create/CET182_Data_Mining_Assignment/main/df_processed_44927.csv", header=TRUE)

# remove variables identified as unhelpful during pre-processing
df_x <- dplyr::select(df,-target,-fnlwgt,-capital.gain,-capital.loss,-index)
# remove variables that have been scaled
df_x <- dplyr::select(df_x,-age,-hours.per.week,-education.num)
colnames(df_x)
# remove variables  (one-hot encoded may not be necessary for dbscan)
df_xy <- df_x[-c(8:88)]

kNNdistplot(df_xy, k=3)
abline(h = 1.6, col = "red") 
# use a red horizontal line to highlight the knee location (change h value)

dbs_euclidean_minpts4 <- dbscan(df_xy, eps = 1.6, minPts = 4) # default - euclidean measure
dbs_euclidean_minpts4
## 66 clusters identified (1004 noise points)
dbs_euclidean_minpts150 <- dbscan(df_xy, eps = 1.6, minPts = 150) # default - euclidean measure
dbs_euclidean_minpts150
## 15 clusters identified (10400 noise points)
df_xy$cluster_mp4 <- dbs_euclidean_minpts4$cluster
df_xy$cluster_mp150 <- dbs_euclidean_minpts150$cluster

# look at top 5 clusters only
df_xy_mp4 <- df_xy[which(df_xy$cluster_mp4==4 | df_xy$cluster_mp4==5 
                         | df_xy$cluster_mp4==7 | df_xy$cluster_mp4==2 
                         | df_xy$cluster_mp4==6), ]
                    
df_xy_mp150 <- df_xy[which(df_xy$cluster_mp150==4 | df_xy$cluster_mp150==5 
                           | df_xy$cluster_mp150==2 | df_xy$cluster_mp150==1
                           | df_xy$cluster_mp150==3), ]

profiles_mp4 <- df_xy_mp4 %>% group_by(cluster_mp4)%>% summarize_all(median)
profiles_mp4

profiles_mp150 <- df_xy_mp150 %>% group_by(cluster_mp150)%>% summarize_all(median)
profiles_mp150