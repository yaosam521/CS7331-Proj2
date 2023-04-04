### PART I: cleaning and Visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step I-01: Loading the Dataset ----------------------------------------------------
#load the csv file/ check how many missing values do we have
library("tidyverse")
library("ggrepel")
library("DT")
library("NbClust")
library("dbscan")
cases <- read_csv("../data/Ohio Covid 03-05 + Census 2020-5yrs + Geo Boundaries.csv")
cases <- cases %>% mutate_if(is.character, factor)
cases
# check for NA values
is.na(cases) %>% sum()

## Step I-02: Aggregation, Normalization and Selection -----------------------------
cases_filtered <- cases %>% mutate(
  female_under_40_ratio= (female_under_5 +
                            female_5_to_9 +
                            female_10_to_14 +
                            female_15_to_17 +
                            female_18_to_19 +
                            female_20 +
                            female_21 +
                            female_22_to_24 +
                            female_25_to_29 +
                            female_30_to_34 +
                            female_35_to_39) / total_pop,
  
  female_pop_P100=female_pop/total_pop*100,
  
  male_under_40_ratio= (male_under_5 +
                          male_5_to_9 +
                          male_10_to_14 +
                          male_15_to_17 +
                          male_18_to_19 +
                          male_20 +
                          male_21 +
                          male_22_to_24 +
                          male_25_to_29 +
                          male_30_to_34 +
                          male_35_to_39) / total_pop,
  
  male_pop_P100=male_pop/total_pop*100,
  
  asian_pop_P1000=asian_pop/total_pop*1000,
  black_pop_P1000=black_pop/total_pop*1000,
  hispanic_pop_P1000=hispanic_pop/total_pop*1000,
  
  deaths_P1000 = deaths/total_pop*1000,
  confirmed_cases_P1000= confirmed_cases/total_pop*1000,
  
  walked_to_work_P1000 = walked_to_work/total_pop*1000,
  commuters_by_public_transportation_P1000 = commuters_by_public_transportation/total_pop*1000,
  commuters_by_carpool_P1000 = commuters_by_carpool/total_pop*1000,
  commuters_drove_alone_P1000 = commuters_drove_alone/total_pop*1000,
  
  pop_density_Pkm= total_pop * 10^6 /area_land_meters
  
)

cases_cleaned <- cases_filtered %>% select(county_name,
                                           # Ground truth
                                           confirmed_cases_P1000,
                                           deaths_P1000,
                                           # first Subset
                                           # gender/Ethnicity and age
                                           female_pop_P100,
                                           male_pop_P100,
                                           female_under_40_ratio,
                                           male_under_40_ratio,
                                           # habits and interactions
                                           walked_to_work_P1000,
                                           commuters_by_public_transportation_P1000,
                                           commuters_by_carpool_P1000,
                                           commuters_drove_alone_P1000,
                                           # financial related
                                           income_per_capita,
                                           
                                           # Second Subset
                                           # gender/Ethnicity and age
                                           asian_pop_P1000,
                                           black_pop_P1000,
                                           hispanic_pop_P1000,
                                           median_age,
                                           # habits and interactions
                                           pop_density_Pkm,
                                           # financial related
                                           median_income,
                                           
                                           
) 

# check for NA values
is.na(cases_cleaned) %>% sum()
cases_cleaned
rm(cases, cases_filtered)

## Step I-03:  Data visualization [deaths and detected maps] -----------------------
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))
cases_OH <- cases_cleaned %>% mutate(county = county_name %>% 
                                       str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH)
rm(counties,counties_OH,cases_OH)


deaths_map <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_P1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Deaths in Ohio State", fill = "Deaths per 1000")
cases_map <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_cases_P1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Cases in Ohio State", fill = "Cases per 1000")

cowplot::plot_grid(deaths_map, cases_map, nrow = 1, ncol = 2)
rm(cases_map, deaths_map)

### PART II: clustering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step II-01:  Prepare Clusters ----------------------------------------------------

# 1. select attributes and scale the values to Z-scores ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

subset01_to_cluster <- subset(cases_cleaned, select = ( c(
  # gender/Ethnicity and age
  female_pop_P100,
  male_pop_P100,
  female_under_40_ratio,
  male_under_40_ratio,
  # habits and interactions
  walked_to_work_P1000,
  commuters_by_public_transportation_P1000,
  commuters_by_carpool_P1000,
  commuters_drove_alone_P1000,
  # financial related
  income_per_capita
))) %>% scale() %>% as_tibble()
subset01_to_cluster <- subset01_to_cluster %>% add_column(county_name = cases_cleaned$county_name) 
summary(subset01_to_cluster)  

subset02_to_cluster <- subset(cases_cleaned, select = ( c(
  # gender/Ethnicity and age
  asian_pop_P1000,
  black_pop_P1000,
  hispanic_pop_P1000,
  median_age,
  # habits and interactions
  pop_density_Pkm,
  # financial related
  median_income
))) %>% scale() %>% as_tibble()
subset02_to_cluster <- subset02_to_cluster %>% add_column(county_name = cases_cleaned$county_name)
summary(subset02_to_cluster)

# 2. perform a PCA analysis and remove outliers ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
# a- subset 01:
PCA <- subset01_to_cluster %>% select(-county_name) %>% prcomp()

PCA <- as_tibble(PCA$x)  %>% add_column(county_name = cases_cleaned$county_name)

lof <- lof(as_tibble(PCA$PC1,PCA$PC2), minPts= 10)
ggplot(PCA %>% add_column(lof = lof), aes(PC1, PC2, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")+
  geom_text_repel(aes(label = county_name), vjust = -1)

ggplot(PCA %>% add_column(outlier = lof >= 5), aes(PC1, PC2, color = outlier)) +
  geom_point()+
  geom_text_repel(aes(label = county_name), vjust = -1)

subset01_to_cluster <- subset01_to_cluster %>% filter(lof < 5) 

# b- subset 02:
PCA <- subset02_to_cluster %>% select(-county_name) %>% prcomp()

PCA <- as_tibble(PCA$x)  %>% add_column(county_name = cases_cleaned$county_name)

lof <- lof(as_tibble(PCA$PC1,PCA$PC2), minPts= 10)
ggplot(PCA %>% add_column(lof = lof), aes(PC1, PC2, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")+
  geom_text_repel(aes(label = county_name), vjust = -1)

ggplot(PCA %>% add_column(outlier = lof >= 1.75), aes(PC1, PC2, color = outlier)) +
  geom_point()+
  geom_text_repel(aes(label = county_name), vjust = -1)

subset02_to_cluster <- subset02_to_cluster %>% filter(lof < 1.75) 


rm(PCA, lof)

## Step II-02:  K-Means -------------------------------------------------------------

# 1- find the number of clusters
# A- Subset 01:
# A-1-1 Elbow Method: Within-Cluster Sum of Squares
set.seed(1234)
ks <- 2:10

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(subset01_to_cluster %>% select(-county_name), centers = k, nstart = 1000)$tot.withinss
})

WCSS_viz <- ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 8, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

# A-1-2 Average Silhouette Width
d <- dist(subset01_to_cluster %>% select(-county_name))


ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(subset01_to_cluster %>% select(-county_name), centers=k, nstart = 1000)$cluster)$avg.silwidth
})

ASW_viz <- ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = 8, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

cowplot::plot_grid(WCSS_viz, ASW_viz, nrow = 1, ncol = 2)

k01=7
km01=kmeans(subset01_to_cluster %>% select(-county_name), centers = k01, nstart = 1000)

# B- Subset 02:
# B-1-1 Elbow Method: Within-Cluster Sum of Squares
set.seed(1234)
ks <- 2:20

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(subset02_to_cluster %>% select(-county_name), centers = k, nstart = 1000)$tot.withinss
})

WCSS_viz <- ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 9, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

# A-1-2 Average Silhouette Width
d <- dist(subset02_to_cluster %>% select(-county_name))


ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(subset02_to_cluster %>% select(-county_name), centers=k, nstart = 1000)$cluster)$avg.silwidth
})

ASW_viz <- ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = 9, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

cowplot::plot_grid(WCSS_viz, ASW_viz, nrow = 1, ncol = 2)

k02=9
km02=kmeans(subset02_to_cluster %>% select(-county_name), centers = k02, nstart = 1000)


#NbClust(data = subset02_to_cluster, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15,
#        method = "kmeans", index = "all", alphaBeale = 0.1)

# displaying details of the cluster:
ggplot(pivot_longer(as_tibble(km01$centers,  rownames = "cluster"), 
                    cols = colnames(km01$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
ggplot(pivot_longer(as_tibble(km02$centers,  rownames = "cluster"), 
                    cols = colnames(km02$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# Last step: visualize the clusters in small adjacent maps:

counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

#a. first subset

cases_OH <- subset01_to_cluster %>% mutate(county = county_name %>% 
                                             str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(km01$cluster)))
km01_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Clusters", subtitle = "Kmeans [Subset 01]")

#b. second subset

cases_OH <- subset02_to_cluster %>% mutate(county = county_name %>% 
                                             str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(km02$cluster)))
km02_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Clusters", subtitle = "Kmeans [Subset 02]")

cowplot::plot_grid(km01_viz, km02_viz, nrow = 1, ncol = 2)

## Step II-03:  Hierarchical --------------------------------------------------------

## Step 1: Add Outliers Back in. We don't need to account for outliers in Hierarchical Clustering
subset01_to_cluster <- subset(cases_cleaned, select = ( c(
  # gender/Ethnicity and age
  female_pop_P100,
  male_pop_P100,
  female_under_40_ratio,
  male_under_40_ratio,
  # habits and interactions
  walked_to_work_P1000,
  commuters_by_public_transportation_P1000,
  commuters_by_carpool_P1000,
  commuters_drove_alone_P1000,
  # financial related
  income_per_capita
))) %>% scale() %>% as_tibble()
subset01_to_cluster <- subset01_to_cluster %>% add_column(county_name = cases_cleaned$county_name) 
df_subset01_to_cluster <- as.data.frame(subset01_to_cluster)
rownames(df_subset01_to_cluster) <- df_subset01_to_cluster$county_name
df_subset01_to_cluster <- df_subset01_to_cluster[,1:9]
head(df_subset01_to_cluster)

subset02_to_cluster <- subset(cases_cleaned, select = ( c(
  # gender/Ethnicity and age
  asian_pop_P1000,
  black_pop_P1000,
  hispanic_pop_P1000,
  median_age,
  # habits and interactions
  pop_density_Pkm,
  # financial related
  median_income
))) %>% scale() %>% as_tibble()
subset02_to_cluster <- subset02_to_cluster %>% add_column(county_name = cases_cleaned$county_name)
df_subset02_to_cluster <- as.data.frame(subset02_to_cluster)
rownames(df_subset02_to_cluster) <- df_subset02_to_cluster$county_name
df_subset02_to_cluster <- df_subset02_to_cluster[,1:6]
head(df_subset02_to_cluster)

#Step 2: Perform the Clusters --------------------------------------------------
library(ggdendro)
# Complete Method
d_h <- dist(df_subset01_to_cluster)
hc_1_complete <- hclust(d_h, method='complete')
ggdendrogram(hc_1_complete, 
             rotate = TRUE, 
             theme_dendro = FALSE, 
             color = "red") +
             labs(title="Hierarchical Clusters", subtitle = "Complete - Subset 01")


d_h_2 <- dist(df_subset02_to_cluster)
hc_2_complete <- hclust(d_h_2, method='complete')
ggdendrogram(hc_2_complete, 
             rotate = TRUE, 
             theme_dendro = FALSE, 
             color = "red") +
             labs(title="Hierarchical Clusters", subtitle = "Complete - Subset 02")


# Ward's Method
d_h_ward <- dist(df_subset01_to_cluster)
hc_1_wards <- hclust(d_h, method='ward.D2')
ggdendrogram(hc_1_wards, 
             rotate = TRUE, 
             size = 4, 
             theme_dendro = FALSE, 
             color = "red") +
             labs(title="Hierarchical Clusters", subtitle = "Ward's - Subset 01")

d_h_2_ward <- dist(df_subset02_to_cluster)
hc_2_wards <- hclust(d_h_2, method='ward.D2')
ggdendrogram(hc_2_wards, 
             rotate = TRUE, 
             size = 4, 
             theme_dendro = FALSE, 
             color = "red") +
             labs(title="Hierarchical Clusters", subtitle = "Ward's - Subset 02")

#Code Derived from "https://uc-r.github.io/kmeans_clustering#gap"

# Decided on NbClust 
hclust <- NbClust(data=df_subset01_to_cluster,method="complete",index="silhouette")
print(hclust$Best.nc)

hclust <- NbClust(data=df_subset02_to_cluster,method="complete",index="silhouette")
print(hclust$Best.nc)

hclust <- NbClust(data=df_subset01_to_cluster,method="ward.D2",index="silhouette")
print(hclust$Best.nc)

hclust <- NbClust(data=df_subset02_to_cluster,method="ward.D2",index="silhouette")
print(hclust$Best.nc)

library(factoextra)

p <- fviz_dend(hc_1_complete,k=2,rotate = TRUE, main ="Hierarchical, Complete, Subset 1")
p$layers[[2]]$data$angle <- 0
p

p <- fviz_dend(hc_2_complete,k=3,rotate = TRUE, main ="Hierarchical, Complete, Subset 2")
p$layers[[2]]$data$angle <- 0
p

p <- fviz_dend(hc_1_wards,k=2,rotate = TRUE, main ="Hierarchical, Wards, Subset 1")
p$layers[[2]]$data$angle <- 0
p

p <- fviz_dend(hc_2_wards,k=3,rotate = TRUE, main ="Hierarchical, Wards, Subset 2")
p$layers[[2]]$data$angle <- 0
p

df_subset01_to_cluster <- df_subset01_to_cluster %>% add_column(county_name = cases_cleaned$county_name) 

cases_OH <- df_subset01_to_cluster %>% mutate(county = county_name %>% 
                                             str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

#Visualizing Hierarchical in Map

#Subset 1
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

clusters <- cutree(hc_1_complete, k = 2)

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(clusters)))
hc01_c_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Hierarchical Clusters", subtitle = "Complete [Subset 01]")

#Subset 2
clusters <- cutree(hc_2_complete, k = 3)

df_subset02_to_cluster <- df_subset02_to_cluster %>% add_column(county_name = cases_cleaned$county_name) 

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(clusters)))
hc02_c_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Hierarchical Clusters", subtitle = "Complete [Subset 02]")

cowplot::plot_grid(hc01_c_viz, hc02_c_viz, nrow = 1, ncol = 2)

#Plotting Ward's Method --------------------------------------------------------
#Subset 01
clusters <- cutree(hc_1_wards, k = 2)

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(clusters)))
hc01_w_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Hierarchical Clusters", subtitle = "Ward's [Subset 01]")

clusters <- cutree(hc_2_wards, k = 3)

#Subset 2
counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(clusters)))
hc02_w_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Hierarchical Clusters", subtitle = "Ward's [Subset 02]")

cowplot::plot_grid(hc01_w_viz, hc02_w_viz, nrow = 1, ncol = 2)

#Visualizing the Distances
library(seriation)

#Distance Matrices
dissplot(d_h, labels = hc_1_complete$cluster, options=list(main="Complete 01"))
dissplot(d_h_2, labels = hc_2_complete$cluster, options=list(main="Complete 02"))
dissplot(d_h_ward, labels = hc_1_wards$cluster, options=list(main="Ward's Method 01"))
dissplot(d_h_2_ward, labels = hc_2_wards$cluster, options=list(main="Ward's Method 02"))

library(cluster)
#Silhouette Plot for Hierarchical
fviz_silhouette(silhouette(cutree(hc_1_complete, k = 2), d_h), main = "Complete 01")
fviz_silhouette(silhouette(cutree(hc_2_complete, k = 3), d_h_2), main = "Complete 02")
fviz_silhouette(silhouette(cutree(hc_1_wards, k = 2), d_h_ward), main = "Ward's Method 01")
fviz_silhouette(silhouette(cutree(hc_2_wards, k = 3), d_h_2_ward), main = "Ward's Method 02")

# Step 4: DBSCAN ---------------------------------------------------------------
library(dbscan)

#for the KNN Graph, I am choosing k=1 because I want each cluster to have a minimum points of 2
knn_01 <- kNNdistplot(subset01_to_cluster[,1:9],k=1)
abline(h = 2.7, col = "red")

knn_02 <- kNNdistplot(subset02_to_cluster[,1:6],k=1)
abline(h = 1.8, col = "red")

#Eps for subset 1 = 2.7, eps for subset2 = 1.8

db01 <- dbscan(subset01_to_cluster[,1:9],eps=2.7,minPts = 2)
db02 <- dbscan(subset02_to_cluster[,1:6],eps=1.8,minPts=2)


#Map Subset 1 Cluster
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(db01$cluster)))
db01_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "DB Scan", subtitle = "Subset 01")

#Map Subset 2 Cluster

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(db02$cluster)))
db02_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "DB Scan", subtitle = "Subset 02")

cowplot::plot_grid(db01_viz, db02_viz, nrow = 1, ncol = 2)

# Part 05: External Validation -------------------------------------------------
entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  
  sum(w * rowSums(e, na.rm = TRUE))
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(w * apply(p, 1, max))
}

ground_truth_km01 <- cases_cleaned %>% select(confirmed_cases_P1000,county_name)
ground_truth_km01 <- ground_truth_km01 %>% filter(county_name != "Noble County")
d_km01 <- dist(subset01_to_cluster %>% select(-county_name))
print(ground_truth_km01)
print(km01$cluster)
print(typeof(d_km01))
print(ground_truth_km01%>% select(-county_name))

#Ground truth for hierarchical complete subset 1--------------------------------
ground_truth_h_comp_1 <- cases_cleaned$confirmed_cases_P1000
d_hc01_complete <- d_h
hcut_h_1 <- cutree(hc_1_complete, k = 2)

#Ground truth for hierarchical complete subset 2--------------------------------
ground_truth_h_comp_2 <- cases_cleaned$confirmed_cases_P1000
d_hc02_complete <- d_h_2
hcut_h_2 <- cutree(hc_2_complete, k = 3)

#Ground truth for hierarchical wards subset 1-----------------------------------
ground_truth_h_wards_1 <- cases_cleaned$confirmed_cases_P1000
d_hc01_wards <- d_h_ward
hcut_h_1_ward <- cutree(hc_1_wards, k = 2)

#Ground truth for hierarchical wards subset 2-----------------------------------
d_hc02_wards <- d_h_2_ward
hcut_h_2_ward <- cutree(hc_2_wards, k = 3)
ground_truth_h_wards_2 <- cases_cleaned$confirmed_cases_P1000

#Ground truth for DBSCAN subset 1-----------------------------------------------
d_db_1 <- dist(subset01_to_cluster)
dbscan_sub1 <- db01
ground_truth_db_1 <- cases_cleaned$confirmed_cases_P1000

#Ground truth for DBSCAN subset 1-----------------------------------------------
d_db_2 <- dist(subset02_to_cluster)
dbscan_sub2 <- db02
ground_truth_db_2 <- cases_cleaned$confirmed_cases_P1000

r <- rbind(
  hierarchical_sub01 = c(
    unlist(fpc::cluster.stats(d_hc01_complete, 
                              hcut_h_1, 
                              ground_truth_h_comp_1, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_1, ground_truth_h_comp_1),
    purity = purity(hcut_h_1, ground_truth_h_comp_1)
  ),
  hierarchical_sub02 = c(
    unlist(fpc::cluster.stats(d_hc02_complete, 
                              hcut_h_2, 
                              ground_truth_h_comp_2, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_2, ground_truth_h_comp_2),
    purity = purity(hcut_h_2, ground_truth_h_comp_2)
  ),
  hierarchical_sub01_wards = c(
    unlist(fpc::cluster.stats(d_hc01_wards, 
                              hcut_h_1_ward, 
                              ground_truth_h_wards_1, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_1_ward, ground_truth_h_wards_1),
    purity = purity(hcut_h_1_ward, ground_truth_h_wards_1)
  ),
  hierarchical_sub02_wards = c(
    unlist(fpc::cluster.stats(d_hc02_wards, 
                              hcut_h_2_ward, 
                              ground_truth_h_wards_2, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_2_ward, ground_truth_h_wards_2),
    purity = purity(hcut_h_2_ward, ground_truth_h_wards_2)
  ),
  dbscan_sub_01 = c(
    unlist(fpc::cluster.stats(d_db_1, 
                              dbscan_sub1$cluster, 
                              ground_truth_db_1, 
                              compareonly = TRUE)),
    entropy = entropy(dbscan_sub1$cluster, ground_truth_db_1),
    purity = purity(dbscan_sub1$cluster, ground_truth_db_1)
  ),
  dbscan_sub_02 = c(
    unlist(fpc::cluster.stats(d_db_2, 
                              dbscan_sub2$cluster, 
                              ground_truth_db_2, 
                              compareonly = TRUE)),
    entropy = entropy(dbscan_sub2$cluster, ground_truth_db_2),
    purity = purity(dbscan_sub2$cluster, ground_truth_db_2)
  )
)

r

#Ground truth for hierarchical complete subset 1--------------------------------
ground_truth_h_comp_1 <- cases_cleaned$deaths_P1000
d_hc01_complete <- d_h
hcut_h_1 <- cutree(hc_1_complete, k = 2)

#Ground truth for hierarchical complete subset 2--------------------------------
ground_truth_h_comp_2 <- cases_cleaned$deaths_P1000
d_hc02_complete <- d_h_2
hcut_h_2 <- cutree(hc_2_complete, k = 3)

#Ground truth for hierarchical wards subset 1-----------------------------------
ground_truth_h_wards_1 <- cases_cleaned$deaths_P1000
d_hc01_wards <- d_h_ward
hcut_h_1_ward <- cutree(hc_1_wards, k = 2)

#Ground truth for hierarchical wards subset 2-----------------------------------
d_hc02_wards <- d_h_2_ward
hcut_h_2_ward <- cutree(hc_2_wards, k = 3)
ground_truth_h_wards_2 <- cases_cleaned$deaths_P1000

#Ground truth for DBSCAN subset 1-----------------------------------------------
d_db_1 <- dist(subset01_to_cluster)
dbscan_sub1 <- db01
ground_truth_db_1 <- cases_cleaned$deaths_P1000

#Ground truth for DBSCAN subset 1-----------------------------------------------
d_db_2 <- dist(subset02_to_cluster)
dbscan_sub2 <- db02
ground_truth_db_2 <- cases_cleaned$deaths_P1000

r <- rbind(
  hierarchical_sub01 = c(
    unlist(fpc::cluster.stats(d_hc01_complete, 
                              hcut_h_1, 
                              ground_truth_h_comp_1, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_1, ground_truth_h_comp_1),
    purity = purity(hcut_h_1, ground_truth_h_comp_1)
  ),
  hierarchical_sub02 = c(
    unlist(fpc::cluster.stats(d_hc02_complete, 
                              hcut_h_2, 
                              ground_truth_h_comp_2, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_2, ground_truth_h_comp_2),
    purity = purity(hcut_h_2, ground_truth_h_comp_2)
  ),
  hierarchical_sub01_wards = c(
    unlist(fpc::cluster.stats(d_hc01_wards, 
                              hcut_h_1_ward, 
                              ground_truth_h_wards_1, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_1_ward, ground_truth_h_wards_1),
    purity = purity(hcut_h_1_ward, ground_truth_h_wards_1)
  ),
  hierarchical_sub02_wards = c(
    unlist(fpc::cluster.stats(d_hc02_wards, 
                              hcut_h_2_ward, 
                              ground_truth_h_wards_2, 
                              compareonly = TRUE)),
    entropy = entropy(hcut_h_2_ward, ground_truth_h_wards_2),
    purity = purity(hcut_h_2_ward, ground_truth_h_wards_2)
  ),
  dbscan_sub_01 = c(
    unlist(fpc::cluster.stats(d_db_1, 
                              dbscan_sub1$cluster, 
                              ground_truth_db_1, 
                              compareonly = TRUE)),
    entropy = entropy(dbscan_sub1$cluster, ground_truth_db_1),
    purity = purity(dbscan_sub1$cluster, ground_truth_db_1)
  ),
  dbscan_sub_02 = c(
    unlist(fpc::cluster.stats(d_db_2, 
                              dbscan_sub2$cluster, 
                              ground_truth_db_2, 
                              compareonly = TRUE)),
    entropy = entropy(dbscan_sub2$cluster, ground_truth_db_2),
    purity = purity(dbscan_sub2$cluster, ground_truth_db_2)
  )
)

r

#TEst---------------------------------------------------------------------------
library(mlbench)
set.seed(1234)
shapes <- mlbench.smiley(n = 500, sd1 = 0.1, sd2 = 0.05)

truth <- as.integer(shapes$class)
shapes <- scale(shapes$x)
d <- dist(shapes)
km <- kmeans(shapes, centers = 7, nstart = 10)

entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  
  sum(w * rowSums(e, na.rm = TRUE))
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(w * apply(p, 1, max))
}

r <- rbind(
  kmeans_7 = c(
    unlist(fpc::cluster.stats(d, km$cluster, truth, compareonly = TRUE)),
    entropy = entropy(km$cluster, truth),
    purity = purity(km$cluster, truth)
  )
)
r

print(length(d))
print(length(km$cluster))
print(length(truth))

















