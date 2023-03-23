### PART I: cleaning and Visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step I-01: Loading the Dataset ----------------------------------------------------
#load the csv file/ check how many missing values do we have
library("tidyverse")
library("DT")
library("NbClust")
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
rm(counties_OH_clust,cases_map, deaths_map)

### PART II: clustering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step II-01:  Prepare Clusters ----------------------------------------------------

# 1. select attributes and scale the values to Z-scores

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
summary(subset02_to_cluster)

# 2. select attributes and scale the values to Z-scores

## Step II-02:  K-Means -------------------------------------------------------------

     

# 1- find the number of clusters

# 1-1 Elbow Method: Within-Cluster Sum of Squares
set.seed(1234)
ks <- 2:20

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(cases_to_cluster, centers = k, nstart = 5)$tot.withinss
})

WCSS_viz <- ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 9, color = "red", linetype = 2)+
  geom_vline(xintercept = 5, color = "blue", linetype = 2)

# 1-2 Average Silhouette Width
d <- dist(cases_to_cluster)


ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(cases_to_cluster, centers=k, nstart = 100)$cluster)$avg.silwidth
})

best_k <- ks[which.max(ASW)]
best_k

ASW_viz <- ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = 9, color = "red", linetype = 2)+
  geom_vline(xintercept = 5, color = "blue", linetype = 2)

plot_grid(WCSS_viz, ASW_viz, nrow = 1, ncol = 2)

#NbClust(data = cases_to_cluster, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15,
#        method = "kmeans", index = "all", alphaBeale = 0.1)

## Step II-03:  Hierarchical --------------------------------------------------------







