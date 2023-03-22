### PART I: cleaning and Visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step I-01: Loading the Dataset ----------------------------------------------------
#load the csv file/ check how many missing values do we have
library("tidyverse")
library("DT")
library("cowplot")
cases <- read_csv("../data/Ohio Covid 03-05 + Census 2020-5yrs + Geo Boundaries.csv")
cases <- cases %>% mutate_if(is.character, factor)
cases
# check for NA values
is.na(cases) %>% sum()



## Step I-02: Aggregation, Normalization and Selection -----------------------------
cases_filtered <- cases %>% mutate(
  
  black_pop_P1000=black_pop/total_pop*1000,
  deaths_P1000 = deaths/total_pop*1000,
  confirmed_cases_P1000= confirmed_cases/total_pop*1000,
  commuters_by_public_transportation_P1000 = commuters_by_public_transportation/total_pop*1000,
  pop_density_Pkm= total_pop * 10^6 /area_land_meters
)

cases_cleaned <- cases_filtered %>% select(county_name,
                                           confirmed_cases_P1000,
                                           deaths_P1000,
                                           total_pop,
                                           black_pop_P1000,
                                           commuters_by_public_transportation_P1000,
                                           median_age,
                                           median_income,
                                           income_per_capita,
                                           pop_density_Pkm
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

plot_grid(deaths_map, cases_map, nrow = 1, ncol = 2)
rm(counties_OH_clust,cases_map, deaths_map)

### PART II: clustering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step II-01:  K-Means -------------------------------------------------------------
cases_to_cluster <- subset(cases_cleaned, select = (c(median_income,
                                                      income_per_capita,
                                                      median_age,
                                                      black_pop_P1000,
                                                      pop_density_Pkm
                                                      )))


