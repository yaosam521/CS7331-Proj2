# CS7331-Proj2
Project 02 for Data Mining calss to apply Covid plus Census dataset analysis in Ohio State

# Retreived Data
The data was retreived on 3/22/2023 using Google cloud services

you can find the used USAFacts US Coronavirus Database in the following link:
https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab&project=cs7331-prij01

This dataset was pulled with 3/5/2023 updates to be combined with USA Census dataset of 2020 (with 5 years average) in order to 

The resulting query was the following:
SELECT *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19
JOIN `bigquery-public-data.census_bureau_acs.county_2020_5yr` acs
ON covid19.county_fips_code = acs.geo_id
WHERE date = DATE_SUB(CURRENT_DATE(), INTERVAL 17 day) and state="OH"

the interval of days can be increased to find previous dates records if desired
