##Which populations are most susceptible to contracting the virus? Why?
library(tidyverse)
library(stringr)

county_cases <- read_csv('./county_case_data.csv')
county_health_data <- read_csv('./county_health_rankings.csv')
county_svi <- read_csv('./social_vulnerability_index.csv')

view(county_cases)

view(county_health_data)

colnames(county_health_data)

aian_indexes <- str_detect(colnames(county_health_data), 'aian')
black_indexes <- str_detect(colnames(county_health_data), 'black')
hispanic_indexes <- str_detect(colnames(county_health_data), 'hispanic')
white_indexes <- str_detect(colnames(county_health_data), 'white')
asian_indexes <- str_detect(colnames(county_health_data), 'asian')
percent_indexes <- str_detect(colnames(county_health_data), '95percent')

indexes <- NULL

for(i in 1:507) {
  if(percent_indexes[i] == TRUE ||
     asian_indexes[i] == TRUE ||
     aian_indexes[i] == TRUE ||
     black_indexes[i] == TRUE ||
     hispanic_indexes[i] == TRUE ||
     white_indexes[i] == TRUE) {
      
      indexes[i] = i
    }
  else{
    indexes[i] = 9
  }
}

county_health_data_clean <- county_health_data[,-c(unique(indexes))]

health_and_cases <- left_join(county_cases,
                              county_health_data_clean,
                              by = c('fips'))

health_and_cases_subset <- health_and_cases %>%
  head(1000) %>%
  group_by(state.x,county.x) %>%
  view()

view(health_and_cases_subset)

view(county_svi)

county_svi <- county_svi[,-c(1:4)]


M_indexes <- str_detect(colnames(county_svi), 'M_')
MP_indexes <- str_detect(colnames(county_svi), 'MP_')

indexes2 <- NULL

for (i in 1:119) {
  
  if(M_indexes[i] == TRUE ||
     MP_indexes[i] == TRUE) {
    indexes2[i] = i
  }
  else{
    indexes2[i] = 5
  }
}


county_svi_subset <- county_svi[,-c(unique(indexes2))]

colnames(county_svi_subset)[1] = 'fips'

county_svi_subset$fips <- as.numeric(county_svi_subset$fips) 

view(county_svi_subset)

county_overall <- left_join(health_and_cases,
                           county_svi_subset,
                           by = c('fips'))

county_overall_subset <- county_overall %>%
  head(1000) %>%
  #filter(county.y == 'Snohomish') %>%
  filter(date == '2020-03-09')

county_overall_subset <- county_overall_subset[,-c(18,147,148)]

country_overall_subset <- county_overall_subset %>%
  na.omit()

county_overall_subset <- county_overall_subset[,-c(7,8)]

county_overall_subset_ranger <- county_overall_subset[,-c(1:4)]

library(caret)

ranger1 <- train(cases ~.,
                 data = county_overall_subset_ranger,
                 method = 'ranger')


