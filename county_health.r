##Which populations are most susceptible to contracting the virus? Why?
library(tidyverse)
library(stringr)

county_cases <- read_csv(file.choose())
county_health_data <- read_csv(file.choose())
county_svi <- read_csv(file.choose())

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

colnames(county_svi_subset)[2] = 'fips'

county_svi_subset$fips <- as.numeric(county_svi_subset$fips) 

view(county_svi_subset)

county_overall <- left_join(health_and_cases,
                           county_svi_subset,
                           by = c('fips'))

county_overall_subset <- county_overall %>%
  #filter(county.y == 'Snohomish') %>%
  filter(date == '4/27/2020')

# county_overall_subset <- county_overall_subset[,-c(18,147,148)]
# 
# country_overall_subset <- county_overall_subset %>%
#   na.omit()
# 
# county_overall_subset <- county_overall_subset[,-c(7,8)]
# 
# county_overall_subset_ranger <- county_overall_subset[,-c(1:4)]

county_overall_subset <- county_overall_subset %>%
  mutate(covid_caserate = cases/population) %>%
  mutate(covid_deathrate = deaths/cases)

model1 <- lm(covid_caserate ~ percent_rural + median_household_income + overcrowding + average_number_of_physically_unhealthy_days + average_number_of_mentally_unhealthy_days
             + percent_smokers + percent_adults_with_obesity + percent_physically_inactive + chlamydia_rate +
               primary_care_physicians_rate + dentist_rate + percent_fair_or_poor_health +
               percent_excessive_drinking + percent_vaccinated + high_school_graduation_rate,
             data = county_overall_subset)

summary(model1)

model2 <- lm(covid_deathrate ~ percent_rural + ep_age65 + ep_crowd + e_groupq + ep_pov
             + spl_themes + rpl_themes,
             data = county_overall_subset)

summary(model2)


# library(caret)
# 
# ranger1 <- train(cases ~.,
#                  data = county_overall_subset_ranger,
#                  method = 'ranger')


