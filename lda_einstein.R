### Statistical Learning Final Project

library(tidyverse)
library(class)
library(devtools)
library(MASS)
library(dplyr)
library(caret)
library(ISLR)
#library(devtools)
#install_github("Displayr/flipMultivariates")
#library(flipMultivariates)

clinical_spectrum <- read_csv('clinical-spectrum.csv')
clinical_spectrum_filtered <- clinical_spectrum[,c(1:39)]
clinical_spectrum_filtered <- clinical_spectrum_filtered[,-c(21, 28)]
clinical_spectrum_clean <- clinical_spectrum_filtered %>%
  na.omit()
# Now, let's convert test result to binary
clinical_spectrum_clean <- clinical_spectrum_clean %>%
  mutate(test_result = ifelse(
    clinical_spectrum_clean$sars_cov_2_exam_result == 'negative',
    'A',
    'B'))
# 1. Plot
plotData <- as.data.frame(
  clinical_spectrum_clean[, c("test_result", "platelets", "hematocrit")])
plot( plotData[ , c(2, 3) ],
     col=plotData[ ,1 ])

lda1 <- lda(test_result ~ platelets + hematocrit,
            data = plotData,
            CV = FALSE)
lda1Predict <- predict(lda1,
                       newdata=plotData[,c(2,3)]
                       )$class


### NOT WORKING WELL, WILL TRY WITH OTHER DATA
#apple_mobility_trends <- read_csv('uncover/apple_mobility_trends/mobility-trends.csv')

#mobilityPlotData <- as.data.frame(
#  clinical_spectrum_clean[, c("test_result", "platelets", "hematocrit")])
plotData1 <- as.data.frame(
  clinical_spectrum_clean[, c("patient_addmited_to_intensive_care_unit_1_yes_0_no",
                              "red_blood_cells")])
lda2 <- lda(patient_addmited_to_intensive_care_unit_1_yes_0_no ~ red_blood_cells,
            data = plotData1,
            CV = FALSE)
lda1Predict <- predict(lda2,
                       newdata=plotData1[,c(2)]
)$class






plotData <- as.data.frame(
  clinical_spectrum_clean[, c("test_result", "leukocytes")])
plot( plotData[ , c(2, 3) ]),
      col=plotData[ ,1 ])

# Predicting covid test results from blood
clinical_spectrum_blood <- clinical_spectrum_clean[,c(7:20, 38)]
lda1 <- lda(test_result ~ .,
            data = clinical_spectrum_blood,
            CV = FALSE)
lda1Predict <- predict(lda1,
                       newdata=clinical_spectrum_blood
)$class
table(lda1Predict, clinical_spectrum_blood$test_result)

# Predicting ICU test results from blood
clinical_spectrum_icu <- clinical_spectrum_clean[,c(6:20)]
ldaICU <- lda(patient_addmited_to_intensive_care_unit_1_yes_0_no ~ .,
            data = clinical_spectrum_icu,
            CV = FALSE)
ldaICUPredict <- predict(ldaICU,
                       newdata=clinical_spectrum_icu
)$class
table(lda1Predict, clinical_spectrum_icu$patient_addmited_to_intensive_care_unit_1_yes_0_no)

# Accuracy = (308+6)/362 = ~.8674
# Kappa = 

NCAA.lda.1 <- lda(Winner ~ season.OR + season.OR1,
                  data = final.data)

predictions.1 <- predict(NCAA.lda.1)$posterior[,2]

final.data.1 <- data.frame(final.data, predictions.1)

final.data.1 %>%
  ggplot(aes(x = season.OR,
             y = season.OR1)) +
  geom_point(aes(color = predictions.1)) + 
  ggtitle("Offensive Rebounds LDA") +
  xlab("Offensive Rebounds") +
  ylab("Opponent's Offensive Rebounds") +
  labs(color = "Predicted Win Prob")
