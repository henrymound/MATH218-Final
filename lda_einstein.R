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
)
table(lda1Predict$class, clinical_spectrum_blood$test_result)
# Accuracy = (308+6)/362 = ~.8674


# Predicting ICU test results from blood
clinical_spectrum_icu <- clinical_spectrum_clean[,c(6:20)]
ldaICU <- lda(patient_addmited_to_intensive_care_unit_1_yes_0_no ~ .,
            data = clinical_spectrum_icu,
            CV = FALSE)
ldaICUPredict <- predict(ldaICU,
                       newdata=clinical_spectrum_icu
)
table(ldaICUPredict$class, clinical_spectrum_icu$patient_addmited_to_intensive_care_unit_1_yes_0_no)
# Accuracy = (334+6)/362 = ~93.9

ldaICUPredictions1 <- ldaICUPredict$posterior[,2]
finalICUData1 <- data.frame(clinical_spectrum_icu, ldaICUPredictions1)
finalICUData1 %>%
  ggplot(aes(x = leukocytes,
             y = monocytes)) +
  geom_point(aes(color = ldaICUPredictions1)) + 
  ggtitle("ICU Predictions LDA") +
  xlab("Leukocytes") +
  ylab("Monocytes") +
  labs(color = "Predicted ICU Prob")

