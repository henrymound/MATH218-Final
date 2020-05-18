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

clinical_spectrum <- read_csv('clinical_spectrum/clinical-spectrum.csv')
clinical_spectrum_filtered <- clinical_spectrum[,c(1:39)]
clinical_spectrum_filtered <- clinical_spectrum_filtered[,-c(21, 28)]
clinical_spectrum_clean <- clinical_spectrum_filtered %>%
  na.omit()
# Now, let's convert test result to binary
clinical_spectrum_clean <- clinical_spectrum_clean %>%
  mutate(test_result = ifelse(
    clinical_spectrum_clean$sars_cov_2_exam_result == 'negative',
    'Blue',
    'Red'))
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

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
# Predicting covid test results from blood
clinical_spectrum_blood <- clinical_spectrum_clean[,c(7:20, 38)]
lda1 <- lda(test_result ~ .,
            data = clinical_spectrum_blood,
            CV = FALSE)
lda1Predict <- predict(lda1,
                       newdata=clinical_spectrum_blood
)
lda1tab <- table(lda1Predict$class, clinical_spectrum_blood$test_result)
accuracy(lda1tab)
# Accuracy = (308+6)/362 = ~.8674


# Predicting ICU test results from blood
clinical_spectrum_icu <- clinical_spectrum_clean[,c(6:20)]
ldaICU <- lda(patient_addmited_to_intensive_care_unit_1_yes_0_no ~ .,
            data = clinical_spectrum_icu,
            CV = FALSE)
ldaICUPredict <- predict(ldaICU,
                       newdata=clinical_spectrum_icu
)
ldaICUtab <- table(ldaICUPredict$class, clinical_spectrum_icu$patient_addmited_to_intensive_care_unit_1_yes_0_no)
accuracy(ldaICUtab)
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


# NOW WE WILL TRY TO IMPLEMENT KNN
# 70% training, 30% testing
set.seed(2)
train_ind <- sample(seq_len(nrow(clinical_spectrum_clean[,c(6:20)])),
                    size = floor(0.75 * nrow(clinical_spectrum_clean[,c(6:20)])))
trainICU <- clinical_spectrum_clean[,c(6:20)][train_ind, ]
testICU <- clinical_spectrum_clean[,c(6:20)][-train_ind, ]

pred <- knn(train = trainICU, 
            test = testICU, 
            cl = trainICU$patient_addmited_to_intensive_care_unit_1_yes_0_no, 
            k = 1)
tab <- table(pred, testICU$patient_addmited_to_intensive_care_unit_1_yes_0_no)
accuracy(tab)

# KNN for test results
clinical_spectrum_clean <- clinical_spectrum_clean %>%
  mutate(test_result = ifelse(
    clinical_spectrum_clean$sars_cov_2_exam_result == 'negative',
    TRUE,
    FALSE))

train_ind1 <- sample(seq_len(nrow(clinical_spectrum_clean[,c(7:20, 38)])),
                    size = floor(0.75 * nrow(clinical_spectrum_clean[,c(7:20, 38)])))
trainCovid <- clinical_spectrum_clean[,c(7:20, 38)][train_ind1, ]
testCovid <- clinical_spectrum_clean[,c(7:20, 38)][-train_ind1, ]

pred <- knn(train = trainCovid, 
            test = testCovid, 
            cl = trainCovid$test_result, 
            k = 1)
tab1 <- table(pred, testCovid$test_result)
accuracy(tab1)
