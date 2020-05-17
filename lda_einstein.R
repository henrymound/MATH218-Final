### Statistical Learning Final Project

library(tidyverse)
library(class)
library(devtools)
library(MASS)
library(dplyr)
library(caret)
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
            CV = TRUE)
lda1Predict <- predict(lda1,
                       newdata=plotData[,c(2,3)]
                       )


### NOT WORKING WELL, WILL TRY WITH OTHER DATA
