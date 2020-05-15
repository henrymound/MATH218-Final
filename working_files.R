### Statistical Learning Final Project
# Challenge: https://www.kaggle.com/roche-data-science-coalition/uncover

library(tidyverse)


#Investigation Question
## Which patient populations pass away from COVID-19?
  
#Task Details:
#The Roche Data Science Coalition is a group of like-minded public
# and private organizations with a common mission and vision to bring
# actionable intelligence to patients, frontline healthcare providers,
# institutions, supply chains, and government. The tasks associated with
# this dataset were developed and evaluated by global frontline healthcare
# providers, hospitals, suppliers, and policy makers. They represent key
# research questions where insights developed by the Kaggle community can be
# most impactful in the areas of at-risk population evaluation and capacity management.

#Evaluation
#One submission will be identified as the best response to the research question
# posed in this task on the timeline outlined below. That submission will be
# marked as the “accepted solution” to that task, and will be reevaluated
# by the next deadline against the new research contributed to that task.

# Submissions will be reviewed on a rolling basis, so participants are 
# encouraged to work publicly and collaboratively to accelerate the research
# available for each task.

clinical_spectrum <- read_csv('clinical-spectrum.csv')
esri_covid <- read_csv('uncover/esri_covid-19/esri_covid-19/cdcs-social-vulnerability-index-svi-2016-overall-svi-census-tract-level.csv')
jh <- read_csv('uncover/johns_hopkins_csse/2019-novel-coronavirus-covid-19-2019-ncov-data-repository-confirmed-deaths-in-the-us.csv')
head(esri_covid)
head(jh)






