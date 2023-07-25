#########################
###Reference screening###
#########################

library(tidyverse)
library(ggplot2)

######LOAD DATA######

#Load GIS data

#Load habitat data
phab_df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_07252023.csv")

#Load bio data, for assessing sensitivity
arthro_df<-read_csv("Data/BioData/Arthros/arthropod_metircs_07252023.csv")

#####ASSESS BIO RESPONSES TO STRESS#####

#Calculate Spearman's Rho between all bio metrics and all hab/gis metrics [stressors only]
#Create scatterplots of strong relationships

##Sepeicifically, look at phab stress measures
#How correlated are they to eachother?
#Look at hab stress metrics and identify appropriate cutoffs through visual inspections of relationships
#Can you pick the "best" one?
#How concordant are they based on cutoffs?
#Is one more sensitive than the others?

####SCREEN

#Start with Ode et al. 2016 screens
#Replace w1_hall with best phab metric(s)
#Adjust gis screens if necessary based on observed relationships with biology


