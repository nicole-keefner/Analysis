### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv



## Use *** to search for errors in the code or areas that need more work
## Use END OF CLEANED CODE to search for the end of the updated/cleaned code. If not found, all code is cleaned.



## Set Working Directory
## Install packages



## Load packages
# # First used for model output in HTML table format using function tab_model***
# library(sjPlot)
# First used for aic function, aictab
library(AICcmodavg)
# First used to make figures using function ggplot
library(ggplot2)
# First used for negative binomial modeling using function glm.nb
library(MASS)
# First used to make 3D model using function scatterplot3d
library(scatterplot3d)
# First used to fit >1 ggplot on same window using function grid.arrange
library(gridExtra)



## Cite R software and packages used
# citation()
# citation(package = "AICcmodavg")
# citation(package = "ggplot2")
# citation(package = "MASS")
# citation(package = "scatterplot3d")



## Import variables.csv dataset
variables <- read.csv(file = "variables.csv", header = T)
# Note that year is treated as type integer and is therefore continuous

# 4 targets: richness of corals, sponges, fishes, and combined
# 3 surrogates: percent coral cover, percent sponge cover, rugosity

# Because there are NA's in the Sponge_Richness column, R will be unable to calculate summary values
# Create subset of data called "sponge_complete" that retains only complete cases for Sponge_Richness
sponge_complete <- variables[complete.cases(variables$Sponge_Richness), ]



# Create dataframe with means and SD's for all variables
avg_name <-c("Percent_Coral_Cover", 
             "Percent_Sponge_Cover", 
             "Rugosity", 
             "Coral_Richness", 
             "Sponge_Richness", 
             "Fish_Richness", 
             "Combined_Richness")
avg_mean <-c(round(x = mean(x = variables$Percent_Coral_Cover), digits = 3), 
             round(x = mean(x = variables$Percent_Sponge_Cover), digits = 3), 
             round(x = mean(x = sponge_complete$Rugosity), digits = 3), 
             round(x = mean(x = variables$Coral_Richness), digits = 3), 
             round(x = mean(x = variables$Sponge_Richness), digits = 3), 
             round(x = mean(x = variables$Fish_Richness), digits = 3),  
             round(x = mean(x = sponge_complete$Combined_Richness), digits = 3))
avg_sd <-c(round(x = sd(x = variables$Percent_Coral_Cover), digits = 3), 
           round(x = sd(x = variables$Percent_Sponge_Cover), digits = 3),
           round(x = sd(x = sponge_complete$Rugosity), digits = 3),
           round(x = sd(x = variables$Coral_Richness), digits = 3),
           round(x = sd(x = variables$Sponge_Richness), digits = 3),
           round(x = sd(x = variables$Fish_Richness), digits = 3),
           round(x = sd(x = sponge_complete$Combined_Richness), digits = 3))
avg_df <- data.frame(avg_name, avg_mean, avg_sd)

# Graph frequency distributions of response variables
hist(x = variables$Percent_Coral_Cover, breaks = 25)
hist(x = variables$Percent_Sponge_Cover, breaks = 25)
hist(x = variables$Rugosity, breaks = 25)
hist(x = variables$Coral_Richness, breaks = 25)
hist(x = variables$Sponge_Richness, breaks = 25)
hist(x = variables$Fish_Richness, breaks = 25)
hist(x = variables$Combined_Richness, breaks = 25)



########################################################################



### Objective 1: Create models that only include terms for surrogates 
### in order to determine which of the 3 candidate surrogates is the best at predicting each target. 

# glm.nb produces warning messages because the data is often underdispersed relative to a negative binomial distribution. 
# Poisson might be more appropriate. However, when the mean = variance the NB distribution will provide similar results as the Poisson.
# Negative binomial distribution is used to model count data with overdispersion; richness is count data.
# Negative binomial distribution models and aic tables for each target with one surrogate

# For model names: first word refers to the target, cc represents coral cover, sc represents sponge cover, r represents rugosity
# (e.g. coral_cc is a model with coral cover as the predictor for coral richness)

# For each target, compare models in aic table
coral_cc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
coral_sc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
coral_r = glm.nb(formula = Coral_Richness ~ Rugosity, data = variables)
coral_surrogate <- aictab(cand.set = list(coral_cc, coral_sc, coral_r), modnames = c("coral_cc", "coral_sc", "coral_r"), digits = 4)
sponge_cc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_sc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sponge_r = glm.nb(formula = Sponge_Richness ~ Rugosity, data = variables)
sponge_surrogate <- aictab(cand.set = list(sponge_cc, sponge_sc, sponge_r), modnames = c("sponge_cc", "sponge_sc", "sponge_r"), digits = 4)
fish_cc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_sc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fish_r = glm.nb(formula = Fish_Richness ~ Rugosity, data = variables)
fish_surrogate <- aictab(cand.set = list(fish_cc, fish_sc, fish_r), modnames = c("fish_cc", "fish_sc", "fish_r"), digits = 4)
combined_cc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_sc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
combined_r = glm.nb(formula = Combined_Richness ~ Rugosity, data = variables)
combined_surrogate <- aictab(cand.set = list(combined_cc, combined_sc, combined_r), modnames = c("combined_cc", "combined_sc", "combined_r"), digits = 4)



########################################################################



### Objective 2: Determine if relationships between targets and candidate 
### surrogates remain consistent over space and time.

## No surrogates in these models:
# Year (time)
coral_yr = glm.nb(formula = Coral_Richness ~ Year, data = variables)
sponge_yr = glm.nb(formula = Sponge_Richness ~ Year, data = variables)
fish_yr = glm.nb(formula = Fish_Richness ~ Year, data = variables)
combined_yr = glm.nb(formula = Combined_Richness ~ Year, data = variables)
# Site (space)
coral_site = glm.nb(formula = Coral_Richness ~ Site, data = variables)
sponge_site = glm.nb(formula = Sponge_Richness ~ Site, data = variables)
fish_site = glm.nb(formula = Fish_Richness ~ Site, data = variables)
combined_site = glm.nb(formula = Combined_Richness ~ Site, data = variables)
# Year + Site
coral_yr_site = glm.nb(formula = Coral_Richness ~ Year + Site, data = variables)
sponge_yr_site = glm.nb(formula = Sponge_Richness ~ Year + Site, data = variables)
fish_yr_site = glm.nb(formula = Fish_Richness ~ Year + Site, data = variables)
combined_yr_site = glm.nb(formula = Combined_Richness ~ Year + Site, data = variables)
# Year + Site + YearxSite
coral_yr_site_yrxsite = glm.nb(formula = Coral_Richness ~ Year + Site + Year*Site, data = variables)
sponge_yr_site_yrxsite = glm.nb(formula = Sponge_Richness ~ Year + Site + Year*Site, data = variables)
fish_yr_site_yrxsite = glm.nb(formula = Fish_Richness ~ Year + Site + Year*Site, data = variables)
combined_yr_site_yrxsite = glm.nb(formula = Combined_Richness ~ Year + Site + Year*Site, data = variables)

## These models have surrogates:
# CC + Year (time)
coral_cc_yr = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year, data = variables)
sponge_cc_yr = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Year, data = variables)
fish_cc_yr = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Year, data = variables)
combined_cc_yr = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Year, data = variables)
# SC + Year (time)
coral_sc_yr = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Year, data = variables)
sponge_sc_yr = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year, data = variables)
fish_sc_yr = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Year, data = variables)
combined_sc_yr = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Year, data = variables)
# R + Year (time)
coral_r_yr = glm.nb(formula = Coral_Richness ~ Rugosity + Year, data = variables)
sponge_r_yr = glm.nb(formula = Sponge_Richness ~ Rugosity + Year, data = variables)
fish_r_yr = glm.nb(formula = Fish_Richness ~ Rugosity + Year, data = variables)
combined_r_yr = glm.nb(formula = Combined_Richness ~ Rugosity + Year, data = variables)
# CC + Site (space)
coral_cc_site = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Site, data = variables)
sponge_cc_site = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Site, data = variables)
fish_cc_site = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Site, data = variables)
combined_cc_site = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Site, data = variables)
# SC + Site (space)
coral_sc_site = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Site, data = variables)
sponge_sc_site = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Site, data = variables)
fish_sc_site = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Site, data = variables)
combined_sc_site = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Site, data = variables)
# R + Site (space)
coral_r_site = glm.nb(formula = Coral_Richness ~ Rugosity + Site, data = variables)
sponge_r_site = glm.nb(formula = Sponge_Richness ~ Rugosity + Site, data = variables)
fish_r_site = glm.nb(formula = Fish_Richness ~ Rugosity + Site, data = variables)
combined_r_site = glm.nb(formula = Combined_Richness ~ Rugosity + Site, data = variables)
# CC + Year + Site
coral_cc_yr_site = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
sponge_cc_yr_site = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
fish_cc_yr_site = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
combined_cc_yr_site = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
# SC + Year + Site
coral_sc_yr_site = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
sponge_sc_yr_site = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
fish_sc_yr_site = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
combined_sc_yr_site = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
# R + Year + Site
coral_r_yr_site = glm.nb(formula = Coral_Richness ~ Rugosity + Year + Site, data = variables)
sponge_r_yr_site = glm.nb(formula = Sponge_Richness ~ Rugosity + Year + Site, data = variables)
fish_r_yr_site = glm.nb(formula = Fish_Richness ~ Rugosity + Year + Site, data = variables)
combined_r_yr_site = glm.nb(formula = Combined_Richness ~ Rugosity + Year + Site, data = variables)
# CC + Year + YearxCC
coral_cc_yr_yrxcc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
sponge_cc_yr_yrxcc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
fish_cc_yr_yrxcc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
combined_cc_yr_yrxcc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
# SC + Year + YearxSC
coral_sc_yr_yrxsc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
sponge_sc_yr_yrxsc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
fish_sc_yr_yrxsc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
combined_sc_yr_yrxsc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
# R + Year + YearxR
coral_r_yr_yrxr = glm.nb(formula = Coral_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
sponge_r_yr_yrxr = glm.nb(formula = Sponge_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
fish_r_yr_yrxr = glm.nb(formula = Fish_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
combined_r_yr_yrxr = glm.nb(formula = Combined_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
# CC + Site + SitexCC
coral_cc_site_sitexcc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
sponge_cc_site_sitexcc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
fish_cc_site_sitexcc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
combined_cc_site_sitexcc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
# SC + Site + SitexSC
coral_sc_site_sitexsc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
sponge_sc_site_sitexsc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
fish_sc_site_sitexsc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
combined_sc_site_sitexsc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
# R + Site + SitexR
coral_r_site_sitexr = glm.nb(formula = Coral_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
sponge_r_site_sitexr = glm.nb(formula = Sponge_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
fish_r_site_sitexr = glm.nb(formula = Fish_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
combined_r_site_sitexr = glm.nb(formula = Combined_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
# # The following models are not included in AIC tables or interpretations
# # CC + Year + Site + YearxSite
# coral_cc_yr_site_yrxsite = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# sponge_cc_yr_site_yrxsite = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# fish_cc_yr_site_yrxsite = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# combined_cc_yr_site_yrxsite = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# # SC + Year + Site + YearxSite
# coral_sc_yr_site_yrxsite = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# sponge_sc_yr_site_yrxsite = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# fish_sc_yr_site_yrxsite = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# combined_sc_yr_site_yrxsite = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# # R + Year + Site + YearxSite
# coral_r_yr_site_yrxsite = glm.nb(formula = Coral_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# sponge_r_yr_site_yrxsite = glm.nb(formula = Sponge_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# fish_r_yr_site_yrxsite = glm.nb(formula = Fish_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# combined_r_yr_site_yrxsite = glm.nb(formula = Combined_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)


## AIC tables to evaluate surrogate effectiveness over space and time
# Names of coral cover models in order they are listed for the AIC tables
cc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "cc", "cc_yr", "cc_site", "cc_yr_site",
                 "cc_yr_yrxcc", "cc_site_sitexcc") 
# AIC to compare coral_cc models
coral_cc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site,
                                   coral_cc_yr_yrxcc, coral_cc_site_sitexcc),
                   modnames = cc_modnames, digits = 4)
# AIC to compare sponge_cc models
sponge_cc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                    sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                    sponge_cc_yr_yrxcc, sponge_cc_site_sitexcc),  
                    modnames = cc_modnames, digits = 4)
# AIC to compare fish_cc models
fish_cc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                    fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site,
                                  fish_cc_yr_yrxcc, fish_cc_site_sitexcc),  
                    modnames = cc_modnames, digits = 4)
# AIC to compare combined_cc models
combined_cc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                    combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site,
                                    combined_cc_yr_yrxcc, combined_cc_site_sitexcc), 
                    modnames = cc_modnames, digits = 4)

# Names of sponge cover models in order they are listed for the AIC tables
sc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "sc", "sc_yr", "sc_site", "sc_yr_site",
                 "sc_yr_yrxsc", "sc_site_sitexsc")  
# AIC to compare coral_sc models
coral_sc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_sc, coral_sc_yr, coral_sc_site, coral_sc_yr_site,
                                   coral_sc_yr_yrxsc, coral_sc_site_sitexsc), 
                   modnames = sc_modnames, digits = 4)
# AIC to compare sponge_sc models
sponge_sc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                   sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site,
                                   sponge_sc_yr_yrxsc, sponge_sc_site_sitexsc),    
                   modnames = sc_modnames, digits = 4)
# AIC to compare fish_sc models
fish_sc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                   fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site,
                                  fish_sc_yr_yrxsc, fish_sc_site_sitexsc),   
                   modnames = sc_modnames, digits = 4)
# AIC to compare combined_sc models
combined_sc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                   combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site,
                                   combined_sc_yr_yrxsc, combined_sc_site_sitexsc),   
                   modnames = sc_modnames, digits = 4)

# Names of rugosity models in order they are listed for the AIC tables
r_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "r", "r_yr", "r_site", "r_yr_site",
                "r_yr_yrxr", "r_site_sitexr")  
# AIC to compare coral_r models
coral_r <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_r, coral_r_yr, coral_r_site, coral_r_yr_site,
                                  coral_r_yr_yrxr, coral_r_site_sitexr), 
                   modnames = r_modnames, digits = 4)
# AIC to compare sponge_r models
sponge_r <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                   sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site,
                                   sponge_r_yr_yrxr, sponge_r_site_sitexr),  
                   modnames = r_modnames, digits = 4)
# AIC to compare fish_r models
fish_r <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                   fish_r, fish_r_yr, fish_r_site, fish_r_yr_site,
                                 fish_r_yr_yrxr, fish_r_site_sitexr),  
                   modnames = r_modnames, digits = 4)
# AIC to compare combined_r models
combined_r <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                   combined_r, combined_r_yr, combined_r_site, combined_r_yr_site,
                                   combined_r_yr_yrxr, combined_r_site_sitexr),  
                   modnames = r_modnames, digits = 4)



## Save AIC tables for the top candidate surrogates for each target as .csv files
# write.table(x = coral_cc, file = "coral_cc.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_cc, file = "sponge_cc.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_r, file = "fish_r.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_r, file = "combined_r.csv", sep = ",", col.names = TRUE,row.names = FALSE)



## Model output for competitive models (<2.0 deltaAIC)
# Look at significance of model coefficients
# For coral richness: coral cover as best surrogate
summary(coral_cc_yr)
summary(coral_cc_yr_yrxcc)
# For sponge richness: coral cover as best surrogate
summary(sponge_yr_site)
summary(sponge_cc_yr_site) #cc coefficient not significant
# For fish richness: rugosity as best surrogate
summary(fish_site)
summary(fish_r_site) #r coefficient not significant
summary(fish_yr_site) #yr coefficient not significant
# For combined richness: rugosity as best surrogate
summary(combined_r_yr_site)



########################################################################



### Objective 3: Determine the best predictors of each target given our model set.
obj_three_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", 
                  "cc", "cc_yr", "cc_site", "cc_yr_site",
                  "sc", "sc_yr", "sc_site", "sc_yr_site",
                  "r", "r_yr", "r_site", "r_yr_site",
                  "cc_yr_yrxcc", "cc_site_sitexcc",
                  "sc_yr_yrxsc", "sc_site_sitexsc",
                  "r_yr_yrxr", "r_site_sitexr") 
# *** I'm unsure as to why, but the next 4 AIC tables can only be produced if I close the script,
# ***re-run the code, and DO NOT run any of the AIC tables above
# ***That is why these tables are saved in separate .csv files.
# AIC to compare all coral models
coral_all <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite,
                                    coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site,
                                    coral_sc, coral_sc_yr, coral_sc_site, coral_sc_yr_site,
                                    coral_r, coral_r_yr, coral_r_site, coral_r_yr_site,
                                    coral_cc_yr_yrxcc, coral_cc_site_sitexcc,
                                    coral_sc_yr_yrxsc, coral_sc_site_sitexsc,
                                    coral_r_yr_yrxr, coral_r_site_sitexr),  
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all sponge models
sponge_all <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite,
                                    sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                    sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site,
                                    sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site,
                                    sponge_cc_yr_yrxcc, sponge_cc_site_sitexcc,
                                    sponge_sc_yr_yrxsc, sponge_sc_site_sitexsc,
                                    sponge_r_yr_yrxr, sponge_r_site_sitexr),  
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all fish models
fish_all <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite,
                                    fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site,
                                    fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site,
                                    fish_r, fish_r_yr, fish_r_site, fish_r_yr_site,
                                    fish_cc_yr_yrxcc, fish_cc_site_sitexcc,
                                    fish_sc_yr_yrxsc, fish_sc_site_sitexsc,
                                    fish_r_yr_yrxr, fish_r_site_sitexr),  
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all "combined" models
combined_all <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite,
                                    combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site,
                                    combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site,
                                    combined_r, combined_r_yr, combined_r_site, combined_r_yr_site,
                                    combined_cc_yr_yrxcc, combined_cc_site_sitexcc,
                                    combined_sc_yr_yrxsc, combined_sc_site_sitexsc,
                                    combined_r_yr_yrxr, combined_r_site_sitexr), 
                    modnames = obj_three_modnames, digits = 4)

## Save these AIC tables as .csv files
# write.table(x = coral_all, file = "coral_all.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_all, file = "sponge_all.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_all, file = "fish_all.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_all, file = "combined_all.csv", sep = ",", col.names = TRUE,row.names = FALSE)



## Model output for competitive models when models are included for all surrogates (<2.0 deltaAIC)
# Look at significance of model coefficients
# For coral richness
summary(coral_cc_yr)
summary(coral_cc_yr_yrxcc)
# For sponge richness
summary(sponge_r_yr_site) #r coefficient not significant (0.07)
summary(sponge_yr_site)
summary(sponge_cc_yr_site) #cc coefficient not significant
# For fish richness
summary(fish_site)
summary(fish_r_site) #r coefficient not significant
summary(fish_yr_site) #yr coefficient not significant
summary(fish_cc_site) #cc coefficient not significant
summary(fish_sc_site) #sc coefficient not significant
# For combined richness
# This is different than expected because this model includes coral cover even though 
# rugosity was the top surrogate for combined richness.
summary(combined_cc_yr_site)



########################################################################



## Should I include sur + yr + sur*yr and/or sur + site + sur*site? 
# These would mean that the effect of surrogate on target is different for different yrs (or sites)
# i.e. the slopes of regression lines are different for different sites or that there are 3 dimensions

## Should I include sur + yr + site + yr*site?
# NOT included because at this level of complexity, the surrogate would arguably not be a "simple" way to predict the target 
# This would mean that the effect of time on the target is different for different sites and that
# the surrogate improves the model, but does not modify the effect of time or site on the target.
# This might be the case if different sites experience different conditions over time 
# (e.g. disturbance is more common at one site than another and the level of disturbance changes over time)
# AND that there was additionally variation in the data that could be further explained by the surrogate



########################################################################



## Checking model assumption of negative binomial***
# Negative binomial models assume the conditional means are not equal to the conditional variances.
# This inequality is captured by estimating a dispersion parameter that is held constant in a Poisson model.
# Thus, the Poisson model is actually nested in the negative binomial model. 
# We can then use a likelihood ratio test to compare these two and test this model assumption. 
# To do this, we will run our model as a Poisson.
# (https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/)

# These are the full models in the candidate set:
# coral_cc_yr_site; coral_sc_yr_site; coral_r_yr_site
# sponge_cc_yr_site; sponge_sc_yr_site; sponge_r_yr_site
# fish_cc_yr_site; fish_sc_yr_site; fish_r_yr_site
# combined_cc_yr_site; combined_sc_yr_site; combined_r_yr_site

# Create set of Poisson models for comparison
coral_cc_yr_site_pn <- glm(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Site, family = "poisson", data = variables)
coral_sc_yr_site_pn <- glm(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Site, family = "poisson", data = variables)
coral_r_yr_site_pn <- glm(formula = Coral_Richness ~ Rugosity + Year + Site, family = "poisson", data = variables)
sponge_cc_yr_site_pn <- glm(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Site, family = "poisson", data = variables)
sponge_sc_yr_site_pn <- glm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Site, family = "poisson", data = variables)
sponge_r_yr_site_pn <- glm(formula = Sponge_Richness ~ Rugosity + Year + Site, family = "poisson", data = variables)
fish_cc_yr_site_pn <- glm(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Site, family = "poisson", data = variables)
fish_sc_yr_site_pn <- glm(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Site, family = "poisson", data = variables)
fish_r_yr_site_pn <- glm(formula = Fish_Richness ~ Rugosity + Year + Site, family = "poisson", data = variables)
combined_cc_yr_site_pn <- glm(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Site, family = "poisson", data = variables)
combined_sc_yr_site_pn <- glm(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Site, family = "poisson", data = variables)
combined_r_yr_site_pn <- glm(formula = Combined_Richness ~ Rugosity + Year + Site, family = "poisson", data = variables)


# From "Mixed Effects Models and Extensions in Ecology with R" by, Zuur, Ieno, et al. pg. 236
## For negative binomial
summary(coral_cc_yr_site)
# Drop each term in turn and compare the full model with the nested model using the drop1() command
drop1(coral_cc_yr_site, test = "Chi")
# Set up plot window, so I can view all diagnostic plots simultaneously
op <- par(mfrow = c(2,2))
# Plot residuals
plot(coral_cc_yr_site)
par(op)

## For Poisson
plot(coral_cc_yr_site_pn)

## Residual graphs do not show a clear winner, so compare negative binomial and poisson using likelihood ratio test
# Log-likelihood for negative binomial
llhNB = logLik(coral_cc_yr_site)
# Log-likelihood for Poisson
llhPoisson = logLik(coral_cc_yr_site_pn)
d <- 2 * (llhNB - llhPoisson)
# Determine p-value for chi-square test; multiply by 0.5 p. 238 *** still unsure as to why divide by 2 here
pval <- 0.5 * pchisq(q = as.numeric(x = d), df = 1, lower.tail = FALSE)
# p = 0.5, so there is no significant difference between using the neg. bin. and the Poisson***
# I looked at all 12 comparisons and for all of them, p = 0.5 suggesting there is no significant dif between neg. bin. and Poisson

# Plot residuals of all full models
# None of these models have patterns in the residuals, 
# suggesting the models are appropriate
op <- par(mfrow = c(2,2))
plot(coral_cc_yr_site)
plot(coral_sc_yr_site)
plot(coral_r_yr_site)
plot(sponge_cc_yr_site)
plot(sponge_sc_yr_site)
plot(sponge_r_yr_site)
plot(fish_cc_yr_site)
plot(fish_sc_yr_site)
plot(fish_r_yr_site)
plot(combined_cc_yr_site)
plot(combined_sc_yr_site)
plot(combined_r_yr_site)
par(op)


# ***Following notes in this section from before
#pchisq(2 * (logLik(coral_cc_yr_site) - logLik(coral_cc_yr_site_pn)), df = 1, lower.tail = FALSE)
# 'log Lik.' 1 (df=10)

# Below are the results in the example at https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/ 
# However, I don't understand how they got 926.03...
# ## 'log Lik.' 2.157e-203 (df=5)
# In this example the associated chi-squared value estimated from 2*(logLik(m1) - logLik(m3)) is 926.03 with one degree of freedom. 
# This strongly suggests the negative binomial model, estimating the dispersion parameter, is more appropriate than the Poisson model.



########################################################################



# ## Figures of basic relationships between surrogates (x) and targets (y)
# 
# # Figure 1. Relationship between coral cover and coral richness.
# basic_coral_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) +
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Coral Cover (%)") +
#                     scale_y_continuous(name = "Coral Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# 
# # Figure 2. Relationship between coral cover and sponge richness.
# basic_sponge_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Coral Cover (%)") +
#                     scale_y_continuous(name = "Sponge Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# # Resulting warning message just lets you know that there are years and sites for which these data are unavailable.
# # This does not change the resulting plot except that it will have fewer points than similar plots.
# 
# # Figure 3. Relationship between coral cover and fish richness.
# basic_fish_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
#                   geom_point(size = 3) +
#                   scale_x_continuous(name = "Coral Cover (%)") +
#                   scale_y_continuous(name = "Fish Richness") +
#                   theme(text = element_text(size = 27),
#                         panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                         panel.background = element_blank(),
#                         axis.line = element_line(colour = "black"))
# 
# # Figure 4. Relationship between coral cover and combined richness.
# basic_combined_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Coral Cover (%)") +
#                     scale_y_continuous(name = "Combined Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# # Resulting warning message similar to that from figure 2.
# # Combined richness was only calculated for sites and years when richness was recorded for all 3 groups.
# 
# # Figure 5. Relationship between sponge cover and coral richness.
# basic_coral_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Coral_Richness)) + 
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Sponge Cover (%)") +
#                     scale_y_continuous(name = "Coral Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
#                   
# # Figure 6. Relationship between sponge cover and sponge richness.
# basic_sponge_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Sponge_Richness)) + 
#                       geom_point(size = 3) +
#                       scale_x_continuous(name = "Sponge Cover (%)") +
#                       scale_y_continuous(name = "Sponge Richness") +
#                       theme(text = element_text(size = 27),
#                             panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                             panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                             panel.background = element_blank(),
#                             axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# 
# # Figure 7. Relationship between sponge cover and fish richness.
# basic_fish_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Fish_Richness)) +
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Sponge Cover (%)") +
#                     scale_y_continuous(name = "Fish Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# 
# # Figure 8. Relationship between sponge cover and combined richness.
# basic_combined_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Combined_Richness)) + 
#                         geom_point(size = 3) +
#                         scale_x_continuous(name = "Sponge Cover (%)") +
#                         scale_y_continuous(name = "Combined Richness") +
#                         theme(text = element_text(size = 27),
#                               panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                               panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                               panel.background = element_blank(),
#                               axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# 
# # Figure 9. Relationship between rugosity and coral richness.
# basic_coral_r <- ggplot(data = variables, aes(x = Rugosity, y = Coral_Richness)) + 
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Rugosity") +
#                     scale_y_continuous(name = "Coral Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# 
# # Figure 10. Relationship between rugosity and sponge richness.
# basic_sponge_r <- ggplot(data = variables, aes(x = Rugosity, y = Sponge_Richness)) + 
#                     geom_point(size = 3) +
#                     scale_x_continuous(name = "Rugosity") +
#                     scale_y_continuous(name = "Sponge Richness") +
#                     theme(text = element_text(size = 27),
#                           panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                           panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# 
# # Figure 11. Relationship between rugosity and fish richness.
# basic_fish_r <- ggplot(data = variables, aes(x = Rugosity, y = Fish_Richness)) + 
#                   geom_point(size = 3) +
#                   scale_x_continuous(name = "Rugosity") +
#                   scale_y_continuous(name = "Fish Richness") +
#                   theme(text = element_text(size = 27),
#                         panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                         panel.background = element_blank(),
#                         axis.line = element_line(colour = "black"))
# 
# # Figure 12. Relationship between rugosity and combined richness.
# basic_combined_r <- ggplot(data = variables, aes(x = Rugosity, y = Combined_Richness)) + 
#                       geom_point(size = 3) +
#                       scale_x_continuous(name = "Rugosity") +
#                       scale_y_continuous(name = "Combined Richness") +
#                       theme(text = element_text(size = 27),
#                             panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#                             panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                             panel.background = element_blank(),
#                             axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# 
# # Organize all of these figures into one window for objective 1
# grid.arrange(basic_coral_cc, basic_coral_sc, basic_coral_r,
#              basic_sponge_cc, basic_sponge_sc, basic_sponge_r,
#              basic_fish_cc, basic_fish_sc, basic_fish_r,
#              basic_combined_cc, basic_combined_sc, basic_combined_r, 
#              ncol = 3, nrow = 4)
# 
# 
# 
# ########################################################################
# 
# 
# 
# ## Figures of basic relationships between time (x) and surrogates/targets (y)
# 
# # Figure 13. Relationship between time and coral cover.
# ggplot(data = variables, aes(x = True_Year, y = Percent_Coral_Cover)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Cover (%)") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 14. Relationship between time and sponge cover.
# ggplot(data = variables, aes(x = True_Year, y = Percent_Sponge_Cover)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Cover (%)") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 15. Relationship between time and rugosity.
# ggplot(data = variables, aes(x = True_Year, y = Rugosity)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Rugosity") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 16. Relationship between time and coral richness.
# ggplot(data = variables, aes(x = True_Year, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 17. Relationship between time and sponge richness.
# ggplot(data = variables, aes(x = True_Year, y = Sponge_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# 
# # Figure 18. Relationship between time and fish richness.
# ggplot(data = variables, aes(x = True_Year, y = Fish_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Fish Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 19. Relationship between time and combined richness.
# ggplot(data = variables, aes(x = True_Year, y = Combined_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Combined Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# 
# 
# 
# ########################################################################
# 
# 
# 
# Create new columns for true year and year values of different types for use in figures
# Note that the column True_Year is type integer
variables$True_Year_Factor <- as.factor(x = variables$True_Year)
variables$True_Year_Numeric <- as.numeric(x = variables$True_Year)
variables$Year_Factor <- as.factor(x = variables$Year)

## Figures of basic relationships between time (x) and surrogates/targets (y) by site (legend)

# Create color palette that is friendly to viewers with color blindness (from https://socviz.co/refineplots.html)
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# # Figure 20. Relationship between time and coral cover by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Percent_Coral_Cover, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Cover (%)") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 21. Relationship between time and sponge cover by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Percent_Sponge_Cover, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Cover (%)") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size= 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 22. Relationship between time and rugosity by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Rugosity, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Rugosity") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 23. Relationship between time and coral richness by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Coral_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Richness") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 24. Relationship between time and sponge richness by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Sponge_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# # The gaps are more apparent in these figures because they have lines instead of points.
# 
# # Figure 25. Relationship between time and fish richness by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Fish_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Fish Richness") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 26. Relationship between time and combined richness by site.
# ggplot(data = variables, aes(x = True_Year_Factor, y = Combined_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name = "Time (Years)") +
#   scale_y_continuous(name = "Combined Richness") +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size = 18), 
#         axis.text.x = element_text(angle = +90, hjust = 0),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# # The gaps are more apparent in these figures because they have lines instead of points.
# 
# 
# 
# ########################################################################
# 
# 
# 
# ## Figures of basic relationships between time (x) and surrogates/targets (y) by site (panels)
# 
# # Figure 27. Relationship between time and coral cover with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Percent_Coral_Cover)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Cover (%)") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# 
# # Figure 28. Relationship between time and sponge cover with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Percent_Sponge_Cover)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Cover (%)") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# 
# # Figure 29. Relationship between time and rugosity with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Rugosity)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Rugosity") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# 
# # Figure 30. Relationship between time and coral richness with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Richness") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# 
# # Figure 31. Relationship between time and sponge richness with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Sponge_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# # Resulting warning message same as that from figure 2.
# 
# # Figure 32. Relationship between time and fish richness with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Fish_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Fish Richness") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# 
# # Figure 33. Relationship between time and combined richness with sites distinguished by panels.
# ggplot(data = variables, aes(x = True_Year, y = Combined_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Combined Richness") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(facets = ~ Site)
# # Resulting warning message same as that from figure 4.



########################################################################
#######################CORAL RICHNESS###################################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for coral richness is coral_cc_yr.
# The function of this model is Coral_Richness ~ Percent_Coral_Cover + Year.
# Create year vector from 0-26 with 100 values each 0.2626 apart
Year <- rep(seq(from = min(variables$Year), to = max(variables$Year), length.out = 100))
# Create coral cover vector from 2.676-61.746 with 100 values 0.596 apart
Percent_Coral_Cover <- rep(seq(from = min(variables$Percent_Coral_Cover), to = max(variables$Percent_Coral_Cover), length.out = 100))
# Make a grid using these vectors (for each Year value, list each value of coral cover)
coral_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
# Add a column for predicted coral richness from these values using the coral_cc_yr model
# Fixed previous issue of richness limits by setting type argument to "response"
coral_pred_grid$Predicted_Coral_Richness <-predict(object = coral_cc_yr, newdata = coral_pred_grid, type = "response")
# Save predictive plot in variable
predicted_coral_plot <- scatterplot3d(x = coral_pred_grid$Year, y = coral_pred_grid$Percent_Coral_Cover, z = coral_pred_grid$Predicted_Coral_Richness,
                                      angle = 60,
                                      color = "dodgerblue",
                                      pch = 1,
                                      xlab = "Time (Year)",
                                      ylab = "Coral Cover (%)",
                                      zlab = "Coral Richness" )
# Figure 34. Relationship between coral cover and coral richness and time shown in 3 dimensions. Negative binomial distribution used.
predicted_coral_plot
# Add a column so the plot will have the true values compared to the predicted
#predicted_coral_plot$points3d(x = variables$Year, y = variables$Percent_Coral_Cover, z = variables$Coral_Richness, pch = 16)


# 2D alternative to 3D figure
# Note that I created a new model for the sake of creating this figure
coral_cc_yr_asfactor <- glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year_Factor, data = variables)
coral_predictions_2d <- data.frame(
  Percent_Coral_Cover = rep(seq(from = min(variables$Percent_Coral_Cover), to = max(variables$Percent_Coral_Cover), length.out = 100), 27),
  Year_Factor = factor(rep(1:27, each = 100), levels = 1:27, labels =
                  levels(variables$Year_Factor)))
coral_predictions_2d <- cbind(coral_predictions_2d, predict(object = coral_cc_yr_asfactor, newdata = coral_predictions_2d, type = "link", se.fit = TRUE))
coral_predictions_2d <- within(coral_predictions_2d, {
  Predicted_Coral_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Figure . Predicted relationship between coral cover and predicted coral richness by year. Negative binomial distribution used.
ggplot(data = coral_predictions_2d, aes(x = Percent_Coral_Cover, y = Predicted_Coral_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
  geom_line(aes(color = Year_Factor), size = 2) +
  labs(x = "Coral Cover (%)", y = "Predicted Coral Richness") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Figure . Relationship between coral cover and coral richness by year. Negative binomial distribution used.
ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Year_Factor)) +
  #scale_color_manual(values = cb_palette) +
  theme(text = element_text(size=27), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


########################################################################
#######################SPONGE RICHNESS##################################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for sponge richness is sponge_yr_site.
# The function of this model is Sponge_Richness ~ Year + Site.
# Create new dataframe of predicted values from this model
sponge_predictions <- data.frame(
  Year = rep(seq(from = min(variables$Year), to = max(variables$Year), length.out = 100), 8),
  Site = factor(rep(1:8, each = 100), levels = 1:8, labels =
                  levels(variables$Site)))
sponge_predictions <- cbind(sponge_predictions, predict(object = sponge_yr_site, newdata = sponge_predictions, type = "link", se.fit = TRUE))
sponge_predictions <- within(sponge_predictions, {
  Predicted_Sponge_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Figure 35. Predicted relationship between time and sponge richness by site. Negative binomial distribution used.
ggplot(data = sponge_predictions, aes(x = Year, y = Predicted_Sponge_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Sponge Richness") +
  scale_color_manual(values = cb_palette) +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Subset predictions by site
sponge_predictions$Site <- as.character(sponge_predictions$Site)
sponge_predictions_oneoftwo <- sponge_predictions[sponge_predictions$Site == "pelican" | 
                                                sponge_predictions$Site == "white" | 
                                                sponge_predictions$Site == "crab" | 
                                                sponge_predictions$Site == "bigelow", ]
sponge_predictions_twooftwo <- sponge_predictions[sponge_predictions$Site == "monkey" | 
                                                sponge_predictions$Site == "iguana" | 
                                                sponge_predictions$Site == "grand" | 
                                                sponge_predictions$Site == "muskN", ]
sponge_predictions_oneoftwo$Site <- as.factor(sponge_predictions_oneoftwo$Site)
sponge_predictions_twooftwo$Site <- as.factor(sponge_predictions_twooftwo$Site)
sponge_predictions$Site <- as.factor(sponge_predictions$Site)
#Same as figure 34, but with 4 sites each
ggplot(data = sponge_predictions_oneoftwo, aes(x = Year, y = Predicted_Sponge_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Sponge Richness") +
  scale_y_continuous(limits = c(10, 35)) +
  scale_color_manual(values = cb_palette[1:4]) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
ggplot(data = sponge_predictions_twooftwo, aes(x = Year, y = Predicted_Sponge_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Sponge Richness") +
  scale_y_continuous(limits = c(10, 35)) +
  scale_color_manual(values = cb_palette[5:8]) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
# Figure 36. Relationship between time and sponge richness by site. Negative binomial distribution used.
ggplot(data = variables, aes(x = Year, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name = "Time (Year)") +
  scale_y_continuous(name = "Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Site)) +
  scale_color_manual(values = cb_palette) +
  theme(text = element_text(size=27),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



########################################################################
#######################FISH RICHNESS####################################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for fish richness is fish_site.

# The function of this model is Fish_Richness ~ Site.
# Create new dataframe of predicted values from this model
fish_predictions <- data.frame(
  Year = rep(seq(from = min(variables$Year), to = max(variables$Year), length.out = 100), 8),
  Site = factor(rep(1:8, each = 100), levels = 1:8, labels =
                  levels(variables$Site)))
fish_predictions <- cbind(fish_predictions, predict(object = fish_site, newdata = fish_predictions, type = "link", se.fit = TRUE))
fish_predictions <- within(fish_predictions, {
  Predicted_Fish_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Figure 37. Predicted relationship between time and fish richness by site. All sites included. Negative binomial distribution used.
ggplot(data = fish_predictions, aes(x = Year, y = Predicted_Fish_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Fish Richness") +
  scale_color_manual(values = cb_palette) +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Subset predictions by site
fish_predictions$Site <- as.character(fish_predictions$Site)
fish_predictions_oneoftwo <- fish_predictions[fish_predictions$Site == "pelican" | 
                                                fish_predictions$Site == "white" | 
                                                fish_predictions$Site == "crab" | 
                                                fish_predictions$Site == "iguana", ]
fish_predictions_twooftwo <- fish_predictions[fish_predictions$Site == "monkey" | 
                                                fish_predictions$Site == "bigelow" | 
                                                fish_predictions$Site == "grand" | 
                                                fish_predictions$Site == "muskN", ]
fish_predictions_oneoftwo$Site <- as.factor(fish_predictions_oneoftwo$Site)
fish_predictions_twooftwo$Site <- as.factor(fish_predictions_twooftwo$Site)
fish_predictions$Site <- as.factor(fish_predictions$Site)
#Same as figure 37, but with 4 sites each
ggplot(data = fish_predictions_oneoftwo, aes(x = Year, y = Predicted_Fish_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Fish Richness") +
  scale_y_continuous(limits = c(10, 35)) +
  scale_color_manual(values = cb_palette[1:4]) +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
ggplot(data = fish_predictions_twooftwo, aes(x = Year, y = Predicted_Fish_Richness)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  labs(x = "Time (Year)", y = "Predicted Fish Richness") +
  scale_y_continuous(limits = c(10, 35)) +
  scale_color_manual(values = cb_palette[5:8]) +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Figure 38. Relationship between time and fish richness by site. Negative binomial distribution used.
ggplot(data = variables, aes(x = Year, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name = "Time (Year)") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Site)) +
  scale_color_manual(values = cb_palette) +
  theme(text = element_text(size=27), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



########################################################################
#######################COMBINED RICHNESS 3D#############################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for combined richness is combined_cc_yr_site.
# The function of this model is Combined_Richness ~ Percent_Coral_Cover + Year + Site.

# Because there are NA's in the Combined_Richness column, R will be unable to calculate summary values
# Create subset of data called "combined_complete" that retains only complete cases for Combined_Richness
combined_complete <- variables[complete.cases(variables$Combined_Richness), ]

# Create new dataframes, subsets of the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
Pelican_Ghut <- combined_complete[which(combined_complete$Site == "pelican"),]
Grand_Ghut <- combined_complete[which(combined_complete$Site == "grand"),]
Crab_Cove <- combined_complete[which(combined_complete$Site == "crab"),]
Muskmelon <- combined_complete[which(combined_complete$Site == "muskN"),]
Bigelow <- combined_complete[which(combined_complete$Site == "bigelow"),]
White_Bay <- combined_complete[which(combined_complete$Site == "white"),]
Monkey_Point <- combined_complete[which(combined_complete$Site == "monkey"),]
Guana_Head <- combined_complete[which(combined_complete$Site == "iguana"),]

# Determine ranges of variables used to set consistent axes
summary(variables$Year)
# Ranges from 0-26, so x axes from 0-30
summary(variables$Rugosity)
# Ranges from 17-78, so y axes from 10-80
summary(variables$Combined_Richness)
# Ranges from 39-75, so z axes from 30-80.

# Figure 39. Relationship between coral cover and coral richness and time and site. Negative binomial distribution used.
# Note that the model used to predict does not have site as a term***
par(mfrow = c(3,3))
Year <- seq(from = min(Pelican_Ghut$Year), to = max(Pelican_Ghut$Year), length.out = 100)
Rugosity <- seq(from = min(Pelican_Ghut$Rugosity), to = max(Pelican_Ghut$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[1],
                                         pch = 1,
                                         xlab = "Time (Year) - Pelican Ghut",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(Grand_Ghut$Year), to = max(Grand_Ghut$Year), length.out = 100)
Rugosity <- seq(from = min(Grand_Ghut$Rugosity), to = max(Grand_Ghut$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[2],
                                         pch = 1,
                                         xlab = "Time (Year) - Grand Ghut",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Yeartest <- seq(from = min(Crab_Cove$Year), to = max(Crab_Cove$Year), length.out = 100)
Rugosity <- seq(from = min(Crab_Cove$Rugosity), to = max(Crab_Cove$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[3],
                                         pch = 1,
                                         xlab = "Time (Year) - Crab Cove",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(Muskmelon$Year), to = max(Muskmelon$Year), length.out = 100)
Rugosity <- seq(from = min(Muskmelon$Rugosity), to = max(Muskmelon$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[4],
                                         pch = 1,
                                         xlab = "Time (Year) - Muskmelon",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(Bigelow$Year), to = max(Bigelow$Year), length.out = 100)
Rugosity <- seq(from = min(Bigelow$Rugosity), to = max(Bigelow$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[5],
                                         pch = 1,
                                         xlab = "Time (Year) - Bigelow",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(White_Bay$Year), to = max(White_Bay$Year), length.out = 100)
Rugosity <- seq(from = min(White_Bay$Rugosity), to = max(White_Bay$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[6],
                                         pch = 1,
                                         xlab = "Time (Year) - White Bay",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(Monkey_Point$Year), to = max(Monkey_Point$Year), length.out = 100)
Rugosity <- seq(from = min(Monkey_Point$Rugosity), to = max(Monkey_Point$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[7],
                                         pch = 1,
                                         xlab = "Time (Year) - Monkey Point",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
Year <- seq(from = min(Guana_Head$Year), to = max(Guana_Head$Year), length.out = 100)
Rugosity <- seq(from = min(Guana_Head$Rugosity), to = max(Guana_Head$Rugosity), length.out = 100)
pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
                                         angle = 60,
                                         color = cb_palette[8],
                                         pch = 1,
                                         xlab = "Time (Year) - Guana Head",
                                         ylab = "Rugosity",
                                         zlab = "Combined Richness",
                                         xlim = c(0, 30),
                                         ylim = c(10, 80),
                                         zlim = c(30, 80))
par(mfrow = c(1,1))



########################################################################
#######################COMBINED RICHNESS 2D#############################



# Remove ghost factors
combined_complete$Year_Factor <- as.character(combined_complete$Year_Factor)
combined_complete <- combined_complete[combined_complete$Year_Factor != "0" | combined_complete$Year_Factor != "4" | combined_complete$Year_Factor != "5" |
                                         combined_complete$Year_Factor != "6" | combined_complete$Year_Factor != "7" | combined_complete$Year_Factor != "12", ]
combined_complete$Year_Factor <- as.factor(combined_complete$Year_Factor)
# Order the factor levels for year
combined_complete$Year_Factor <- ordered(combined_complete$Year_Factor, 
                                         levels = c("1", "2", "3", "8", "9", "10", "11", "13", "14", "15", 
                                                    "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"))
# Note that model created with year as factor for graphing purposes
combined_r_yr_asfactor <- glm.nb(formula = Combined_Richness ~ Rugosity + Year_Factor, data = combined_complete)
# Re-create subsets without ghost factors
Pelican_Ghut <- combined_complete[which(combined_complete$Site == "pelican"),]
Grand_Ghut <- combined_complete[which(combined_complete$Site == "grand"),]
Crab_Cove <- combined_complete[which(combined_complete$Site == "crab"),]
Muskmelon <- combined_complete[which(combined_complete$Site == "muskN"),]
Bigelow <- combined_complete[which(combined_complete$Site == "bigelow"),]
White_Bay <- combined_complete[which(combined_complete$Site == "white"),]
Monkey_Point <- combined_complete[which(combined_complete$Site == "monkey"),]
Guana_Head <- combined_complete[which(combined_complete$Site == "iguana"),]



# Pelican_Ghut
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Pelican_Ghut$Rugosity), to = max(Pelican_Ghut$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Pelican_Ghut$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Pelican_Ghut
Pelican_Ghut_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                              geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                              geom_line(aes(color = Year_Factor), size = 2) +
                              labs(x = "Rugosity - Pelican Ghut", y = "Predicted Combined Richness") +
                              scale_x_continuous(limits = c(10, 80)) +
                              scale_y_continuous(limits = c(30, 80)) +
                              theme(text = element_text(size = 18), 
                                    panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                    panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                    panel.background = element_blank(), 
                                    axis.line = element_line(colour = "black")
                                    #legend.position = "none"
                                    )
# Grand_Ghut
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Grand_Ghut$Rugosity), to = max(Grand_Ghut$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Grand_Ghut$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Grand_Ghut
Grand_Ghut_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                            geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                            geom_line(aes(color = Year_Factor), size = 2) +
                            labs(x = "Rugosity - Grand Ghut", y = "Predicted Combined Richness") +
                            scale_x_continuous(limits = c(10, 80)) +
                            scale_y_continuous(limits = c(30, 80)) +
                            theme(text = element_text(size = 18), 
                                  panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                  panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                  panel.background = element_blank(), 
                                  axis.line = element_line(colour = "black")
                                  #legend.position = "none"
                                  )
# Crab_Cove
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Crab_Cove$Rugosity), to = max(Crab_Cove$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Crab_Cove$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Crab_Cove
Crab_Cove_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                            geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                            geom_line(aes(color = Year_Factor), size = 2) +
                            labs(x = "Rugosity - Crab Cove", y = "Predicted Combined Richness") +
                            scale_x_continuous(limits = c(10, 80)) +
                            scale_y_continuous(limits = c(30, 80)) +
                            theme(text = element_text(size = 18), 
                                  panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                  panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                  panel.background = element_blank(), 
                                  axis.line = element_line(colour = "black")
                                  #legend.position = "none"
                                  )
# Muskmelon
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Muskmelon$Rugosity), to = max(Muskmelon$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Muskmelon$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Muskmelon
Muskmelon_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                            geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                            geom_line(aes(color = Year_Factor), size = 2) +
                            labs(x = "Rugosity - Muskmelon", y = "Predicted Combined Richness") +
                            scale_x_continuous(limits = c(10, 80)) +
                            scale_y_continuous(limits = c(30, 80)) +
                            theme(text = element_text(size = 18), 
                                  panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                  panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                  panel.background = element_blank(), 
                                  axis.line = element_line(colour = "black")
                                  #legend.position = "none"
                                  )
# Bigelow
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Bigelow$Rugosity), to = max(Bigelow$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Bigelow$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Bigelow
Bigelow_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                          geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                          geom_line(aes(color = Year_Factor), size = 2) +
                          labs(x = "Rugosity - Bigelow", y = "Predicted Combined Richness") +
                          scale_x_continuous(limits = c(10, 80)) +
                          scale_y_continuous(limits = c(30, 80)) +
                          theme(text = element_text(size = 18), 
                                panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                panel.background = element_blank(), 
                                axis.line = element_line(colour = "black")
                                #legend.position = "none"
                                )
# White_Bay
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(White_Bay$Rugosity), to = max(White_Bay$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(White_Bay$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for White_Bay
White_Bay_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                            geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                            geom_line(aes(color = Year_Factor), size = 2) +
                            labs(x = "Rugosity - White Bay", y = "Predicted Combined Richness") +
                            scale_x_continuous(limits = c(10, 80)) +
                            scale_y_continuous(limits = c(30, 80)) +
                            theme(text = element_text(size = 18), 
                                  panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                  panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                  panel.background = element_blank(), 
                                  axis.line = element_line(colour = "black")
                                  #legend.position = "none"
                                  )
# Monkey_Point
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Monkey_Point$Rugosity), to = max(Monkey_Point$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Monkey_Point$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Monkey_Point
Monkey_Point_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                              geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                              geom_line(aes(color = Year_Factor), size = 2) +
                              labs(x = "Rugosity - Monkey Point", y = "Predicted Combined Richness") +
                              scale_x_continuous(limits = c(10, 80)) +
                              scale_y_continuous(limits = c(30, 80)) +
                              theme(text = element_text(size = 18), 
                                    panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                    panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                    panel.background = element_blank(), 
                                    axis.line = element_line(colour = "black")
                                    #legend.position = "none"
                                    )
# Guana_Head
combined_predictions_2d <- data.frame(
  Rugosity = rep(seq(from = min(Guana_Head$Rugosity), to = max(Guana_Head$Rugosity), length.out = 100), 21),
  Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
                         levels(Guana_Head$Year_Factor)))
combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
combined_predictions_2d <- within(combined_predictions_2d, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
# Plot for Guana_Head
Guana_Head_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
                            geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
                            geom_line(aes(color = Year_Factor), size = 2) +
                            labs(x = "Rugosity - Guana Head", y = "Predicted Combined Richness") +
                            scale_x_continuous(limits = c(10, 80)) +
                            scale_y_continuous(limits = c(30, 80)) +
                            theme(text = element_text(size = 18), 
                                  panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
                                  panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                                  panel.background = element_blank(), 
                                  axis.line = element_line(colour = "black"))

# Organize all of these figures into one window
grid.arrange(Pelican_Ghut_combined_2d, Grand_Ghut_combined_2d, Crab_Cove_combined_2d, 
             Muskmelon_combined_2d, Bigelow_combined_2d, White_Bay_combined_2d, 
             Monkey_Point_combined_2d, Guana_Head_combined_2d, ncol = 3, nrow = 3)
