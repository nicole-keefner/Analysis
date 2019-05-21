### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv



## Use *** to search for errors in the code or areas that need more work
## Use END OF CLEANED CODE to search for the end of the updated/cleaned code



## Set Working Directory
## Install packages



## Load packages
# First used for model output in HTML table format using function tab_model
library(sjPlot)
# First used for aic function, aictab
library(AICcmodavg)
# First used to make figures using function ggplot
library(ggplot2)
# First used for negative binomial modeling using function glm.nb
library(MASS)
# First used to make 3D model using function scatterplot3d
library(scatterplot3d)



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

# These are the most complex models in the candidate set:
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

# ***
pchisq(2 * (logLik(coral_cc_yr_site) - logLik(coral_cc_yr_site_pn)), df = 1, lower.tail = FALSE)
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
# ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Coral Cover (%)") +
#   scale_y_continuous(name = "Coral Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 2. Relationship between coral cover and sponge richness.
# ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Coral Cover (%)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message just lets you know that there are years and sites for which these data are unavailable.
# # This does not change the resulting plot except that it will have fewer points than similar plots.
# 
# # Figure 3. Relationship between coral cover and fish richness.
# ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Coral Cover (%)") +
#   scale_y_continuous(name = "Fish Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 4. Relationship between coral cover and combined richness.
# ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Coral Cover (%)") +
#   scale_y_continuous(name = "Combined Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message similar to that from figure 2.
# # Combined richness was only calculated for sites and years when richness was recorded for all 3 groups.
# 
# # Figure 5. Relationship between sponge cover and coral richness.
# ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Sponge Cover (%)") +
#   scale_y_continuous(name = "Coral Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 6. Relationship between sponge cover and sponge richness.
# ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Sponge_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Sponge Cover (%)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# 
# # Figure 7. Relationship between sponge cover and fish richness.
# ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Fish_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Sponge Cover (%)") +
#   scale_y_continuous(name = "Fish Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 8. Relationship between sponge cover and combined richness.
# ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Combined_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Sponge Cover (%)") +
#   scale_y_continuous(name = "Combined Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# 
# # Figure 9. Relationship between rugosity and coral richness.
# ggplot(data = variables, aes(x = Rugosity, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Rugosity") +
#   scale_y_continuous(name = "Coral Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 10. Relationship between rugosity and sponge richness.
# ggplot(data = variables, aes(x = Rugosity, y = Sponge_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Rugosity") +
#   scale_y_continuous(name = "Sponge Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 2.
# 
# # Figure 11. Relationship between rugosity and fish richness.
# ggplot(data = variables, aes(x = Rugosity, y = Fish_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Rugosity") +
#   scale_y_continuous(name = "Fish Richness") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# 
# # Figure 12. Relationship between rugosity and combined richness.
# ggplot(data = variables, aes(x = Rugosity, y = Combined_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Rugosity") +
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
# Create new columns for true year values of different types for use in figures
# Note that the column True_Year is type integer
variables$True_Year_Factor <- as.factor(x = variables$True_Year)
variables$True_Year_Numeric <- as.numeric(x = variables$True_Year)

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
# Figure 37. Predicted relationship between time and fish richness by site. Negative binomial distribution used.
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
#######################COMBINED RICHNESS################################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for combined richness is combined_cc_yr_site.
# The function of this model is Combined_Richness ~ Percent_Coral_Cover + Year + Site.

# Create new dataframes, subsets of the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
Pelican_Ghut <- variables[which(variables$Site == "pelican"),]
Grand_Ghut <- variables[which(variables$Site == "grand"),]
Crab_Cove <- variables[which(variables$Site == "crab"),]
Muskmelon <- variables[which(variables$Site == "muskN"),]
Bigelow <- variables[which(variables$Site == "bigelow"),]
White_Bay <- variables[which(variables$Site == "white"),]
Monkey_Point <- variables[which(variables$Site == "monkey"),]
Guana_Head <- variables[which(variables$Site == "iguana"),]

# Figure 39. Relationship between coral cover and coral richness and time and site. Negative binomial distribution used.
# ***


par(mfrow = c(3,3))
Year <- rep(seq(from = min(Pelican_Ghut$Year), to = max(Pelican_Ghut$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Pelican_Ghut$Percent_Coral_Cover), to = max(Pelican_Ghut$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                      angle = 60,
                                      color = cb_palette[1],
                                      pch = 1,
                                      xlab = "Time (Year) - Pelican Ghut",
                                      ylab = "Coral Cover (%)",
                                      zlab = "Combined Richness")

## Another way 
# xlim = c(0, 30), 
# ylim = c(0, 70), 
# zlim = c(0, 80)
##One way
#xlim = c(floor(min(variables$Year)), ceiling(max(variables$Year))), 
#ylim = c(floor(min(variables$Percent_Coral_Cover)), ceiling(max(variables$Percent_Coral_Cover))), 
#zlim = c(floor(min(variables$Combined_Richness)), ceiling(max(variables$Combined_Richness))))



Year <- rep(seq(from = min(Grand_Ghut$Year), to = max(Grand_Ghut$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Grand_Ghut$Percent_Coral_Cover), to = max(Grand_Ghut$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[2],
                                         pch = 1,
                                         xlab = "Time (Year) - Grand Ghut",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(Crab_Cove$Year), to = max(Crab_Cove$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Crab_Cove$Percent_Coral_Cover), to = max(Crab_Cove$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[3],
                                         pch = 1,
                                         xlab = "Time (Year) - Crab Cove",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(Muskmelon$Year), to = max(Muskmelon$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Muskmelon$Percent_Coral_Cover), to = max(Muskmelon$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[4],
                                         pch = 1,
                                         xlab = "Time (Year) - Muskmelon",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(Bigelow$Year), to = max(Bigelow$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Bigelow$Percent_Coral_Cover), to = max(Bigelow$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[5],
                                         pch = 1,
                                         xlab = "Time (Year)",
                                         ylab = "Coral Cover (%) - Bigelow",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(White_Bay$Year), to = max(White_Bay$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(White_Bay$Percent_Coral_Cover), to = max(White_Bay$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[6],
                                         pch = 1,
                                         xlab = "Time (Year) - White Bay",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(Monkey_Point$Year), to = max(Monkey_Point$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Monkey_Point$Percent_Coral_Cover), to = max(Monkey_Point$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[7],
                                         pch = 1,
                                         xlab = "Time (Year) - Monkey Point",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
Year <- rep(seq(from = min(Guana_Head$Year), to = max(Guana_Head$Year), length.out = 100))
Percent_Coral_Cover <- rep(seq(from = min(Guana_Head$Percent_Coral_Cover), to = max(Guana_Head$Percent_Coral_Cover), length.out = 100))
combined_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
combined_pred_grid$Predicted_Combined_Richness <-predict(object = combined_cc_yr, newdata = combined_pred_grid, type = "response")
predicted_combined_plot <- scatterplot3d(x = combined_pred_grid$Year, y = combined_pred_grid$Percent_Coral_Cover, z = combined_pred_grid$Predicted_Coral_Richness,
                                         angle = 60,
                                         color = cb_palette[8],
                                         pch = 1,
                                         xlab = "Time (Year) - Guana Head",
                                         ylab = "Coral Cover (%)",
                                         zlab = "Combined Richness")
par(mfrow = c(1,1))


########################################################################