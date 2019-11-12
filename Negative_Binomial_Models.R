### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv



## Use *** to search for errors in the code or areas that need more work
## Use END OF CLEANED CODE to search for the end of the updated/cleaned code. If not found, all code is cleaned.



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
# The code that uses this package is commented out.
#library(scatterplot3d)
# First used to fit >1 ggplot on same window using function grid.arrange
library(gridExtra)



## Cite R software and packages used
# citation()
# citation(package = "AICcmodavg")
# citation(package = "ggplot2")
# citation(package = "MASS")
## citation(package = "scatterplot3d")
# citation(package = "gridExtra")



## Import variables.csv dataset
variables <- read.csv(file = "variables.csv", header = T)
# Note that year is treated as type integer and is therefore continuous

# 4 targets: richness of corals, sponges, fishes, and combined
# 3 surrogates: percent coral cover, percent sponge cover, rugosity

# Because there are NA's in the Sponge_Richness column, R will be unable to calculate summary values
# Create subset of data called "sponge_complete" that retains only complete cases for Sponge_Richness
sponge_complete <- variables[complete.cases(variables$Sponge_Richness), ]
# Create new column in this dataset with year as a factor
sponge_complete$Year_Factor <- as.factor(sponge_complete$Year)
# Order this new year_factor column
sponge_complete$Year_Factor <- factor(x = sponge_complete$Year_Factor, levels = c("1", "2", "3", "8", "9", "10", "11", "13", "14", 
                                                                              "15", "16", "17", "18", "19", "20", "21", "22", 
                                                                              "23", "24", "25", "26"), ordered = TRUE)


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


# Create dataframe with means and SD's for all variables
# ***include this table in paper but specify that sponge richness and combined richnesses were calculated using complete cases
avg_name <-c("Percent_Coral_Cover", 
             "Percent_Sponge_Cover", 
             "Rugosity", 
             "Coral_Richness", 
             "Sponge_Richness", 
             "Fish_Richness", 
             "Combined_Richness")
avg_mean <-c(round(x = mean(x = variables$Percent_Coral_Cover), digits = 3), 
             round(x = mean(x = variables$Percent_Sponge_Cover), digits = 3), 
             round(x = mean(x = variables$Rugosity), digits = 3), 
             round(x = mean(x = variables$Coral_Richness), digits = 3), 
             round(x = mean(x = sponge_complete$Sponge_Richness), digits = 3), 
             round(x = mean(x = variables$Fish_Richness), digits = 3),  
             round(x = mean(x = sponge_complete$Combined_Richness), digits = 3))
avg_sd <-c(round(x = sd(x = variables$Percent_Coral_Cover), digits = 3), 
           round(x = sd(x = variables$Percent_Sponge_Cover), digits = 3),
           round(x = sd(x = variables$Rugosity), digits = 3),
           round(x = sd(x = variables$Coral_Richness), digits = 3),
           round(x = sd(x = sponge_complete$Sponge_Richness), digits = 3),
           round(x = sd(x = variables$Fish_Richness), digits = 3),
           round(x = sd(x = sponge_complete$Combined_Richness), digits = 3))
avg_df <- data.frame(avg_name, avg_mean, avg_sd)

# Graph frequency distributions of variables
hist(x = variables$Percent_Coral_Cover, breaks = 25)
hist(x = variables$Percent_Sponge_Cover, breaks = 25)
hist(x = variables$Rugosity, breaks = 25)
hist(x = variables$Coral_Richness, breaks = 25)
hist(x = variables$Sponge_Richness, breaks = 25)
hist(x = variables$Fish_Richness, breaks = 25)
hist(x = variables$Combined_Richness, breaks = 25)
# ***standardize figure formatting if histograms used in publication



########################################################################



### Objective 1: Create models that only include terms for surrogates 
### in order to determine which of the 3 candidate surrogates is the best at predicting each target. 

# glm.nb produces warning messages because the data is often underdispersed relative to a negative binomial distribution. 
# Poisson might be more appropriate. However, when the mean = variance the NB distribution will provide similar results as the Poisson.
# Negative binomial distribution is used to model count data with overdispersion; richness is count data. ***explain in methods
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

## Save AIC tables for determining the top surrogate for each target as .csv files
# write.table(x = coral_surrogate, file = "coral_surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_surrogate, file = "sponge_surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_surrogate, file = "fish_surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_surrogate, file = "combined_surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)

# HTML formatting to get the Nagelkerke's pseudo-r-squared for surrogate-only models
tab_model(coral_sc)
tab_model(coral_cc)
tab_model(coral_r)
tab_model(sponge_cc)
tab_model(sponge_sc)
tab_model(sponge_r)
tab_model(fish_r)
tab_model(fish_cc)
tab_model(fish_sc)
tab_model(combined_r)
tab_model(combined_cc)
tab_model(combined_sc)
# Values in order: 0.032; 0.619; 0.232; 0.283; 0.236; 0.053; 0.625; 0.529; 0.214; 0.383; 0.316; 0.008
# Create dataframe of pseudo-r-squared values
pseudo_basic_models <- c("coral_sc", "coral_cc", "coral_r", "sponge_cc", "sponge_sc", "sponge_r", "fish_r", "fish_cc", "fish_sc", "combined_r", "combined_cc", "combined_sc")
pseudo_basic_values <- c(0.032, 0.619, 0.232, 0.283, 0.236, 0.053, 0.625, 0.529, 0.214, 0.383, 0.316, 0.008)
table_pseudo_basic_values <- data.frame(pseudo_basic_models, pseudo_basic_values)
# HTML formatting to get the Nagelkerke's pseudo-r-squared for competitive models
tab_model(coral_cc_yr)
tab_model(coral_cc_yr_yrxcc)
tab_model(sponge_cc_yr_site)
tab_model(fish_r_site)
tab_model(fish_r_yr_site)
tab_model(combined_r_yr_site)
# Values in order: 0.032; 0.619; 0.232; 0.283; 0.236; 0.053; 0.625; 0.529; 0.214; 0.383; 0.316; 0.008
# Create dataframe of pseudo-r-squared values
pseudo_comp_models <- c("coral_cc_yr", "coral_cc_yr_yrxcc", "sponge_cc_yr_site", "fish_r_site", "fish_r_yr_site", "combined_r_yr_site")
pseudo_comp_values <- c(0.688, 0.690, 0.712, 0.824, 0.824, 0.649)
table_pseudo_comp_values <- data.frame(pseudo_comp_models, pseudo_comp_values)



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
# # These models include terms for each surrogate, site, year, and an interaction term for sitexyear
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


## AIC tables to evaluate surrogate effectiveness over space and time for models WITH and WITHOUT surrogates
# *** It's important to note that these AIC tables have the same names as the models themselves (e.g. coral_cc)
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



## Save AIC tables for the top candidate surrogates for each target as .csv files (models with and without surrogates)
# write.table(x = coral_cc, file = "coral_cc.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_cc, file = "sponge_cc.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_r, file = "fish_r.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_r, file = "combined_r.csv", sep = ",", col.names = TRUE,row.names = FALSE)


## AIC tables to evaluate surrogate effectiveness over space and time for models ONLY with surrogates
# Names of coral cover models in order they are listed for the AIC tables
cc_modnames_surrogate_only <- c("cc", "cc_yr", "cc_site", "cc_yr_site", "cc_yr_yrxcc", "cc_site_sitexcc") 
# AIC to compare coral_cc models
coral_cc_surrogate_only <- aictab(cand.set = list(coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site,
                                   coral_cc_yr_yrxcc, coral_cc_site_sitexcc),
                   modnames = cc_modnames_surrogate_only, digits = 4)
# AIC to compare sponge_cc models
sponge_cc_surrogate_only <- aictab(cand.set = list(sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                    sponge_cc_yr_yrxcc, sponge_cc_site_sitexcc),  
                    modnames = cc_modnames_surrogate_only, digits = 4)
# Names of rugosity models in order they are listed for the AIC tables
r_modnames_surrogate_only <- c("r", "r_yr", "r_site", "r_yr_site",
                "r_yr_yrxr", "r_site_sitexr")  
# AIC to compare fish_r models
fish_r_surrogate_only <- aictab(cand.set = list(fish_r, fish_r_yr, fish_r_site, fish_r_yr_site,
                                 fish_r_yr_yrxr, fish_r_site_sitexr),  
                 modnames = r_modnames_surrogate_only, digits = 4)
# AIC to compare combined_r models
combined_r_surrogate_only <- aictab(cand.set = list(combined_r, combined_r_yr, combined_r_site, combined_r_yr_site,
                                     combined_r_yr_yrxr, combined_r_site_sitexr),  
                     modnames = r_modnames_surrogate_only, digits = 4)
## Save AIC tables for the top candidate surrogates for each target as .csv files (models ONLY with surrogates)
# write.table(x = coral_cc_surrogate_only, file = "coral_cc_Only_Models_With_Surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_cc_surrogate_only, file = "sponge_cc_Only_Models_With_Surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_r_surrogate_only, file = "fish_r_Only_Models_With_Surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_r_surrogate_only, file = "combined_r_Only_Models_With_Surrogate.csv", sep = ",", col.names = TRUE,row.names = FALSE)



## Model output for competitive models (<2.0 deltaAIC)
# ***mention these outputs in results
# Look at significance of model coefficients
# For coral richness: coral cover as best surrogate
summary(coral_cc_yr)
summary(coral_cc_yr_yrxcc)
# For sponge richness: coral cover as best surrogate
summary(sponge_yr_site) # no surrogate
summary(sponge_cc_yr_site) #cc coefficient not significant
# For fish richness: rugosity as best surrogate
summary(fish_site) # no surrogate
summary(fish_r_site) #r coefficient not significant
summary(fish_yr_site) #yr coefficient not significant; no surrogate
# For combined richness: rugosity as best surrogate
summary(combined_r_yr_site)



########################################################################



# *** This info is only relevant for appendix
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



## Should I include sur + yr + sur*yr and/or sur + site + sur*site? ***
# These would mean that the effect of surrogate on target is different for different yrs (or sites)
# i.e. the slopes of regression lines are different for different sites or that there are 3 dimensions

## Should I include sur + yr + site + yr*site? ***
# NOT included because at this level of complexity, the surrogate would arguably not be a "simple" way to predict the target 
# This would mean that the effect of time on the target is different for different sites and that
# the surrogate improves the model, but does not modify the effect of time or site on the target.
# This might be the case if different sites experience different conditions over time 
# (e.g. disturbance is more common at one site than another and the level of disturbance changes over time)
# AND that there was additionally variation in the data that could be further explained by the surrogate



########################################################################

# Double-check Model Assumptions 11/10/19
# From "Mixed Effects Models and Extensions in Ecology with R" by, Zuur, Ieno, et al. pg. 231
# Overdispersion is when variance > mean (use negative binomial); variance = mean (use Poisson)
# There shouldn't be patterns in the deviance or Pearson residuals.
# Variables checked variables$Year, Site, Percent_Coral_Cover, Percent_Sponge_Cover, Rugosity, each of the 4 richnesses
# When you re-run with Poisson, there is not really a difference
#model <- glm(Coral_Richness ~ Percent_Coral_Cover, family = "poisson", data = variables)
# There should be no patterns in these residuals.


#cr_cc
cr_cc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
cr_cc_pearson <- residuals(object = cr_cc, type = "pearson")
cr_cc_deviance <- residuals(object = cr_cc, type = "deviance")
cr_cc_mu <- predict(cr_cc, type = "response")
cr_cc_response <- variables$Coral_Richness - cr_cc_mu
cr_cc_scaled <- cr_cc_response / sqrt(7.630148 * cr_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_cc_mu, y = cr_cc_response, main = "Response residuals")
plot(x = cr_cc_mu, y = cr_cc_pearson, main = "Pearson residuals")
plot(x = cr_cc_mu, y = cr_cc_scaled, main = "Pearson residuals scaled")
plot(x = cr_cc_mu, y = cr_cc_deviance, main = "Deviance residuals")
par(op)

#cr_sc
cr_sc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
cr_sc_pearson <- residuals(object = cr_sc, type = "pearson")
cr_sc_deviance <- residuals(object = cr_sc, type = "deviance")
cr_sc_mu <- predict(cr_sc, type = "response")
cr_sc_response <- variables$Coral_Richness - cr_sc_mu
cr_sc_scaled <- cr_sc_response / sqrt(7.630148 * cr_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_sc_mu, y = cr_sc_response, main = "Response residuals")
plot(x = cr_sc_mu, y = cr_sc_pearson, main = "Pearson residuals")
plot(x = cr_sc_mu, y = cr_sc_scaled, main = "Pearson residuals scaled")
plot(x = cr_sc_mu, y = cr_sc_deviance, main = "Deviance residuals")
par(op)

#cr_ru
cr_ru = glm.nb(formula = Coral_Richness ~ Rugosity, data = variables)
cr_ru_pearson <- residuals(object = cr_ru, type = "pearson")
cr_ru_deviance <- residuals(object = cr_ru, type = "deviance")
cr_ru_mu <- predict(cr_ru, type = "response")
cr_ru_response <- variables$Coral_Richness - cr_ru_mu
cr_ru_scaled <- cr_ru_response / sqrt(7.630148 * cr_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_ru_mu, y = cr_ru_response, main = "Response residuals")
plot(x = cr_ru_mu, y = cr_ru_pearson, main = "Pearson residuals")
plot(x = cr_ru_mu, y = cr_ru_scaled, main = "Pearson residuals scaled")
plot(x = cr_ru_mu, y = cr_ru_deviance, main = "Deviance residuals")
par(op)

#cr_yr
cr_yr = glm.nb(formula = Coral_Richness ~ Year, data = variables)
cr_yr_pearson <- residuals(object = cr_yr, type = "pearson")
cr_yr_deviance <- residuals(object = cr_yr, type = "deviance")
cr_yr_mu <- predict(cr_yr, type = "response")
cr_yr_response <- variables$Coral_Richness - cr_yr_mu
cr_yr_scaled <- cr_yr_response / sqrt(7.630148 * cr_yr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_yr_mu, y = cr_yr_response, main = "Response residuals")
plot(x = cr_yr_mu, y = cr_yr_pearson, main = "Pearson residuals")
plot(x = cr_yr_mu, y = cr_yr_scaled, main = "Pearson residuals scaled")
plot(x = cr_yr_mu, y = cr_yr_deviance, main = "Deviance residuals")
par(op)

#cr_st
cr_st = glm.nb(formula = Coral_Richness ~ Site, data = variables)
cr_st_pearson <- residuals(object = cr_st, type = "pearson")
cr_st_deviance <- residuals(object = cr_st, type = "deviance")
cr_st_mu <- predict(cr_st, type = "response")
cr_st_response <- variables$Coral_Richness - cr_st_mu
cr_st_scaled <- cr_st_response / sqrt(7.630148 * cr_st_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_st_mu, y = cr_st_response, main = "Response residuals")
plot(x = cr_st_mu, y = cr_st_pearson, main = "Pearson residuals")
plot(x = cr_st_mu, y = cr_st_scaled, main = "Pearson residuals scaled")
plot(x = cr_st_mu, y = cr_st_deviance, main = "Deviance residuals")
par(op)

#cr_sr
cr_sr = glm.nb(formula = Coral_Richness ~ Sponge_Richness, data = variables)
cr_sr_pearson <- residuals(object = cr_sr, type = "pearson")
cr_sr_deviance <- residuals(object = cr_sr, type = "deviance")
cr_sr_mu <- predict(cr_sr, type = "response")
cr_sr_response <- variables$Coral_Richness - cr_sr_mu
cr_sr_scaled <- cr_sr_response / sqrt(7.630148 * cr_sr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_sr_mu, y = cr_sr_response, main = "Response residuals")
plot(x = cr_sr_mu, y = cr_sr_pearson, main = "Pearson residuals")
plot(x = cr_sr_mu, y = cr_sr_scaled, main = "Pearson residuals scaled")
plot(x = cr_sr_mu, y = cr_sr_deviance, main = "Deviance residuals")
par(op)

#cr_fr
cr_fr = glm.nb(formula = Coral_Richness ~ Fish_Richness, data = variables)
cr_fr_pearson <- residuals(object = cr_fr, type = "pearson")
cr_fr_deviance <- residuals(object = cr_fr, type = "deviance")
cr_fr_mu <- predict(cr_fr, type = "response")
cr_fr_response <- variables$Coral_Richness - cr_fr_mu
cr_fr_scaled <- cr_fr_response / sqrt(7.630148 * cr_fr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_fr_mu, y = cr_fr_response, main = "Response residuals")
plot(x = cr_fr_mu, y = cr_fr_pearson, main = "Pearson residuals")
plot(x = cr_fr_mu, y = cr_fr_scaled, main = "Pearson residuals scaled")
plot(x = cr_fr_mu, y = cr_fr_deviance, main = "Deviance residuals")
par(op)

#cr_cd
cr_cd = glm.nb(formula = Coral_Richness ~ Combined_Richness, data = variables)
cr_cd_pearson <- residuals(object = cr_cd, type = "pearson")
cr_cd_deviance <- residuals(object = cr_cd, type = "deviance")
cr_cd_mu <- predict(cr_cd, type = "response")
cr_cd_response <- variables$Coral_Richness - cr_cd_mu
cr_cd_scaled <- cr_cd_response / sqrt(7.630148 * cr_cd_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cr_cd_mu, y = cr_cd_response, main = "Response residuals")
plot(x = cr_cd_mu, y = cr_cd_pearson, main = "Pearson residuals")
plot(x = cr_cd_mu, y = cr_cd_scaled, main = "Pearson residuals scaled")
plot(x = cr_cd_mu, y = cr_cd_deviance, main = "Deviance residuals")
par(op)

#sr_cc
sr_cc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sr_cc_pearson <- residuals(object = sr_cc, type = "pearson")
sr_cc_deviance <- residuals(object = sr_cc, type = "deviance")
sr_cc_mu <- predict(sr_cc, type = "response")
sr_cc_response <- variables$Sponge_Richness - sr_cc_mu
sr_cc_scaled <- sr_cc_response / sqrt(7.630148 * sr_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.***
op <- par(mfrow = c(2, 2))
plot(x = sr_cc_mu, y = sr_cc_response, main = "Response residuals")
plot(x = sr_cc_mu, y = sr_cc_pearson, main = "Pearson residuals")
plot(x = sr_cc_mu, y = sr_cc_scaled, main = "Pearson residuals scaled")
plot(x = sr_cc_mu, y = sr_cc_deviance, main = "Deviance residuals")
par(op)
#***Note that the successful output here does not include response residuals or scaled pearson residuals due to the missing years

#sr_sc
sr_sc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sr_sc_pearson <- residuals(object = sr_sc, type = "pearson")
sr_sc_deviance <- residuals(object = sr_sc, type = "deviance")
sr_sc_mu <- predict(sr_sc, type = "response")
sr_sc_response <- variables$Sponge_Richness - sr_sc_mu
sr_sc_scaled <- sr_sc_response / sqrt(7.630148 * sr_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_sc_mu, y = sr_sc_response, main = "Response residuals")
plot(x = sr_sc_mu, y = sr_sc_pearson, main = "Pearson residuals")
plot(x = sr_sc_mu, y = sr_sc_scaled, main = "Pearson residuals scaled")
plot(x = sr_sc_mu, y = sr_sc_deviance, main = "Deviance residuals")
par(op)

#sr_ru
sr_ru = glm.nb(formula = Sponge_Richness ~ Rugosity, data = variables)
sr_ru_pearson <- residuals(object = sr_ru, type = "pearson")
sr_ru_deviance <- residuals(object = sr_ru, type = "deviance")
sr_ru_mu <- predict(sr_ru, type = "response")
sr_ru_response <- variables$Sponge_Richness - sr_ru_mu
sr_ru_scaled <- sr_ru_response / sqrt(7.630148 * sr_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_ru_mu, y = sr_ru_response, main = "Response residuals")
plot(x = sr_ru_mu, y = sr_ru_pearson, main = "Pearson residuals")
plot(x = sr_ru_mu, y = sr_ru_scaled, main = "Pearson residuals scaled")
plot(x = sr_ru_mu, y = sr_ru_deviance, main = "Deviance residuals")
par(op)

#sr_yr
sr_yr = glm.nb(formula = Sponge_Richness ~ Year, data = variables)
sr_yr_pearson <- residuals(object = sr_yr, type = "pearson")
sr_yr_deviance <- residuals(object = sr_yr, type = "deviance")
sr_yr_mu <- predict(sr_yr, type = "response")
sr_yr_response <- variables$Sponge_Richness - sr_yr_mu
sr_yr_scaled <- sr_yr_response / sqrt(7.630148 * sr_yr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_yr_mu, y = sr_yr_response, main = "Response residuals")
plot(x = sr_yr_mu, y = sr_yr_pearson, main = "Pearson residuals")
plot(x = sr_yr_mu, y = sr_yr_scaled, main = "Pearson residuals scaled")
plot(x = sr_yr_mu, y = sr_yr_deviance, main = "Deviance residuals")
par(op)

#sr_st
sr_st = glm.nb(formula = Sponge_Richness ~ Site, data = variables)
sr_st_pearson <- residuals(object = sr_st, type = "pearson")
sr_st_deviance <- residuals(object = sr_st, type = "deviance")
sr_st_mu <- predict(sr_st, type = "response")
sr_st_response <- variables$Sponge_Richness - sr_st_mu
sr_st_scaled <- sr_st_response / sqrt(7.630148 * sr_st_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_st_mu, y = sr_st_response, main = "Response residuals")
plot(x = sr_st_mu, y = sr_st_pearson, main = "Pearson residuals")
plot(x = sr_st_mu, y = sr_st_scaled, main = "Pearson residuals scaled")
plot(x = sr_st_mu, y = sr_st_deviance, main = "Deviance residuals")
par(op)

#sr_fr
sr_fr = glm.nb(formula = Sponge_Richness ~ Fish_Richness, data = variables)
sr_fr_pearson <- residuals(object = sr_fr, type = "pearson")
sr_fr_deviance <- residuals(object = sr_fr, type = "deviance")
sr_fr_mu <- predict(sr_fr, type = "response")
sr_fr_response <- variables$Sponge_Richness - sr_fr_mu
sr_fr_scaled <- sr_fr_response / sqrt(7.630148 * sr_fr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_fr_mu, y = sr_fr_response, main = "Response residuals")
plot(x = sr_fr_mu, y = sr_fr_pearson, main = "Pearson residuals")
plot(x = sr_fr_mu, y = sr_fr_scaled, main = "Pearson residuals scaled")
plot(x = sr_fr_mu, y = sr_fr_deviance, main = "Deviance residuals")
par(op)

#sr_cd
sr_cd = glm.nb(formula = Sponge_Richness ~ Combined_Richness, data = variables)
sr_cd_pearson <- residuals(object = sr_cd, type = "pearson")
sr_cd_deviance <- residuals(object = sr_cd, type = "deviance")
sr_cd_mu <- predict(sr_cd, type = "response")
sr_cd_response <- variables$Sponge_Richness - sr_cd_mu
sr_cd_scaled <- sr_cd_response / sqrt(7.630148 * sr_cd_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sr_cd_mu, y = sr_cd_response, main = "Response residuals")
plot(x = sr_cd_mu, y = sr_cd_pearson, main = "Pearson residuals")
plot(x = sr_cd_mu, y = sr_cd_scaled, main = "Pearson residuals scaled")
plot(x = sr_cd_mu, y = sr_cd_deviance, main = "Deviance residuals")
par(op)

#fr_cc
fr_cc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fr_cc_pearson <- residuals(object = fr_cc, type = "pearson")
fr_cc_deviance <- residuals(object = fr_cc, type = "deviance")
fr_cc_mu <- predict(fr_cc, type = "response")
fr_cc_response <- variables$Fish_Richness - fr_cc_mu
fr_cc_scaled <- fr_cc_response / sqrt(7.630148 * fr_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_cc_mu, y = fr_cc_response, main = "Response residuals")
plot(x = fr_cc_mu, y = fr_cc_pearson, main = "Pearson residuals")
plot(x = fr_cc_mu, y = fr_cc_scaled, main = "Pearson residuals scaled")
plot(x = fr_cc_mu, y = fr_cc_deviance, main = "Deviance residuals")
par(op)

#fr_sc
fr_sc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fr_sc_pearson <- residuals(object = fr_sc, type = "pearson")
fr_sc_deviance <- residuals(object = fr_sc, type = "deviance")
fr_sc_mu <- predict(fr_sc, type = "response")
fr_sc_response <- variables$Fish_Richness - fr_sc_mu
fr_sc_scaled <- fr_sc_response / sqrt(7.630148 * fr_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_sc_mu, y = fr_sc_response, main = "Response residuals")
plot(x = fr_sc_mu, y = fr_sc_pearson, main = "Pearson residuals")
plot(x = fr_sc_mu, y = fr_sc_scaled, main = "Pearson residuals scaled")
plot(x = fr_sc_mu, y = fr_sc_deviance, main = "Deviance residuals")
par(op)

#fr_ru
fr_ru = glm.nb(formula = Fish_Richness ~ Rugosity, data = variables)
fr_ru_pearson <- residuals(object = fr_ru, type = "pearson")
fr_ru_deviance <- residuals(object = fr_ru, type = "deviance")
fr_ru_mu <- predict(fr_ru, type = "response")
fr_ru_response <- variables$Fish_Richness - fr_ru_mu
fr_ru_scaled <- fr_ru_response / sqrt(7.630148 * fr_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_ru_mu, y = fr_ru_response, main = "Response residuals")
plot(x = fr_ru_mu, y = fr_ru_pearson, main = "Pearson residuals")
plot(x = fr_ru_mu, y = fr_ru_scaled, main = "Pearson residuals scaled")
plot(x = fr_ru_mu, y = fr_ru_deviance, main = "Deviance residuals")
par(op)

#fr_yr
fr_yr = glm.nb(formula = Fish_Richness ~ Year, data = variables)
fr_yr_pearson <- residuals(object = fr_yr, type = "pearson")
fr_yr_deviance <- residuals(object = fr_yr, type = "deviance")
fr_yr_mu <- predict(fr_yr, type = "response")
fr_yr_response <- variables$Fish_Richness - fr_yr_mu
fr_yr_scaled <- fr_yr_response / sqrt(7.630148 * fr_yr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_yr_mu, y = fr_yr_response, main = "Response residuals")
plot(x = fr_yr_mu, y = fr_yr_pearson, main = "Pearson residuals")
plot(x = fr_yr_mu, y = fr_yr_scaled, main = "Pearson residuals scaled")
plot(x = fr_yr_mu, y = fr_yr_deviance, main = "Deviance residuals")
par(op)

#fr_st
fr_st = glm.nb(formula = Fish_Richness ~ Site, data = variables)
fr_st_pearson <- residuals(object = fr_st, type = "pearson")
fr_st_deviance <- residuals(object = fr_st, type = "deviance")
fr_st_mu <- predict(fr_st, type = "response")
fr_st_response <- variables$Fish_Richness - fr_st_mu
fr_st_scaled <- fr_st_response / sqrt(7.630148 * fr_st_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_st_mu, y = fr_st_response, main = "Response residuals")
plot(x = fr_st_mu, y = fr_st_pearson, main = "Pearson residuals")
plot(x = fr_st_mu, y = fr_st_scaled, main = "Pearson residuals scaled")
plot(x = fr_st_mu, y = fr_st_deviance, main = "Deviance residuals")
par(op)

#fr_cd
fr_cd = glm.nb(formula = Fish_Richness ~ Combined_Richness, data = variables)
fr_cd_pearson <- residuals(object = fr_cd, type = "pearson")
fr_cd_deviance <- residuals(object = fr_cd, type = "deviance")
fr_cd_mu <- predict(fr_cd, type = "response")
fr_cd_response <- variables$Fish_Richness - fr_cd_mu
fr_cd_scaled <- fr_cd_response / sqrt(7.630148 * fr_cd_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = fr_cd_mu, y = fr_cd_response, main = "Response residuals")
plot(x = fr_cd_mu, y = fr_cd_pearson, main = "Pearson residuals")
plot(x = fr_cd_mu, y = fr_cd_scaled, main = "Pearson residuals scaled")
plot(x = fr_cd_mu, y = fr_cd_deviance, main = "Deviance residuals")
par(op)

#cd_cc
cd_cc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
cd_cc_pearson <- residuals(object = cd_cc, type = "pearson")
cd_cc_deviance <- residuals(object = cd_cc, type = "deviance")
cd_cc_mu <- predict(cd_cc, type = "response")
cd_cc_response <- variables$Combined_Richness - cd_cc_mu
cd_cc_scaled <- cd_cc_response / sqrt(7.630148 * cd_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cd_cc_mu, y = cd_cc_response, main = "Response residuals")
plot(x = cd_cc_mu, y = cd_cc_pearson, main = "Pearson residuals")
plot(x = cd_cc_mu, y = cd_cc_scaled, main = "Pearson residuals scaled")
plot(x = cd_cc_mu, y = cd_cc_deviance, main = "Deviance residuals")
par(op)

#cd_sc
cd_sc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
cd_sc_pearson <- residuals(object = cd_sc, type = "pearson")
cd_sc_deviance <- residuals(object = cd_sc, type = "deviance")
cd_sc_mu <- predict(cd_sc, type = "response")
cd_sc_response <- variables$Combined_Richness - cd_sc_mu
cd_sc_scaled <- cd_sc_response / sqrt(7.630148 * cd_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cd_sc_mu, y = cd_sc_response, main = "Response residuals")
plot(x = cd_sc_mu, y = cd_sc_pearson, main = "Pearson residuals")
plot(x = cd_sc_mu, y = cd_sc_scaled, main = "Pearson residuals scaled")
plot(x = cd_sc_mu, y = cd_sc_deviance, main = "Deviance residuals")
par(op)

#cd_ru
cd_ru = glm.nb(formula = Combined_Richness ~ Rugosity, data = variables)
cd_ru_pearson <- residuals(object = cd_ru, type = "pearson")
cd_ru_deviance <- residuals(object = cd_ru, type = "deviance")
cd_ru_mu <- predict(cd_ru, type = "response")
cd_ru_response <- variables$Combined_Richness - cd_ru_mu
cd_ru_scaled <- cd_ru_response / sqrt(7.630148 * cd_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cd_ru_mu, y = cd_ru_response, main = "Response residuals")
plot(x = cd_ru_mu, y = cd_ru_pearson, main = "Pearson residuals")
plot(x = cd_ru_mu, y = cd_ru_scaled, main = "Pearson residuals scaled")
plot(x = cd_ru_mu, y = cd_ru_deviance, main = "Deviance residuals")
par(op)

#cd_yr
cd_yr = glm.nb(formula = Combined_Richness ~ Year, data = variables)
cd_yr_pearson <- residuals(object = cd_yr, type = "pearson")
cd_yr_deviance <- residuals(object = cd_yr, type = "deviance")
cd_yr_mu <- predict(cd_yr, type = "response")
cd_yr_response <- variables$Combined_Richness - cd_yr_mu
cd_yr_scaled <- cd_yr_response / sqrt(7.630148 * cd_yr_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cd_yr_mu, y = cd_yr_response, main = "Response residuals")
plot(x = cd_yr_mu, y = cd_yr_pearson, main = "Pearson residuals")
plot(x = cd_yr_mu, y = cd_yr_scaled, main = "Pearson residuals scaled")
plot(x = cd_yr_mu, y = cd_yr_deviance, main = "Deviance residuals")
par(op)

#cd_st
cd_st = glm.nb(formula = Combined_Richness ~ Site, data = variables)
cd_st_pearson <- residuals(object = cd_st, type = "pearson")
cd_st_deviance <- residuals(object = cd_st, type = "deviance")
cd_st_mu <- predict(cd_st, type = "response")
cd_st_response <- variables$Combined_Richness - cd_st_mu
cd_st_scaled <- cd_st_response / sqrt(7.630148 * cd_st_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cd_st_mu, y = cd_st_response, main = "Response residuals")
plot(x = cd_st_mu, y = cd_st_pearson, main = "Pearson residuals")
plot(x = cd_st_mu, y = cd_st_scaled, main = "Pearson residuals scaled")
plot(x = cd_st_mu, y = cd_st_deviance, main = "Deviance residuals")
par(op)

#yr_cc
yr_cc = glm.nb(formula = Year ~ Percent_Coral_Cover, data = variables)
yr_cc_pearson <- residuals(object = yr_cc, type = "pearson")
yr_cc_deviance <- residuals(object = yr_cc, type = "deviance")
yr_cc_mu <- predict(yr_cc, type = "response")
yr_cc_response <- variables$Year - yr_cc_mu
yr_cc_scaled <- yr_cc_response / sqrt(7.630148 * yr_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = yr_cc_mu, y = yr_cc_response, main = "Response residuals")
plot(x = yr_cc_mu, y = yr_cc_pearson, main = "Pearson residuals")
plot(x = yr_cc_mu, y = yr_cc_scaled, main = "Pearson residuals scaled")
plot(x = yr_cc_mu, y = yr_cc_deviance, main = "Deviance residuals")
par(op)

#yr_sc
yr_sc = glm.nb(formula = Year ~ Percent_Sponge_Cover, data = variables)
yr_sc_pearson <- residuals(object = yr_sc, type = "pearson")
yr_sc_deviance <- residuals(object = yr_sc, type = "deviance")
yr_sc_mu <- predict(yr_sc, type = "response")
yr_sc_response <- variables$Year - yr_sc_mu
yr_sc_scaled <- yr_sc_response / sqrt(7.630148 * yr_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = yr_sc_mu, y = yr_sc_response, main = "Response residuals")
plot(x = yr_sc_mu, y = yr_sc_pearson, main = "Pearson residuals")
plot(x = yr_sc_mu, y = yr_sc_scaled, main = "Pearson residuals scaled")
plot(x = yr_sc_mu, y = yr_sc_deviance, main = "Deviance residuals")
par(op)

#yr_ru
yr_ru = glm.nb(formula = Year ~ Rugosity, data = variables)
yr_ru_pearson <- residuals(object = yr_ru, type = "pearson")
yr_ru_deviance <- residuals(object = yr_ru, type = "deviance")
yr_ru_mu <- predict(yr_ru, type = "response")
yr_ru_response <- variables$Year - yr_ru_mu
yr_ru_scaled <- yr_ru_response / sqrt(7.630148 * yr_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = yr_ru_mu, y = yr_ru_response, main = "Response residuals")
plot(x = yr_ru_mu, y = yr_ru_pearson, main = "Pearson residuals")
plot(x = yr_ru_mu, y = yr_ru_scaled, main = "Pearson residuals scaled")
plot(x = yr_ru_mu, y = yr_ru_deviance, main = "Deviance residuals")
par(op)

#yr_st
yr_st = glm.nb(formula = Year ~ Site, data = variables)
yr_st_pearson <- residuals(object = yr_st, type = "pearson")
yr_st_deviance <- residuals(object = yr_st, type = "deviance")
yr_st_mu <- predict(yr_st, type = "response")
yr_st_response <- variables$Year - yr_st_mu
yr_st_scaled <- yr_st_response / sqrt(7.630148 * yr_st_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = yr_st_mu, y = yr_st_response, main = "Response residuals")
plot(x = yr_st_mu, y = yr_st_pearson, main = "Pearson residuals")
plot(x = yr_st_mu, y = yr_st_scaled, main = "Pearson residuals scaled")
plot(x = yr_st_mu, y = yr_st_deviance, main = "Deviance residuals")
par(op)

#st_cc
st_cc = glm.nb(formula = Site ~ Percent_Coral_Cover, data = variables)
st_cc_pearson <- residuals(object = st_cc, type = "pearson")
st_cc_deviance <- residuals(object = st_cc, type = "deviance")
st_cc_mu <- predict(st_cc, type = "response")
st_cc_response <- variables$Site - st_cc_mu
st_cc_scaled <- st_cc_response / sqrt(7.630148 * st_cc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = st_cc_mu, y = st_cc_response, main = "Response residuals")
plot(x = st_cc_mu, y = st_cc_pearson, main = "Pearson residuals")
plot(x = st_cc_mu, y = st_cc_scaled, main = "Pearson residuals scaled")
plot(x = st_cc_mu, y = st_cc_deviance, main = "Deviance residuals")
par(op)

#st_sc
st_sc = glm.nb(formula = Site ~ Percent_Sponge_Cover, data = variables)
st_sc_pearson <- residuals(object = st_sc, type = "pearson")
st_sc_deviance <- residuals(object = st_sc, type = "deviance")
st_sc_mu <- predict(st_sc, type = "response")
st_sc_response <- variables$Site - st_sc_mu
st_sc_scaled <- st_sc_response / sqrt(7.630148 * st_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = st_sc_mu, y = st_sc_response, main = "Response residuals")
plot(x = st_sc_mu, y = st_sc_pearson, main = "Pearson residuals")
plot(x = st_sc_mu, y = st_sc_scaled, main = "Pearson residuals scaled")
plot(x = st_sc_mu, y = st_sc_deviance, main = "Deviance residuals")
par(op)

#st_ru
st_ru = glm.nb(formula = Site ~ Rugosity, data = variables)
st_ru_pearson <- residuals(object = st_ru, type = "pearson")
st_ru_deviance <- residuals(object = st_ru, type = "deviance")
st_ru_mu <- predict(st_ru, type = "response")
st_ru_response <- variables$Site - st_ru_mu
st_ru_scaled <- st_ru_response / sqrt(7.630148 * st_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = st_ru_mu, y = st_ru_response, main = "Response residuals")
plot(x = st_ru_mu, y = st_ru_pearson, main = "Pearson residuals")
plot(x = st_ru_mu, y = st_ru_scaled, main = "Pearson residuals scaled")
plot(x = st_ru_mu, y = st_ru_deviance, main = "Deviance residuals")
par(op)

#cc_sc
cc_sc = glm.nb(formula = Percent_Coral_Cover ~ Percent_Sponge_Cover, data = variables)
cc_sc_pearson <- residuals(object = cc_sc, type = "pearson")
cc_sc_deviance <- residuals(object = cc_sc, type = "deviance")
cc_sc_mu <- predict(cc_sc, type = "response")
cc_sc_response <- variables$Percent_Coral_Cover - cc_sc_mu
cc_sc_scaled <- cc_sc_response / sqrt(7.630148 * cc_sc_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = cc_sc_mu, y = cc_sc_response, main = "Response residuals")
plot(x = cc_sc_mu, y = cc_sc_pearson, main = "Pearson residuals")
plot(x = cc_sc_mu, y = cc_sc_scaled, main = "Pearson residuals scaled")
plot(x = cc_sc_mu, y = cc_sc_deviance, main = "Deviance residuals")
par(op)

#sc_ru
sc_ru = glm.nb(formula = Percent_Sponge_Cover ~ Rugosity, data = variables)
sc_ru_pearson <- residuals(object = sc_ru, type = "pearson")
sc_ru_deviance <- residuals(object = sc_ru, type = "deviance")
sc_ru_mu <- predict(sc_ru, type = "response")
sc_ru_response <- variables$Percent_Sponge_Cover - sc_ru_mu
sc_ru_scaled <- sc_ru_response / sqrt(7.630148 * sc_ru_mu) 
# The authors explain that they have to manually scale the residuals because the residual function doesn't account 
# for overdispersion, however, I'm not sure why they used 7.63... in this calculation.
op <- par(mfrow = c(2, 2))
plot(x = sc_ru_mu, y = sc_ru_response, main = "Response residuals")
plot(x = sc_ru_mu, y = sc_ru_pearson, main = "Pearson residuals")
plot(x = sc_ru_mu, y = sc_ru_scaled, main = "Pearson residuals scaled")
plot(x = sc_ru_mu, y = sc_ru_deviance, main = "Deviance residuals")
par(op)

#####Below are previous attempts




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



## Figures of basic relationships between surrogates (x) and targets (y)
# Note that some axis titles are missing in order to make it easier to visualize all figures next to each other

# Figure 1. Relationship between coral cover and coral richness.
basic_coral_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 65)) +
                    scale_y_continuous(name = "Coral Richness", limits = c(0, 25)) +
                    #labs(title = "Target: Coral Species Richness") +
                    annotate(geom = "text", size = 10, x = 50, y = 2.5, label = "italic(R) ['N'] ^ 2 == 0.62", parse = TRUE) +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          #plot.title = element_text(size = 22),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))

# Figure 2. Relationship between coral cover and sponge richness.
basic_sponge_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 65)) +
                    scale_y_continuous(name = "Sponge Richness", limits = c(0, 40)) +
                    #labs(title = "Target: Sponge Species Richness") +
                    annotate(geom = "text", size = 10, x = 50, y = 3, label = "italic(R) ['N'] ^ 2 == 0.28", parse = TRUE) +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          #plot.title = element_text(size = 22),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))
# Resulting warning message just lets you know that there are years and sites for which these data are unavailable.
# This does not change the resulting plot except that it will have fewer points than similar plots.

# Figure 3. Relationship between coral cover and fish richness.
basic_fish_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) +
                  geom_point(size = 3, color = "gray") +
                  scale_x_continuous(name = "", limits = c(0, 65)) +
                  scale_y_continuous(name = "Fish Richness", limits = c(0, 40)) +
                  #labs(title = "Target: Fish Species Richness") +                
                  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                  theme(text = element_text(size = 27),
                        #plot.title = element_text(size = 22),
                        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"))

# Figure 4. Relationship between coral cover and combined richness.
basic_combined_cc <- ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "Coral Cover (%)", limits = c(0, 65)) +
                    scale_y_continuous(name = "Combined Richness", limits = c(0, 80)) +
                    #labs(title = "Target: Combined Species Richness") +                  
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          #plot.title = element_text(size = 22),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))
# Resulting warning message similar to that from figure 2.
# Combined richness was only calculated for sites and years when richness was recorded for all 3 groups.

# Figure 5. Relationship between sponge cover and coral richness.
basic_coral_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Coral_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 30)) +
                    scale_y_continuous(name = "", limits = c(0, 25)) +
                    #labs(title = "") +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))

# Figure 6. Relationship between sponge cover and sponge richness.
basic_sponge_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Sponge_Richness)) +
                      geom_point(size = 3, color = "gray") +
                      scale_x_continuous(name = "", limits = c(0, 30)) +
                      scale_y_continuous(name = "", limits = c(0, 40)) +
                      #labs(title = "") +
                      geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                      theme(text = element_text(size = 27),
                            panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                            panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                            panel.background = element_blank(),
                            axis.line = element_line(colour = "black"))
# Resulting warning message same as that from figure 2.

# Figure 7. Relationship between sponge cover and fish richness.
basic_fish_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Fish_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 30)) +
                    scale_y_continuous(name = "", limits = c(0, 40)) +
                    #labs(title = "") +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))

# Figure 8. Relationship between sponge cover and combined richness.
basic_combined_sc <- ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Combined_Richness)) +
                        geom_point(size = 3, color = "gray") +
                        scale_x_continuous(name = "Sponge Cover (%)", limits = c(0, 30)) +
                        scale_y_continuous(name = "", limits = c(0, 80)) +
                        #labs(title = "") +
                        geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                        theme(text = element_text(size = 27),
                              panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                              panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                              panel.background = element_blank(),
                              axis.line = element_line(colour = "black"))
# Resulting warning message same as that from figure 4.

# Figure 9. Relationship between rugosity and coral richness.
basic_coral_r <- ggplot(data = variables, aes(x = Rugosity, y = Coral_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 80)) +
                    scale_y_continuous(name = "", limits = c(0, 25)) +
                    #labs(title = "") +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))

# Figure 10. Relationship between rugosity and sponge richness.
basic_sponge_r <- ggplot(data = variables, aes(x = Rugosity, y = Sponge_Richness)) +
                    geom_point(size = 3, color = "gray") +
                    scale_x_continuous(name = "", limits = c(0, 80)) +
                    scale_y_continuous(name = "", limits = c(0, 40)) +
                    #labs(title = "") +
                    geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                    theme(text = element_text(size = 27),
                          panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                          panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))
# Resulting warning message same as that from figure 2.

# Figure 11. Relationship between rugosity and fish richness.
basic_fish_r <- ggplot(data = variables, aes(x = Rugosity, y = Fish_Richness)) +
                  geom_point(size = 3, color = "gray") +
                  scale_x_continuous(name = "", limits = c(0, 80)) +
                  scale_y_continuous(name = "", limits = c(0, 40)) +
                  #labs(title = "") +
                  annotate(geom = "text", size = 10, x = 60, y = 5, label = "italic(R) ['N'] ^ 2 == 0.63", parse = TRUE) +
                  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                  theme(text = element_text(size = 27),
                        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"))

# Figure 12. Relationship between rugosity and combined richness.
basic_combined_r <- ggplot(data = variables, aes(x = Rugosity, y = Combined_Richness)) +
                      geom_point(size = 3, color = "gray") +
                      scale_x_continuous(name = "Rugosity", limits = c(0, 80)) +
                      scale_y_continuous(name = "", limits = c(0, 80)) +
                      #labs(title = "") +
                      annotate(geom = "text", size = 10, x = 60, y = 10, label = "italic(R) ['N'] ^ 2 == 0.38", parse = TRUE) +
                      geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
                      theme(text = element_text(size = 27),
                            panel.grid.major = element_line(colour = "light gray", size = (0.5)),
                            panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
                            panel.background = element_blank(),
                            axis.line = element_line(colour = "black"))
# Resulting warning message same as that from figure 4.

# Organize all of these figures into one window for objective 1
grid.arrange(basic_coral_cc, basic_coral_sc, basic_coral_r,
             basic_sponge_cc, basic_sponge_sc, basic_sponge_r,
             basic_fish_cc, basic_fish_sc, basic_fish_r,
             basic_combined_cc, basic_combined_sc, basic_combined_r,
             ncol = 3, nrow = 4)
 
 
 
# ########################################################################
# 
# These figures aren't needed for publication.
# 
# ## Figures of basic relationships between time (x) and surrogates/targets (y)
# 
# # Figure 13. Relationship between time and coral cover.
# ggplot(data = variables, aes(x = True_Year, y = Percent_Coral_Cover)) +
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Time (Years)") +
#   scale_y_continuous(name = "Coral Cover (%)") +
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
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
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
#   theme(text = element_text(size = 27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)),
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
# # Resulting warning message same as that from figure 4.
# 
# 
# 
########################################################################
 
 
 
# Create new columns for true year and year values of different types for use in figures
# Note that the column True_Year is type integer
variables$True_Year_Factor <- as.factor(x = variables$True_Year)
variables$True_Year_Numeric <- as.numeric(x = variables$True_Year)
variables$Year_Factor <- as.factor(x = variables$Year)


# Create color palette that is friendly to viewers with color blindness (from https://socviz.co/refineplots.html)
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ## Figures of basic relationships between time (x) and surrogates/targets (y) by site (legend)
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

# # Create a 3D figure
# # Create year vector from 0-26 with 100 values each 0.2626 apart
# Year <- rep(seq(from = min(variables$Year), to = max(variables$Year), length.out = 100))
# # Create coral cover vector from 2.676-61.746 with 100 values 0.596 apart
# Percent_Coral_Cover <- rep(seq(from = min(variables$Percent_Coral_Cover), to = max(variables$Percent_Coral_Cover), length.out = 100))
# # Make a grid using these vectors (for each Year value, list each value of coral cover)
# coral_pred_grid <- expand.grid(Year = Year, Percent_Coral_Cover = Percent_Coral_Cover)
# # Add a column for predicted coral richness from these values using the coral_cc_yr model
# # Fixed previous issue of richness limits by setting type argument to "response"
# coral_pred_grid$Predicted_Coral_Richness <-predict(object = coral_cc_yr, newdata = coral_pred_grid, type = "response")
# # Save predictive plot in variable
# predicted_coral_plot <- scatterplot3d(x = coral_pred_grid$Year, y = coral_pred_grid$Percent_Coral_Cover, z = coral_pred_grid$Predicted_Coral_Richness,
#                                       angle = 35,
#                                       color = "black",
#                                       pch = 1,
#                                       xlab = "Time (Year)",
#                                       ylab = "Coral Cover (%)",
#                                       zlab = "Coral Richness" )
# # Figure 34. Relationship between coral cover and coral richness and time shown in 3 dimensions. Negative binomial distribution used.
# predicted_coral_plot
# # Add a column so the plot will have the true values compared to the predicted
# #predicted_coral_plot$points3d(x = variables$Year, y = variables$Percent_Coral_Cover, z = variables$Coral_Richness, pch = 16)


# 2D alternative to 3D figure
# Note that I created a new model for the sake of creating this figure
coral_cc_yr_asfactor <- glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Year_Factor, data = variables)
coral_predictions_2d <- data.frame(
  Percent_Coral_Cover = rep(seq(from = min(variables$Percent_Coral_Cover), to = max(variables$Percent_Coral_Cover), length.out = 100), 27),
  Year_Factor = factor(rep(1:27, each = 100), levels = 1:27, labels =
                  levels(variables$Year_Factor)))
coral_predictions_2d <- cbind(coral_predictions_2d, predict(object = coral_cc_yr_asfactor, newdata = coral_predictions_2d, type = "link", se.fit = TRUE))
# 95% confidence intervals
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
# # Figure . Relationship between coral cover and coral richness by year. Negative binomial distribution used.
# ggplot(data = variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
#   geom_point(size = 3) +
#   scale_x_continuous(name = "Coral Cover (%)") +
#   scale_y_continuous(name = "Coral Richness") +
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Year_Factor)) +
#   #scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size=27), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# # Figure with prediction lines and true points colored by year
# ggplot(data = coral_predictions_2d, aes(x = Percent_Coral_Cover, y = Predicted_Coral_Richness, color = Year_Factor)) +
#   #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Year_Factor), alpha = 0.25) +
#   geom_line(aes(color = Year_Factor), size = 2) +
#   geom_point(data = variables, size = 3, aes(x = Percent_Coral_Cover, y = Coral_Richness)) +
#   #scale_color_gradient(low="lightblue", high="darkblue") +
#   #scale_colour_viridis_d(option = "plasma") +
#   scale_colour_viridis_d() +
#   labs(x = "Coral Cover (%)", y = "Coral Richness", color = "Year") +
#   theme(text = element_text(size = 18), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))

# THIS IS THE CODE THAT WORKS TO CREATE A FIGURE OF CORAL COVER VS CORAL RICHNESS WITH YEAR PREDICTION LINES AND TRUE POINTS WITH A COLOR GRADIENT
# The code above also works in this case, but I wanted to follow the same formatting as the other figures which requires using a 4 digit year instead of the indexed year
coral_test_model <- glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + True_Year_Factor, data = variables)
coral_test <- data.frame(
  Percent_Coral_Cover = rep(seq(from = min(variables$Percent_Coral_Cover), to = max(variables$Percent_Coral_Cover), length.out = 100), 27),
  True_Year_Factor = factor(rep(1:27, each = 100), levels = 1:27, labels = levels(variables$True_Year_Factor)))

coral_test <- cbind(coral_test, predict(object = coral_test_model, newdata = coral_test, type = "link", se.fit = TRUE, na.action = na.omit))
# 95% confidence intervals
coral_test <- within(coral_test, {
  Predicted_Coral_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)      
})
# Figure with prediction lines and true points colored by year
ggplot(data = coral_test, aes(x = Percent_Coral_Cover, y = Predicted_Coral_Richness, color = True_Year_Factor)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Year_Factor), alpha = 0.25) +
  geom_line(aes(color = True_Year_Factor), size = 2) +
  geom_point(data = variables, size = 3, aes(x = Percent_Coral_Cover, y = Coral_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  ## Order numbers in legend, but can't use viridis
  #scale_colour_discrete(c("1", "2", "3", "8", "9", "10", "11", "13", "14", "15", 
  #                       "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26")) +
  scale_colour_viridis_d() +
  labs(x = "Coral Cover (%)", y = "Coral Richness", color = "Year") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
###END OF CODE FOR THIS FIGURE



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
predicted_sponge_yr_site_A <- ggplot(data = sponge_predictions_oneoftwo, aes(x = Year, y = Predicted_Sponge_Richness)) +
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
predicted_sponge_yr_site_B <- ggplot(data = sponge_predictions_twooftwo, aes(x = Year, y = Predicted_Sponge_Richness)) +
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
grid.arrange(predicted_sponge_yr_site_A, predicted_sponge_yr_site_B, ncol = 2, nrow = 1)
# # Figure 36. Relationship between time and sponge richness by site. Negative binomial distribution used.
# ggplot(data = variables, aes(x = Year, y = Sponge_Richness)) + 
#   geom_point(size = 3)+
#   scale_x_continuous(name = "Time (Year)") +
#   scale_y_continuous(name = "Sponge Richness") +
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Site)) +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size=27),
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))


# THIS IS THE CODE THAT WORKS TO CREATE A FIGURE OF CORAL COVER VS SPONGE RICHNESS WITH YEAR PREDICTION LINES AND TRUE POINTS WITH A COLOR GRADIENT
sponge_complete$True_Year_Factor <- as.character(sponge_complete$True_Year_Factor)
sponge_complete <- sponge_complete[sponge_complete$True_Year_Factor != "1992" | sponge_complete$True_Year_Factor != "1995" | sponge_complete$True_Year_Factor != "1996" |
                                     sponge_complete$True_Year_Factor != "1997" | sponge_complete$True_Year_Factor != "1998" | sponge_complete$True_Year_Factor != "2003", ]
sponge_complete$True_Year_Factor <- as.factor(sponge_complete$True_Year_Factor)

stest_model <- glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + True_Year_Factor, data = sponge_complete)
stest <- data.frame(
  Percent_Coral_Cover = rep(seq(from = min(sponge_complete$Percent_Coral_Cover), to = max(sponge_complete$Percent_Coral_Cover), length.out = 100), 21),
  True_Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels = levels(sponge_complete$True_Year_Factor)))

stest <- cbind(stest, predict(object = stest_model, newdata = stest, type = "link", se.fit = TRUE, na.action = na.omit))
# 95% confidence intervals
stest <- within(stest, {
  Predicted_Sponge_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)      
})
# Figure with prediction lines and true points colored by year
ggplot(data = stest, aes(x = Percent_Coral_Cover, y = Predicted_Sponge_Richness, color = True_Year_Factor)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Year_Factor), alpha = 0.25) +
  geom_line(aes(color = True_Year_Factor), size = 2) +
  geom_point(data = sponge_complete, size = 3, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  ## Order numbers in legend, but can't use viridis
  #scale_colour_discrete(c("1", "2", "3", "8", "9", "10", "11", "13", "14", "15", 
  #                       "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26")) +
  scale_colour_viridis_d() +
  labs(x = "Coral Cover (%)", y = "Sponge Richness", color = "Year") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
###END OF CODE FOR THIS FIGURE



# Note that I created a new model for the sake of creating this figure
sponge_cc_site_fig_model <- glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Site, data = sponge_complete)
sponge_cc_site_fig <- data.frame(
  Percent_Coral_Cover = rep(seq(from = min(sponge_complete$Percent_Coral_Cover), to = max(sponge_complete$Percent_Coral_Cover), length.out = 100), 8),
  Site = factor(rep(1:8, each = 100), levels = 1:8, labels = levels(sponge_complete$Site)))
sponge_cc_site_fig <- cbind(sponge_cc_site_fig, predict(object = sponge_cc_site_fig_model, newdata = sponge_cc_site_fig,
                                                                        type = "link", se.fit = TRUE))
# 95% confidence intervals
sponge_cc_site_fig <- within(sponge_cc_site_fig, {
  Predicted_Sponge_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

# Figure with prediction lines and true points colored by site
ggplot(data = sponge_cc_site_fig, aes(x = Percent_Coral_Cover, y = Predicted_Sponge_Richness, color = Site)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  geom_point(data = variables, size = 3, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  scale_colour_viridis_d() +
  labs(x = "Coral Cover (%)", y = "Sponge Richness", color = "Site") +
  theme(text = element_text(size = 18), 
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
predicted_fish_site_A <- ggplot(data = fish_predictions_oneoftwo, aes(x = Year, y = Predicted_Fish_Richness)) +
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
predicted_fish_site_B <- ggplot(data = fish_predictions_twooftwo, aes(x = Year, y = Predicted_Fish_Richness)) +
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
grid.arrange(predicted_fish_site_A, predicted_fish_site_B, ncol = 2, nrow = 1)
# # Figure 38. Relationship between time and fish richness by site. Negative binomial distribution used.
# ggplot(data = variables, aes(x = Year, y = Fish_Richness)) + 
#   geom_point(size = 3)+
#   scale_x_continuous(name = "Time (Year)") +
#   scale_y_continuous(name = "Fish Richness") +
#   geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = Site)) +
#   scale_color_manual(values = cb_palette) +
#   theme(text = element_text(size=27), 
#         panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))


# Note that I created a new model for the sake of creating this figure
fish_r_site_fig_model <- glm.nb(formula = Fish_Richness ~ Rugosity + Site, data = variables)
fish_r_site_fig <- data.frame(
  Rugosity = rep(seq(from = min(variables$Rugosity), to = max(variables$Rugosity), length.out = 100), 8),
  Site = factor(rep(1:8, each = 100), levels = 1:8, labels = levels(variables$Site)))
fish_r_site_fig <- cbind(fish_r_site_fig, predict(object = fish_r_site_fig_model, newdata = fish_r_site_fig, 
                                                        type = "link", se.fit = TRUE))
# 95% confidence intervals
fish_r_site_fig <- within(fish_r_site_fig, {
  Predicted_Fish_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

# Figure with prediction lines and true points colored by site
ggplot(data = fish_r_site_fig, aes(x = Rugosity, y = Predicted_Fish_Richness, color = Site)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  geom_point(data = variables, size = 3, aes(x = Rugosity, y = Fish_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  scale_colour_viridis_d() +
  labs(x = "Rugosity", y = "Fish Richness", color = "Site") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



########################################################################
#######################COMBINED RICHNESS################################



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for combined richness is combined_cc_yr_site.
# The function of this model is Combined_Richness ~ Percent_Coral_Cover + Year + Site.

# # These figures are for 3D, but they won't be used.
# # Figure 39. Relationship between coral cover and coral richness and time and site. Negative binomial distribution used.
# # Note that the model used to predict does not have site as a term***
# par(mfrow = c(3,3))
# Year <- seq(from = min(Pelican_Ghut$Year), to = max(Pelican_Ghut$Year), length.out = 100)
# Rugosity <- seq(from = min(Pelican_Ghut$Rugosity), to = max(Pelican_Ghut$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[1],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Pelican Ghut",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(Grand_Ghut$Year), to = max(Grand_Ghut$Year), length.out = 100)
# Rugosity <- seq(from = min(Grand_Ghut$Rugosity), to = max(Grand_Ghut$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[2],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Grand Ghut",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Yeartest <- seq(from = min(Crab_Cove$Year), to = max(Crab_Cove$Year), length.out = 100)
# Rugosity <- seq(from = min(Crab_Cove$Rugosity), to = max(Crab_Cove$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[3],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Crab Cove",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(Muskmelon$Year), to = max(Muskmelon$Year), length.out = 100)
# Rugosity <- seq(from = min(Muskmelon$Rugosity), to = max(Muskmelon$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[4],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Muskmelon",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(Bigelow$Year), to = max(Bigelow$Year), length.out = 100)
# Rugosity <- seq(from = min(Bigelow$Rugosity), to = max(Bigelow$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[5],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Bigelow",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(White_Bay$Year), to = max(White_Bay$Year), length.out = 100)
# Rugosity <- seq(from = min(White_Bay$Rugosity), to = max(White_Bay$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[6],
#                                          pch = 1,
#                                          xlab = "Time (Year) - White Bay",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(Monkey_Point$Year), to = max(Monkey_Point$Year), length.out = 100)
# Rugosity <- seq(from = min(Monkey_Point$Rugosity), to = max(Monkey_Point$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[7],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Monkey Point",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# Year <- seq(from = min(Guana_Head$Year), to = max(Guana_Head$Year), length.out = 100)
# Rugosity <- seq(from = min(Guana_Head$Rugosity), to = max(Guana_Head$Rugosity), length.out = 100)
# pred_grid <- expand.grid(Year = Year, Rugosity = Rugosity)
# pred_grid$Predicted_Combined_Richness <-predict(object = combined_r_yr, newdata = pred_grid, type = "response")
# predicted_combined_plot <- scatterplot3d(x = pred_grid$Year, y = pred_grid$Rugosity, z = pred_grid$Predicted_Combined_Richness,
#                                          angle = 60,
#                                          color = cb_palette[8],
#                                          pch = 1,
#                                          xlab = "Time (Year) - Guana Head",
#                                          ylab = "Rugosity",
#                                          zlab = "Combined Richness",
#                                          xlim = c(0, 30),
#                                          ylim = c(10, 80),
#                                          zlim = c(30, 80))
# par(mfrow = c(1,1))



# THIS IS THE CODE THAT WORKS TO CREATE A FIGURE OF RUGOSITY VS COMBINED RICHNESS WITH YEAR PREDICTION LINES AND TRUE POINTS WITH A COLOR GRADIENT
combined_complete$True_Year_Factor <- as.character(combined_complete$True_Year_Factor)
combined_complete <- combined_complete[combined_complete$True_Year_Factor != "1992" | combined_complete$True_Year_Factor != "1995" | combined_complete$True_Year_Factor != "1996" |
                                         combined_complete$True_Year_Factor != "1997" | combined_complete$True_Year_Factor != "1998" | combined_complete$True_Year_Factor != "2003", ]
combined_complete$True_Year_Factor <- as.factor(combined_complete$True_Year_Factor)

ctest_model <- glm.nb(formula = Combined_Richness ~ Rugosity + True_Year_Factor, data = sponge_complete)
ctest <- data.frame(
  Rugosity = rep(seq(from = min(combined_complete$Rugosity), to = max(combined_complete$Rugosity), length.out = 100), 21),
  True_Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels = levels(combined_complete$True_Year_Factor)))

ctest <- cbind(ctest, predict(object = ctest_model, newdata = ctest, type = "link", se.fit = TRUE, na.action = na.omit))
# 95% confidence intervals
ctest <- within(ctest, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)      
})
# Figure with prediction lines and true points colored by year
ggplot(data = ctest, aes(x = Rugosity, y = Predicted_Combined_Richness, color = True_Year_Factor)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Year_Factor), alpha = 0.25) +
  geom_line(aes(color = True_Year_Factor), size = 2) +
  geom_point(data = combined_complete, size = 3, aes(x = Rugosity, y = Combined_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  ## Order numbers in legend, but can't use viridis
  #scale_colour_discrete(c("1", "2", "3", "8", "9", "10", "11", "13", "14", "15", 
  #                       "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26")) +
  scale_colour_viridis_d() +
  labs(x = "Rugosity", y = "Combined Richness", color = "Year") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
###END OF CODE FOR THIS FIGURE



# Note that I created a new model for the sake of creating this figure
combined_r_site_fig_model <- glm.nb(formula = Combined_Richness ~ Rugosity + Site, data = sponge_complete)
combined_r_site_fig <- data.frame(
  Rugosity = rep(seq(from = min(sponge_complete$Rugosity), to = max(sponge_complete$Rugosity), length.out = 100), 8),
  Site = factor(rep(1:8, each = 100), levels = 1:8, labels = levels(sponge_complete$Site)))
combined_r_site_fig <- cbind(combined_r_site_fig, predict(object = combined_r_site_fig_model, newdata = combined_r_site_fig, 
                                                        type = "link", se.fit = TRUE))
# 95% confidence intervals
combined_r_site_fig <- within(combined_r_site_fig, {
  Predicted_Combined_Richness <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

# Figure with prediction lines and true points colored by site
ggplot(data = combined_r_site_fig, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Site)) +
  #geom_ribbon(aes(ymin = 0, ymax = 35, fill = Site), alpha = 0.25) +
  geom_line(aes(color = Site), size = 2) +
  geom_point(data = variables, size = 3, aes(x = Rugosity, y = Combined_Richness)) +
  #scale_color_gradient(low="lightblue", high="darkblue") +
  #scale_colour_viridis_d(option = "plasma") +
  scale_colour_viridis_d() +
  labs(x = "Rugosity", y = "Combined Richness", color = "Site") +
  theme(text = element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



########################################################################
#######################COMBINED RICHNESS################################



# # These figures will likely not be used in publication
# # Create figures and merge them into larger image with a panel for each site of  x = rugosity and y = combined richness colored by year with confidence intervals
# # Remove ghost factors
# combined_complete$Year_Factor <- as.character(combined_complete$Year_Factor)
# combined_complete <- combined_complete[combined_complete$Year_Factor != "0" | combined_complete$Year_Factor != "4" | combined_complete$Year_Factor != "5" |
#                                          combined_complete$Year_Factor != "6" | combined_complete$Year_Factor != "7" | combined_complete$Year_Factor != "12", ]
# combined_complete$Year_Factor <- as.factor(combined_complete$Year_Factor)
# # Order the factor levels for year
# combined_complete$Year_Factor <- ordered(combined_complete$Year_Factor, 
#                                          levels = c("1", "2", "3", "8", "9", "10", "11", "13", "14", "15", 
#                                                     "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"))
# # Note that model created with year as factor for graphing purposes
# combined_r_yr_asfactor <- glm.nb(formula = Combined_Richness ~ Rugosity + Year_Factor, data = combined_complete)
# # Re-create subsets without ghost factors
# Pelican_Ghut <- combined_complete[which(combined_complete$Site == "pelican"),]
# Grand_Ghut <- combined_complete[which(combined_complete$Site == "grand"),]
# Crab_Cove <- combined_complete[which(combined_complete$Site == "crab"),]
# Muskmelon <- combined_complete[which(combined_complete$Site == "muskN"),]
# Bigelow <- combined_complete[which(combined_complete$Site == "bigelow"),]
# White_Bay <- combined_complete[which(combined_complete$Site == "white"),]
# Monkey_Point <- combined_complete[which(combined_complete$Site == "monkey"),]
# Guana_Head <- combined_complete[which(combined_complete$Site == "iguana"),]
# 
# 
# 
# # Pelican_Ghut
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Pelican_Ghut$Rugosity), to = max(Pelican_Ghut$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Pelican_Ghut$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Pelican_Ghut
# Pelican_Ghut_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                               geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                               geom_line(aes(color = Year_Factor), size = 2) +
#                               labs(x = "Rugosity - Pelican Ghut", y = "Predicted Combined Richness") +
#                               scale_x_continuous(limits = c(10, 80)) +
#                               scale_y_continuous(limits = c(30, 80)) +
#                               theme(text = element_text(size = 18), 
#                                     panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                     panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                     panel.background = element_blank(), 
#                                     axis.line = element_line(colour = "black")
#                                     #legend.position = "none"
#                                     )
# # Grand_Ghut
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Grand_Ghut$Rugosity), to = max(Grand_Ghut$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Grand_Ghut$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Grand_Ghut
# Grand_Ghut_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                             geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                             geom_line(aes(color = Year_Factor), size = 2) +
#                             labs(x = "Rugosity - Grand Ghut", y = "Predicted Combined Richness") +
#                             scale_x_continuous(limits = c(10, 80)) +
#                             scale_y_continuous(limits = c(30, 80)) +
#                             theme(text = element_text(size = 18), 
#                                   panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                   panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                   panel.background = element_blank(), 
#                                   axis.line = element_line(colour = "black")
#                                   #legend.position = "none"
#                                   )
# # Crab_Cove
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Crab_Cove$Rugosity), to = max(Crab_Cove$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Crab_Cove$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Crab_Cove
# Crab_Cove_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                             geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                             geom_line(aes(color = Year_Factor), size = 2) +
#                             labs(x = "Rugosity - Crab Cove", y = "Predicted Combined Richness") +
#                             scale_x_continuous(limits = c(10, 80)) +
#                             scale_y_continuous(limits = c(30, 80)) +
#                             theme(text = element_text(size = 18), 
#                                   panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                   panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                   panel.background = element_blank(), 
#                                   axis.line = element_line(colour = "black")
#                                   #legend.position = "none"
#                                   )
# # Muskmelon
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Muskmelon$Rugosity), to = max(Muskmelon$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Muskmelon$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Muskmelon
# Muskmelon_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                             geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                             geom_line(aes(color = Year_Factor), size = 2) +
#                             labs(x = "Rugosity - Muskmelon", y = "Predicted Combined Richness") +
#                             scale_x_continuous(limits = c(10, 80)) +
#                             scale_y_continuous(limits = c(30, 80)) +
#                             theme(text = element_text(size = 18), 
#                                   panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                   panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                   panel.background = element_blank(), 
#                                   axis.line = element_line(colour = "black")
#                                   #legend.position = "none"
#                                   )
# # Bigelow
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Bigelow$Rugosity), to = max(Bigelow$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Bigelow$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Bigelow
# Bigelow_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                           geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                           geom_line(aes(color = Year_Factor), size = 2) +
#                           labs(x = "Rugosity - Bigelow", y = "Predicted Combined Richness") +
#                           scale_x_continuous(limits = c(10, 80)) +
#                           scale_y_continuous(limits = c(30, 80)) +
#                           theme(text = element_text(size = 18), 
#                                 panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                 panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                 panel.background = element_blank(), 
#                                 axis.line = element_line(colour = "black")
#                                 #legend.position = "none"
#                                 )
# # White_Bay
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(White_Bay$Rugosity), to = max(White_Bay$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(White_Bay$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for White_Bay
# White_Bay_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                             geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                             geom_line(aes(color = Year_Factor), size = 2) +
#                             labs(x = "Rugosity - White Bay", y = "Predicted Combined Richness") +
#                             scale_x_continuous(limits = c(10, 80)) +
#                             scale_y_continuous(limits = c(30, 80)) +
#                             theme(text = element_text(size = 18), 
#                                   panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                   panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                   panel.background = element_blank(), 
#                                   axis.line = element_line(colour = "black")
#                                   #legend.position = "none"
#                                   )
# # Monkey_Point
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Monkey_Point$Rugosity), to = max(Monkey_Point$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Monkey_Point$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Monkey_Point
# Monkey_Point_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                               geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                               geom_line(aes(color = Year_Factor), size = 2) +
#                               labs(x = "Rugosity - Monkey Point", y = "Predicted Combined Richness") +
#                               scale_x_continuous(limits = c(10, 80)) +
#                               scale_y_continuous(limits = c(30, 80)) +
#                               theme(text = element_text(size = 18), 
#                                     panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                     panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                     panel.background = element_blank(), 
#                                     axis.line = element_line(colour = "black")
#                                     #legend.position = "none"
#                                     )
# # Guana_Head
# combined_predictions_2d <- data.frame(
#   Rugosity = rep(seq(from = min(Guana_Head$Rugosity), to = max(Guana_Head$Rugosity), length.out = 100), 21),
#   Year_Factor = factor(rep(1:21, each = 100), levels = 1:21, labels =
#                          levels(Guana_Head$Year_Factor)))
# combined_predictions_2d <- cbind(combined_predictions_2d, predict(object = combined_r_yr_asfactor, newdata = combined_predictions_2d, type = "link", se.fit = TRUE))
# combined_predictions_2d <- within(combined_predictions_2d, {
#   Predicted_Combined_Richness <- exp(fit)
#   LL <- exp(fit - 1.96 * se.fit)
#   UL <- exp(fit + 1.96 * se.fit)
# })
# # Plot for Guana_Head
# Guana_Head_combined_2d <- ggplot(data = combined_predictions_2d, aes(x = Rugosity, y = Predicted_Combined_Richness, color = Year_Factor)) +
#                             geom_ribbon(aes(ymin = LL, ymax = UL, fill = Year_Factor), alpha = 0.25) +
#                             geom_line(aes(color = Year_Factor), size = 2) +
#                             labs(x = "Rugosity - Guana Head", y = "Predicted Combined Richness") +
#                             scale_x_continuous(limits = c(10, 80)) +
#                             scale_y_continuous(limits = c(30, 80)) +
#                             theme(text = element_text(size = 18), 
#                                   panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
#                                   panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
#                                   panel.background = element_blank(), 
#                                   axis.line = element_line(colour = "black"))
# 
# # Organize all of these figures into one window
# grid.arrange(Pelican_Ghut_combined_2d, Grand_Ghut_combined_2d, Crab_Cove_combined_2d, 
#              Muskmelon_combined_2d, Bigelow_combined_2d, White_Bay_combined_2d, 
#              Monkey_Point_combined_2d, Guana_Head_combined_2d, ncol = 3, nrow = 3)



########################################################################
#######################SIMPLE RELATIONSHIPS BETWEEN GROUPS##############

# R-squared value for fish and coral richness
summary(lm(data = variables, formula = Fish_Richness ~ Coral_Richness))$r.squared
# Figure of coral richness vs fish richness with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Coral_Richness, y = Fish_Richness)) +
  geom_point(data = variables, size = 3, aes(x = Coral_Richness, y = Fish_Richness)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Coral Richness", y = "Fish Richness") +
  annotate(geom = "text", x = 18, y = 13, label = "italic(R) ^ 2 == 0.23", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


# R-squared value for sponge and coral richness
summary(lm(data = variables, formula = Sponge_Richness ~ Coral_Richness))$r.squared
# Figure of coral richness vs sponge richness with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Coral_Richness, y = Sponge_Richness)) +
  geom_point(data = variables, size = 3, aes(x = Coral_Richness, y = Sponge_Richness)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Coral Richness", y = "Sponge Richness") +
  annotate(geom = "text", x = 18, y = 35, label = "italic(R) ^ 2 == 0.06", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# R-squared value for fish and sponge richness
summary(lm(data = variables, formula = Fish_Richness ~ Sponge_Richness))$r.squared
# Figure of sponge richness vs fish richness with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Sponge_Richness, y = Fish_Richness)) +
  geom_point(data = variables, size = 3, aes(x = Sponge_Richness, y = Fish_Richness)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Sponge Richness", y = "Fish Richness") +
  annotate(geom = "text", x = 30, y = 35, label = "italic(R) ^ 2 == 0.10", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# R-squared value for cc and sc
summary(lm(data = variables, formula = Percent_Coral_Cover ~ Percent_Sponge_Cover))$r.squared
# Figure of coral cover vs sponge cover with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Percent_Coral_Cover)) +
  geom_point(data = variables, size = 3, aes(x = Percent_Sponge_Cover, y = Percent_Coral_Cover)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Sponge Cover (%)", y = "Coral Cover (%)") +
  annotate(geom = "text", x = 25, y = 35, label = "italic(R) ^ 2 == 0.11", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# R-squared value for cc and r
summary(lm(data = variables, formula = Percent_Coral_Cover ~ Rugosity))$r.squared
# Figure of coral cover vs rugosity with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Rugosity, y = Percent_Coral_Cover)) +
  geom_point(data = variables, size = 3, aes(x = Rugosity, y = Percent_Coral_Cover)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Rugosity", y = "Coral Cover (%)") +
  annotate(geom = "text", x = 25, y = 35, label = "italic(R) ^ 2 == 0.39", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# R-squared value for r and sc
summary(lm(data = variables, formula = Rugosity ~ Percent_Sponge_Cover))$r.squared
# Figure of rugosity vs sponge cover with basic LINEAR line of best fit
ggplot(data = variables, aes(x = Percent_Sponge_Cover, y = Rugosity)) +
  geom_point(data = variables, size = 3, aes(x = Percent_Sponge_Cover, y = Rugosity)) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Sponge Cover (%)", y = "Rugosity") +
  annotate(geom = "text", x = 25, y = 35, label = "italic(R) ^ 2 == 0.12", parse = TRUE) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)),
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


## END OF SCRIPT

# thoughts for results
# > ght<-variables[which(variables$True_Year>2007),]
# > ghtlessthan20<-ght[which(ght$Percent_Coral_Cover<20),]
# > View(ghtlessthan20)
# > summary(ghtlessthan20$Sponge_Richness)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   13.00   20.75   24.50   24.67   29.00   36.00       3 
# > thg<-variables[which(variables$True_Year<2008),]
# > thglessthan20<-thg[which(thg$Percent_Coral_Cover<20),]
# > summary(thglessthan20$Sponge_Richness)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 13.00   18.25   22.00   21.61   24.00   30.00      20 