### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv



#*obj* for completing clear objectives
#*7* = need on 3/26 OR errors OR extraneous code
## Use *** to search for errors in the code or areas that need more work
## Use ***AIC to search for beginning of AIC code
## Use ***Simple Figures with Models to search for beginning of this figure code
## Use ***Simple Figures for Fish Richness to search for beginning of this figure code
## Use ***Simple Figures for Sponge Richness to search for beginning of this figure code
## Use ***Simple Figures for Coral Richness to search for beginning of this figure code
## Use ***Simple Figures for Combined Richness to search for beginning of this figure code



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



## Import variables.csv dataset
variables <- read.csv("variables.csv", header = T)
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
avg_mean <-c(round(mean(variables$Percent_Coral_Cover), digits = 3), 
             round(mean(variables$Percent_Sponge_Cover), digits = 3), 
             round(mean(sponge_complete$Rugosity), digits = 3), 
             round(mean(variables$Coral_Richness), digits = 3), 
             round(mean(variables$Sponge_Richness), digits = 3), 
             round(mean(variables$Fish_Richness), digits = 3),  
             round(mean(sponge_complete$Combined_Richness), digits = 3))
avg_sd <-c(round(sd(variables$Percent_Coral_Cover), digits = 3), 
           round(sd(variables$Percent_Sponge_Cover), digits = 3),
           round(sd(sponge_complete$Rugosity), digits = 3),
           round(sd(variables$Coral_Richness), digits = 3),
           round(sd(variables$Sponge_Richness), digits = 3),
           round(sd(variables$Fish_Richness), digits = 3),
           round(sd(sponge_complete$Combined_Richness), digits = 3))
avg_df <- data.frame(avg_name, avg_mean, avg_sd)

# Graph frequency distributions of response variables
hist(variables$Percent_Coral_Cover, breaks = 25)
hist(variables$Percent_Sponge_Cover, breaks = 25)
hist(variables$Rugosity, breaks = 25)
hist(variables$Coral_Richness, breaks = 25)
hist(variables$Sponge_Richness, breaks = 25)
hist(variables$Fish_Richness, breaks = 25)
hist(variables$Combined_Richness, breaks = 25)

########################################################################

### Objective 1: Create models that only include terms for surrogates 
### in order to determine which of the 3 candidate surrogates is the best at predicting each target. 
# Negative binomial distribution models and aic tables for each target with one surrogate
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
coral_yr = glm.nb(Coral_Richness ~ Year, data = variables)
sponge_yr = glm.nb(Sponge_Richness ~ Year, data = variables)
fish_yr = glm.nb(Fish_Richness ~ Year, data = variables)
combined_yr = glm.nb(Combined_Richness ~ Year, data = variables)
# Site (space)
coral_site = glm.nb(Coral_Richness ~ Site, data = variables)
sponge_site = glm.nb(Sponge_Richness ~ Site, data = variables)
fish_site = glm.nb(Fish_Richness ~ Site, data = variables)
combined_site = glm.nb(Combined_Richness ~ Site, data = variables)
# Year + Site
coral_yr_site = glm.nb(Coral_Richness ~ Year + Site, data = variables)
sponge_yr_site = glm.nb(Sponge_Richness ~ Year + Site, data = variables)
fish_yr_site = glm.nb(Fish_Richness ~ Year + Site, data = variables)
combined_yr_site = glm.nb(Combined_Richness ~ Year + Site, data = variables)
# Year + Site + YearxSite
coral_yr_site_yrxsite = glm.nb(Coral_Richness ~ Year + Site + Year*Site, data = variables)
sponge_yr_site_yrxsite = glm.nb(Sponge_Richness ~ Year + Site + Year*Site, data = variables)
fish_yr_site_yrxsite = glm.nb(Fish_Richness ~ Year + Site + Year*Site, data = variables)
combined_yr_site_yrxsite = glm.nb(Combined_Richness ~ Year + Site + Year*Site, data = variables)

## These models have surrogates:
# CC + Year (time)
coral_cc_yr = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Year, data = variables)
sponge_cc_yr = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Year, data = variables)
fish_cc_yr = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Year, data = variables)
combined_cc_yr = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Year, data = variables)
# SC + Year (time)
coral_sc_yr = glm.nb(Coral_Richness ~ Percent_Sponge_Cover + Year, data = variables)
sponge_sc_yr = glm.nb(Sponge_Richness ~ Percent_Sponge_Cover + Year, data = variables)
fish_sc_yr = glm.nb(Fish_Richness ~ Percent_Sponge_Cover + Year, data = variables)
combined_sc_yr = glm.nb(Combined_Richness ~ Percent_Sponge_Cover + Year, data = variables)
# R + Year (time)
coral_r_yr = glm.nb(Coral_Richness ~ Rugosity + Year, data = variables)
sponge_r_yr = glm.nb(Sponge_Richness ~ Rugosity + Year, data = variables)
fish_r_yr = glm.nb(Fish_Richness ~ Rugosity + Year, data = variables)
combined_r_yr = glm.nb(Combined_Richness ~ Rugosity + Year, data = variables)
# CC + Site (space)
coral_cc_site = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Site, data = variables)
sponge_cc_site = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Site, data = variables)
fish_cc_site = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Site, data = variables)
combined_cc_site = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Site, data = variables)
# SC + Site (space)
coral_sc_site = glm.nb(Coral_Richness ~ Percent_Sponge_Cover + Site, data = variables)
sponge_sc_site = glm.nb(Sponge_Richness ~ Percent_Sponge_Cover + Site, data = variables)
fish_sc_site = glm.nb(Fish_Richness ~ Percent_Sponge_Cover + Site, data = variables)
combined_sc_site = glm.nb(Combined_Richness ~ Percent_Sponge_Cover + Site, data = variables)
# R + Site (space)
coral_r_site = glm.nb(Coral_Richness ~ Rugosity + Site, data = variables)
sponge_r_site = glm.nb(Sponge_Richness ~ Rugosity + Site, data = variables)
fish_r_site = glm.nb(Fish_Richness ~ Rugosity + Site, data = variables)
combined_r_site = glm.nb(Combined_Richness ~ Rugosity + Site, data = variables)
# CC + Year + Site
coral_cc_yr_site = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
sponge_cc_yr_site = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
fish_cc_yr_site = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
combined_cc_yr_site = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
# SC + Year + Site
coral_sc_yr_site = glm.nb(Coral_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
sponge_sc_yr_site = glm.nb(Sponge_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
fish_sc_yr_site = glm.nb(Fish_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
combined_sc_yr_site = glm.nb(Combined_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
# R + Year + Site
coral_r_yr_site = glm.nb(Coral_Richness ~ Rugosity + Year + Site, data = variables)
sponge_r_yr_site = glm.nb(Sponge_Richness ~ Rugosity + Year + Site, data = variables)
fish_r_yr_site = glm.nb(Fish_Richness ~ Rugosity + Year + Site, data = variables)
combined_r_yr_site = glm.nb(Combined_Richness ~ Rugosity + Year + Site, data = variables)
# # The following models are not included in AIC tables or interpretations
# # CC + Year + Site + YearxSite
# coral_cc_yr_site_yrxsite = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# sponge_cc_yr_site_yrxsite = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# fish_cc_yr_site_yrxsite = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# combined_cc_yr_site_yrxsite = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# # SC + Year + Site + YearxSite
# coral_sc_yr_site_yrxsite = glm.nb(Coral_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# sponge_sc_yr_site_yrxsite = glm.nb(Sponge_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# fish_sc_yr_site_yrxsite = glm.nb(Fish_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# combined_sc_yr_site_yrxsite = glm.nb(Combined_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# # R + Year + Site + YearxSite
# coral_r_yr_site_yrxsite = glm.nb(Coral_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# sponge_r_yr_site_yrxsite = glm.nb(Sponge_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# fish_r_yr_site_yrxsite = glm.nb(Fish_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# combined_r_yr_site_yrxsite = glm.nb(Combined_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)

## AIC tables to evaluate surrogate effectiveness over space and time
# Names of coral cover models in order they are listed for the AIC tables
cc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "cc", "cc_yr", "cc_site", "cc_yr_site") 
# AIC to compare coral_cc models
coral_cc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site), 
                   modnames = cc_modnames, digits = 4)
# AIC to compare sponge_cc models
sponge_cc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                    sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site), 
                    modnames = cc_modnames, digits = 4)
# AIC to compare fish_cc models
fish_cc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                    fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site), 
                    modnames = cc_modnames, digits = 4)
# AIC to compare combined_cc models
combined_cc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                    combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site), 
                    modnames = cc_modnames, digits = 4)
# Names of coral cover models in order they are listed for the AIC tables
sc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "sc", "sc_yr", "sc_site", "sc_yr_site") 
# AIC to compare coral_sc models
coral_sc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_sc, coral_sc_yr, coral_sc_site, coral_sc_yr_site), 
                   modnames = sc_modnames, digits = 4)
# AIC to compare sponge_sc models
sponge_sc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                   sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site), 
                   modnames = sc_modnames, digits = 4)
# AIC to compare fish_sc models
fish_sc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                   fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site), 
                   modnames = sc_modnames, digits = 4)
# AIC to compare combined_sc models
combined_sc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                   combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site), 
                   modnames = sc_modnames, digits = 4)
# Names of coral cover models in order they are listed for the AIC tables
r_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "r", "r_yr", "r_site", "r_yr_site") 
# AIC to compare coral_r models
coral_r <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_r, coral_r_yr, coral_r_site, coral_r_yr_site), 
                   modnames = r_modnames, digits = 4)
# AIC to compare sponge_r models
sponge_r <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                   sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site), 
                   modnames = r_modnames, digits = 4)
# AIC to compare fish_r models
fish_r <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                   fish_r, fish_r_yr, fish_r_site, fish_r_yr_site), 
                   modnames = r_modnames, digits = 4)
# AIC to compare combined_r models
combined_r <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                   combined_r, combined_r_yr, combined_r_site, combined_r_yr_site), 
                   modnames = r_modnames, digits = 4)

########################################################################

### Objective 3:Determine the best predictors of each target given our model set.
obj_three_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", 
                  "cc", "cc_yr", "cc_site", "cc_yr_site",
                  "sc", "sc_yr", "sc_site", "sc_yr_site",
                  "r", "r_yr", "r_site", "r_yr_site") 
# AIC to compare all coral models
coral_all <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite,
                                    coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site,
                                    coral_sc, coral_sc_yr, coral_sc_site, coral_sc_yr_site,
                                    coral_r, coral_r_yr, coral_r_site, coral_r_yr_site), 
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all sponge models
sponge_all <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite,
                                    sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                    sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site,
                                    sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site), 
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all fish models
fish_all <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite,
                                    fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site,
                                    fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site,
                                    fish_r, fish_r_yr, fish_r_site, fish_r_yr_site), 
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all "combined" models
combined_all <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite,
                                    combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site,
                                    combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site,
                                    combined_r, combined_r_yr, combined_r_site, combined_r_yr_site), 
                    modnames = obj_three_modnames, digits = 4)

## Model output for competitive models (<2.0 deltaAIC)
# Look at significance of model coefficients
# For coral richness
summary(coral_cc_yr)
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
summary(combined_cc_yr_site)

########################################################################

## Should I include sur + yr + sur*yr and/or sur + site + sur*site? 
# NO ecological justification
# I can only think that the first one might be relevant if something large timescale happened 
# that changed the relationship between number of species and surrogate like coral cover. 
# But I doubt anything like that would be captured in only 27 years.
# These would mean that the effect of surrogate on target is different for different yrs (or sites)
# i.e. the slopes of regression lines are different for different sites or that there are 3 dimensions

## Should I include sur + yr + site + yr*site?
# NO ecological justification
# This would mean that the effect of time on the target is different for different sites and that
# the surrogate improves the model, but does not modify the effect of time or site on the target.
# This might be the case if different sites experience different conditions over time 
# (e.g. disturbance is more common at one site than another and the level of disturbance changes over time)
# AND that there was additionally variation in the data that could be further explained by the surrogate


#*obj* Really consider how to evaluate the models for objective 2
#*obj* Really figure out what the top models mean for objective 3
#*obj* Test model assumptions and normality of residuals (Bird_regression files and logistic_lab)
#*obj* Objective 4: Create new script for linear models with log and power models

#*obj* Work on figures of top models (see image on phone)
#*obj* Make sure to include prediction lines on model figures
#*obj* Basic graphs

########################################################################

# For Poisson distribution:
#fish_year_p = glm(Fish_Richness ~ Year, data = variables, family = poisson)

# Compare log liklihoods between negative binomial (m1; df=2) and Poisson (m2; df=1)
m1 = glm.nb(Fish_Richness ~ 1, data = variables)
m2 = glm(Fish_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
# Found this on 2 different websites, but I don't know how to interpret the result
#pchisq(2 * (logLik(m1) - logLik(m2)), df = 1, lower.tail = FALSE)

m1 = glm.nb(Coral_Richness ~ 1, data = variables)
m2 = glm(Coral_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Sponge_Richness ~ 1, data = variables)
m2 = glm(Sponge_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Sponge_and_Fish_Richness ~ 1, data = sponge_complete)
m2 = glm(Sponge_and_Fish_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Combined_Richness ~ 1, data = variables)
m2 = glm(Combined_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)



# ***When feel comfortable regarding models, review again with year as categorical for more support



# Interactive models
fish_site_rugosity_rugositysite = glm.nb(Fish_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
fish_site_cover_coversite = glm.nb(Fish_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
fish_site_coralrichness_coralrichnesssite = glm.nb(Fish_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
fish_year_rugosity_rugosityyear = glm.nb(Fish_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
fish_year_cover_coveryear = glm.nb(Fish_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
fish_year_coralrichness_coralrichnessyear = glm.nb(Fish_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
fish_rugosity_log = glm.nb(Fish_Richness ~ log(Rugosity), data = variables)
fish_rugosity_site_log = glm.nb(Fish_Richness ~ log(Rugosity) + Site, data = variables)
fish_rugosity_year_log = glm.nb(Fish_Richness ~ log(Rugosity) + Year, data = variables)
fish_rugosity_year_site_log = glm.nb(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
fish_year_site_yearsite_rugosity_log = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
fish_site_rugosity_rugositysite_log = glm.nb(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
fish_year_rugosity_rugosityyear_log = glm.nb(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
fish_cover_log = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover), data = variables)
fish_cover_site_log = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
fish_cover_year_log = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
fish_cover_year_site_log = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
fish_year_site_yearsite_cover_log = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
fish_site_cover_coversite_log = glm.nb(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
fish_year_cover_coveryear_log = glm.nb(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
fish_coralrichness_log = glm.nb(Fish_Richness ~ log(Coral_Richness), data = variables)
fish_coralrichness_site_log = glm.nb(Fish_Richness ~ log(Coral_Richness) + Site, data = variables)
fish_coralrichness_year_log = glm.nb(Fish_Richness ~ log(Coral_Richness) + Year, data = variables)
fish_coralrichness_year_site_log = glm.nb(Fish_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
fish_year_site_yearsite_coralrichness_log = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
fish_site_coralrichness_coralrichnesssite_log = glm.nb(Fish_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
fish_year_coralrichness_coralrichnessyear_log = glm.nb(Fish_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
glm.nb(log(Fish_Richness) ~ log(Rugosity), data = variables)
# a = intercept, b = log(x), use these to make y = (exp(a))*x^b
glm.nb(log(Fish_Richness) ~ log(Percent_Coral_Cover), data = variables)
glm.nb(log(Fish_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity*7* NOT SURE HOW TO INCLUDE POWER FUNCTION FOR ADDITIVE AND INTERACTIVE MODELS
fish_rugosity_power = glm.nb(Fish_Richness ~ exp(1.3053 + 0.4944*log(Rugosity)), data = variables)
# fish_rugosity_site_power = glm.nb(Fish_Richness ~ log(Rugosity) + Site, data = variables)
# fish_rugosity_year_power = glm.nb(Fish_Richness ~ log(Rugosity) + Year, data = variables)
# fish_rugosity_year_site_power = glm.nb(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
# fish_year_site_yearsite_rugosity_power = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# fish_site_rugosity_rugositysite_power = glm.nb(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# fish_year_rugosity_rugosityyear_power = glm.nb(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
fish_cover_power = glm.nb(Fish_Richness ~ exp(2.5059 + 0.2305*log(Percent_Coral_Cover)), data = variables)
# fish_cover_site_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# fish_cover_year_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# fish_cover_year_site_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# fish_year_site_yearsite_cover_power = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# fish_site_cover_coversite_power = glm.nb(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# fish_year_cover_coveryear_power = glm.nb(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
fish_coralrichness_power = glm.nb(Fish_Richness ~ exp(2.1422 + 0.4041*log(Coral_Richness)), data = variables)
# fish_coralrichness_site_power = glm.nb(Fish_Richness ~ log(Coral_Richness) + Site, data = variables)
# fish_coralrichness_year_power = glm.nb(Fish_Richness ~ log(Coral_Richness) + Year, data = variables)
# fish_coralrichness_year_site_power = glm.nb(Fish_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# fish_year_site_yearsite_coralrichness_power = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# fish_site_coralrichness_coralrichnesssite_power = glm.nb(Fish_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# fish_year_coralrichness_coralrichnessyear_power = glm.nb(Fish_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
# 
fish_year_sqyear = glm.nb(Fish_Richness ~ Year + Year^2, data = variables)



# # An example of model output in table format
# tab_model(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
#           fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
#           fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
#           fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
#           fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
#           fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear)
# # With logarithmic and power models
# tab_model(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
#           fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
#           fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
#           fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
#           fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
#           fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear, 
#           fish_rugosity_log, fish_rugosity_site_log, fish_rugosity_year_log, fish_rugosity_year_site_log, 
#           fish_year_site_yearsite_rugosity_log, fish_site_rugosity_rugositysite_log, fish_year_rugosity_rugosityyear_log, 
#           fish_cover_log, fish_cover_site_log, fish_cover_year_log, fish_cover_year_site_log, 
#           fish_year_site_yearsite_cover_log, fish_site_cover_coversite_log, fish_year_cover_coveryear_log, 
#           fish_coralrichness_log, fish_coralrichness_site_log, fish_coralrichness_year_log, 
#           fish_coralrichness_year_site_log, fish_year_site_yearsite_coralrichness_log, 
#           fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log,
#           fish_rugosity_power, fish_rugosity_site_power, fish_rugosity_year_power, fish_rugosity_year_site_power, 
#           fish_year_site_yearsite_rugosity_power, fish_site_rugosity_rugositysite_power, fish_year_rugosity_rugosityyear_power, 
#           fish_cover_power, fish_cover_site_power, fish_cover_year_power, fish_cover_year_site_power, 
#           fish_year_site_yearsite_cover_power, fish_site_cover_coversite_power, fish_year_cover_coveryear_power, 
#           fish_coralrichness_power, fish_coralrichness_site_power, fish_coralrichness_year_power, 
#           fish_coralrichness_year_site_power, fish_year_site_yearsite_coralrichness_power, 
#           fish_site_coralrichness_coralrichnesssite_power, fish_year_coralrichness_coralrichnessyear_power)





# Interactive models
sponge_site_rugosity_rugositysite = glm.nb(Sponge_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
sponge_site_cover_coversite = glm.nb(Sponge_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
sponge_site_coralrichness_coralrichnesssite = glm.nb(Sponge_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
sponge_year_rugosity_rugosityyear = glm.nb(Sponge_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
sponge_year_cover_coveryear = glm.nb(Sponge_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
sponge_year_coralrichness_coralrichnessyear = glm.nb(Sponge_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
sponge_rugosity_log = glm.nb(Sponge_Richness ~ log(Rugosity), data = variables)
sponge_rugosity_site_log = glm.nb(Sponge_Richness ~ log(Rugosity) + Site, data = variables)
sponge_rugosity_year_log = glm.nb(Sponge_Richness ~ log(Rugosity) + Year, data = variables)
sponge_rugosity_year_site_log = glm.nb(Sponge_Richness ~ log(Rugosity) + Year + Site, data = variables)
sponge_year_site_yearsite_rugosity_log = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
sponge_site_rugosity_rugositysite_log = glm.nb(Sponge_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
sponge_year_rugosity_rugosityyear_log = glm.nb(Sponge_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
sponge_cover_log = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover), data = variables)
sponge_cover_site_log = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
sponge_cover_year_log = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
sponge_cover_year_site_log = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
sponge_year_site_yearsite_cover_log = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
sponge_site_cover_coversite_log = glm.nb(Sponge_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
sponge_year_cover_coveryear_log = glm.nb(Sponge_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
sponge_coralrichness_log = glm.nb(Sponge_Richness ~ log(Coral_Richness), data = variables)
sponge_coralrichness_site_log = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Site, data = variables)
sponge_coralrichness_year_log = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Year, data = variables)
sponge_coralrichness_year_site_log = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
sponge_year_site_yearsite_coralrichness_log = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
sponge_site_coralrichness_coralrichnesssite_log = glm.nb(Sponge_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
sponge_year_coralrichness_coralrichnessyear_log = glm.nb(Sponge_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
glm.nb(log(Sponge_Richness) ~ log(Rugosity), data = variables)
glm.nb(log(Sponge_Richness) ~ log(Percent_Coral_Cover), data = variables)
glm.nb(log(Sponge_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity
sponge_rugosity_power = glm.nb(Sponge_Richness ~ exp(3.4258 + -0.1012*log(Rugosity)), data = variables)
# sponge_rugosity_site_power = glm.nb(Sponge_Richness ~ log(Rugosity) + Site, data = variables)
# sponge_rugosity_year_power = glm.nb(Sponge_Richness ~ log(Rugosity) + Year, data = variables)
# sponge_rugosity_year_site_power = glm.nb(Sponge_Richness ~ log(Rugosity) + Year + Site, data = variables)
# sponge_year_site_yearsite_rugosity_power = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# sponge_site_rugosity_rugositysite_power = glm.nb(Sponge_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# sponge_year_rugosity_rugosityyear_power = glm.nb(Sponge_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
sponge_cover_power = glm.nb(Sponge_Richness ~ exp(3.4226 + -0.1344*log(Percent_Coral_Cover)), data = variables)
# sponge_cover_site_power = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# sponge_cover_year_power = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# sponge_cover_year_site_power = glm.nb(Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# sponge_year_site_yearsite_cover_power = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# sponge_site_cover_coversite_power = glm.nb(Sponge_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# sponge_year_cover_coveryear_power = glm.nb(Sponge_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
sponge_coralrichness_power = glm.nb(Sponge_Richness ~ exp(3.5587 + -0.2029*log(Coral_Richness)), data = variables)
# sponge_coralrichness_site_power = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Site, data = variables)
# sponge_coralrichness_year_power = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Year, data = variables)
# sponge_coralrichness_year_site_power = glm.nb(Sponge_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# sponge_year_site_yearsite_coralrichness_power = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# sponge_site_coralrichness_coralrichnesssite_power = glm.nb(Sponge_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# sponge_year_coralrichness_coralrichnessyear_power = glm.nb(Sponge_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
# 
sponge_year_sqyear = glm.nb(Sponge_Richness ~ Year + Year^2, data = variables)



# Interactive models
coral_year_site_yearsite = glm.nb(Coral_Richness ~ Year + Site + Year*Site, data = variables)
coral_year_site_yearsite_rugosity = glm.nb(Coral_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
coral_year_site_yearsite_cover = glm.nb(Coral_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
#coral_year_site_yearsite_coralrichness = glm.nb(Coral_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
coral_site_rugosity_rugositysite = glm.nb(Coral_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
coral_site_cover_coversite = glm.nb(Coral_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
#coral_site_coralrichness_coralrichnesssite = glm.nb(Coral_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
coral_year_rugosity_rugosityyear = glm.nb(Coral_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
coral_year_cover_coveryear = glm.nb(Coral_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
#coral_year_coralrichness_coralrichnessyear = glm.nb(Coral_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
coral_rugosity_log = glm.nb(Coral_Richness ~ log(Rugosity), data = variables)
coral_rugosity_site_log = glm.nb(Coral_Richness ~ log(Rugosity) + Site, data = variables)
coral_rugosity_year_log = glm.nb(Coral_Richness ~ log(Rugosity) + Year, data = variables)
coral_rugosity_year_site_log = glm.nb(Coral_Richness ~ log(Rugosity) + Year + Site, data = variables)
coral_year_site_yearsite_rugosity_log = glm.nb(Coral_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
coral_site_rugosity_rugositysite_log = glm.nb(Coral_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
coral_year_rugosity_rugosityyear_log = glm.nb(Coral_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
coral_cover_log = glm.nb(Coral_Richness ~ log(Percent_Coral_Cover), data = variables)
coral_cover_site_log = glm.nb(Coral_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
coral_cover_year_log = glm.nb(Coral_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
coral_cover_year_site_log = glm.nb(Coral_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
coral_year_site_yearsite_cover_log = glm.nb(Coral_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
coral_site_cover_coversite_log = glm.nb(Coral_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
coral_year_cover_coveryear_log = glm.nb(Coral_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
## Get coefficients for power models
glm.nb(log(Coral_Richness) ~ log(Rugosity), data = variables)
glm.nb(log(Coral_Richness) ~ log(Percent_Coral_Cover), data = variables)
# # Power models for rugosity
coral_rugosity_power = glm.nb(Coral_Richness ~ exp(1.2763 + 0.3302*log(Rugosity)), data = variables)
# coral_rugosity_site_power = glm.nb(Fish_Richness ~ log(Rugosity) + Site, data = variables)
# coral_rugosity_year_power = glm.nb(Fish_Richness ~ log(Rugosity) + Year, data = variables)
# coral_rugosity_year_site_power = glm.nb(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
# coral_year_site_yearsite_rugosity_power = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# coral_site_rugosity_rugositysite_power = glm.nb(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# coral_year_rugosity_rugosityyear_power = glm.nb(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
coral_cover_power = glm.nb(Coral_Richness ~ exp(1.6342 + 0.3108*log(Percent_Coral_Cover)), data = variables)
# coral_cover_site_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# coral_cover_year_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# coral_cover_year_site_power = glm.nb(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# coral_year_site_yearsite_cover_power = glm.nb(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# coral_site_cover_coversite_power = glm.nb(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# coral_year_cover_coveryear_power = glm.nb(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# 
coral_year_sqyear = glm.nb(Coral_Richness ~ Year + Year^2, data = variables)



# Models for combined richness
# Interactive models
combined_year_site_yearsite = glm.nb(Combined_Richness ~ Year + Site + Year*Site, data = variables)
combined_year_site_yearsite_rugosity = glm.nb(Combined_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
combined_year_site_yearsite_cover = glm.nb(Combined_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
#combined_year_site_yearsite_coralrichness = glm.nb(Combined_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
combined_site_rugosity_rugositysite = glm.nb(Combined_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
combined_site_cover_coversite = glm.nb(Combined_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
#combined_site_coralrichness_coralrichnesssite = glm.nb(Combined_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
combined_year_rugosity_rugosityyear = glm.nb(Combined_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
combined_year_cover_coveryear = glm.nb(Combined_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
#combined_year_coralrichness_coralrichnessyear = glm.nb(Combined_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
combined_rugosity_log = glm.nb(Combined_Richness ~ log(Rugosity), data = variables)
combined_rugosity_site_log = glm.nb(Combined_Richness ~ log(Rugosity) + Site, data = variables)
combined_rugosity_year_log = glm.nb(Combined_Richness ~ log(Rugosity) + Year, data = variables)
combined_rugosity_year_site_log = glm.nb(Combined_Richness ~ log(Rugosity) + Year + Site, data = variables)
combined_year_site_yearsite_rugosity_log = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
combined_site_rugosity_rugositysite_log = glm.nb(Combined_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
combined_year_rugosity_rugosityyear_log = glm.nb(Combined_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
combined_cover_log = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover), data = variables)
combined_cover_site_log = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
combined_cover_year_log = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
combined_cover_year_site_log = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
combined_year_site_yearsite_cover_log = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
combined_site_cover_coversite_log = glm.nb(Combined_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
combined_year_cover_coveryear_log = glm.nb(Combined_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
#combined_coralrichness_log = glm.nb(Combined_Richness ~ log(Coral_Richness), data = variables)
#combined_coralrichness_site_log = glm.nb(Combined_Richness ~ log(Coral_Richness) + Site, data = variables)
#combined_coralrichness_year_log = glm.nb(Combined_Richness ~ log(Coral_Richness) + Year, data = variables)
#combined_coralrichness_year_site_log = glm.nb(Combined_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
#combined_year_site_yearsite_coralrichness_log = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
#combined_site_coralrichness_coralrichnesssite_log = glm.nb(Combined_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
#combined_year_coralrichness_coralrichnessyear_log = glm.nb(Combined_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
glm.nb(log(Combined_Richness) ~ log(Rugosity), data = variables)
glm.nb(log(Combined_Richness) ~ log(Percent_Coral_Cover), data = variables)
#glm.nb(log(Combined_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity
combined_rugosity_power = glm.nb(Combined_Richness ~ exp(3.2653 + 0.2149*log(Rugosity)), data = variables)
# combined_rugosity_site_power = glm.nb(Combined_Richness ~ log(Rugosity) + Site, data = variables)
# combined_rugosity_year_power = glm.nb(Combined_Richness ~ log(Rugosity) + Year, data = variables)
# combined_rugosity_year_site_power = glm.nb(Combined_Richness ~ log(Rugosity) + Year + Site, data = variables)
# combined_year_site_yearsite_rugosity_power = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# combined_site_rugosity_rugositysite_power = glm.nb(Combined_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# combined_year_rugosity_rugosityyear_power = glm.nb(Combined_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
combined_cover_power = glm.nb(Combined_Richness ~ exp(3.79088 + 0.09888*log(Percent_Coral_Cover)), data = variables)
# combined_cover_site_power = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# combined_cover_year_power = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# combined_cover_year_site_power = glm.nb(Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# combined_year_site_yearsite_cover_power = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# combined_site_cover_coversite_power = glm.nb(Combined_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# combined_year_cover_coveryear_power = glm.nb(Combined_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
# combined_coralrichness_power = glm.nb(Combined_Richness ~ exp(3.3032 + 0.3036*log(Coral_Richness)), data = variables)
# # combined_coralrichness_site_power = glm.nb(Combined_Richness ~ log(Coral_Richness) + Site, data = variables)
# # combined_coralrichness_year_power = glm.nb(Combined_Richness ~ log(Coral_Richness) + Year, data = variables)
# # combined_coralrichness_year_site_power = glm.nb(Combined_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# # combined_year_site_yearsite_coralrichness_power = glm.nb(Combined_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# # combined_site_coralrichness_coralrichnesssite_power = glm.nb(Combined_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# # combined_year_coralrichness_coralrichnessyear_power = glm.nb(Combined_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
# 
combined_year_sqyear = glm.nb(Combined_Richness ~ Year + Year^2, data = variables)

##################################***AIC









## AIC

## Fish models including all logarithmic models and simple power models
fish_models <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
                    fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
                    fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
                    fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
                    fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
                    fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear, 
                    fish_rugosity_log, fish_rugosity_site_log, fish_rugosity_year_log, fish_rugosity_year_site_log, 
                    fish_year_site_yearsite_rugosity_log, fish_site_rugosity_rugositysite_log, fish_year_rugosity_rugosityyear_log, 
                    fish_cover_log, fish_cover_site_log, fish_cover_year_log, fish_cover_year_site_log, 
                    fish_year_site_yearsite_cover_log, fish_site_cover_coversite_log, fish_year_cover_coveryear_log, 
                    fish_coralrichness_log, fish_coralrichness_site_log, fish_coralrichness_year_log, 
                    fish_coralrichness_year_site_log, fish_year_site_yearsite_coralrichness_log, 
                    fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log, 
                    fish_rugosity_power, fish_cover_power, fish_coralrichness_power, fish_year_sqyear)
# Name each model in the same order they're listed in fish_models
model_names <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
                 "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
                 "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
                 "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
                 "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
                 "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year",
                 "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year", 
                 "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site", 
                 "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year", 
                 "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site", 
                 "log(cover) + year + log(cover)*year", "log(coralrichness)", 
                 "log(coralrichness) + site", "log(coralrichness) + year", "log(coralrichness) + site + year", 
                 "log(coralrichness) + site + year + year*site", "log(coralrichness) + site + log(coralrichness)*site", 
                 "log(coralrichness) + year + log(coralrichness)*year",
                 "power(rugosity)", "power(cover)", "power(coralrichness)", "year_sqyear")
# AIC table
fish_aic_table <- aictab(fish_models, modnames = model_names, digits = 4)
# Create file with AIC table
#write.csv(fish_aic_table, file = "nb_fish_aic_table.csv")




## Sponge models including all logarithmic models and simple power models
sponge_models <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
                      sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
                      sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
                      sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
                      sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
                      sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear, 
                      sponge_rugosity_log, sponge_rugosity_site_log, sponge_rugosity_year_log, sponge_rugosity_year_site_log, 
                      sponge_year_site_yearsite_rugosity_log, sponge_site_rugosity_rugositysite_log, sponge_year_rugosity_rugosityyear_log, 
                      sponge_cover_log, sponge_cover_site_log, sponge_cover_year_log, sponge_cover_year_site_log, 
                      sponge_year_site_yearsite_cover_log, sponge_site_cover_coversite_log, sponge_year_cover_coveryear_log, 
                      sponge_coralrichness_log, sponge_coralrichness_site_log, sponge_coralrichness_year_log, 
                      sponge_coralrichness_year_site_log, sponge_year_site_yearsite_coralrichness_log, 
                      sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log, 
                      sponge_rugosity_power, sponge_cover_power, sponge_coralrichness_power, sponge_year_sqyear)
# AIC table
sponge_aic_table <- aictab(sponge_models, modnames = model_names, digits = 4)
# Create file with AIC table
#write.csv(sponge_aic_table, file = "nb_sponge_aic_table.csv")




## Coral models including all logarithmic models and simple power models
coral_models <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site,
                     coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site,
                     coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity,
                     coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear,
                     coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log,
                     coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log,
                     coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log,
                     coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log,
                     coral_rugosity_power, coral_cover_power, coral_year_sqyear)
# Name each model in the same order they're listed in coral_models
coral_model_names <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
                       "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
                       "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
                       "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
                       "year + cover + cover*year",
                       "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
                       "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
                       "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
                       "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
                       "log(cover) + year + log(cover)*year", "power(rugosity)", "power(cover)", "year_sqyear")


# AIC table
coral_aic_table <- aictab(coral_models, modnames = coral_model_names, digits = 4)
# Create file with AIC table
#write.csv(coral_aic_table, file = "nb_coral_aic_table.csv")


## Combined models including all logarithmic models and simple power models
combined_models <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_year_site, combined_rugosity_site, 
                        combined_cover_site, combined_rugosity_year, combined_cover_year,
                        combined_rugosity_year_site, combined_cover_year_site, combined_year_site_yearsite,
                        combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity,
                        combined_site_rugosity_rugositysite, combined_site_cover_coversite,
                        combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, 
                        combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
                        combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
                        combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
                        combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
                        combined_rugosity_power, combined_cover_power, combined_year_sqyear)
# AIC table
combined_aic_table <- aictab(combined_models, modnames = coral_model_names, digits = 4)
# Create file with AIC table
#write.csv(combined_aic_table, file = "nb_combined_aic_table.csv")
########################################################################















# ***Simple Figures with Models



# Figure. Rugosity as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  #geom_point(size = 5, aes(color = variables$Site))+
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.3053 + 0.4944*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        #axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.5059 + 0.2305*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.1422 + 0.4041*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4258 + -0.1012*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4226 + -0.1344*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.5587 + -0.2029*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for coral richness with linear model only.
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(0.8607 + 0.9674*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for coral richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.6342 + 0.3108*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.2653 + 0.2149*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.79088 + 0.09888*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.3032 + 0.3036*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)
# Rugosity over time
ggplot(variables, aes(x = True_Year, y = Rugosity, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Rugosity") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Coral cover over time
ggplot(variables, aes(x = True_Year, y = Percent_Coral_Cover, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Cover (%)") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



## ***Simple Figures for Fish Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Fish_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Sponge Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# ***Notice that this figure has line breaks where the value for Sponge Richness in that given year is NA
# # The following code will remove the line breaks by deleting the years without values and merging the line segments 
# ggplot(variables[!is.na(variables$Sponge_Richness),], aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name ="Time (year)") +
#   scale_y_continuous(name ="Sponge Richness")

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Coral Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Coral_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Combined Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Combined_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

################################################################

# Create HTML files for most parsimonious models and models with one variable
tab_model(fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year)
tab_model(sponge_rugosity_year_site_log, sponge_site, sponge_cover, sponge_coralrichness, sponge_year, sponge_rugosity)
tab_model(coral_cover_year_log, coral_cover, coral_site, coral_rugosity, coral_year)
tab_model(combined_cover_year_site_log, combined_site, combined_rugosity, combined_cover, combined_year)
tab_model(fishsponge_year_site, fishsponge_site, fishsponge_coralrichness, fishsponge_year)

##############################################################################################################################################################








## Figures for NEAFWA

# Coral richness across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Sponge richness across time and with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness across time and with separate site lines when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_and_Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Combined richness across time and with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness with separate site lines when x = Time
ggplot(variables, aes(x = True_Year, y = Sponge_and_Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with site
ggplot(variables, aes(x = Site, y = Fish_Richness, fill = Site)) + 
  scale_fill_viridis_d()+
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  scale_y_continuous(name = "Fish Richness") +
  theme(legend.position = "none",
        text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#######################################################################################################################
coral_cc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
coral_sc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
coral_r = glm.nb(formula = Coral_Richness ~ Rugosity, data = variables)
coral_cc_sc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
coral_cc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
coral_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
coral_cc_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
coral_surrogate <- aictab(cand.set = list(coral_cc, coral_sc, coral_r, coral_cc_sc, coral_cc_r, coral_sc_r, coral_cc_sc_r), 
                          modnames = c("coral_cc", "coral_sc", "coral_r", "coral_cc_sc", "coral_cc_r", "coral_sc_r", "coral_cc_sc_r"), 
                          digits = 4)
sponge_cc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_sc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sponge_r = glm.nb(formula = Sponge_Richness ~ Rugosity, data = variables)
sponge_cc_sc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
sponge_cc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
sponge_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
sponge_cc_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
sponge_surrogate <- aictab(cand.set = list(sponge_cc, sponge_sc, sponge_r, sponge_cc_sc, sponge_cc_r, sponge_sc_r, sponge_cc_sc_r), 
                           modnames = c("sponge_cc", "sponge_sc", "sponge_r", "sponge_cc_sc", "sponge_cc_r", "sponge_sc_r", "sponge_cc_sc_r"), 
                           digits = 4)
fish_cc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_sc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fish_r = glm.nb(formula = Fish_Richness ~ Rugosity, data = variables)
fish_cc_sc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
fish_cc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
fish_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
fish_cc_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
fish_surrogate <- aictab(cand.set = list(fish_cc, fish_sc, fish_r, fish_cc_sc, fish_cc_r, fish_sc_r, fish_cc_sc_r), 
                         modnames = c("fish_cc", "fish_sc", "fish_r", "fish_cc_sc", "fish_cc_r", "fish_sc_r", "fish_cc_sc_r"), 
                         digits = 4)
combined_cc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_sc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
combined_r = glm.nb(formula = Combined_Richness ~ Rugosity, data = variables)
combined_cc_sc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
combined_cc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
combined_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
combined_cc_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
combined_surrogate <- aictab(cand.set = list(combined_cc, combined_sc, combined_r, combined_cc_sc, combined_cc_r, combined_sc_r, combined_cc_sc_r), 
                             modnames = c("combined_cc", "combined_sc", "combined_r", "combined_cc_sc", "combined_cc_r", "combined_sc_r", "combined_cc_sc_r"), 
                             digits = 4)