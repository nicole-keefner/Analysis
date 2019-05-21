### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv
### Objective 4: Compare results of linear and negative binomial models
### Linear in this script; negbin in other script that also has general data analysis independent of distribution used



## Use *** to search for errors in the code or areas that need more work
## Use END OF CLEANED CODE to search for the end of the updated/cleaned code



## Set Working Directory
## Install packages



## Load packages
# First used for aic function, aictab
library(AICcmodavg)
# First used to make figures using function ggplot
library(ggplot2)
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



########################################################################



### Objective 1 for Linear: Create models that only include terms for surrogates 
### in order to determine which of the 3 candidate surrogates is the best at predicting each target. 

# For model names: first word refers to the target, cc represents coral cover, sc represents sponge cover, r represents rugosity
# (e.g. coral_cc is a model with coral cover as the predictor for coral richness)

# For each target, compare models in aic table
coral_cc = lm(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
coral_sc = lm(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
coral_r = lm(formula = Coral_Richness ~ Rugosity, data = variables)
coral_surrogate <- aictab(cand.set = list(coral_cc, coral_sc, coral_r), modnames = c("coral_cc", "coral_sc", "coral_r"), digits = 4)
sponge_cc = lm(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_sc = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sponge_r = lm(formula = Sponge_Richness ~ Rugosity, data = variables)
sponge_surrogate <- aictab(cand.set = list(sponge_cc, sponge_sc, sponge_r), modnames = c("sponge_cc", "sponge_sc", "sponge_r"), digits = 4)
fish_cc = lm(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_sc = lm(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fish_r = lm(formula = Fish_Richness ~ Rugosity, data = variables)
fish_surrogate <- aictab(cand.set = list(fish_cc, fish_sc, fish_r), modnames = c("fish_cc", "fish_sc", "fish_r"), digits = 4)
combined_cc = lm(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_sc = lm(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
combined_r = lm(formula = Combined_Richness ~ Rugosity, data = variables)
combined_surrogate <- aictab(cand.set = list(combined_cc, combined_sc, combined_r), modnames = c("combined_cc", "combined_sc", "combined_r"), digits = 4)



########################################################################



### Objective 2 for Linear: Determine if relationships between targets and candidate 
### surrogates remain consistent over space and time.

## No surrogates in these models:
# Year (time)
coral_yr = lm(formula = Coral_Richness ~ Year, data = variables)
sponge_yr = lm(formula = Sponge_Richness ~ Year, data = variables)
fish_yr = lm(formula = Fish_Richness ~ Year, data = variables)
combined_yr = lm(formula = Combined_Richness ~ Year, data = variables)
# Site (space)
coral_site = lm(formula = Coral_Richness ~ Site, data = variables)
sponge_site = lm(formula = Sponge_Richness ~ Site, data = variables)
fish_site = lm(formula = Fish_Richness ~ Site, data = variables)
combined_site = lm(formula = Combined_Richness ~ Site, data = variables)
# Year + Site
coral_yr_site = lm(formula = Coral_Richness ~ Year + Site, data = variables)
sponge_yr_site = lm(formula = Sponge_Richness ~ Year + Site, data = variables)
fish_yr_site = lm(formula = Fish_Richness ~ Year + Site, data = variables)
combined_yr_site = lm(formula = Combined_Richness ~ Year + Site, data = variables)
# Year + Site + YearxSite
coral_yr_site_yrxsite = lm(formula = Coral_Richness ~ Year + Site + Year*Site, data = variables)
sponge_yr_site_yrxsite = lm(formula = Sponge_Richness ~ Year + Site + Year*Site, data = variables)
fish_yr_site_yrxsite = lm(formula = Fish_Richness ~ Year + Site + Year*Site, data = variables)
combined_yr_site_yrxsite = lm(formula = Combined_Richness ~ Year + Site + Year*Site, data = variables)

## These models have surrogates:
# CC + Year (time)
coral_cc_yr = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Year, data = variables)
sponge_cc_yr = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Year, data = variables)
fish_cc_yr = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Year, data = variables)
combined_cc_yr = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Year, data = variables)
# SC + Year (time)
coral_sc_yr = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Year, data = variables)
sponge_sc_yr = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year, data = variables)
fish_sc_yr = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Year, data = variables)
combined_sc_yr = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Year, data = variables)
# R + Year (time)
coral_r_yr = lm(formula = Coral_Richness ~ Rugosity + Year, data = variables)
sponge_r_yr = lm(formula = Sponge_Richness ~ Rugosity + Year, data = variables)
fish_r_yr = lm(formula = Fish_Richness ~ Rugosity + Year, data = variables)
combined_r_yr = lm(formula = Combined_Richness ~ Rugosity + Year, data = variables)
# CC + Site (space)
coral_cc_site = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Site, data = variables)
sponge_cc_site = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Site, data = variables)
fish_cc_site = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Site, data = variables)
combined_cc_site = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Site, data = variables)
# SC + Site (space)
coral_sc_site = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Site, data = variables)
sponge_sc_site = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Site, data = variables)
fish_sc_site = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Site, data = variables)
combined_sc_site = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Site, data = variables)
# R + Site (space)
coral_r_site = lm(formula = Coral_Richness ~ Rugosity + Site, data = variables)
sponge_r_site = lm(formula = Sponge_Richness ~ Rugosity + Site, data = variables)
fish_r_site = lm(formula = Fish_Richness ~ Rugosity + Site, data = variables)
combined_r_site = lm(formula = Combined_Richness ~ Rugosity + Site, data = variables)
# CC + Year + Site
coral_cc_yr_site = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
sponge_cc_yr_site = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
fish_cc_yr_site = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
combined_cc_yr_site = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
# SC + Year + Site
coral_sc_yr_site = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
sponge_sc_yr_site = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
fish_sc_yr_site = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
combined_sc_yr_site = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Site, data = variables)
# R + Year + Site
coral_r_yr_site = lm(formula = Coral_Richness ~ Rugosity + Year + Site, data = variables)
sponge_r_yr_site = lm(formula = Sponge_Richness ~ Rugosity + Year + Site, data = variables)
fish_r_yr_site = lm(formula = Fish_Richness ~ Rugosity + Year + Site, data = variables)
combined_r_yr_site = lm(formula = Combined_Richness ~ Rugosity + Year + Site, data = variables)
# CC + Year + YearxCC
coral_cc_yr_yrxcc = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
sponge_cc_yr_yrxcc = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
fish_cc_yr_yrxcc = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
combined_cc_yr_yrxcc = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Year*Percent_Coral_Cover, data = variables)
# SC + Year + YearxSC
coral_sc_yr_yrxsc = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
sponge_sc_yr_yrxsc = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
fish_sc_yr_yrxsc = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
combined_sc_yr_yrxsc = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Year*Percent_Sponge_Cover, data = variables)
# R + Year + YearxR
coral_r_yr_yrxr = lm(formula = Coral_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
sponge_r_yr_yrxr = lm(formula = Sponge_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
fish_r_yr_yrxr = lm(formula = Fish_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
combined_r_yr_yrxr = lm(formula = Combined_Richness ~ Rugosity + Year + Year*Rugosity, data = variables)
# CC + Site + SitexCC
coral_cc_site_sitexcc = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
sponge_cc_site_sitexcc = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
fish_cc_site_sitexcc = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
combined_cc_site_sitexcc = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Site + Site*Percent_Coral_Cover, data = variables)
# SC + Site + SitexSC
coral_sc_site_sitexsc = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
sponge_sc_site_sitexsc = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
fish_sc_site_sitexsc = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
combined_sc_site_sitexsc = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Site + Site*Percent_Sponge_Cover, data = variables)
# R + Site + SitexR
coral_r_site_sitexr = lm(formula = Coral_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
sponge_r_site_sitexr = lm(formula = Sponge_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
fish_r_site_sitexr = lm(formula = Fish_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
combined_r_site_sitexr = lm(formula = Combined_Richness ~ Rugosity + Site + Site*Rugosity, data = variables)
# # The following models are not included in AIC tables or interpretations
# # CC + Year + Site + YearxSite
# coral_cc_yr_site_yrxsite = lm(formula = Coral_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# sponge_cc_yr_site_yrxsite = lm(formula = Sponge_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# fish_cc_yr_site_yrxsite = lm(formula = Fish_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# combined_cc_yr_site_yrxsite = lm(formula = Combined_Richness ~ Percent_Coral_Cover + Year + Site + Year*Site, data = variables)
# # SC + Year + Site + YearxSite
# coral_sc_yr_site_yrxsite = lm(formula = Coral_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# sponge_sc_yr_site_yrxsite = lm(formula = Sponge_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# fish_sc_yr_site_yrxsite = lm(formula = Fish_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# combined_sc_yr_site_yrxsite = lm(formula = Combined_Richness ~ Percent_Sponge_Cover + Year + Site + Year*Site, data = variables)
# # R + Year + Site + YearxSite
# coral_r_yr_site_yrxsite = lm(formula = Coral_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# sponge_r_yr_site_yrxsite = lm(formula = Sponge_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# fish_r_yr_site_yrxsite = lm(formula = Fish_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)
# combined_r_yr_site_yrxsite = lm(formula = Combined_Richness ~ Rugosity + Year + Site + Year*Site, data = variables)



## These models have surrogates:
## Logarithmic Models
coral_cc_log = lm(formula = Coral_Richness ~ log(Percent_Coral_Cover), data = variables)
coral_sc_log = lm(formula = Coral_Richness ~ log(Percent_Sponge_Cover), data = variables)
coral_r_log = lm(formula = Coral_Richness ~ log(Rugosity), data = variables)
sponge_cc_log = lm(formula = Sponge_Richness ~ log(Percent_Coral_Cover), data = variables)
sponge_sc_log = lm(formula = Sponge_Richness ~ log(Percent_Sponge_Cover), data = variables)
sponge_r_log = lm(formula = Sponge_Richness ~ log(Rugosity), data = variables)
fish_cc_log = lm(formula = Fish_Richness ~ log(Percent_Coral_Cover), data = variables)
fish_sc_log = lm(formula = Fish_Richness ~ log(Percent_Sponge_Cover), data = variables)
fish_r_log = lm(formula = Fish_Richness ~ log(Rugosity), data = variables)
combined_cc_log = lm(formula = Combined_Richness ~ log(Percent_Coral_Cover), data = variables)
combined_sc_log = lm(formula = Combined_Richness ~ log(Percent_Sponge_Cover), data = variables)
combined_r_log = lm(formula = Combined_Richness ~ log(Rugosity), data = variables)
# CC + Year (time)
coral_cc_yr_log = lm(formula = Coral_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
sponge_cc_yr_log = lm(formula = Sponge_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
fish_cc_yr_log = lm(formula = Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
combined_cc_yr_log = lm(formula = Combined_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# SC + Year (time)
coral_sc_yr_log = lm(formula = Coral_Richness ~ log(Percent_Sponge_Cover) + Year, data = variables)
sponge_sc_yr_log = lm(formula = Sponge_Richness ~ log(Percent_Sponge_Cover) + Year, data = variables)
fish_sc_yr_log = lm(formula = Fish_Richness ~ log(Percent_Sponge_Cover) + Year, data = variables)
combined_sc_yr_log = lm(formula = Combined_Richness ~ log(Percent_Sponge_Cover) + Year, data = variables)
# R + Year (time)
coral_r_yr_log = lm(formula = Coral_Richness ~ log(Rugosity) + Year, data = variables)
sponge_r_yr_log = lm(formula = Sponge_Richness ~ log(Rugosity) + Year, data = variables)
fish_r_yr_log = lm(formula = Fish_Richness ~ log(Rugosity) + Year, data = variables)
combined_r_yr_log = lm(formula = Combined_Richness ~ log(Rugosity) + Year, data = variables)
# CC + Site (space)
coral_cc_site_log = lm(formula = Coral_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
sponge_cc_site_log = lm(formula = Sponge_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
fish_cc_site_log = lm(formula = Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
combined_cc_site_log = lm(formula = Combined_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# SC + Site (space)
coral_sc_site_log = lm(formula = Coral_Richness ~ log(Percent_Sponge_Cover) + Site, data = variables)
sponge_sc_site_log = lm(formula = Sponge_Richness ~ log(Percent_Sponge_Cover) + Site, data = variables)
fish_sc_site_log = lm(formula = Fish_Richness ~ log(Percent_Sponge_Cover) + Site, data = variables)
combined_sc_site_log = lm(formula = Combined_Richness ~ log(Percent_Sponge_Cover) + Site, data = variables)
# R + Site (space)
coral_r_site_log = lm(formula = Coral_Richness ~ log(Rugosity) + Site, data = variables)
sponge_r_site_log = lm(formula = Sponge_Richness ~ log(Rugosity) + Site, data = variables)
fish_r_site_log = lm(formula = Fish_Richness ~ log(Rugosity) + Site, data = variables)
combined_r_site_log = lm(formula = Combined_Richness ~ log(Rugosity) + Site, data = variables)
# CC + Year + Site
coral_cc_yr_site_log = lm(formula = Coral_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
sponge_cc_yr_site_log = lm(formula = Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
fish_cc_yr_site_log = lm(formula = Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
combined_cc_yr_site_log = lm(formula = Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# SC + Year + Site
coral_sc_yr_site_log = lm(formula = Coral_Richness ~ log(Percent_Sponge_Cover) + Year + Site, data = variables)
sponge_sc_yr_site_log = lm(formula = Sponge_Richness ~ log(Percent_Sponge_Cover) + Year + Site, data = variables)
fish_sc_yr_site_log = lm(formula = Fish_Richness ~ log(Percent_Sponge_Cover) + Year + Site, data = variables)
combined_sc_yr_site_log = lm(formula = Combined_Richness ~ log(Percent_Sponge_Cover) + Year + Site, data = variables)
# R + Year + Site
coral_r_yr_site_log = lm(formula = Coral_Richness ~ log(Rugosity) + Year + Site, data = variables)
sponge_r_yr_site_log = lm(formula = Sponge_Richness ~ log(Rugosity) + Year + Site, data = variables)
fish_r_yr_site_log = lm(formula = Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
combined_r_yr_site_log = lm(formula = Combined_Richness ~ log(Rugosity) + Year + Site, data = variables)
# # The following models are not included in AIC tables or interpretations
# # CC + Year + Site + YearxSite
# coral_cc_yr_site_yrxsite_log = lm(formula = Coral_Richness ~ log(Percent_Coral_Cover) + Year + Site + Year*Site, data = variables)
# sponge_cc_yr_site_yrxsite_log = lm(formula = Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site + Year*Site, data = variables)
# fish_cc_yr_site_yrxsite_log = lm(formula = Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site + Year*Site, data = variables)
# combined_cc_yr_site_yrxsite_log = lm(formula = Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site + Year*Site, data = variables)
# # SC + Year + Site + YearxSite
# coral_sc_yr_site_yrxsite_log = lm(formula = Coral_Richness ~ log(Percent_Sponge_Cover) + Year + Site + Year*Site, data = variables)
# sponge_sc_yr_site_yrxsite_log = lm(formula = Sponge_Richness ~ log(Percent_Sponge_Cover) + Year + Site + Year*Site, data = variables)
# fish_sc_yr_site_yrxsite_log = lm(formula = Fish_Richness ~ log(Percent_Sponge_Cover) + Year + Site + Year*Site, data = variables)
# combined_sc_yr_site_yrxsite_log = lm(formula = Combined_Richness ~ log(Percent_Sponge_Cover) + Year + Site + Year*Site, data = variables)
# # R + Year + Site + YearxSite
# coral_r_yr_site_yrxsite_log = lm(formula = Coral_Richness ~ log(Rugosity) + Year + Site + Year*Site, data = variables)
# sponge_r_yr_site_yrxsite_log = lm(formula = Sponge_Richness ~ log(Rugosity) + Year + Site + Year*Site, data = variables)
# fish_r_yr_site_yrxsite_log = lm(formula = Fish_Richness ~ log(Rugosity) + Year + Site + Year*Site, data = variables)
# combined_r_yr_site_yrxsite_log = lm(formula = Combined_Richness ~ log(Rugosity) + Year + Site + Year*Site, data = variables)



## These models have surrogates:
## Power Models
# Not sure how to create power models for additive or interactive models***
## Determine coefficients for power models
# lm(log(Coral_Richness) ~ log(Percent_Coral_Cover), data = variables)
# lm(log(Sponge_Richness) ~ log(Percent_Coral_Cover), data = variables)
# lm(log(Fish_Richness) ~ log(Percent_Coral_Cover), data = variables)
# lm(log(Combined_Richness) ~ log(Percent_Coral_Cover), data = variables)
# lm(log(Coral_Richness) ~ log(Percent_Sponge_Cover), data = variables)
# lm(log(Sponge_Richness) ~ log(Percent_Sponge_Cover), data = variables)
# lm(log(Fish_Richness) ~ log(Percent_Sponge_Cover), data = variables)
# lm(log(Combined_Richness) ~ log(Percent_Sponge_Cover), data = variables)
# lm(log(Coral_Richness) ~ log(Rugosity), data = variables)
# lm(log(Sponge_Richness) ~ log(Rugosity), data = variables)
# lm(log(Fish_Richness) ~ log(Rugosity), data = variables)
# lm(log(Combined_Richness) ~ log(Rugosity), data = variables)
# # a = intercept, b = log(x), use these to make y = (exp(a))*x^b


coral_cc_power = lm(formula = Coral_Richness ~ exp(1.6105 + 0.3192*log(Percent_Coral_Cover)), data = variables)
coral_sc_power = lm(formula = Coral_Richness ~ exp(2.61614 + -0.05529*log(Percent_Sponge_Cover)), data = variables)
coral_r_power = lm(formula = Coral_Richness ~ exp(1.2232 + 0.3443*log(Rugosity)), data = variables)
sponge_cc_power = lm(formula = Sponge_Richness ~ exp(3.4085 + -0.1281*log(Percent_Coral_Cover)), data = variables)
sponge_sc_power = lm(formula = Sponge_Richness ~ exp(2.7577 + 0.1532*log(Percent_Sponge_Cover)), data = variables)
sponge_r_power = lm(formula = Sponge_Richness ~ exp(3.4420 + -0.1045*log(Rugosity)), data = variables)
fish_cc_power = lm(formula = Fish_Richness ~ exp(2.4905 + 0.2335*log(Percent_Coral_Cover)), data = variables)
fish_sc_power = lm(formula = Fish_Richness ~ exp(3.3994 + -0.1335*log(Percent_Sponge_Cover)), data = variables)
fish_r_power = lm(formula = Fish_Richness ~ exp(1.304 + 0.493*log(Rugosity)), data = variables)
combined_cc_power = lm(formula = Combined_Richness ~ exp(3.7769 + 0.1033*log(Percent_Coral_Cover)), data = variables)
combined_sc_power = lm(formula = Combined_Richness ~ exp(4.09341 + -0.01529*log(Percent_Sponge_Cover)), data = variables)
combined_r_power = lm(formula = Combined_Richness ~ exp(3.2614 + 0.2155*log(Rugosity)), data = variables)



## AIC tables to evaluate surrogate effectiveness over space and time
# Names of coral cover models in order they are listed for the AIC tables
cc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "cc", "cc_yr", "cc_site", "cc_yr_site",
                 "cc_log", "cc_yr_log", "cc_site_log", "cc_yr_site_log", "cc_power",
                 "cc_yr_yrxcc", "cc_site_sitexcc") 
# AIC to compare coral_cc models
coral_cc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_cc, coral_cc_yr, coral_cc_site, coral_cc_yr_site,
                                   coral_cc_log, coral_cc_yr_log, coral_cc_site_log, coral_cc_yr_site_log,
                                   coral_cc_power,
                                   coral_cc_yr_yrxcc, coral_cc_site_sitexcc), 
                   modnames = cc_modnames, digits = 4)
# AIC to compare sponge_cc models
sponge_cc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                    sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                    sponge_cc_log, sponge_cc_yr_log, sponge_cc_site_log, sponge_cc_yr_site_log,
                                    sponge_cc_power,
                                    sponge_cc_yr_yrxcc, sponge_cc_site_sitexcc), 
                    modnames = cc_modnames, digits = 4)
# AIC to compare fish_cc models
fish_cc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                  fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site,
                                  fish_cc_log, fish_cc_yr_log, fish_cc_site_log, fish_cc_yr_site_log,
                                  fish_cc_power,
                                  fish_cc_yr_yrxcc, fish_cc_site_sitexcc), 
                  modnames = cc_modnames, digits = 4)
# AIC to compare combined_cc models
combined_cc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                      combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site,
                                      combined_cc_log, combined_cc_yr_log, combined_cc_site_log, combined_cc_yr_site_log,
                                      combined_cc_power,
                                      combined_cc_yr_yrxcc, combined_cc_site_sitexcc), 
                      modnames = cc_modnames, digits = 4)

# Names of sponge cover models in order they are listed for the AIC tables
sc_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "sc", "sc_yr", "sc_site", "sc_yr_site",
                 "sc_log", "sc_yr_log", "sc_site_log", "sc_yr_site_log", "sc_power",
                 "sc_yr_yrxsc", "sc_site_sitexsc") 
# AIC to compare coral_sc models
coral_sc <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                   coral_sc, coral_sc_yr, coral_sc_site, coral_sc_yr_site,
                                   coral_sc_log, coral_sc_yr_log, coral_sc_site_log, coral_sc_yr_site_log,
                                   coral_sc_power,
                                   coral_sc_yr_yrxsc, coral_sc_site_sitexsc), 
                   modnames = sc_modnames, digits = 4)
# AIC to compare sponge_sc models
sponge_sc <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                    sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site,
                                    sponge_sc_log, sponge_sc_yr_log, sponge_sc_site_log, sponge_sc_yr_site_log,
                                    sponge_sc_power,
                                    sponge_sc_yr_yrxsc, sponge_sc_site_sitexsc), 
                    modnames = sc_modnames, digits = 4)
# AIC to compare fish_sc models
fish_sc <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                  fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site,
                                  fish_sc_log, fish_sc_yr_log, fish_sc_site_log, fish_sc_yr_site_log,
                                  fish_sc_power,
                                  fish_sc_yr_yrxsc, fish_sc_site_sitexsc), 
                  modnames = sc_modnames, digits = 4)
# AIC to compare combined_sc models
combined_sc <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                      combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site,
                                      combined_sc_log, combined_sc_yr_log, combined_sc_site_log, combined_sc_yr_site_log,
                                      combined_sc_power,
                                      combined_sc_yr_yrxsc, combined_sc_site_sitexsc), 
                      modnames = sc_modnames, digits = 4)

# Names of rugosity models in order they are listed for the AIC tables
r_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", "r", "r_yr", "r_site", "r_yr_site",
                "r_log", "r_yr_log", "r_site_log", "r_yr_site_log", "r_power",
                "r_yr_yrxr", "r_site_sitexr")  
# AIC to compare coral_r models
coral_r <- aictab(cand.set = list(coral_yr, coral_site, coral_yr_site, coral_yr_site_yrxsite, 
                                  coral_r, coral_r_yr, coral_r_site, coral_r_yr_site,
                                  coral_r_log, coral_r_yr_log, coral_r_site_log, coral_r_yr_site_log,
                                  coral_r_power,
                                  coral_r_yr_yrxr, coral_r_site_sitexr), 
                  modnames = r_modnames, digits = 4)
# AIC to compare sponge_r models
sponge_r <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite, 
                                   sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site,
                                   sponge_r_log, sponge_r_yr_log, sponge_r_site_log, sponge_r_yr_site_log,
                                   sponge_r_power,
                                   sponge_r_yr_yrxr, sponge_r_site_sitexr), 
                   modnames = r_modnames, digits = 4)
# AIC to compare fish_r models
fish_r <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite, 
                                 fish_r, fish_r_yr, fish_r_site, fish_r_yr_site,
                                 fish_r_log, fish_r_yr_log, fish_r_site_log, fish_r_yr_site_log,
                                 fish_r_power,
                                 fish_r_yr_yrxr, fish_r_site_sitexr), 
                 modnames = r_modnames, digits = 4)
# AIC to compare combined_r models
combined_r <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite, 
                                     combined_r, combined_r_yr, combined_r_site, combined_r_yr_site,
                                     combined_r_log, combined_r_yr_log, combined_r_site_log, combined_r_yr_site_log,
                                     combined_r_power,
                                     combined_r_yr_yrxr, combined_r_site_sitexr), 
                     modnames = r_modnames, digits = 4)



########################################################################



### Objective 3 for Linear: Determine the best predictors of each target given our model set.
obj_three_modnames <- c("yr", "site", "yr_site", "yr_site_yrxsite", 
                        "cc", "cc_yr", "cc_site", "cc_yr_site",
                        "sc", "sc_yr", "sc_site", "sc_yr_site",
                        "r", "r_yr", "r_site", "r_yr_site",
                        "cc_log", "cc_yr_log", "cc_site_log", "cc_yr_site_log", "cc_power",
                        "sc_log", "sc_yr_log", "sc_site_log", "sc_yr_site_log", "sc_power",
                        "r_log", "r_yr_log", "r_site_log", "r_yr_site_log", "r_power",
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
                                    coral_cc_log, coral_cc_yr_log, coral_cc_site_log, coral_cc_yr_site_log,
                                    coral_cc_power,
                                    coral_sc_log, coral_sc_yr_log, coral_sc_site_log, coral_sc_yr_site_log,
                                    coral_sc_power,
                                    coral_r_log, coral_r_yr_log, coral_r_site_log, coral_r_yr_site_log,
                                    coral_r_power,
                                    coral_cc_yr_yrxcc, coral_cc_site_sitexcc,
                                    coral_sc_yr_yrxsc, coral_sc_site_sitexsc,
                                    coral_r_yr_yrxr, coral_r_site_sitexr), 
                    modnames = obj_three_modnames, digits = 4)
# AIC to compare all sponge models
sponge_all <- aictab(cand.set = list(sponge_yr, sponge_site, sponge_yr_site, sponge_yr_site_yrxsite,
                                     sponge_cc, sponge_cc_yr, sponge_cc_site, sponge_cc_yr_site,
                                     sponge_sc, sponge_sc_yr, sponge_sc_site, sponge_sc_yr_site,
                                     sponge_r, sponge_r_yr, sponge_r_site, sponge_r_yr_site,
                                     sponge_cc_log, sponge_cc_yr_log, sponge_cc_site_log, sponge_cc_yr_site_log,
                                     sponge_cc_power,
                                     sponge_sc_log, sponge_sc_yr_log, sponge_sc_site_log, sponge_sc_yr_site_log,
                                     sponge_sc_power,
                                     sponge_r_log, sponge_r_yr_log, sponge_r_site_log, sponge_r_yr_site_log,
                                     sponge_r_power,
                                     sponge_cc_yr_yrxcc, sponge_cc_site_sitexcc,
                                     sponge_sc_yr_yrxsc, sponge_sc_site_sitexsc,
                                     sponge_r_yr_yrxr, sponge_r_site_sitexr), 
                     modnames = obj_three_modnames, digits = 4)
# AIC to compare all fish models
fish_all <- aictab(cand.set = list(fish_yr, fish_site, fish_yr_site, fish_yr_site_yrxsite,
                                   fish_cc, fish_cc_yr, fish_cc_site, fish_cc_yr_site,
                                   fish_sc, fish_sc_yr, fish_sc_site, fish_sc_yr_site,
                                   fish_r, fish_r_yr, fish_r_site, fish_r_yr_site,
                                   fish_cc_log, fish_cc_yr_log, fish_cc_site_log, fish_cc_yr_site_log,
                                   fish_cc_power,
                                   fish_sc_log, fish_sc_yr_log, fish_sc_site_log, fish_sc_yr_site_log,
                                   fish_sc_power,
                                   fish_r_log, fish_r_yr_log, fish_r_site_log, fish_r_yr_site_log,
                                   fish_r_power,
                                   fish_cc_yr_yrxcc, fish_cc_site_sitexcc,
                                   fish_sc_yr_yrxsc, fish_sc_site_sitexsc,
                                   fish_r_yr_yrxr, fish_r_site_sitexr), 
                   modnames = obj_three_modnames, digits = 4)
# AIC to compare all "combined" models
combined_all <- aictab(cand.set = list(combined_yr, combined_site, combined_yr_site, combined_yr_site_yrxsite,
                                       combined_cc, combined_cc_yr, combined_cc_site, combined_cc_yr_site,
                                       combined_sc, combined_sc_yr, combined_sc_site, combined_sc_yr_site,
                                       combined_r, combined_r_yr, combined_r_site, combined_r_yr_site,
                                       combined_cc_log, combined_cc_yr_log, combined_cc_site_log, combined_cc_yr_site_log,
                                       combined_cc_power,
                                       combined_sc_log, combined_sc_yr_log, combined_sc_site_log, combined_sc_yr_site_log,
                                       combined_sc_power,
                                       combined_r_log, combined_r_yr_log, combined_r_site_log, combined_r_yr_site_log,
                                       combined_r_power,
                                       combined_cc_yr_yrxcc, combined_cc_site_sitexcc,
                                       combined_sc_yr_yrxsc, combined_sc_site_sitexsc,
                                       combined_r_yr_yrxr, combined_r_site_sitexr),  
                       modnames = obj_three_modnames, digits = 4)

## Save these AIC tables as .csv files
# write.table(x = coral_all, file = "coral_all_linear.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = sponge_all, file = "sponge_all_linear.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = fish_all, file = "fish_all_linear.csv", sep = ",", col.names = TRUE,row.names = FALSE)
# write.table(x = combined_all, file = "combined_all_linear.csv", sep = ",", col.names = TRUE,row.names = FALSE)



########################################################################



## Checking model assumptions***


# These are the most complex models in the candidate set [not including log and power]:
# coral_cc_yr_site; coral_sc_yr_site; coral_r_yr_site
# sponge_cc_yr_site; sponge_sc_yr_site; sponge_r_yr_site
# fish_cc_yr_site; fish_sc_yr_site; fish_r_yr_site
# combined_cc_yr_site; combined_sc_yr_site; combined_r_yr_site




########################################################################




########################################################################
#######################CORAL RICHNESS###################################***



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for coral richness is .
# The function of this model is .
# Model averages?



########################################################################
#######################SPONGE RICHNESS##################################***



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for sponge richness is .
# The function of this model is .
# Model averages?



########################################################################
#######################FISH RICHNESS####################################***



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for fish richness is .
# The function of this model is .
# Model averages?



########################################################################
#######################COMBINED RICHNESS################################***



## Figures of competitive models (<2.0 deltaAIC)

# The most parsimonious model for sponge richness is .
# The function of this model is .
# Model averages?



########################################################################