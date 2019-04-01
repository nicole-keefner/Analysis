### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv

#*7* = need on 3/26

## Use *** to search for errors in the code or areas that need more work
## Use ***Figures to search for beginning of figure code


## Set Working Directory
## Install packages



## Load packages
# First used for model output in HTML table format using function tab_model
library(sjPlot)
# First used for aic function, aictab
library(AICcmodavg)
# First used to make figures using function ggplot
library(ggplot2)

# library(tidyverse)
# library(tidyr)
# library(plyr)
# library(dplyr)
# library(reshape2)

## Import variables.csv dataset
variables <- read.csv("variables.csv", header = T)


# ***When feel comfortable regarding models, review again with year as categorical for more support
# Change year to numeric to make it continuous for models
variables$Year <- as.numeric(variables$Year)

# Models for fish richness
fish_year = lm(Fish_Richness ~ Year, data = variables)
fish_site = lm(Fish_Richness ~ Site, data = variables)
fish_rugosity = lm(Fish_Richness ~ Rugosity, data = variables)
fish_cover = lm(Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_coralrichness = lm(Fish_Richness ~ Coral_Richness, data = variables)
# Additive models
fish_year_site = lm(Fish_Richness ~ Year + Site, data = variables)
fish_rugosity_site = lm(Fish_Richness ~ Rugosity + Site, data = variables)
fish_cover_site = lm(Fish_Richness ~ Percent_Coral_Cover + Site, data = variables)
fish_coralrichness_site = lm(Fish_Richness ~ Coral_Richness + Site, data = variables)
fish_rugosity_year = lm(Fish_Richness ~ Rugosity + Year, data = variables)
fish_cover_year = lm(Fish_Richness ~ Percent_Coral_Cover + Year, data = variables)
fish_coralrichness_year = lm(Fish_Richness ~ Coral_Richness + Year, data = variables)
fish_rugosity_year_site = lm(Fish_Richness ~ Rugosity + Year + Site, data = variables)
fish_cover_year_site = lm(Fish_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
fish_coralrichness_year_site = lm(Fish_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
fish_year_site_yearsite = lm(Fish_Richness ~ Year + Site + Year*Site, data = variables)
fish_year_site_yearsite_cover = lm(Fish_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
fish_year_site_yearsite_rugosity = lm(Fish_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
fish_year_site_yearsite_coralrichness = lm(Fish_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
fish_site_rugosity_rugositysite = lm(Fish_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
fish_site_cover_coversite = lm(Fish_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
fish_site_coralrichness_coralrichnesssite = lm(Fish_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
fish_year_rugosity_rugosityyear = lm(Fish_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
fish_year_cover_coveryear = lm(Fish_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
fish_year_coralrichness_coralrichnessyear = lm(Fish_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
fish_rugosity_log = lm(Fish_Richness ~ log(Rugosity), data = variables)
fish_rugosity_site_log = lm(Fish_Richness ~ log(Rugosity) + Site, data = variables)
fish_rugosity_year_log = lm(Fish_Richness ~ log(Rugosity) + Year, data = variables)
fish_rugosity_year_site_log = lm(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
fish_year_site_yearsite_rugosity_log = lm(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
fish_site_rugosity_rugositysite_log = lm(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
fish_year_rugosity_rugosityyear_log = lm(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
fish_cover_log = lm(Fish_Richness ~ log(Percent_Coral_Cover), data = variables)
fish_cover_site_log = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
fish_cover_year_log = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
fish_cover_year_site_log = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
fish_year_site_yearsite_cover_log = lm(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
fish_site_cover_coversite_log = lm(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
fish_year_cover_coveryear_log = lm(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
fish_coralrichness_log = lm(Fish_Richness ~ log(Coral_Richness), data = variables)
fish_coralrichness_site_log = lm(Fish_Richness ~ log(Coral_Richness) + Site, data = variables)
fish_coralrichness_year_log = lm(Fish_Richness ~ log(Coral_Richness) + Year, data = variables)
fish_coralrichness_year_site_log = lm(Fish_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
fish_year_site_yearsite_coralrichness_log = lm(Fish_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
fish_site_coralrichness_coralrichnesssite_log = lm(Fish_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
fish_year_coralrichness_coralrichnessyear_log = lm(Fish_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
lm(log(Fish_Richness) ~ log(Rugosity), data = variables)
# a = intercept, b = log(x), use these to make y = (exp(a))*x^b
lm(log(Fish_Richness) ~ log(Percent_Coral_Cover), data = variables)
lm(log(Fish_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity*7* NOT SURE HOW TO INCLUDE POWER FUNCTION FOR ADDITIVE AND INTERACTIVE MODELS
fish_rugosity_power = lm(Fish_Richness ~ exp(1.3536 + 0.4806*log(Rugosity)), data = variables)
# fish_rugosity_site_power = lm(Fish_Richness ~ log(Rugosity) + Site, data = variables)
# fish_rugosity_year_power = lm(Fish_Richness ~ log(Rugosity) + Year, data = variables)
# fish_rugosity_year_site_power = lm(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
# fish_year_site_yearsite_rugosity_power = lm(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# fish_site_rugosity_rugositysite_power = lm(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# fish_year_rugosity_rugosityyear_power = lm(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
fish_cover_power = lm(Fish_Richness ~ exp(2.5184 + 0.2247*log(Percent_Coral_Cover)), data = variables)
# fish_cover_site_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# fish_cover_year_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# fish_cover_year_site_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# fish_year_site_yearsite_cover_power = lm(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# fish_site_cover_coversite_power = lm(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# fish_year_cover_coveryear_power = lm(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
fish_coralrichness_power = lm(Fish_Richness ~ exp(2.1755 + 0.3899*log(Coral_Richness)), data = variables)
# fish_coralrichness_site_power = lm(Fish_Richness ~ log(Coral_Richness) + Site, data = variables)
# fish_coralrichness_year_power = lm(Fish_Richness ~ log(Coral_Richness) + Year, data = variables)
# fish_coralrichness_year_site_power = lm(Fish_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# fish_year_site_yearsite_coralrichness_power = lm(Fish_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# fish_site_coralrichness_coralrichnesssite_power = lm(Fish_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# fish_year_coralrichness_coralrichnessyear_power = lm(Fish_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)



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





# Models for sponge richness
sponge_year = lm(Sponge_Richness ~ Year, data = variables)
sponge_site = lm(Sponge_Richness ~ Site, data = variables)
sponge_rugosity = lm(Sponge_Richness ~ Rugosity, data = variables)
sponge_cover = lm(Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_coralrichness = lm(Sponge_Richness ~ Coral_Richness, data = variables)
# Additive models
sponge_year_site = lm(Sponge_Richness ~ Year + Site, data = variables)
sponge_rugosity_site = lm(Sponge_Richness ~ Rugosity + Site, data = variables)
sponge_cover_site = lm(Sponge_Richness ~ Percent_Coral_Cover + Site, data = variables)
sponge_coralrichness_site = lm(Sponge_Richness ~ Coral_Richness + Site, data = variables)
sponge_rugosity_year = lm(Sponge_Richness ~ Rugosity + Year, data = variables)
sponge_cover_year = lm(Sponge_Richness ~ Percent_Coral_Cover + Year, data = variables)
sponge_coralrichness_year = lm(Sponge_Richness ~ Coral_Richness + Year, data = variables)
sponge_rugosity_year_site = lm(Sponge_Richness ~ Rugosity + Year + Site, data = variables)
sponge_cover_year_site = lm(Sponge_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
sponge_coralrichness_year_site = lm(Sponge_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
sponge_year_site_yearsite = lm(Sponge_Richness ~ Year + Site + Year*Site, data = variables)
sponge_year_site_yearsite_cover = lm(Sponge_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
sponge_year_site_yearsite_rugosity = lm(Sponge_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
sponge_year_site_yearsite_coralrichness = lm(Sponge_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
sponge_site_rugosity_rugositysite = lm(Sponge_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
sponge_site_cover_coversite = lm(Sponge_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
sponge_site_coralrichness_coralrichnesssite = lm(Sponge_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
sponge_year_rugosity_rugosityyear = lm(Sponge_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
sponge_year_cover_coveryear = lm(Sponge_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
sponge_year_coralrichness_coralrichnessyear = lm(Sponge_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
sponge_rugosity_log = lm(Sponge_Richness ~ log(Rugosity), data = variables)
sponge_rugosity_site_log = lm(Sponge_Richness ~ log(Rugosity) + Site, data = variables)
sponge_rugosity_year_log = lm(Sponge_Richness ~ log(Rugosity) + Year, data = variables)
sponge_rugosity_year_site_log = lm(Sponge_Richness ~ log(Rugosity) + Year + Site, data = variables)
sponge_year_site_yearsite_rugosity_log = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
sponge_site_rugosity_rugositysite_log = lm(Sponge_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
sponge_year_rugosity_rugosityyear_log = lm(Sponge_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
sponge_cover_log = lm(Sponge_Richness ~ log(Percent_Coral_Cover), data = variables)
sponge_cover_site_log = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
sponge_cover_year_log = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
sponge_cover_year_site_log = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
sponge_year_site_yearsite_cover_log = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
sponge_site_cover_coversite_log = lm(Sponge_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
sponge_year_cover_coveryear_log = lm(Sponge_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
sponge_coralrichness_log = lm(Sponge_Richness ~ log(Coral_Richness), data = variables)
sponge_coralrichness_site_log = lm(Sponge_Richness ~ log(Coral_Richness) + Site, data = variables)
sponge_coralrichness_year_log = lm(Sponge_Richness ~ log(Coral_Richness) + Year, data = variables)
sponge_coralrichness_year_site_log = lm(Sponge_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
sponge_year_site_yearsite_coralrichness_log = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
sponge_site_coralrichness_coralrichnesssite_log = lm(Sponge_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
sponge_year_coralrichness_coralrichnessyear_log = lm(Sponge_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
lm(log(Sponge_Richness) ~ log(Rugosity), data = variables)
lm(log(Sponge_Richness) ~ log(Percent_Coral_Cover), data = variables)
lm(log(Sponge_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity
sponge_rugosity_power = lm(Sponge_Richness ~ exp(3.4814 + -0.1145*log(Rugosity)), data = variables)
# sponge_rugosity_site_power = lm(Sponge_Richness ~ log(Rugosity) + Site, data = variables)
# sponge_rugosity_year_power = lm(Sponge_Richness ~ log(Rugosity) + Year, data = variables)
# sponge_rugosity_year_site_power = lm(Sponge_Richness ~ log(Rugosity) + Year + Site, data = variables)
# sponge_year_site_yearsite_rugosity_power = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# sponge_site_rugosity_rugositysite_power = lm(Sponge_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# sponge_year_rugosity_rugosityyear_power = lm(Sponge_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
sponge_cover_power = lm(Sponge_Richness ~ exp(3.4389 + -0.1381*log(Percent_Coral_Cover)), data = variables)
# sponge_cover_site_power = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# sponge_cover_year_power = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# sponge_cover_year_site_power = lm(Sponge_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# sponge_year_site_yearsite_cover_power = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# sponge_site_cover_coversite_power = lm(Sponge_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# sponge_year_cover_coveryear_power = lm(Sponge_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
sponge_coralrichness_power = lm(Sponge_Richness ~ exp(3.5384 + -0.1928*log(Coral_Richness)), data = variables)
# sponge_coralrichness_site_power = lm(Sponge_Richness ~ log(Coral_Richness) + Site, data = variables)
# sponge_coralrichness_year_power = lm(Sponge_Richness ~ log(Coral_Richness) + Year, data = variables)
# sponge_coralrichness_year_site_power = lm(Sponge_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# sponge_year_site_yearsite_coralrichness_power = lm(Sponge_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# sponge_site_coralrichness_coralrichnesssite_power = lm(Sponge_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# sponge_year_coralrichness_coralrichnessyear_power = lm(Sponge_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)



# # An example of model output in table format
# tab_model(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
#           sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
#           sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
#           sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
#           sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
#           sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear)
# # With logarithmic and power models
# tab_model(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
#           sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
#           sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
#           sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
#           sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
#           sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear, 
#           sponge_rugosity_log, sponge_rugosity_site_log, sponge_rugosity_year_log, sponge_rugosity_year_site_log, 
#           sponge_year_site_yearsite_rugosity_log, sponge_site_rugosity_rugositysite_log, sponge_year_rugosity_rugosityyear_log, 
#           sponge_cover_log, sponge_cover_site_log, sponge_cover_year_log, sponge_cover_year_site_log, 
#           sponge_year_site_yearsite_cover_log, sponge_site_cover_coversite_log, sponge_year_cover_coveryear_log, 
#           sponge_coralrichness_log, sponge_coralrichness_site_log, sponge_coralrichness_year_log, 
#           sponge_coralrichness_year_site_log, sponge_year_site_yearsite_coralrichness_log, 
#           sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log,
#           sponge_rugosity_power, sponge_rugosity_site_power, sponge_rugosity_year_power, sponge_rugosity_year_site_power, 
#           sponge_year_site_yearsite_rugosity_power, sponge_site_rugosity_rugositysite_power, sponge_year_rugosity_rugosityyear_power, 
#           sponge_cover_power, sponge_cover_site_power, sponge_cover_year_power, sponge_cover_year_site_power, 
#           sponge_year_site_yearsite_cover_power, sponge_site_cover_coversite_power, sponge_year_cover_coveryear_power, 
#           sponge_coralrichness_power, sponge_coralrichness_site_power, sponge_coralrichness_year_power, 
#           sponge_coralrichness_year_site_power, sponge_year_site_yearsite_coralrichness_power, 
#           sponge_site_coralrichness_coralrichnesssite_power, sponge_year_coralrichness_coralrichnessyear_power)





# Models for coral richness
coral_year = lm(Coral_Richness ~ Year, data = variables)
coral_site = lm(Coral_Richness ~ Site, data = variables)
coral_rugosity = lm(Coral_Richness ~ Rugosity, data = variables)
coral_cover = lm(Coral_Richness ~ Percent_Coral_Cover, data = variables)
#coral_coralrichness = lm(Coral_Richness ~ Coral_Richness, data = variables)
# Additive models
coral_year_site = lm(Coral_Richness ~ Year + Site, data = variables)
coral_rugosity_site = lm(Coral_Richness ~ Rugosity + Site, data = variables)
coral_cover_site = lm(Coral_Richness ~ Percent_Coral_Cover + Site, data = variables)
#coral_coralrichness_site = lm(Coral_Richness ~ Coral_Richness + Site, data = variables)
coral_rugosity_year = lm(Coral_Richness ~ Rugosity + Year, data = variables)
coral_cover_year = lm(Coral_Richness ~ Percent_Coral_Cover + Year, data = variables)
#coral_coralrichness_year = lm(Coral_Richness ~ Coral_Richness + Year, data = variables)
coral_rugosity_year_site = lm(Coral_Richness ~ Rugosity + Year + Site, data = variables)
coral_cover_year_site = lm(Coral_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
#coral_coralrichness_year_site = lm(Coral_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
coral_year_site_yearsite = lm(Coral_Richness ~ Year + Site + Year*Site, data = variables)
coral_year_site_yearsite_cover = lm(Coral_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
coral_year_site_yearsite_rugosity = lm(Coral_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
#coral_year_site_yearsite_coralrichness = lm(Coral_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
coral_site_rugosity_rugositysite = lm(Coral_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
coral_site_cover_coversite = lm(Coral_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
#coral_site_coralrichness_coralrichnesssite = lm(Coral_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
coral_year_rugosity_rugosityyear = lm(Coral_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
coral_year_cover_coveryear = lm(Coral_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
#coral_year_coralrichness_coralrichnessyear = lm(Coral_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
coral_rugosity_log = lm(Coral_Richness ~ log(Rugosity), data = variables)
coral_rugosity_site_log = lm(Coral_Richness ~ log(Rugosity) + Site, data = variables)
coral_rugosity_year_log = lm(Coral_Richness ~ log(Rugosity) + Year, data = variables)
coral_rugosity_year_site_log = lm(Coral_Richness ~ log(Rugosity) + Year + Site, data = variables)
coral_year_site_yearsite_rugosity_log = lm(Coral_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
coral_site_rugosity_rugositysite_log = lm(Coral_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
coral_year_rugosity_rugosityyear_log = lm(Coral_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
coral_cover_log = lm(Coral_Richness ~ log(Percent_Coral_Cover), data = variables)
coral_cover_site_log = lm(Coral_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
coral_cover_year_log = lm(Coral_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
coral_cover_year_site_log = lm(Coral_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
coral_year_site_yearsite_cover_log = lm(Coral_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
coral_site_cover_coversite_log = lm(Coral_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
coral_year_cover_coveryear_log = lm(Coral_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
## Get coefficients for power models
lm(log(Coral_Richness) ~ log(Rugosity), data = variables)
lm(log(Coral_Richness) ~ log(Percent_Coral_Cover), data = variables)
# # Power models for rugosity
coral_rugosity_power = lm(Coral_Richness ~ exp(1.267 + 0.332*log(Rugosity)), data = variables)
# coral_rugosity_site_power = lm(Fish_Richness ~ log(Rugosity) + Site, data = variables)
# coral_rugosity_year_power = lm(Fish_Richness ~ log(Rugosity) + Year, data = variables)
# coral_rugosity_year_site_power = lm(Fish_Richness ~ log(Rugosity) + Year + Site, data = variables)
# coral_year_site_yearsite_rugosity_power = lm(Fish_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# coral_site_rugosity_rugositysite_power = lm(Fish_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# coral_year_rugosity_rugosityyear_power = lm(Fish_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
coral_cover_power = lm(Coral_Richness ~ exp(1.6023 + 0.3209*log(Percent_Coral_Cover)), data = variables)
# coral_cover_site_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# coral_cover_year_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# coral_cover_year_site_power = lm(Fish_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# coral_year_site_yearsite_cover_power = lm(Fish_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# coral_site_cover_coversite_power = lm(Fish_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# coral_year_cover_coveryear_power = lm(Fish_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)



# # An example of model output in table format
# tab_model(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
#           coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
#           coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
#           coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear)
# # With logarithmic and power models
# tab_model(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
#           coral_cover_site, coral_rugosity_year, coral_cover_year, 
#           coral_rugosity_year_site, coral_cover_year_site, coral_year_site_yearsite,
#           coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity,
#           coral_site_rugosity_rugositysite, coral_site_cover_coversite,
#           coral_year_rugosity_rugosityyear, coral_year_cover_coveryear,  
#           coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log, 
#           coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log, 
#           coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log, 
#           coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log, 
#           coral_rugosity_power, coral_rugosity_site_power, coral_rugosity_year_power, coral_rugosity_year_site_power, 
#           coral_year_site_yearsite_rugosity_power, coral_site_rugosity_rugositysite_power, coral_year_rugosity_rugosityyear_power, 
#           coral_cover_power, coral_cover_site_power, coral_cover_year_power, coral_cover_year_site_power, 
#           coral_year_site_yearsite_cover_power, coral_site_cover_coversite_power, coral_year_cover_coveryear_power)




# Models for combined richness
combined_year = lm(Combined_Richness ~ Year, data = variables)
combined_site = lm(Combined_Richness ~ Site, data = variables)
combined_rugosity = lm(Combined_Richness ~ Rugosity, data = variables)
combined_cover = lm(Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_coralrichness = lm(Combined_Richness ~ Coral_Richness, data = variables)
# Additive models
combined_year_site = lm(Combined_Richness ~ Year + Site, data = variables)
combined_rugosity_site = lm(Combined_Richness ~ Rugosity + Site, data = variables)
combined_cover_site = lm(Combined_Richness ~ Percent_Coral_Cover + Site, data = variables)
combined_coralrichness_site = lm(Combined_Richness ~ Coral_Richness + Site, data = variables)
combined_rugosity_year = lm(Combined_Richness ~ Rugosity + Year, data = variables)
combined_cover_year = lm(Combined_Richness ~ Percent_Coral_Cover + Year, data = variables)
combined_coralrichness_year = lm(Combined_Richness ~ Coral_Richness + Year, data = variables)
combined_rugosity_year_site = lm(Combined_Richness ~ Rugosity + Year + Site, data = variables)
combined_cover_year_site = lm(Combined_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
combined_coralrichness_year_site = lm(Combined_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
combined_year_site_yearsite = lm(Combined_Richness ~ Year + Site + Year*Site, data = variables)
combined_year_site_yearsite_cover = lm(Combined_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
combined_year_site_yearsite_rugosity = lm(Combined_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
combined_year_site_yearsite_coralrichness = lm(Combined_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
combined_site_rugosity_rugositysite = lm(Combined_Richness ~ Site + Rugosity + Rugosity*Site, data = variables)
combined_site_cover_coversite = lm(Combined_Richness ~ Site + Percent_Coral_Cover + Percent_Coral_Cover*Site, data = variables)
combined_site_coralrichness_coralrichnesssite = lm(Combined_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
combined_year_rugosity_rugosityyear = lm(Combined_Richness ~ Year + Rugosity + Rugosity*Year, data = variables)
combined_year_cover_coveryear = lm(Combined_Richness ~ Year + Percent_Coral_Cover + Percent_Coral_Cover*Year, data = variables)
combined_year_coralrichness_coralrichnessyear = lm(Combined_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
# Logarithmic models for rugosity
combined_rugosity_log = lm(Combined_Richness ~ log(Rugosity), data = variables)
combined_rugosity_site_log = lm(Combined_Richness ~ log(Rugosity) + Site, data = variables)
combined_rugosity_year_log = lm(Combined_Richness ~ log(Rugosity) + Year, data = variables)
combined_rugosity_year_site_log = lm(Combined_Richness ~ log(Rugosity) + Year + Site, data = variables)
combined_year_site_yearsite_rugosity_log = lm(Combined_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
combined_site_rugosity_rugositysite_log = lm(Combined_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
combined_year_rugosity_rugosityyear_log = lm(Combined_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# Logarithmic models for cover
combined_cover_log = lm(Combined_Richness ~ log(Percent_Coral_Cover), data = variables)
combined_cover_site_log = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
combined_cover_year_log = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
combined_cover_year_site_log = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
combined_year_site_yearsite_cover_log = lm(Combined_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
combined_site_cover_coversite_log = lm(Combined_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
combined_year_cover_coveryear_log = lm(Combined_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# Logarithmic models for coral richness
combined_coralrichness_log = lm(Combined_Richness ~ log(Coral_Richness), data = variables)
combined_coralrichness_site_log = lm(Combined_Richness ~ log(Coral_Richness) + Site, data = variables)
combined_coralrichness_year_log = lm(Combined_Richness ~ log(Coral_Richness) + Year, data = variables)
combined_coralrichness_year_site_log = lm(Combined_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
combined_year_site_yearsite_coralrichness_log = lm(Combined_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
combined_site_coralrichness_coralrichnesssite_log = lm(Combined_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
combined_year_coralrichness_coralrichnessyear_log = lm(Combined_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
## Get coefficients for power models
lm(log(Combined_Richness) ~ log(Rugosity), data = variables)
lm(log(Combined_Richness) ~ log(Percent_Coral_Cover), data = variables)
lm(log(Combined_Richness) ~ log(Coral_Richness), data = variables)
# # Power models for rugosity
combined_rugosity_power = lm(Combined_Richness ~ exp(3.2946 + 0.2075*log(Rugosity)), data = variables)
# combined_rugosity_site_power = lm(Combined_Richness ~ log(Rugosity) + Site, data = variables)
# combined_rugosity_year_power = lm(Combined_Richness ~ log(Rugosity) + Year, data = variables)
# combined_rugosity_year_site_power = lm(Combined_Richness ~ log(Rugosity) + Year + Site, data = variables)
# combined_year_site_yearsite_rugosity_power = lm(Combined_Richness ~ Year + Site + Year*Site + log(Rugosity), data = variables)
# combined_site_rugosity_rugositysite_power = lm(Combined_Richness ~ Site + log(Rugosity) + log(Rugosity)*Site, data = variables)
# combined_year_rugosity_rugosityyear_power = lm(Combined_Richness ~ Year + log(Rugosity) + log(Rugosity)*Year, data = variables)
# # Power models for cover
combined_cover_power = lm(Combined_Richness ~ exp(3.79667 + 0.09738*log(Percent_Coral_Cover)), data = variables)
# combined_cover_site_power = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Site, data = variables)
# combined_cover_year_power = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Year, data = variables)
# combined_cover_year_site_power = lm(Combined_Richness ~ log(Percent_Coral_Cover) + Year + Site, data = variables)
# combined_year_site_yearsite_cover_power = lm(Combined_Richness ~ Year + Site + Year*Site + log(Percent_Coral_Cover), data = variables)
# combined_site_cover_coversite_power = lm(Combined_Richness ~ Site + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Site, data = variables)
# combined_year_cover_coveryear_power = lm(Combined_Richness ~ Year + log(Percent_Coral_Cover) + log(Percent_Coral_Cover)*Year, data = variables)
# # Power models for coral richness
combined_coralrichness_power = lm(Combined_Richness ~ exp(3.3461 + 0.2876*log(Coral_Richness)), data = variables)
# combined_coralrichness_site_power = lm(Combined_Richness ~ log(Coral_Richness) + Site, data = variables)
# combined_coralrichness_year_power = lm(Combined_Richness ~ log(Coral_Richness) + Year, data = variables)
# combined_coralrichness_year_site_power = lm(Combined_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
# combined_year_site_yearsite_coralrichness_power = lm(Combined_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
# combined_site_coralrichness_coralrichnesssite_power = lm(Combined_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
# combined_year_coralrichness_coralrichnessyear_power = lm(Combined_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)



# # An example of model output in table format
# tab_model(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
#           combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
#           combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
#           combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
#           combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
#           combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear)
# # With logarithmic and power models
# tab_model(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
#           combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
#           combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
#           combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
#           combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
#           combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear, 
#           combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
#           combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
#           combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
#           combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
#           combined_coralrichness_log, combined_coralrichness_site_log, combined_coralrichness_year_log, 
#           combined_coralrichness_year_site_log, combined_year_site_yearsite_coralrichness_log, 
#           combined_site_coralrichness_coralrichnesssite_log, combined_year_coralrichness_coralrichnessyear_log,
#           combined_rugosity_power, combined_rugosity_site_power, combined_rugosity_year_power, combined_rugosity_year_site_power, 
#           combined_year_site_yearsite_rugosity_power, combined_site_rugosity_rugositysite_power, combined_year_rugosity_rugosityyear_power, 
#           combined_cover_power, combined_cover_site_power, combined_cover_year_power, combined_cover_year_site_power, 
#           combined_year_site_yearsite_cover_power, combined_site_cover_coversite_power, combined_year_cover_coveryear_power, 
#           combined_coralrichness_power, combined_coralrichness_site_power, combined_coralrichness_year_power, 
#           combined_coralrichness_year_site_power, combined_year_site_yearsite_coralrichness_power, 
#           combined_site_coralrichness_coralrichnesssite_power, combined_year_coralrichness_coralrichnessyear_power)







## AIC

# Create a list of all of the fish models
fish_models <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
                    fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
                    fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
                    fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
                    fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
                    fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear)
# Name each model in the same order they're listed in fish_models AND sponge_models
model_names <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
                 "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
                 "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
                 "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
                 "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
                 "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year")
# AIC table
fish_aic_table <- aictab(fish_models, modnames = model_names, digits = 4)
#write.csv(fish_aic_table, file = "fish_aic_table.csv")
## With Logarithmic models
fish_models_log <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
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
                        fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log)
# Name each model in the same order they're listed in fish_models AND sponge_models
model_names_log <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
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
                 "log(coralrichness) + year + log(coralrichness)*year")
# AIC table with log models
fish_aic_table_log <- aictab(fish_models_log, modnames = model_names_log, digits = 4)
# ## With Logarithmic and power models
# fish_models_log_power <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
#                                fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
#                                fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
#                                fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
#                                fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
#                                fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear, 
#                                fish_rugosity_log, fish_rugosity_site_log, fish_rugosity_year_log, fish_rugosity_year_site_log, 
#                                fish_year_site_yearsite_rugosity_log, fish_site_rugosity_rugositysite_log, fish_year_rugosity_rugosityyear_log, 
#                                fish_cover_log, fish_cover_site_log, fish_cover_year_log, fish_cover_year_site_log, 
#                                fish_year_site_yearsite_cover_log, fish_site_cover_coversite_log, fish_year_cover_coveryear_log, 
#                                fish_coralrichness_log, fish_coralrichness_site_log, fish_coralrichness_year_log, 
#                                fish_coralrichness_year_site_log, fish_year_site_yearsite_coralrichness_log, 
#                                fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log,
#                                fish_rugosity_power, fish_rugosity_site_power, fish_rugosity_year_power, fish_rugosity_year_site_power, 
#                                fish_year_site_yearsite_rugosity_power, fish_site_rugosity_rugositysite_power, fish_year_rugosity_rugosityyear_power, 
#                                fish_cover_power, fish_cover_site_power, fish_cover_year_power, fish_cover_year_site_power, 
#                                fish_year_site_yearsite_cover_power, fish_site_cover_coversite_power, fish_year_cover_coveryear_power, 
#                                fish_coralrichness_power, fish_coralrichness_site_power, fish_coralrichness_year_power, 
#                                fish_coralrichness_year_site_power, fish_year_site_yearsite_coralrichness_power, 
#                                fish_site_coralrichness_coralrichnesssite_power, fish_year_coralrichness_coralrichnessyear_power)
# # Name each model in the same order they're listed in fish_models AND sponge_models
# model_names_log_power <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
#                      "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
#                      "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
#                      "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
#                      "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
#                      "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year",
#                      "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year", 
#                      "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site", 
#                      "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year", 
#                      "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site", 
#                      "log(cover) + year + log(cover)*year", "log(coralrichness)", 
#                      "log(coralrichness) + site", "log(coralrichness) + year", "log(coralrichness) + site + year", 
#                      "log(coralrichness) + site + year + year*site", "log(coralrichness) + site + log(coralrichness)*site", 
#                      "log(coralrichness) + year + log(coralrichness)*year",
#                      "power(rugosity)", "power(rugosity) + site", "power(rugosity) + year", "power(rugosity) + site + year", 
#                      "power(rugosity) + site + year + year*site", "power(rugosity) + site + power(rugosity)*site", 
#                      "power(rugosity) + year + power(rugosity)*year", "power(cover)", "power(cover) + site", "power(cover) + year", 
#                      "power(cover) + site + year", "power(cover) + site + year + year*site", "power(cover) + site + power(cover)*site", 
#                      "power(cover) + year + power(cover)*year", "power(coralrichness)", 
#                      "power(coralrichness) + site", "power(coralrichness) + year", "power(coralrichness) + site + year", 
#                      "power(coralrichness) + site + year + year*site", "power(coralrichness) + site + power(coralrichness)*site", 
#                      "power(coralrichness) + year + power(coralrichness)*year")
# # AIC table with log and power models
# fish_aic_table_log_power <- aictab(fish_models_log_power, modnames = model_names_log_power, digits = 4)


# Create a list of all of the sponge models
sponge_models <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
                     sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
                     sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
                     sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
                     sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
                     sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear)
# AIC table
sponge_aic_table <- aictab(sponge_models, modnames = model_names, digits = 4)
#write.csv(sponge_aic_table, file = "sponge_aic_table.csv")
## With Logarithmic models
sponge_models_log <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
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
                        sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log)
# AIC table with log models
sponge_aic_table_log <- aictab(sponge_models_log, modnames = model_names_log, digits = 4)
# ## With Logarithmic and power models
# sponge_models_log_power <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
#                                sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
#                                sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
#                                sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
#                                sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
#                                sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear, 
#                                sponge_rugosity_log, sponge_rugosity_site_log, sponge_rugosity_year_log, sponge_rugosity_year_site_log, 
#                                sponge_year_site_yearsite_rugosity_log, sponge_site_rugosity_rugositysite_log, sponge_year_rugosity_rugosityyear_log, 
#                                sponge_cover_log, sponge_cover_site_log, sponge_cover_year_log, sponge_cover_year_site_log, 
#                                sponge_year_site_yearsite_cover_log, sponge_site_cover_coversite_log, sponge_year_cover_coveryear_log, 
#                                sponge_coralrichness_log, sponge_coralrichness_site_log, sponge_coralrichness_year_log, 
#                                sponge_coralrichness_year_site_log, sponge_year_site_yearsite_coralrichness_log, 
#                                sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log,
#                                sponge_rugosity_power, sponge_rugosity_site_power, sponge_rugosity_year_power, sponge_rugosity_year_site_power, 
#                                sponge_year_site_yearsite_rugosity_power, sponge_site_rugosity_rugositysite_power, sponge_year_rugosity_rugosityyear_power, 
#                                sponge_cover_power, sponge_cover_site_power, sponge_cover_year_power, sponge_cover_year_site_power, 
#                                sponge_year_site_yearsite_cover_power, sponge_site_cover_coversite_power, sponge_year_cover_coveryear_power, 
#                                sponge_coralrichness_power, sponge_coralrichness_site_power, sponge_coralrichness_year_power, 
#                                sponge_coralrichness_year_site_power, sponge_year_site_yearsite_coralrichness_power, 
#                                sponge_site_coralrichness_coralrichnesssite_power, sponge_year_coralrichness_coralrichnessyear_power)
# # AIC table with log and power models
# sponge_aic_table_log_power <- aictab(sponge_models_log_power, modnames = model_names_log_power, digits = 4)




# Create a list of all of the coral models
coral_models <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
                     coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
                     coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
                     coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear)
# Name each model in the same order they're listed in coral_models
coral_model_names <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site", 
                       "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site", 
                       "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity", 
                       "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year", "year + cover + cover*year")
# AIC table
coral_aic_table <- aictab(coral_models, modnames = coral_model_names, digits = 4)
#write.csv(coral_aic_table, file = "coral_aic_table.csv")
## With Logarithmic models
coral_models_log <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
                        coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
                        coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
                        coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear, 
                        coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log, 
                        coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log, 
                        coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log, 
                        coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log)
coral_model_names_log <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
                           "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
                           "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
                           "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
                           "year + cover + cover*year",
                           "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
                           "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
                           "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
                           "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
                           "log(cover) + year + log(cover)*year")
# AIC table with log models
coral_aic_table_log <- aictab(coral_models_log, modnames = model_names_log, digits = 4)
# ## With Logarithmic and power models
# coral_models_log_power <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
#                               coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
#                               coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
#                               coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear, 
#                               coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log, 
#                               coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log, 
#                               coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log, 
#                               coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log,
#                                coral_rugosity_power, coral_rugosity_site_power, coral_rugosity_year_power, coral_rugosity_year_site_power,
#                                coral_year_site_yearsite_rugosity_power, coral_site_rugosity_rugositysite_power, coral_year_rugosity_rugosityyear_power,
#                                coral_cover_power, coral_cover_site_power, coral_cover_year_power, coral_cover_year_site_power,
#                                coral_year_site_yearsite_cover_power, coral_site_cover_coversite_power, coral_year_cover_coveryear_power)
# coral_model_names_log_power <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
#                                  "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
#                                  "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
#                                  "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
#                                  "year + cover + cover*year",
#                                  "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
#                                  "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
#                                  "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
#                                  "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
#                                  "log(cover) + year + log(cover)*year",
#                                  "power(rugosity)", "power(rugosity) + site", "power(rugosity) + year", "power(rugosity) + site + year",
#                                  "power(rugosity) + site + year + year*site", "power(rugosity) + site + power(rugosity)*site",
#                                  "power(rugosity) + year + power(rugosity)*year", "power(cover)", "power(cover) + site", "power(cover) + year",
#                                  "power(cover) + site + year", "power(cover) + site + year + year*site", "power(cover) + site + power(cover)*site",
#                                  "power(cover) + year + power(cover)*year")
# # AIC table with log and power models
# coral_aic_table_log_power <- aictab(coral_models_log_power, modnames = model_names_log_power, digits = 4)


# Create a list of all of the "combined" models
combined_models <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
                        combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
                        combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
                        combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
                        combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
                        combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear)
# AIC table
combined_aic_table <- aictab(combined_models, modnames = model_names, digits = 4)
#write.csv(combined_aic_table, file = "combined_aic_table.csv")
## With Logarithmic models
combined_models_log <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
                          combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
                          combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
                          combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
                          combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
                          combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear, 
                          combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
                          combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
                          combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
                          combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
                          combined_coralrichness_log, combined_coralrichness_site_log, combined_coralrichness_year_log, 
                          combined_coralrichness_year_site_log, combined_year_site_yearsite_coralrichness_log, 
                          combined_site_coralrichness_coralrichnesssite_log, combined_year_coralrichness_coralrichnessyear_log)
# AIC table with log models
combined_aic_table_log <- aictab(combined_models_log, modnames = model_names_log, digits = 4)
# ## With Logarithmic and power models
# combined_models_log_power <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
#                                combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
#                                combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
#                                combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
#                                combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
#                                combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear, 
#                                combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
#                                combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
#                                combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
#                                combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
#                                combined_coralrichness_log, combined_coralrichness_site_log, combined_coralrichness_year_log, 
#                                combined_coralrichness_year_site_log, combined_year_site_yearsite_coralrichness_log, 
#                                combined_site_coralrichness_coralrichnesssite_log, combined_year_coralrichness_coralrichnessyear_log,
#                                combined_rugosity_power, combined_rugosity_site_power, combined_rugosity_year_power, combined_rugosity_year_site_power, 
#                                combined_year_site_yearsite_rugosity_power, combined_site_rugosity_rugositysite_power, combined_year_rugosity_rugosityyear_power, 
#                                combined_cover_power, combined_cover_site_power, combined_cover_year_power, combined_cover_year_site_power, 
#                                combined_year_site_yearsite_cover_power, combined_site_cover_coversite_power, combined_year_cover_coveryear_power, 
#                                combined_coralrichness_power, combined_coralrichness_site_power, combined_coralrichness_year_power, 
#                                combined_coralrichness_year_site_power, combined_year_site_yearsite_coralrichness_power, 
#                                combined_site_coralrichness_coralrichnesssite_power, combined_year_coralrichness_coralrichnessyear_power)
# # AIC table with log and power models
# combined_aic_table_log_power <- aictab(combined_models_log_power, modnames = model_names_log_power, digits = 4)

########################################################################
# Most parsimonious model
tab_model(coral_cover_year_site)
# Surrogate only
tab_model(coral_cover)
tab_model(coral_rugosity)

tab_model(fish_rugosity_site)
tab_model(sponge_year_site_yearsite)
tab_model(combined_year_site_yearsite_coralrichness)
tab_model(coral_cover)












############










# ***Figures


#######################Plots by site over time################################
# Verify year is factor for x-axis labels
variables$Year <- as.factor(variables$Year)

# Make plots for x = time
ggplot(variables, aes(x = Year, y = Coral_Richness, group = Site, color = Site)) + 
  # To show points
  # geom_point() +
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Coral Richness")
#To view each of the lines separately by groups
#  facet_wrap(~ Site)

ggplot(variables, aes(x = Year, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Sponge Richness")
# ***Notice that this figure has line breaks where the value for Sponge Richness in that given year is NA
# # The following code will remove the line breaks by deleting the years without values and merging the line segments 
# ggplot(variables[!is.na(variables$Sponge_Richness),], aes(x = Year, y = Sponge_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name ="Time (year)") +
#   scale_y_continuous(name ="Sponge Richness")

ggplot(variables, aes(x = Year, y = Fish_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Fish Richness")

ggplot(variables, aes(x = Year, y = Combined_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Combined Richness")

ggplot(variables, aes(x = Year, y = Percent_Coral_Cover, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Coral Cover (%)")

ggplot(variables, aes(x = Year, y = Rugosity, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name ="Time (year)") +
  scale_y_continuous(name ="Rugosity") +
  theme(text=element_text(size=20), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))






#######################Plots with site################################
# Plots where x = % cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness")

# Plots where x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness")
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness")
ggplot(variables, aes(x = Rugosity, y = Fish_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness")
ggplot(variables, aes(x = Rugosity, y = Combined_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness")

# Plots where x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness")
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness")
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness, color = Site)) + 
  #geom_line(size = 1.1) +
  geom_point(size = 5)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness")










# Change year to numeric to allow for color gradient
variables$Year <- as.numeric(variables$Year)

#######################Plots with year################################
## Make plots for x = % cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")

# Make plots for x = rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")

# Make plots for x = coral richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue")










#######################Plots with site and year################################
## Make plots for x = % cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)

# Make plots for x = rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)

# Make plots for x = coral richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  #geom_line(size = 1.1) +
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)
################################################################





# AIC Table and Figures for presentation *7*

coral_rugosity
coral_cover


#Figure 1. Rugosity as a surrogate for coral richness with linear model only.
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "lm", formula = y ~ x, aes(color = "Linear")) +
  scale_colour_manual(name = "Models", values = c("blue")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Figure 2. Coral cover as a surrogate for coral richness with linear model only.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Percent Coral Cover") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "lm", formula = y ~ x, aes(color = "Linear")) +
  scale_colour_manual(name = "Models", values = c("blue")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# #Coral Richness and Rugosity
# ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
#   geom_point(size = 3)+
#   scale_x_continuous(name ="Rugosity") +
#   scale_y_continuous(name ="Coral Richness") +
#   geom_smooth(size = 1.2, method = "lm", formula = y ~ x, aes(color = "Linear")) +
#   geom_smooth(size = 1.2, method = "lm", formula = y ~ log(x), aes(color = "Logarithmic")) +
#   geom_smooth(size = 1.2, method = "lm", formula = y ~ exp(1.267 + 0.332*log(x)), aes(color = "Power")) +
#   scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
#   theme(text=element_text(size=27), 
#         panel.grid.major = element_line(colour="light gray", size = (0.5)), 
#         panel.grid.minor = element_line(colour="light gray", size = (0.5)),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))

# Figure 4.
#Coral Richness and Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  #geom_line(size = 1.1) +
  #geom_point(size = 5, aes(color = variables$Site))+
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "lm", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "lm", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "lm", formula = y ~ exp(1.6023 + 0.3209*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        #axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Figure 3
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = Year), size = 2.8) +
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  facet_wrap(~ Site)

#AIC for presentation
coral_models_presentation <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site,
                              coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site,
                              coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity,
                              coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear,
                              coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log,
                              coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log,
                              coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log,
                              coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log,
                               coral_rugosity_power, coral_cover_power)
coral_model_names_presentation <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
                                 "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
                                 "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
                                 "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
                                 "year + cover + cover*year",
                                 "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
                                 "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
                                 "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
                                 "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
                                 "log(cover) + year + log(cover)*year",
                                 "power(rugosity)", "power(cover)")
# AIC table with log and power models
coral_aic_table_presentation <- aictab(coral_models_presentation, modnames = coral_model_names_presentation, digits = 4)
#write.csv(coral_aic_table_presentation, file = "coral_aic_table_presentation.csv")

# Export AIC tables
# write.csv(coral_aic_table_presentation, file = "coral_aic_table.csv")
# write.csv(combined_aic_table_log, file = "combined_aic_table.csv")
# write.csv(fish_aic_table_log, file = "fish_aic_table.csv")
# write.csv(sponge_aic_table_log, file = "sponge_aic_table.csv")
# Create HTML files for most parsimonious models and models with one variable
tab_model(fish_rugosity_site, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year)
tab_model(sponge_year_site_yearsite, sponge_site, sponge_cover, sponge_coralrichness, sponge_year, sponge_rugosity)
tab_model(coral_cover_year_site_log, coral_cover, coral_site, coral_rugosity, coral_year)
tab_model(combined_year_site_yearsite_coralrichness, combined_site, combined_coralrichness, combined_rugosity, combined_cover, combined_year)
