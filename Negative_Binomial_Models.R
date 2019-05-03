### Nicole Keefner
### Master's Thesis: Develop models and figures using variables.csv



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
# First used for negative binomial modeling using function gglm.nb.nb
library(MASS)



## Import variables.csv dataset
variables <- read.csv("variables.csv", header = T)
# Note that year is treated as type integer and is therefore continuous



# Because there are NA's in the Sponge_Richness column, R will be unable to calculate summary values
# Create subset of data called "sponge_complete" that retains only complete cases for Sponge_Richness
sponge_complete <- variables[complete.cases(variables$Sponge_Richness), ]



# Determine means and SD's for all variables
c("Coral_Richness", round(mean(variables$Coral_Richness), digits = 3), round(sd(variables$Coral_Richness), digits = 3),
  "Fish_Richness", round(mean(variables$Fish_Richness), digits = 3), round(sd(variables$Fish_Richness), digits = 3),
  "Sponge_Richness", round(mean(sponge_complete$Sponge_Richness), digits = 3), round(sd(sponge_complete$Sponge_Richness), digits = 3),
  "Rugosity", round(mean(variables$Rugosity), digits = 3), round(sd(variables$Rugosity), digits = 3),
  "Percent_Coral_Cover", round(mean(variables$Percent_Coral_Cover), digits = 3), round(sd(variables$Percent_Coral_Cover), digits = 3),
  "Sponge_and_Fish_Richness", round(mean(sponge_complete$Sponge_and_Fish_Richness), digits = 3), round(sd(sponge_complete$Sponge_and_Fish_Richness), digits = 3),
  "Combined_Richness", round(mean(sponge_complete$Combined_Richness), digits = 3), round(sd(sponge_complete$Combined_Richness), digits = 3))

# Graph frequency distributions of response variables
hist(variables$Fish_Richness, breaks = 25)
hist(variables$Sponge_Richness, breaks = 25)
hist(variables$Coral_Richness, breaks = 25)
hist(variables$Combined_Richness, breaks = 25)
hist(variables$Sponge_and_Fish_Richness, breaks = 25)


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



# Models for fish richness
fish_year = glm.nb(Fish_Richness ~ Year, data = variables)
fish_site = glm.nb(Fish_Richness ~ Site, data = variables)
fish_rugosity = glm.nb(Fish_Richness ~ Rugosity, data = variables)
fish_cover = glm.nb(Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_coralrichness = glm.nb(Fish_Richness ~ Coral_Richness, data = variables)
# Additive models
fish_year_site = glm.nb(Fish_Richness ~ Year + Site, data = variables)
fish_rugosity_site = glm.nb(Fish_Richness ~ Rugosity + Site, data = variables)
fish_cover_site = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Site, data = variables)
fish_coralrichness_site = glm.nb(Fish_Richness ~ Coral_Richness + Site, data = variables)
fish_rugosity_year = glm.nb(Fish_Richness ~ Rugosity + Year, data = variables)
fish_cover_year = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Year, data = variables)
fish_coralrichness_year = glm.nb(Fish_Richness ~ Coral_Richness + Year, data = variables)
fish_rugosity_year_site = glm.nb(Fish_Richness ~ Rugosity + Year + Site, data = variables)
fish_cover_year_site = glm.nb(Fish_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
fish_coralrichness_year_site = glm.nb(Fish_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
fish_year_site_yearsite = glm.nb(Fish_Richness ~ Year + Site + Year*Site, data = variables)
fish_year_site_yearsite_rugosity = glm.nb(Fish_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
fish_year_site_yearsite_cover = glm.nb(Fish_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
fish_year_site_yearsite_coralrichness = glm.nb(Fish_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
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





# Models for sponge richness
sponge_year = glm.nb(Sponge_Richness ~ Year, data = variables)
sponge_site = glm.nb(Sponge_Richness ~ Site, data = variables)
sponge_rugosity = glm.nb(Sponge_Richness ~ Rugosity, data = variables)
sponge_cover = glm.nb(Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_coralrichness = glm.nb(Sponge_Richness ~ Coral_Richness, data = variables)
# Additive models
sponge_year_site = glm.nb(Sponge_Richness ~ Year + Site, data = variables)
sponge_rugosity_site = glm.nb(Sponge_Richness ~ Rugosity + Site, data = variables)
sponge_cover_site = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Site, data = variables)
sponge_coralrichness_site = glm.nb(Sponge_Richness ~ Coral_Richness + Site, data = variables)
sponge_rugosity_year = glm.nb(Sponge_Richness ~ Rugosity + Year, data = variables)
sponge_cover_year = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Year, data = variables)
sponge_coralrichness_year = glm.nb(Sponge_Richness ~ Coral_Richness + Year, data = variables)
sponge_rugosity_year_site = glm.nb(Sponge_Richness ~ Rugosity + Year + Site, data = variables)
sponge_cover_year_site = glm.nb(Sponge_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
sponge_coralrichness_year_site = glm.nb(Sponge_Richness ~ Coral_Richness + Year + Site, data = variables)
# Interactive models
sponge_year_site_yearsite = glm.nb(Sponge_Richness ~ Year + Site + Year*Site, data = variables)
sponge_year_site_yearsite_rugosity = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + Rugosity, data = variables)
sponge_year_site_yearsite_cover = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + Percent_Coral_Cover, data = variables)
sponge_year_site_yearsite_coralrichness = glm.nb(Sponge_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
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
coral_year = glm.nb(Coral_Richness ~ Year, data = variables)
coral_site = glm.nb(Coral_Richness ~ Site, data = variables)
coral_rugosity = glm.nb(Coral_Richness ~ Rugosity, data = variables)
coral_cover = glm.nb(Coral_Richness ~ Percent_Coral_Cover, data = variables)
#coral_coralrichness = glm.nb(Coral_Richness ~ Coral_Richness, data = variables)
# Additive models
coral_year_site = glm.nb(Coral_Richness ~ Year + Site, data = variables)
coral_rugosity_site = glm.nb(Coral_Richness ~ Rugosity + Site, data = variables)
coral_cover_site = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Site, data = variables)
#coral_coralrichness_site = glm.nb(Coral_Richness ~ Coral_Richness + Site, data = variables)
coral_rugosity_year = glm.nb(Coral_Richness ~ Rugosity + Year, data = variables)
coral_cover_year = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Year, data = variables)
#coral_coralrichness_year = glm.nb(Coral_Richness ~ Coral_Richness + Year, data = variables)
coral_rugosity_year_site = glm.nb(Coral_Richness ~ Rugosity + Year + Site, data = variables)
coral_cover_year_site = glm.nb(Coral_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
#coral_coralrichness_year_site = glm.nb(Coral_Richness ~ Coral_Richness + Year + Site, data = variables)
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


# Models for richness of sponge and fish
fishsponge_year = glm.nb(Sponge_and_Fish_Richness ~ Year, data = variables)
fishsponge_site = glm.nb(Sponge_and_Fish_Richness ~ Site, data = variables)
fishsponge_year_site = glm.nb(Sponge_and_Fish_Richness ~ Year + Site, data = variables)
fishsponge_year_site_yearsite = glm.nb(Sponge_and_Fish_Richness ~ Year + Site + Year*Site, data = variables)
fishsponge_coralrichness = glm.nb(Sponge_and_Fish_Richness ~ Coral_Richness, data = variables)
fishsponge_coralrichness_site = glm.nb(Sponge_and_Fish_Richness ~ Coral_Richness + Site, data = variables)
fishsponge_coralrichness_year = glm.nb(Sponge_and_Fish_Richness ~ Coral_Richness + Year, data = variables)
fishsponge_coralrichness_year_site = glm.nb(Sponge_and_Fish_Richness ~ Coral_Richness + Year + Site, data = variables)
fishsponge_year_site_yearsite_coralrichness = glm.nb(Sponge_and_Fish_Richness ~ Year + Site + Year*Site + Coral_Richness, data = variables)
fishsponge_site_coralrichness_coralrichnesssite = glm.nb(Sponge_and_Fish_Richness ~ Site + Coral_Richness + Coral_Richness*Site, data = variables)
fishsponge_year_coralrichness_coralrichnessyear = glm.nb(Sponge_and_Fish_Richness ~ Year + Coral_Richness + Coral_Richness*Year, data = variables)
fishsponge_coralrichness_log = glm.nb(Sponge_and_Fish_Richness ~ log(Coral_Richness), data = variables)
fishsponge_coralrichness_site_log = glm.nb(Sponge_and_Fish_Richness ~ log(Coral_Richness) + Site, data = variables)
fishsponge_coralrichness_year_log = glm.nb(Sponge_and_Fish_Richness ~ log(Coral_Richness) + Year, data = variables)
fishsponge_coralrichness_year_site_log = glm.nb(Sponge_and_Fish_Richness ~ log(Coral_Richness) + Year + Site, data = variables)
fishsponge_year_site_yearsite_coralrichness_log = glm.nb(Sponge_and_Fish_Richness ~ Year + Site + Year*Site + log(Coral_Richness), data = variables)
fishsponge_site_coralrichness_coralrichnesssite_log = glm.nb(Sponge_and_Fish_Richness ~ Site + log(Coral_Richness) + log(Coral_Richness)*Site, data = variables)
fishsponge_year_coralrichness_coralrichnessyear_log = glm.nb(Sponge_and_Fish_Richness ~ Year + log(Coral_Richness) + log(Coral_Richness)*Year, data = variables)
glm.nb(log(Sponge_and_Fish_Richness) ~ log(Coral_Richness), data = variables)
fishsponge_coralrichness_power = glm.nb(Sponge_and_Fish_Richness ~ exp(3.4928 + 0.1297*log(Coral_Richness)), data = variables)
fishsponge_year_sqyear = glm.nb(Sponge_and_Fish_Richness ~ Year + Year^2, data = variables)

# Models for combined richness
combined_year = glm.nb(Combined_Richness ~ Year, data = variables)
combined_site = glm.nb(Combined_Richness ~ Site, data = variables)
combined_rugosity = glm.nb(Combined_Richness ~ Rugosity, data = variables)
combined_cover = glm.nb(Combined_Richness ~ Percent_Coral_Cover, data = variables)
#combined_coralrichness = glm.nb(Combined_Richness ~ Coral_Richness, data = variables)
# Additive models
combined_year_site = glm.nb(Combined_Richness ~ Year + Site, data = variables)
combined_rugosity_site = glm.nb(Combined_Richness ~ Rugosity + Site, data = variables)
combined_cover_site = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Site, data = variables)
#combined_coralrichness_site = glm.nb(Combined_Richness ~ Coral_Richness + Site, data = variables)
combined_rugosity_year = glm.nb(Combined_Richness ~ Rugosity + Year, data = variables)
combined_cover_year = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Year, data = variables)
#combined_coralrichness_year = glm.nb(Combined_Richness ~ Coral_Richness + Year, data = variables)
combined_rugosity_year_site = glm.nb(Combined_Richness ~ Rugosity + Year + Site, data = variables)
combined_cover_year_site = glm.nb(Combined_Richness ~ Percent_Coral_Cover + Year + Site, data = variables)
#combined_coralrichness_year_site = glm.nb(Combined_Richness ~ Coral_Richness + Year + Site, data = variables)
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






##################################***AIC









## AIC

# # Create a list of all of the fish models
# fish_models_simple <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
#                     fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
#                     fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
#                     fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
#                     fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
#                     fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear)
# # Name each model in the same order they're listed in fish_models_simple
# model_names_simple <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
#                  "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
#                  "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
#                  "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
#                  "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
#                  "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year")
# # AIC table
# fish_aic_table_simple <- aictab(fish_models_simple, modnames = model_names_simple, digits = 4)
# #write.csv(fish_aic_table_simple, file = "fish_aic_table_simple.csv")
# ## With Logarithmic models
# fish_models_log <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
#                         fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
#                         fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
#                         fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
#                         fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
#                         fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear, 
#                         fish_rugosity_log, fish_rugosity_site_log, fish_rugosity_year_log, fish_rugosity_year_site_log, 
#                         fish_year_site_yearsite_rugosity_log, fish_site_rugosity_rugositysite_log, fish_year_rugosity_rugosityyear_log, 
#                         fish_cover_log, fish_cover_site_log, fish_cover_year_log, fish_cover_year_site_log, 
#                         fish_year_site_yearsite_cover_log, fish_site_cover_coversite_log, fish_year_cover_coveryear_log, 
#                         fish_coralrichness_log, fish_coralrichness_site_log, fish_coralrichness_year_log, 
#                         fish_coralrichness_year_site_log, fish_year_site_yearsite_coralrichness_log, 
#                         fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log)
# # Name each model in the same order they're listed above
# model_names_log <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
#                  "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
#                  "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
#                  "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
#                  "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
#                  "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year",
#                  "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year", 
#                  "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site", 
#                  "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year", 
#                  "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site", 
#                  "log(cover) + year + log(cover)*year", "log(coralrichness)", 
#                  "log(coralrichness) + site", "log(coralrichness) + year", "log(coralrichness) + site + year", 
#                  "log(coralrichness) + site + year + year*site", "log(coralrichness) + site + log(coralrichness)*site", 
#                  "log(coralrichness) + year + log(coralrichness)*year")
# # AIC table with log models
# fish_aic_table_log <- aictab(fish_models_log, modnames = model_names_log, digits = 4)
# # ## With Logarithmic and power models
# # fish_models_log_power <- list(fish_year, fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year_site, fish_rugosity_site, 
# #                                fish_cover_site, fish_coralrichness_site, fish_rugosity_year, fish_cover_year, fish_coralrichness_year,
# #                                fish_rugosity_year_site, fish_cover_year_site, fish_coralrichness_year_site, fish_year_site_yearsite,
# #                                fish_year_site_yearsite_cover, fish_year_site_yearsite_rugosity, fish_year_site_yearsite_coralrichness,
# #                                fish_site_rugosity_rugositysite, fish_site_cover_coversite, fish_site_coralrichness_coralrichnesssite,
# #                                fish_year_rugosity_rugosityyear, fish_year_cover_coveryear, fish_year_coralrichness_coralrichnessyear, 
# #                                fish_rugosity_log, fish_rugosity_site_log, fish_rugosity_year_log, fish_rugosity_year_site_log, 
# #                                fish_year_site_yearsite_rugosity_log, fish_site_rugosity_rugositysite_log, fish_year_rugosity_rugosityyear_log, 
# #                                fish_cover_log, fish_cover_site_log, fish_cover_year_log, fish_cover_year_site_log, 
# #                                fish_year_site_yearsite_cover_log, fish_site_cover_coversite_log, fish_year_cover_coveryear_log, 
# #                                fish_coralrichness_log, fish_coralrichness_site_log, fish_coralrichness_year_log, 
# #                                fish_coralrichness_year_site_log, fish_year_site_yearsite_coralrichness_log, 
# #                                fish_site_coralrichness_coralrichnesssite_log, fish_year_coralrichness_coralrichnessyear_log,
# #                                fish_rugosity_power, fish_rugosity_site_power, fish_rugosity_year_power, fish_rugosity_year_site_power, 
# #                                fish_year_site_yearsite_rugosity_power, fish_site_rugosity_rugositysite_power, fish_year_rugosity_rugosityyear_power, 
# #                                fish_cover_power, fish_cover_site_power, fish_cover_year_power, fish_cover_year_site_power, 
# #                                fish_year_site_yearsite_cover_power, fish_site_cover_coversite_power, fish_year_cover_coveryear_power, 
# #                                fish_coralrichness_power, fish_coralrichness_site_power, fish_coralrichness_year_power, 
# #                                fish_coralrichness_year_site_power, fish_year_site_yearsite_coralrichness_power, 
# #                                fish_site_coralrichness_coralrichnesssite_power, fish_year_coralrichness_coralrichnessyear_power)
# # # Name each model in the same order they're listed above
# # model_names_log_power <- c("year", "site", "rugosity", "cover", "coralrichness", "year + site", "rugosity + site", 
# #                      "cover + site", "coralrichness + site", "rugosity + year", "cover + year", "coralrichness + year",
# #                      "rugosity + year + site", "cover + year + site", "coralrichness + year + site", "year + site + year*site",
# #                      "year + site + year*site + cover", "year + site + year*site + rugosity", "year + site + year*site + coralrichness",
# #                      "site + rugosity + rugosity*site", "site + cover + cover*site", "site + coralrichness + coralrichness*site",
# #                      "year + rugosity + rugosity*year", "year + cover + cover*year", "year + coralrichness + coralrichness*year",
# #                      "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year", 
# #                      "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site", 
# #                      "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year", 
# #                      "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site", 
# #                      "log(cover) + year + log(cover)*year", "log(coralrichness)", 
# #                      "log(coralrichness) + site", "log(coralrichness) + year", "log(coralrichness) + site + year", 
# #                      "log(coralrichness) + site + year + year*site", "log(coralrichness) + site + log(coralrichness)*site", 
# #                      "log(coralrichness) + year + log(coralrichness)*year",
# #                      "power(rugosity)", "power(rugosity) + site", "power(rugosity) + year", "power(rugosity) + site + year", 
# #                      "power(rugosity) + site + year + year*site", "power(rugosity) + site + power(rugosity)*site", 
# #                      "power(rugosity) + year + power(rugosity)*year", "power(cover)", "power(cover) + site", "power(cover) + year", 
# #                      "power(cover) + site + year", "power(cover) + site + year + year*site", "power(cover) + site + power(cover)*site", 
# #                      "power(cover) + year + power(cover)*year", "power(coralrichness)", 
# #                      "power(coralrichness) + site", "power(coralrichness) + year", "power(coralrichness) + site + year", 
# #                      "power(coralrichness) + site + year + year*site", "power(coralrichness) + site + power(coralrichness)*site", 
# #                      "power(coralrichness) + year + power(coralrichness)*year")
# # # AIC table with log and power models
# # fish_aic_table_log_power <- aictab(fish_models_log_power, modnames = model_names_log_power, digits = 4)

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



# # Create a list of all of the sponge models
# sponge_models_simple <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
#                      sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
#                      sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
#                      sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
#                      sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
#                      sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear)
# # AIC table
# sponge_aic_table_simple <- aictab(sponge_models_simple, modnames = model_names_simple, digits = 4)
# #write.csv(sponge_aic_table_simple, file = "sponge_aic_table.csv")
# ## With Logarithmic models
# sponge_models_log <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
#                         sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
#                         sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
#                         sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
#                         sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
#                         sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear, 
#                         sponge_rugosity_log, sponge_rugosity_site_log, sponge_rugosity_year_log, sponge_rugosity_year_site_log, 
#                         sponge_year_site_yearsite_rugosity_log, sponge_site_rugosity_rugositysite_log, sponge_year_rugosity_rugosityyear_log, 
#                         sponge_cover_log, sponge_cover_site_log, sponge_cover_year_log, sponge_cover_year_site_log, 
#                         sponge_year_site_yearsite_cover_log, sponge_site_cover_coversite_log, sponge_year_cover_coveryear_log, 
#                         sponge_coralrichness_log, sponge_coralrichness_site_log, sponge_coralrichness_year_log, 
#                         sponge_coralrichness_year_site_log, sponge_year_site_yearsite_coralrichness_log, 
#                         sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log)
# # AIC table with log models
# sponge_aic_table_log <- aictab(sponge_models_log, modnames = model_names_log, digits = 4)
# # ## With Logarithmic and power models
# # sponge_models_log_power <- list(sponge_year, sponge_site, sponge_rugosity, sponge_cover, sponge_coralrichness, sponge_year_site, sponge_rugosity_site, 
# #                                sponge_cover_site, sponge_coralrichness_site, sponge_rugosity_year, sponge_cover_year, sponge_coralrichness_year,
# #                                sponge_rugosity_year_site, sponge_cover_year_site, sponge_coralrichness_year_site, sponge_year_site_yearsite,
# #                                sponge_year_site_yearsite_cover, sponge_year_site_yearsite_rugosity, sponge_year_site_yearsite_coralrichness,
# #                                sponge_site_rugosity_rugositysite, sponge_site_cover_coversite, sponge_site_coralrichness_coralrichnesssite,
# #                                sponge_year_rugosity_rugosityyear, sponge_year_cover_coveryear, sponge_year_coralrichness_coralrichnessyear, 
# #                                sponge_rugosity_log, sponge_rugosity_site_log, sponge_rugosity_year_log, sponge_rugosity_year_site_log, 
# #                                sponge_year_site_yearsite_rugosity_log, sponge_site_rugosity_rugositysite_log, sponge_year_rugosity_rugosityyear_log, 
# #                                sponge_cover_log, sponge_cover_site_log, sponge_cover_year_log, sponge_cover_year_site_log, 
# #                                sponge_year_site_yearsite_cover_log, sponge_site_cover_coversite_log, sponge_year_cover_coveryear_log, 
# #                                sponge_coralrichness_log, sponge_coralrichness_site_log, sponge_coralrichness_year_log, 
# #                                sponge_coralrichness_year_site_log, sponge_year_site_yearsite_coralrichness_log, 
# #                                sponge_site_coralrichness_coralrichnesssite_log, sponge_year_coralrichness_coralrichnessyear_log,
# #                                sponge_rugosity_power, sponge_rugosity_site_power, sponge_rugosity_year_power, sponge_rugosity_year_site_power, 
# #                                sponge_year_site_yearsite_rugosity_power, sponge_site_rugosity_rugositysite_power, sponge_year_rugosity_rugosityyear_power, 
# #                                sponge_cover_power, sponge_cover_site_power, sponge_cover_year_power, sponge_cover_year_site_power, 
# #                                sponge_year_site_yearsite_cover_power, sponge_site_cover_coversite_power, sponge_year_cover_coveryear_power, 
# #                                sponge_coralrichness_power, sponge_coralrichness_site_power, sponge_coralrichness_year_power, 
# #                                sponge_coralrichness_year_site_power, sponge_year_site_yearsite_coralrichness_power, 
# #                                sponge_site_coralrichness_coralrichnesssite_power, sponge_year_coralrichness_coralrichnessyear_power)
# # # AIC table with log and power models
# # sponge_aic_table_log_power <- aictab(sponge_models_log_power, modnames = model_names_log_power, digits = 4)

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



# # Create a list of all of the coral models
# coral_models_simple <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
#                      coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
#                      coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
#                      coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear)
# # Name each model in the same order they're listed in coral_models
# coral_model_names_simple <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site", 
#                        "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site", 
#                        "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity", 
#                        "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year", "year + cover + cover*year")
# # AIC table
# coral_aic_table_simple <- aictab(coral_models_simple, modnames = coral_model_names_simple, digits = 4)
# #write.csv(coral_aic_table_simple, file = "coral_aic_table_simple.csv")
# ## With Logarithmic models
# coral_models_log <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
#                         coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
#                         coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
#                         coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear, 
#                         coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log, 
#                         coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log, 
#                         coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log, 
#                         coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log)
# coral_model_names_log <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
#                            "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
#                            "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
#                            "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
#                            "year + cover + cover*year",
#                            "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
#                            "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
#                            "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
#                            "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
#                            "log(cover) + year + log(cover)*year")
# # AIC table with log models
# coral_aic_table_log <- aictab(coral_models_log, modnames = model_names_log, digits = 4)
# # ## With Logarithmic and power models
# # coral_models_log_power <- list(coral_year, coral_site, coral_rugosity, coral_cover, coral_year_site, coral_rugosity_site, 
# #                               coral_cover_site, coral_rugosity_year, coral_cover_year,coral_rugosity_year_site, coral_cover_year_site, 
# #                               coral_year_site_yearsite, coral_year_site_yearsite_cover, coral_year_site_yearsite_rugosity, 
# #                               coral_site_rugosity_rugositysite, coral_site_cover_coversite, coral_year_rugosity_rugosityyear, coral_year_cover_coveryear, 
# #                               coral_rugosity_log, coral_rugosity_site_log, coral_rugosity_year_log, coral_rugosity_year_site_log, 
# #                               coral_year_site_yearsite_rugosity_log, coral_site_rugosity_rugositysite_log, coral_year_rugosity_rugosityyear_log, 
# #                               coral_cover_log, coral_cover_site_log, coral_cover_year_log, coral_cover_year_site_log, 
# #                               coral_year_site_yearsite_cover_log, coral_site_cover_coversite_log, coral_year_cover_coveryear_log,
# #                                coral_rugosity_power, coral_rugosity_site_power, coral_rugosity_year_power, coral_rugosity_year_site_power,
# #                                coral_year_site_yearsite_rugosity_power, coral_site_rugosity_rugositysite_power, coral_year_rugosity_rugosityyear_power,
# #                                coral_cover_power, coral_cover_site_power, coral_cover_year_power, coral_cover_year_site_power,
# #                                coral_year_site_yearsite_cover_power, coral_site_cover_coversite_power, coral_year_cover_coveryear_power)
# # coral_model_names_log_power <- c("year", "site", "rugosity", "cover", "year + site", "rugosity + site",
# #                                  "cover + site", "rugosity + year", "cover + year","rugosity + year + site", "cover + year + site",
# #                                  "year + site + year*site", "year + site + year*site + cover", "year + site + year*site + rugosity",
# #                                  "site + rugosity + rugosity*site", "site + cover + cover*site", "year + rugosity + rugosity*year",
# #                                  "year + cover + cover*year",
# #                                  "log(rugosity)", "log(rugosity) + site", "log(rugosity) + year", "log(rugosity) + site + year",
# #                                  "log(rugosity) + site + year + year*site", "log(rugosity) + site + log(rugosity)*site",
# #                                  "log(rugosity) + year + log(rugosity)*year", "log(cover)", "log(cover) + site", "log(cover) + year",
# #                                  "log(cover) + site + year", "log(cover) + site + year + year*site", "log(cover) + site + log(cover)*site",
# #                                  "log(cover) + year + log(cover)*year",
# #                                  "power(rugosity)", "power(rugosity) + site", "power(rugosity) + year", "power(rugosity) + site + year",
# #                                  "power(rugosity) + site + year + year*site", "power(rugosity) + site + power(rugosity)*site",
# #                                  "power(rugosity) + year + power(rugosity)*year", "power(cover)", "power(cover) + site", "power(cover) + year",
# #                                  "power(cover) + site + year", "power(cover) + site + year + year*site", "power(cover) + site + power(cover)*site",
# #                                  "power(cover) + year + power(cover)*year")
# # # AIC table with log and power models
# # coral_aic_table_log_power <- aictab(coral_models_log_power, modnames = model_names_log_power, digits = 4)

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

## Fish and sponge models including all logarithmic models and simple power models
fishsponge_models <- list(fishsponge_year, fishsponge_site, fishsponge_year_site, fishsponge_year_site_yearsite, fishsponge_coralrichness, 
                          fishsponge_coralrichness_site, fishsponge_coralrichness_year, fishsponge_coralrichness_year_site, 
                          fishsponge_year_site_yearsite_coralrichness, fishsponge_site_coralrichness_coralrichnesssite, 
                          fishsponge_year_coralrichness_coralrichnessyear, fishsponge_coralrichness_log, 
                          fishsponge_coralrichness_site_log, fishsponge_coralrichness_year_log, fishsponge_coralrichness_year_site_log, 
                          fishsponge_year_site_yearsite_coralrichness_log, fishsponge_site_coralrichness_coralrichnesssite_log, 
                          fishsponge_year_coralrichness_coralrichnessyear_log, fishsponge_coralrichness_power, fishsponge_year_sqyear)
# Name each model in the same order they're listed in coral_models
fishsponge_model_names <- c("year", "site", "year + site", "year + site + year*site", "coralrichness",
                            "coralrichness + site", "coralrichness + year", "coralrichness + year + site",
                            "coralrichness + year+ site + year*site", "coralrichness + site + coralrichness*site", 
                            "coralrichness + year + coralrichness*year", "log(coralrichness)",
                            "log(coralrichness) + site", "log(coralrichness) + year", "log(coralrichness) + site + year",
                            "log(coralrichness) + site + year + year*site", "log(coralrichness) + site + log(coralrichness)*site",
                            "log(coralrichness) + year + log(coralrichness)*year", "power(coralrichness)", "year_sqyear")
# AIC table
fishsponge_aic_table <- aictab(fishsponge_models, modnames = fishsponge_model_names, digits = 4)
# Create file with AIC table
#write.csv(fishsponge_aic_table, file = "nb_spongeandfish_aic_table.csv")



# # Create a list of all of the "combined" models
# combined_models_simple <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
#                         combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
#                         combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
#                         combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
#                         combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
#                         combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear)
# # AIC table
# combined_aic_table_simple <- aictab(combined_models_simple, modnames = model_names_simple, digits = 4)
# #write.csv(combined_aic_table_simple, file = "combined_aic_table_simple.csv")
# ## With Logarithmic models
# combined_models_log <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
#                           combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
#                           combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
#                           combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
#                           combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
#                           combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear, 
#                           combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
#                           combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
#                           combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
#                           combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
#                           combined_coralrichness_log, combined_coralrichness_site_log, combined_coralrichness_year_log, 
#                           combined_coralrichness_year_site_log, combined_year_site_yearsite_coralrichness_log, 
#                           combined_site_coralrichness_coralrichnesssite_log, combined_year_coralrichness_coralrichnessyear_log)
# # AIC table with log models
# combined_aic_table_log <- aictab(combined_models_log, modnames = model_names_log, digits = 4)
# # ## With Logarithmic and power models
# # combined_models_log_power <- list(combined_year, combined_site, combined_rugosity, combined_cover, combined_coralrichness, combined_year_site, combined_rugosity_site, 
# #                                combined_cover_site, combined_coralrichness_site, combined_rugosity_year, combined_cover_year, combined_coralrichness_year,
# #                                combined_rugosity_year_site, combined_cover_year_site, combined_coralrichness_year_site, combined_year_site_yearsite,
# #                                combined_year_site_yearsite_cover, combined_year_site_yearsite_rugosity, combined_year_site_yearsite_coralrichness,
# #                                combined_site_rugosity_rugositysite, combined_site_cover_coversite, combined_site_coralrichness_coralrichnesssite,
# #                                combined_year_rugosity_rugosityyear, combined_year_cover_coveryear, combined_year_coralrichness_coralrichnessyear, 
# #                                combined_rugosity_log, combined_rugosity_site_log, combined_rugosity_year_log, combined_rugosity_year_site_log, 
# #                                combined_year_site_yearsite_rugosity_log, combined_site_rugosity_rugositysite_log, combined_year_rugosity_rugosityyear_log, 
# #                                combined_cover_log, combined_cover_site_log, combined_cover_year_log, combined_cover_year_site_log, 
# #                                combined_year_site_yearsite_cover_log, combined_site_cover_coversite_log, combined_year_cover_coveryear_log, 
# #                                combined_coralrichness_log, combined_coralrichness_site_log, combined_coralrichness_year_log, 
# #                                combined_coralrichness_year_site_log, combined_year_site_yearsite_coralrichness_log, 
# #                                combined_site_coralrichness_coralrichnesssite_log, combined_year_coralrichness_coralrichnessyear_log,
# #                                combined_rugosity_power, combined_rugosity_site_power, combined_rugosity_year_power, combined_rugosity_year_site_power, 
# #                                combined_year_site_yearsite_rugosity_power, combined_site_rugosity_rugositysite_power, combined_year_rugosity_rugosityyear_power, 
# #                                combined_cover_power, combined_cover_site_power, combined_cover_year_power, combined_cover_year_site_power, 
# #                                combined_year_site_yearsite_cover_power, combined_site_cover_coversite_power, combined_year_cover_coveryear_power, 
# #                                combined_coralrichness_power, combined_coralrichness_site_power, combined_coralrichness_year_power, 
# #                                combined_coralrichness_year_site_power, combined_year_site_yearsite_coralrichness_power, 
# #                                combined_site_coralrichness_coralrichnesssite_power, combined_year_coralrichness_coralrichnessyear_power)
# # # AIC table with log and power models
# # combined_aic_table_log_power <- aictab(combined_models_log_power, modnames = model_names_log_power, digits = 4)

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
