### Nicole Keefner
### Master's Thesis: Create dataset called variables.csv that I will work with to develop models and figures



## Use *** to search for errors in the code or areas that need more work



## Set Working Directory
## Install packages



## Load packages
# first needed for revalue function
library(plyr)
# first needed for tally function
library(tidyverse)



## Import coral dataset
benthic_raw <- read.csv("benthic.csv", header = T)

# Only select rows and columns for which there are values entered (benthic_raw has extraneous rows)
benthic_raw <- benthic_raw[1:1057,1:137]

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
benthic_raw$site <- as.character(benthic_raw$site)
benthic_raw <- benthic_raw[benthic_raw$site == "muskN" | benthic_raw$site == "pelican" | benthic_raw$site == "crab" | 
                             benthic_raw$site == "bigelow" | benthic_raw$site == "monkey" | benthic_raw$site == "iguana" |
                             benthic_raw$site == "white" | benthic_raw$site == "grand", ]
benthic_raw$site <- as.factor(benthic_raw$site)

# Remove RLF as an observer because in 2014 there were two observers
benthic_raw$observer <- as.character(benthic_raw$observer)
benthic_raw <- benthic_raw[benthic_raw$observer != "RLF", ]
benthic_raw$observer <- as.factor(benthic_raw$observer)

# Only retain observations where survey == main
benthic_raw$survey <- as.character(benthic_raw$survey)
benthic_raw <- benthic_raw[benthic_raw$survey == "main", ]
benthic_raw$survey <- as.factor(benthic_raw$survey)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
benthic_raw <- transform(benthic_raw, Site_Year = paste(benthic_raw$year, benthic_raw$site, sep="_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
benthic_raw$Taxa <- "Coral"


# Create a new dataframe with the number (n) of transects for each Site_Year
coral_sample_size <- tally(group_by(benthic_raw, benthic_raw$Site_Year))

# Rename column in sample_size_coral dataframe to use as a key to merge this information with the raw data
colnames(coral_sample_size)[colnames(coral_sample_size) == "benthic_raw$Site_Year"] <- "Site_Year"
benthic_raw <- merge(benthic_raw, coral_sample_size, by = "Site_Year", all = T)

# Create a new function called imin that returns the minimum of 2 values
imin <- function(x, y){
  if (x > y){
    return(y)
  }
  return(x)
}

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant
benthic_raw_three <- benthic_raw %>% group_by(benthic_raw$Site_Year) %>% sample_n(imin(3, benthic_raw$n))
# Double-check: coral_sample_size should have 216 obs. (different Site_Year's), so benthic_raw_three should have 648 obs.
#***Make sure that, even though this is random, this code is replicable (sample_n is used for all 3 groups so check for the others too)

# Now we need to ungroup the data to avoid confusing errors later
benthic_raw_three <- ungroup(benthic_raw_three)


# Put dataset into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
benthic_raw_longform <- benthic_raw_three %>%
  gather(key = "Taxonomic_Group", value = "Count", acpa:sosp)

# Double-check
summary(benthic_raw_longform)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
benthic_raw_longform$Taxonomic_Group <- as.factor(benthic_raw_longform$Taxonomic_Group)

# Can also make Year a factor instead of an integer
benthic_raw_longform$year <- as.factor(benthic_raw_longform$year)

# Summary Information
str(benthic_raw_longform)
summary(benthic_raw_longform)
summary(benthic_raw_longform$site)
summary(benthic_raw_longform$year)

# Aggregate groups by the site and year then averages the percent covers for each of these across all transects
coral_percentcover <- aggregate(benthic_raw_longform$livecoral_all_cover, 
                                by = list(benthic_raw_longform$site, benthic_raw_longform$year), 
                                FUN = mean)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 values for percentcover
# Rename columns to match the data in them
names(coral_percentcover) <- c("Site", "Year", "Percent_Coral_Cover")

# Aggregate groups by the site, year, taxgroup then averages the counts for each of these across all transects
coral_averages <- aggregate(benthic_raw_longform$Count, 
                            by = list(benthic_raw_longform$site, benthic_raw_longform$year, benthic_raw_longform$Taxonomic_Group), 
                            FUN = mean)
# Double-check environment to make sure output makes sense: 8sites*27years*32taxgroups = 6912 averages
#***change column headers to match the data in them

# Define new function to use in the aggregate function to calculate richness similar to how mean was used above
richness = function(x){
  return(length(x[x>0]))
}

# Aggregate groups by the site and year then uses the function we created above to calculate richness
coral_richness <- aggregate(coral_averages$x, 
                            by = list(coral_averages$Group.1, coral_averages$Group.2), 
                            FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 values for richness
# Rename columns to match the data in them
names(coral_richness) <- c("Site", "Year", "Coral_Richness")



## Import sponge dataset
sponge_raw <- read.csv("sponge.csv", header = T)

# Only select rows and columns for which there are values entered (sponge_raw has extraneous rows and columns)
sponge_raw <- sponge_raw[1:617,1:65]

# Sometimes site names were entered using different capitalization. 
# Correct these entry mistakes by making names consistent
summary(sponge_raw$Site)
sponge_raw$Site <- revalue(sponge_raw$Site, c("Pelican" = "pelican"))

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
sponge_raw$Site <- as.character(sponge_raw$Site)
sponge_raw <- sponge_raw[sponge_raw$Site == "muskN" | sponge_raw$Site == "pelican" | sponge_raw$Site == "crab" | 
                           sponge_raw$Site == "bigelow" | sponge_raw$Site == "monkey" | sponge_raw$Site == "iguana" |
                           sponge_raw$Site == "white" | sponge_raw$Site == "grand", ]
sponge_raw$Site <- as.factor(sponge_raw$Site)

# Remove October 2017 post-hurricane survey information
sponge_raw$Notes <- as.character(sponge_raw$Notes)
sponge_raw <- sponge_raw[sponge_raw$Notes != "post-hurricane survey", ]
sponge_raw$Notes <- as.factor(sponge_raw$Notes)

# Some "ghost" factors are being retained for transect and observer
# Remove these ghost factors
summary(sponge_raw)
sponge_raw$Observer <- as.character(sponge_raw$Observer)
sponge_raw <- sponge_raw[sponge_raw$Observer == "E MacLean" | sponge_raw$Observer == "L Jarecki", ]
sponge_raw$Observer <- as.factor(sponge_raw$Observer)

sponge_raw$Transect <- as.character(sponge_raw$Transect)
sponge_raw <- sponge_raw[sponge_raw$Transect == "1" | sponge_raw$Transect == "2" | sponge_raw$Transect == "3" | 
                           sponge_raw$Transect == "4" | sponge_raw$Transect == "T", ]
sponge_raw$Transect <- as.factor(sponge_raw$Transect)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
sponge_raw <- transform(sponge_raw, Site_Year = paste(sponge_raw$Year, sponge_raw$Site, sep="_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
sponge_raw$Taxa <- "Sponge"


# Create a new dataframe with the number (n) of transects for each Site_Year
sponge_sample_size <- tally(group_by(sponge_raw, sponge_raw$Site_Year))

# Rename column in sample_size_sponge dataframe to use as a key to merge this information with the raw data
colnames(sponge_sample_size)[colnames(sponge_sample_size) == "sponge_raw$Site_Year"] <- "Site_Year"
sponge_raw <- merge(sponge_raw, sponge_sample_size, by = "Site_Year", all = T)

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant (using imin function created above)
sponge_raw_three <- sponge_raw %>% group_by(sponge_raw$Site_Year) %>% sample_n(imin(3, sponge_raw$n))
# Double-check: sponge_sample_size should have 164 obs. (different Site_Year's), so sponge_raw_three should have 492 obs.

# Now we need to ungroup the data to avoid confusing errors later
sponge_raw_three <- ungroup(sponge_raw_three)


# Put dataset into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
sponge_raw_longform <- sponge_raw_three %>%
  gather(key = "Taxonomic_Group", value = "Count", Agelas.clathrodes..Agelas.citrina.or.Clathria.faviformis:Black..spiny..purple.exudate.but.not.slimy)

# Double-check
summary(sponge_raw_longform)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
sponge_raw_longform$Taxonomic_Group <- as.factor(sponge_raw_longform$Taxonomic_Group)

# Can also make Year a factor instead of an integer
sponge_raw_longform$Year <- as.factor(sponge_raw_longform$Year)

# ***When transect length is 20 m, multiply counts by 3/2
#tlength_short <- sponge_raw[sponge_raw$Transect.Length..m. == "20", ]

# Summary Information
str(sponge_raw_longform)
summary(sponge_raw_longform)
summary(sponge_raw_longform$Site)
summary(sponge_raw_longform$Year)

# Aggregate groups by the site, year, taxgroup then averages the counts for each of these across all transects
sponge_averages <- aggregate(sponge_raw_longform$Count, 
                             by = list(sponge_raw_longform$Site, sponge_raw_longform$Year, sponge_raw_longform$Taxonomic_Group), 
                             FUN = mean)
#*** Double-check environment to make sure output makes sense: (8sites*20years - 4site_yearsthatweren'tsurveyed)*58taxgroups = 9048 averages
#***change column headers to match the data in them

# Aggregate groups by the site and year then uses the function we created above to calculate richness
sponge_richness <- aggregate(sponge_averages$x, 
                             by = list(sponge_averages$Group.1, sponge_averages$Group.2), 
                             FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*20years - 4site_yearsthatweren'tsurveyed = 156 averages
# Rename columns to match the data in them
names(sponge_richness) <- c("Site", "Year", "Sponge_Richness")

# Some data checking for sponges:
# # Convert to wide form
# sponge_raw_wideform <- spread(sponge_raw_longform, Taxonomic_Group, Count)
# # ***Error because spread function doesn't work with duplicate row identifiers***
# # ***So, need to figure out how to group by site-year
# # In this case, sometimes the same transect was recorded 3 times in the same year at a given site e.g.:
# # Transect  Site  Year
# # 2         muskN 2012
# # 2         muskN 2012
# 
# # Check that observations were made at every site for every year
# # 20 years and 8 sites = 160 observations expected
# check1 <- unique(sponge_raw_longform$Site_Year)
# # because check1 has 156 levels, almost every site was visited for all years
# # After closer inspection, 1993_crab, 2014_pelican, 2017_pelican, and 2017_bigelow are missing
# 
# # Create new subset for all the times where transect length is not 30 m
#not30 <-sponge_raw[sponge_raw$Transect.Length..m. != "30", 1:7]



## Import fish datasets (see "Fish Metadata.docx" for more information)
fish_raw <- read.csv("fish.csv", header = T)

# Sometimes site names were entered using different number of spaces. 
# Correct these entry mistakes by making names consistent
summary(fish_raw$site)
fish_raw$site <- revalue(fish_raw$site, c("pelican   " = "pelican"))

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
fish_raw$site <- as.character(fish_raw$site)
fish_raw <- fish_raw[fish_raw$site == "muskN" | fish_raw$site == "pelican" | fish_raw$site == "crab" | 
                       fish_raw$site == "bigelow" | fish_raw$site == "monkey" | fish_raw$site == "iguana" |
                       fish_raw$site == "white" | fish_raw$site == "grand", ]
fish_raw$site <- as.factor(fish_raw$site)

# Only retain observations where survey == main
fish_raw$survey <- as.character(fish_raw$survey)
fish_raw <- fish_raw[fish_raw$survey == "main", ]
fish_raw$survey <- as.factor(fish_raw$survey)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
fish_raw <- transform(fish_raw, Site_Year = paste(fish_raw$year, fish_raw$site, sep = "_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
fish_raw$Taxa <- "Fish"


# Create a new dataframe with the number (n) of transects for each Site_Year
fish_sample_size <- tally(group_by(fish_raw, fish_raw$Site_Year))

# Rename column in sample_size_fish dataframe to use as a key to merge this information with the raw data
colnames(fish_sample_size)[colnames(fish_sample_size) == "fish_raw$Site_Year"] <- "Site_Year"
fish_raw <- merge(fish_raw, fish_sample_size, by = "Site_Year", all = T)

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant (using imin function created above)
fish_raw_three <- fish_raw %>% group_by(fish_raw$Site_Year) %>% sample_n(imin(3, fish_raw$n))
# Double-check: fish_sample_size should have 216 obs. (different Site_Year's), so benthic_raw_three should have 648 obs.

# Now we need to ungroup the data to avoid confusing errors later
fish_raw_three <- ungroup(fish_raw_three)


# Put fish_raw into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
fish_raw_longform <- fish_raw_three %>% gather(key = "KEY", value = "Count", popaa:kysea)

# Because the species codes end with j or a to distinguish adult and juveniles,
# Split new KEY column into species_code and age_class columns
KEY <- as.character(fish_raw_longform$KEY)
fish_split <- data.frame("Species_Code" = substr(KEY, 1, (nchar(KEY)-1)), "Age_Class" = substr(KEY, nchar(KEY), nchar(KEY)))

# Combine long-form dataset with the split columns dataframe
fish_raw_longform <- cbind(fish_raw_longform, fish_split)
# ***^This step adds extraneous 56 rows

# Only retain observations where Age_Class == adult
#***Clear Justification
fish_raw_longform$Age_Class <- as.character(fish_raw_longform$Age_Class)
fish_raw_longform <- fish_raw_longform[fish_raw_longform$Age_Class == "a", ]
fish_raw_longform$Age_Class <- as.factor(fish_raw_longform$Age_Class)

# Import fish codes to get common names
fish_codes <- read.csv("fish_codes.csv", header = T)

# Rename notes column in fish_codes dataframe to avoid confusion with other notes column
colnames(fish_codes)[colnames(fish_codes) == "notes"] <- "Fish taxonomy notes"

# Rename column in fish_codes dataframe to use as a key to merge this information with the raw data
colnames(fish_codes)[colnames(fish_codes) == "new_code"] <- "KEY"
fish_raw_longform <- merge(fish_raw_longform, fish_codes, by = "KEY", all = T)

# # Remove observations where Count is NA
# #***Are NA's zeroes?
# fish_raw_longform <- fish_raw_longform[!is.na(fish_raw_longform$Count),]

# Can also make Year a factor instead of an integer
fish_raw_longform$year <- as.factor(fish_raw_longform$year)

# Summary Information
str(fish_raw_longform)
summary(fish_raw_longform)
summary(fish_raw_longform$site)
summary(fish_raw_longform$year)
# ************** There are NA's throughout the dataset. This is definitely a mistake early on in the code that I need to fix.

# Aggregate groups by the site, year, speciescode then averages the counts for each of these across all transects
fish_averages <- aggregate(fish_raw_longform$Count, 
                           by = list(fish_raw_longform$site, fish_raw_longform$year, fish_raw_longform$Species_Code), 
                           FUN = mean)
# ***Double-check environment to make sure output makes sense: 8sites*27years*119taxgroups = 25,704 averages
# ***but this gives 24,624 averages, need to look into why this may be (probably something to do with the taxgroups)
#***change column headers to match the data in them

# Aggregate groups by the site and year then uses the function we created above to calculate richness
fish_richness <- aggregate(fish_averages$x, 
                           by = list(fish_averages$Group.1, fish_averages$Group.2), 
                           FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 richness values
# Rename columns to match the data in them
names(fish_richness) <- c("Site", "Year", "Fish_Richness")

# Data checking for fishes
# # Failed attempts at reformatting data to wide form
# #***Error because spread function doesn't work with duplicate row identifiers***
# # Reduce number of columns to convert to wide form
# fish_raw_longform_reduced <- fish_raw_longform[,c("notes","year", "month", "day", "site", "transect", 
#                                                   "fixed_transect", "Count", "Species_Code", "Family", 
#                                                   "common.name", "Fish taxonomy notes", "Site_Year", "Taxa")]
# # Keep only the columns that I need to convert to wide form
# fish_raw_longform_minimum <- fish_raw_longform_reduced[,c("notes", "year", "month", "day", "site", "transect", 
#                                                           "fixed_transect", "Count", "Species_Code")]
# # Try using melt
# fish_raw_wideform_melt <- melt(fish_raw_longform_minimum, id.vars = fish_raw_longform_minimum$Species_Code, measure.vars = fish_raw_longform_minimum$Count,
#      variable.name = "variable")
# # Convert to wide form
# fish_raw_wideform <- spread(fish_raw_longform_reduced, Species_Code, Count)
# fish_raw_wideform_min <- spread(fish_raw_longform_minimum, Species_Code, Count)
# # Note that there are 2 times more observations now than in the original raw wide format - 
# summary(fish_raw$site)
# summary(fish_raw_wideform$site)
# # this is because A/J are in different rows, not listed in different columns

# # Check that observations were made at every site for every year
# # 25 years and 8 sites = 200 observations expected
# check2 <- unique(fish_raw_longform_reduced$Site_Year)
# # because check2 has 200 levels, every site was visited for all 25 years
# 
# # ***Add transect length from sponge dataset to the corresponding observations in the fish dataset?***
# summary(fish_raw_longform_reduced$transect)
# summary(sponge_raw_longform$Transect)
# # Because the levels are not the same, this may be difficult
# 
# # Check that all of the counts are integers
# sum(sponge_raw_longform$Count)
# sum(fish_raw_longform_reduced$Count)
# # ***There is definitely a more efficient way to do this***
# # Because the sum of fish counts has a decimal, create a subset of non-zero counts to ID the culprit
# fish_nozero <- fish_raw_longform_reduced[fish_raw_longform_reduced$Count != "0", ]
# # Looks like there are some 1.5's and 1.875's, so create a subset that removes these observations to 
# # determine if there are other non-integer values for Count
# fish_integertest <- fish_nozero[fish_nozero$Count == 1 | fish_nozero$Count >= 2, ]
# # Now try again,
# sum(fish_integertest$Count)
# # From a closer look at the data: 1.5, 1.875, 3.75, 4.5, 7.5, 10.5, 16.5, 19.5, 22.5, 28.5, 31.5, 37.5, and 49.5
# # are identified as non-integers, but there may be more.
# 
# # Check that for each Site_Year there are 3 transects
# sponge_num_transects <- as.data.frame(table(sponge_raw$Site_Year), responseName="num_transects")
# fish_num_transects <- as.data.frame(table(fish_raw$Site_Year), responseName="num_transects")
# # Create subsets that only include Site_Year's with more than 3 transects
# sponge_num_transects <- sponge_num_transects[sponge_num_transects$num_transects > 3, ]
# fish_num_transects <- fish_num_transects[fish_num_transects$num_transects > 3, ]
# 
# # ***Check that the transects within a given site-year are not repeated***



## Import rugosity dataset
rugosity_raw <- read.csv("Reef rugosity site x year means 2019.csv", header = T)
# Rename columns to match the format for other dataframes
names(rugosity_raw) <- c("Site", "Year", "Rugosity")


# Summarizing these variables for use in models into one table
# Year
# Site
# coral_percentcover
# coral_richness
# sponge_richness
# fish_richness

# Create new column called "Site_Year" for each of these dataframes that combines the year and site as ####_Sitename
coral_percentcover <- transform(coral_percentcover, Site_Year = paste(coral_percentcover$Year, coral_percentcover$Site, sep = "_"))
rugosity <- transform(rugosity_raw, Site_Year = paste(rugosity_raw$Year, rugosity_raw$Site, sep = "_"))
coral_richness <- transform(coral_richness, Site_Year = paste(coral_richness$Year, coral_richness$Site, sep = "_"))
sponge_richness <- transform(sponge_richness, Site_Year = paste(sponge_richness$Year, sponge_richness$Site, sep = "_"))
fish_richness <- transform(fish_richness, Site_Year = paste(fish_richness$Year, fish_richness$Site, sep = "_"))

# Merge the dataframes into one table
variables <- merge(coral_percentcover, rugosity, by = "Site_Year", all = T)
variables <- merge(variables, coral_richness, by = "Site_Year", all = T)
variables <- merge(variables, sponge_richness, by = "Site_Year", all = T)
variables <- merge(variables, fish_richness, by = "Site_Year", all = T)
variables <- variables[,c("Site_Year","Percent_Coral_Cover", "Rugosity", "Coral_Richness", "Sponge_Richness", "Fish_Richness")]

# ***Add a column called Sponge_and_Fish_Richness that adds the richness of sponges and fishes
# However, the sponge and fish richness will only be calculated for the Site_Year combinations where
# richness values are present for both groups.
# Subset variables data to only Site_Year and richnesses
sponge_and_fish_richness <- variables[,c("Site_Year", "Sponge_Richness", "Fish_Richness")]
# Check for complete cases and remove any rows with missing data
sponge_and_fish_richness <- sponge_and_fish_richness[complete.cases(sponge_and_fish_richness), ]
# Create new column called Sponge_and_Fish_Richness with the sum of coral, sponge, and fish richness
sponge_and_fish_richness$Sponge_and_Fish_Richness <- sponge_and_fish_richness$Sponge_Richness + sponge_and_fish_richness$Fish_Richness
# Remove richness columns except combined
sponge_and_fish_richness <- sponge_and_fish_richness[,c("Site_Year", "Sponge_and_Fish_Richness")]

# Merge sponge_and_fish_richness with the variables dataframe
variables <- merge(variables, sponge_and_fish_richness, by = "Site_Year", all = T)

# ***Add a column called Combined_Richness that adds the richness of corals, sponges, and fishes
# However, the combined richness will only be calculated for the Site_Year combinations where
# richness values are present for all 3 groups.
# Subset variables data to only Site_Year and richnesses
combined_richness <- variables[,c("Site_Year", "Coral_Richness", "Sponge_Richness", "Fish_Richness")]
# Check for complete cases and remove any rows with missing data
combined_richness <- combined_richness[complete.cases(combined_richness), ]
# Create new column called Combined_Richness with the sum of coral, sponge, and fish richness
combined_richness$Combined_Richness <- combined_richness$Coral_Richness + combined_richness$Sponge_Richness + combined_richness$Fish_Richness
# Remove richness columns except combined
combined_richness <- combined_richness[,c("Site_Year", "Combined_Richness")]

# Merge combined_richness with the variables dataframe
variables <- merge(variables, combined_richness, by = "Site_Year", all = T)



# Split Site_Year column into separate Year and Site columns
variables$Site_Year <- as.character(variables$Site_Year)
variables <- separate(variables, Site_Year, into = c("Year", "Site"), sep="_")

# Save this variables table as a .csv
#write.table(variables, file="variables.csv", sep=",", col.names=TRUE,row.names=FALSE)

# This concludes the data import and cleaning
# See Models.R script for modeling and figures