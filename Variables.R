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
benthic_raw <- read.csv(file = "benthic.csv", header = T)

# Only select rows and columns for which there are values entered (benthic_raw has extraneous rows and columns)
benthic_raw <- benthic_raw[1:1057,1:137]

# Notice that there are many sites that are not from the main survey
#summary(benthic_raw$site)
# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
# The code below assumes that the name of the sites remains consistent. For example, if the original file has "Pelican" instead of "pelican", this observation will not be included.
# I use the as.character() and as.factor() functions frequently in this code to avoid the retainment of factor levels that are unwanted.
benthic_raw$site <- as.character(benthic_raw$site)
benthic_raw <- benthic_raw[benthic_raw$site == "muskN" | benthic_raw$site == "pelican" | benthic_raw$site == "crab" | 
                             benthic_raw$site == "bigelow" | benthic_raw$site == "monkey" | benthic_raw$site == "iguana" |
                             benthic_raw$site == "white" | benthic_raw$site == "grand", ]
benthic_raw$site <- as.factor(benthic_raw$site)

# Remove RLF as an observer because in 2014 there were two observers
benthic_raw$observer <- as.character(benthic_raw$observer)
benthic_raw <- benthic_raw[benthic_raw$observer != "RLF", ]
benthic_raw$observer <- as.factor(benthic_raw$observer)

# Only retain observations for the main survey
benthic_raw$survey <- as.character(benthic_raw$survey)
benthic_raw <- benthic_raw[benthic_raw$survey == "main", ]
benthic_raw$survey <- as.factor(benthic_raw$survey)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
# This column will be very useful throughout the rest of the code because our observations will be for each site and for each year
benthic_raw <- transform(benthic_raw, Site_Year = paste(benthic_raw$year, benthic_raw$site, sep = "_"))

# Create new column in all datasets called Taxa, so I know which dataset the information came from depending on how the dataframes are combined
# Here I've done this for the coral dataset
benthic_raw$Taxa <- "Coral"

# Create a new dataframe with the number (n) of transects for each Site_Year
coral_sample_size <- tally(group_by(benthic_raw, benthic_raw$Site_Year))
# Check the minimum and maximum number if transect per Site_Year
#min(coral_sample_size$n)
#max(coral_sample_size$n)

# Rename column in coral_sample_size dataframe to use as a key to merge sample size information with the raw data
colnames(coral_sample_size)[colnames(coral_sample_size) == "benthic_raw$Site_Year"] <- "Site_Year"
benthic_raw <- merge(x = benthic_raw, y = coral_sample_size, by = "Site_Year", all = T)

# Create a new function called imin that returns the minimum of 2 values
imin <- function(x, y){
  if (x > y){
    return(y)
  }
  return(x)
}

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant. 
# For example, if there were 2 transects sampled, then the average will be taken for those two transects.
benthic_raw_three <- benthic_raw %>% group_by(benthic_raw$Site_Year) %>% sample_n(size = imin(x = 3, y = benthic_raw$n))
# The warnings here are just to let us know that there are 3 or more transects for that site_year
# Double-check: coral_sample_size should have 216 obs. (number of unique Site_Year's), so benthic_raw_three should have 648 obs. (216*3)

# Now we need to ungroup the data to avoid confusing errors later
benthic_raw_three <- ungroup(x = benthic_raw_three)

# Put dataset into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
benthic_raw_longform <- gather(data = benthic_raw_three, key = "Taxonomic_Group", value = "Count", acpa:sosp)

# Double-check dataset looks correct
#summary(benthic_raw_longform)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
benthic_raw_longform$Taxonomic_Group <- as.factor(x = benthic_raw_longform$Taxonomic_Group)

# Can also make Year a factor instead of an integer
benthic_raw_longform$year <- as.factor(benthic_raw_longform$year)

# Check that there are the same number of observations for each site and for each year
#summary(benthic_raw_longform$site)
#summary(benthic_raw_longform$year)
# Summary Information
#str(benthic_raw_longform)
#summary(benthic_raw_longform)

# For Percent Coral Cover
# Aggregate groups by the site and year then find the mean for each of these Site_Year's across all transects (in this case, 3 transects per site_year)
coral_percentcover <- aggregate(x = benthic_raw_longform$livecoral_all_cover, 
                                by = list(benthic_raw_longform$site, benthic_raw_longform$year), 
                                FUN = mean)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 values for percentcover
# Rename columns to match the data in them
names(x = coral_percentcover) <- c("Site", "Year", "Percent_Coral_Cover")

# For Mean Counts of corals for each site_year (this step is not absolutely necessary for this paper, but might be useful if you want to look at differences over time and site for a particular species)
# Aggregate groups by the site, year, and taxonomic group then averages the counts for each of these across all transects (in this case, 3 transects per site_year)
coral_averages <- aggregate(x = benthic_raw_longform$Count, 
                            by = list(benthic_raw_longform$site, benthic_raw_longform$year, benthic_raw_longform$Taxonomic_Group), 
                            FUN = mean)
# Double-check environment to make sure output makes sense: 8sites*27years*32taxgroups = 6912 averages
# Rename columns to match the data in them
names(coral_averages) <- c("Site", "Year", "Species_Code", "Mean_Count")

# Define new function to use in the aggregate function to calculate richness similar to how mean was used above
richness = function(x){
  return(length(x[x>0]))
}

# For coral richness
# Aggregate groups by the site and year then uses the function we created above to calculate richness
coral_richness <- aggregate(x = coral_averages$Mean_Count, 
                            by = list(coral_averages$Site, coral_averages$Year), 
                            FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 values for richness
# Rename columns to match the data in them
names(x = coral_richness) <- c("Site", "Year", "Coral_Richness")

# For sponge cover
# Aggregate groups by the site and year then then averages the sponge cover for each of these across all transects (in this case, 3 transects per site_year)
sponge_percentcover <- aggregate(x = benthic_raw_longform$sponge_all_cover, 
                                by = list(benthic_raw_longform$site, benthic_raw_longform$year), 
                                FUN = mean)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 values for sponge cover
# Rename columns to match the data in them
names(x = sponge_percentcover) <- c("Site", "Year", "Percent_Sponge_Cover")





## Import sponge dataset
sponge_raw <- read.csv(file = "sponge.csv", header = T)

# Only select rows and columns for which there are values entered (sponge_raw has extraneous rows and columns)
sponge_raw <- sponge_raw[1:617,1:65]

# Sometimes site names were entered using different capitalization. 
#summary(sponge_raw$Site)
# Correct these entry mistakes by making names consistent
sponge_raw$Site <- revalue(x = sponge_raw$Site, replace = c("Pelican" = "pelican"))

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

# Some "ghost" factors (factors with no existing observations) are being retained for transect and observer
# Remove these ghost factors
#summary(sponge_raw)
sponge_raw$Observer <- as.character(sponge_raw$Observer)
sponge_raw <- sponge_raw[sponge_raw$Observer == "E MacLean" | sponge_raw$Observer == "L Jarecki", ]
sponge_raw$Observer <- as.factor(sponge_raw$Observer)

sponge_raw$Transect <- as.character(sponge_raw$Transect)
sponge_raw <- sponge_raw[sponge_raw$Transect == "1" | sponge_raw$Transect == "2" | sponge_raw$Transect == "3" | 
                           sponge_raw$Transect == "4" | sponge_raw$Transect == "T", ]
sponge_raw$Transect <- as.factor(sponge_raw$Transect)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
sponge_raw <- transform(sponge_raw, Site_Year = paste(sponge_raw$Year, sponge_raw$Site, sep="_"))

# Create new column in all datasets called Taxa, so I know which dataset the information came from depending on how the dataframes are combined
# Here I've done this for the sponge dataset
sponge_raw$Taxa <- "Sponge"

# Create a new dataframe with the number (n) of transects for each Site_Year
sponge_sample_size <- tally(group_by(sponge_raw, sponge_raw$Site_Year))

# Rename column in sample_size_sponge dataframe to use as a key to merge this information with the raw data
colnames(sponge_sample_size)[colnames(sponge_sample_size) == "sponge_raw$Site_Year"] <- "Site_Year"
sponge_raw <- merge(x = sponge_raw, y = sponge_sample_size, by = "Site_Year", all = T)

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant (using imin function created above)
sponge_raw_three <- sponge_raw %>% group_by(sponge_raw$Site_Year) %>% sample_n(imin(x = 3, y = sponge_raw$n))
# The warnings here are just to let us know that there are 3 or more transects for that site_year
# Double-check: sponge_sample_size should have 164 obs. (different Site_Year's), so sponge_raw_three should have 492 obs. (164*3)

# Now we need to ungroup the data to avoid confusing errors later
sponge_raw_three <- ungroup(x = sponge_raw_three)

# Put dataset into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
sponge_raw_longform <- gather(data = sponge_raw_three, key = "Taxonomic_Group", value = "Count", Agelas.clathrodes..Agelas.citrina.or.Clathria.faviformis:Black..spiny..purple.exudate.but.not.slimy)

# Double-check dataset looks correct
#summary(sponge_raw_longform)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
sponge_raw_longform$Taxonomic_Group <- as.factor(x = sponge_raw_longform$Taxonomic_Group)

# Can also make Year a factor instead of an integer
sponge_raw_longform$Year <- as.factor(x = sponge_raw_longform$Year)

# Because the sponge dataset has information about transect length, we can check to see if all transects are 30m long
#not30 <-sponge_raw[sponge_raw$Transect.Length..m. != "30", 1:7]
# Most transects are within 50 cm of the average 30 m transect length
# However, there are 2 transects that are only 20 m: 1993_White and 1994_White
#sponge_raw[sponge_raw$Transect.Length..m. == "20", 1:7]
# ***For 1994_White, I could exclude this from the dataset as there are >3 transects for this site_year
# ***For 1993_White, I could multiply counts by 3/2, but this would only change counts, not richness, but we know richness tends to increase with an increase in effort
# ***For 1993_White, I could instead exclude it, but then the averages will only be of 2 transects leading to an effort disparity as mentioned above
# ***For 1993_White, OR I could just make a note of it if the sponge richness for Bigelow in 1993 is exceptionally low

# Summary Information
#str(sponge_raw_longform)
#summary(sponge_raw_longform)
# Check that there are the same number of observations for each site and for each year
# In this case, 1993, 2014, 2017, bigelow, crab, and pelican are the exceptions
#summary(sponge_raw_longform$Site)
#summary(sponge_raw_longform$Year)

# For Mean Counts of sponges for each site_year (this step is not absolutely necessary for this paper, but might be useful if you want to look at differences over time and site for a particular species)
# Aggregate groups by the site, year, taxgroup then averages the counts for each of these across all transects (in this case, 3 transects per site_year)
sponge_averages <- aggregate(x = sponge_raw_longform$Count, 
                             by = list(sponge_raw_longform$Site, sponge_raw_longform$Year, sponge_raw_longform$Taxonomic_Group), 
                             FUN = mean)
# Double-check environment to make sure output makes sense: (8sites*21years - 4site_yearsthatweren'tsurveyed)*58taxgroups = 9512 averages
# Rename columns to match the data in them
names(x = sponge_averages) <- c("Site", "Year", "Species_Code", "Mean_Count")

# For sponge richness
# Aggregate groups by the site and year then uses the function we created above to calculate richness
sponge_richness <- aggregate(x = sponge_averages$Mean_Count, 
                             by = list(sponge_averages$Site, sponge_averages$Year), 
                             FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*21years - 4site_yearsthatweren'tsurveyed = 164 averages
# Rename columns to match the data in them
names(x = sponge_richness) <- c("Site", "Year", "Sponge_Richness")

# After closer inspection, 1993_crab, 2014_pelican, 2017_pelican, and 2017_bigelow are missing





## Import fish datasets (see "Fish Metadata.docx" for more information)
fish_raw <- read.csv(file = "fish.csv", header = T)

# Sometimes site names were entered using different number of spaces. 
#summary(fish_raw$site)
# Correct these entry mistakes by making names consistent
fish_raw$site <- revalue(x = fish_raw$site, replace = c("pelican   " = "pelican"))

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
fish_raw$site <- as.character(x = fish_raw$site)
fish_raw <- fish_raw[fish_raw$site == "muskN" | fish_raw$site == "pelican" | fish_raw$site == "crab" | 
                       fish_raw$site == "bigelow" | fish_raw$site == "monkey" | fish_raw$site == "iguana" |
                       fish_raw$site == "white" | fish_raw$site == "grand", ]
fish_raw$site <- as.factor(x = fish_raw$site)

# Only retain observations for the main survey
fish_raw$survey <- as.character(x = fish_raw$survey)
fish_raw <- fish_raw[fish_raw$survey == "main", ]
fish_raw$survey <- as.factor(x = fish_raw$survey)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
fish_raw <- transform(fish_raw, Site_Year = paste(fish_raw$year, fish_raw$site, sep = "_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
# Here I've done this for the fish dataset
fish_raw$Taxa <- "Fish"

# Create a new dataframe with the number (n) of transects for each Site_Year
fish_sample_size <- tally(group_by(fish_raw, fish_raw$Site_Year))

# Rename column in sample_size_fish dataframe to use as a key to merge this information with the raw data
colnames(fish_sample_size)[colnames(fish_sample_size) == "fish_raw$Site_Year"] <- "Site_Year"
fish_raw <- merge(x = fish_raw, y = fish_sample_size, by = "Site_Year", all = T)

# Randomly select 3 transects for each Site_Year
# If there are <3 transects sampled, the number that were sampled will remain constant (using imin function created above)
fish_raw_three <- fish_raw %>% group_by(fish_raw$Site_Year) %>% sample_n(imin(x = 3, y = fish_raw$n))
# The warnings here are just to let us know that there are 3 or more transects for that site_year
# Double-check: fish_sample_size should have 216 obs. (different Site_Year's), so benthic_raw_three should have 648 obs.

# Now we need to ungroup the data to avoid confusing errors later
fish_raw_three <- ungroup(x = fish_raw_three)

# Put fish_raw into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
fish_raw_longform <- gather(data = fish_raw_three, key = "KEY", value = "Count", popaa:kysea)

# Because the species codes end with j or a to distinguish adult and juveniles,
# Split new KEY column into species_code and age_class columns
KEY <- as.character(x = fish_raw_longform$KEY)
fish_split <- data.frame("Species_Code" = substr(x = KEY, start = 1, stop = (nchar(KEY)-1)), "Age_Class" = substr(x = KEY, start = nchar(x = KEY), stop = nchar(x = KEY)))

# Combine long-form dataset with the split columns dataframe
fish_raw_longform <- cbind(fish_raw_longform, fish_split)

# Only retain observations of adult fish
# Newly recruited juvenile fishes (< 1 month on the reef) were excluded 
# because their abundance is strongly affected by lunar cycles, 
# which complicates the detection of long-term trends. 
fish_raw_longform$Age_Class <- as.character(x = fish_raw_longform$Age_Class)
fish_raw_longform <- fish_raw_longform[fish_raw_longform$Age_Class == "a", ]
fish_raw_longform$Age_Class <- as.factor(x = fish_raw_longform$Age_Class)

# Import fish codes to get common names
fish_codes <- read.csv(file = "fish_codes.csv", header = T)

# Rename notes column in fish_codes dataframe to avoid confusion with other notes column
colnames(fish_codes)[colnames(fish_codes) == "notes"] <- "Fish taxonomy notes"

# Rename column in fish_codes dataframe to use as a key to merge this information with the raw data
colnames(fish_codes)[colnames(fish_codes) == "new_code"] <- "KEY"
fish_raw_longform <- merge(x = fish_raw_longform, y = fish_codes, by = "KEY", all.y = F)

# Can also make Year a factor instead of an integer
fish_raw_longform$year <- as.factor(x = fish_raw_longform$year)

# Summary Information
#str(fish_raw_longform)
#summary(fish_raw_longform)
# Check that there are the same number of observations for each site and for each year
#summary(fish_raw_longform$site)
#summary(fish_raw_longform$year)

# For Mean Counts of fishes for each site_year (this step is not absolutely necessary for this paper, but might be useful if you want to look at differences over time and site for a particular species)
# Aggregate groups by the site, year, speciescode then averages the counts for each of these across all transects (in this case, 3 transects per site_year)
fish_averages <- aggregate(x = fish_raw_longform$Count, 
                           by = list(fish_raw_longform$site, fish_raw_longform$year, fish_raw_longform$Species_Code), 
                           FUN = mean)
# ***Double-check environment to make sure output makes sense: 8sites*27years*119taxgroups = 25,704 averages
# ***but this gives 24,624 averages, need to look into why this may be
# ***25704-24624=1080 which is 8*135 or 27*40, so probably something to do with site or year
# Rename columns to match the data in them
names(x = fish_averages) <- c("Site", "Year", "Species_Code", "Mean_Count")

# For fish richness
# Aggregate groups by the site and year then uses the function we created above to calculate richness
fish_richness <- aggregate(x = fish_averages$Mean_Count, 
                           by = list(fish_averages$Site, fish_averages$Year), 
                           FUN = richness)
# Double-check environment to make sure output makes sense: 8sites*27years = 216 richness values
# Rename columns to match the data in them
names(x = fish_richness) <- c("Site", "Year", "Fish_Richness")




## Import rugosity dataset
rugosity_raw <- read.csv(file = "Reef rugosity site x year means 2019.csv", header = T)
# Rename columns to match the format for other dataframes
names(x = rugosity_raw) <- c("Site", "Year", "Rugosity")


# This next section summarizes these variables for use in models into one table:
# Year
# Site
# coral_percentcover
# sponge_percentcover
# rugosity
# coral_richness
# sponge_richness
# fish_richness


# Create new column called "Site_Year" for each of these dataframes that combines the year and site as ####_Sitename
coral_percentcover <- transform(coral_percentcover, Site_Year = paste(coral_percentcover$Year, coral_percentcover$Site, sep = "_"))
sponge_percentcover <- transform(sponge_percentcover, Site_Year = paste(sponge_percentcover$Year, sponge_percentcover$Site, sep = "_"))
rugosity <- transform(rugosity_raw, Site_Year = paste(rugosity_raw$Year, rugosity_raw$Site, sep = "_"))
coral_richness <- transform(coral_richness, Site_Year = paste(coral_richness$Year, coral_richness$Site, sep = "_"))
sponge_richness <- transform(sponge_richness, Site_Year = paste(sponge_richness$Year, sponge_richness$Site, sep = "_"))
fish_richness <- transform(fish_richness, Site_Year = paste(fish_richness$Year, fish_richness$Site, sep = "_"))

# Merge the dataframes into one table
# Because Site_Year is a column shared by all dataframes, there will be warning messages about these columns being duplicated in the new dataframes
# This is resolved in the last of these steps when selecting only for those columns with variables we desire
variables <- merge(x = coral_percentcover, y = sponge_percentcover, by = "Site_Year", all = T)
variables <- merge(x = variables, y = rugosity, by = "Site_Year", all = T)
variables <- merge(x = variables, y = coral_richness, by = "Site_Year", all = T)
variables <- merge(x = variables, y = sponge_richness, by = "Site_Year", all = T)
variables <- merge(x = variables, y = fish_richness, by = "Site_Year", all = T)
variables <- variables[,c("Site_Year","Percent_Coral_Cover", "Percent_Sponge_Cover", "Rugosity", "Coral_Richness", "Sponge_Richness", "Fish_Richness")]

# Add a column called Combined_Richness that adds the richness of corals, sponges, and fishes
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
variables <- merge(x = variables, y = combined_richness, by = "Site_Year", all = T)



# Split Site_Year column into separate Year and Site columns
variables$Site_Year <- as.character(x = variables$Site_Year)
variables <- separate(data = variables, col = Site_Year, into = c("Year", "Site"), sep="_")
# Make Site type factor and Year type integer
variables$Site <- as.factor(x = variables$Site)
variables$Year <- as.integer(x = variables$Year)

# Rename Year column True_Year
colnames(variables)[colnames(variables) == "Year"] <- "True_Year"
# Create new column called Year with the first year (1992) set to 0
# Using time starting at zero helps interpreting the intercept of the model and gives numerical stability
variables$Year <- variables$True_Year - 1992

# Save this variables table as a .csv
#write.table(variables, file="variables.csv", sep=",", col.names=TRUE,row.names=FALSE)

# This concludes the data import and cleaning
# See Models.R script for modeling and figures