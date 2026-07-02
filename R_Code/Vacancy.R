setwd("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0001_csv")
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)
library(scales) #for percent formatting
nhgis0001_ts_nominal_county <- read_csv("nhgis0001_ts_nominal_county.csv")

#Drop all states except Florida
florida_county <- subset(nhgis0001_ts_nominal_county, STATE == "Florida")

#Drop all counties except Leon
leon_county <- subset(florida_county, COUNTY == "Leon County")

#######Leon County ########


#Rename to differentiate
leon_county <- leon_county %>%
  rename(Occupied_Housing2010 = A43AA2010,
         Occupied_Housing06_10 = A43AA105,
         Occupied_Housing10_14 = A43AA145,
         Occupied_Housing14_18 = A43AA185,
         Occupied_Housing18_22 = A43AA225,
         Occupied_Housing2020 = A43AA2020,
         Vacant_Housing2010 = A43AB2010,
         Vacant_Housing06_10 = A43AB105,
         Vacant_Housing10_14 = A43AB145,
         Vacant_Housing14_18 = A43AB185,
         Vacant_Housing18_22 = A43AB225,
         Vacant_Housing2020 = A43AB2020)

#Dropping margin of error rows to avoid mess
leon_county <- leon_county %>%
  select(-A43AA105M, -A43AA145M, -A43AA185M, -A43AA225M, -A43AB105M, -A43AB145M, -A43AB185M, -A43AB225M)


#Save as new Dataset
write.csv(leon_county, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/leon_county.csv", row.names = FALSE)


# Calculate the total housing for 2010
leon_county$Housing_Total_2010 <- leon_county$Occupied_Housing2010 + leon_county$Vacant_Housing2010

# Calculate the vacant housing ratio for 2010
leon_county$Vacant_Ratio_2010 <- leon_county$Vacant_Housing2010 / leon_county$Housing_Total_2010

# Calculate the total housing for 2020
leon_county$Housing_Total_2020 <- leon_county$Occupied_Housing2020 + leon_county$Vacant_Housing2020

# Calculate the vacant housing ratio for 2020
leon_county$Vacant_Ratio_2020 <- leon_county$Vacant_Housing2020 / leon_county$Housing_Total_2020

# Calculate total housing and vacant ratios for each five year period

# For 2006-2010
leon_county$Housing_Total_06_10 <- leon_county$Occupied_Housing06_10 + leon_county$Vacant_Housing06_10
leon_county$Vacant_Ratio_06_10 <- leon_county$Vacant_Housing06_10 / leon_county$Housing_Total_06_10

# For 2010-2014
leon_county$Housing_Total_10_14 <- leon_county$Occupied_Housing10_14 + leon_county$Vacant_Housing10_14
leon_county$Vacant_Ratio_10_14 <- leon_county$Vacant_Housing10_14 / leon_county$Housing_Total_10_14

# For 2014-2018
leon_county$Housing_Total_14_18 <- leon_county$Occupied_Housing14_18 + leon_county$Vacant_Housing14_18
leon_county$Vacant_Ratio_14_18 <- leon_county$Vacant_Housing14_18 / leon_county$Housing_Total_14_18

# For 2018-2022
leon_county$Housing_Total_18_22 <- leon_county$Occupied_Housing18_22 + leon_county$Vacant_Housing18_22
leon_county$Vacant_Ratio_18_22 <- leon_county$Vacant_Housing18_22 / leon_county$Housing_Total_18_22

# For 2010
leon_county$Housing_Total_2010 <- leon_county$Occupied_Housing2010 + leon_county$Vacant_Housing2010
leon_county$Vacant_Ratio_2010 <- leon_county$Vacant_Housing2010 / leon_county$Housing_Total_2010

# For 2020
leon_county$Housing_Total_2020 <- leon_county$Occupied_Housing2020 + leon_county$Vacant_Housing2020
leon_county$Vacant_Ratio_2020 <- leon_county$Vacant_Housing2020 / leon_county$Housing_Total_2020

# First, create a long format dataframe suitable for ggplot2
leon_county_long <- data.frame(
  Period = c("06_10", "10_14", "14_18", "18_22"),
  Vacant_Ratio = c(leon_county$Vacant_Ratio_06_10,
                   leon_county$Vacant_Ratio_10_14,
                   leon_county$Vacant_Ratio_14_18,
                   leon_county$Vacant_Ratio_18_22)
)

# Create the line graph using ggplot2
ggplot(leon_county_long, aes(x = Period, y = Vacant_Ratio, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Vacant Housing Ratios Over Time",
       x = "Time Period",
       y = "Vacant Housing Ratio") +
  theme_minimal()


###### Miami-Dade County #######
#Drop all states except Florida
florida_county <- subset(nhgis0001_ts_nominal_county, STATE == "Florida")

#Drop all counties except Miami-Dade
miami_dade_county <- subset(florida_county, COUNTY == "Miami-Dade County")

#Rename to differentiate
miami_dade_county <- miami_dade_county %>%
  rename(Occupied_Housing2010 = A43AA2010,
         Occupied_Housing06_10 = A43AA105,
         Occupied_Housing10_14 = A43AA145,
         Occupied_Housing14_18 = A43AA185,
         Occupied_Housing18_22 = A43AA225,
         Occupied_Housing2020 = A43AA2020,
         Vacant_Housing2010 = A43AB2010,
         Vacant_Housing06_10 = A43AB105,
         Vacant_Housing10_14 = A43AB145,
         Vacant_Housing14_18 = A43AB185,
         Vacant_Housing18_22 = A43AB225,
         Vacant_Housing2020 = A43AB2020)

#Dropping margin of error rows to avoid mess
miami_dade_county <- miami_dade_county %>%
  select(-A43AA105M, -A43AA145M, -A43AA185M, -A43AA225M, -A43AB105M, -A43AB145M, -A43AB185M, -A43AB225M)

# For 2006-2010
miami_dade_county$Housing_Total_06_10 <- miami_dade_county$Occupied_Housing06_10 + miami_dade_county$Vacant_Housing06_10
miami_dade_county$Vacant_Ratio_06_10 <- miami_dade_county$Vacant_Housing06_10 / miami_dade_county$Housing_Total_06_10

# For 2010-2014
miami_dade_county$Housing_Total_10_14 <- miami_dade_county$Occupied_Housing10_14 + miami_dade_county$Vacant_Housing10_14
miami_dade_county$Vacant_Ratio_10_14 <- miami_dade_county$Vacant_Housing10_14 / miami_dade_county$Housing_Total_10_14

# For 2014-2018
miami_dade_county$Housing_Total_14_18 <- miami_dade_county$Occupied_Housing14_18 + miami_dade_county$Vacant_Housing14_18
miami_dade_county$Vacant_Ratio_14_18 <- miami_dade_county$Vacant_Housing14_18 / miami_dade_county$Housing_Total_14_18

# For 2018-2022
miami_dade_county$Housing_Total_18_22 <- miami_dade_county$Occupied_Housing18_22 + miami_dade_county$Vacant_Housing18_22
miami_dade_county$Vacant_Ratio_18_22 <- miami_dade_county$Vacant_Housing18_22 / miami_dade_county$Housing_Total_18_22

# For 2010
miami_dade_county$Housing_Total_2010 <- miami_dade_county$Occupied_Housing2010 + miami_dade_county$Vacant_Housing2010
miami_dade_county$Vacant_Ratio_2010 <- miami_dade_county$Vacant_Housing2010 / miami_dade_county$Housing_Total_2010

# For 2020
miami_dade_county$Housing_Total_2020 <- miami_dade_county$Occupied_Housing2020 + miami_dade_county$Vacant_Housing2020
miami_dade_county$Vacant_Ratio_2020 <- miami_dade_county$Vacant_Housing2020 / miami_dade_county$Housing_Total_2020


# First, create a long format dataframe suitable for ggplot2
miami_dade_county_long <- data.frame(
  Period = c("06_10", "10_14", "14_18", "18_22"),
  Vacant_Ratio = c(miami_dade_county$Vacant_Ratio_06_10,
                   miami_dade_county$Vacant_Ratio_10_14,
                   miami_dade_county$Vacant_Ratio_14_18,
                   miami_dade_county$Vacant_Ratio_18_22)
)

# Create the line graph using ggplot2
ggplot(miami_dade_county_long, aes(x = Period, y = Vacant_Ratio, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Vacant Housing Ratios Over Time in Miami-Dade County",
       x = "Time Period",
       y = "Vacant Housing Ratio") +
  theme_minimal()

####### More Data ###########

#more years
leon_county2 <- read.csv("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0002_csv/nhgis0002_ts_nominal_county.csv")

#Drop all states except Florida
leon_county2 <- subset(leon_county2, STATE == "Florida")

#Drop all counties except Leon or Miami-Dade
miami_dade_county2 <- subset(leon_county2, COUNTY == "Miami-Dade County")

leon_county2 <- subset(leon_county2, COUNTY == "Leon County")

#Rename to differentiate
leon_county2 <- leon_county2 %>%
  rename(Occupied_Housing2010 = A43AA2010,
         Occupied_Housing06_10 = A43AA105,
         Occupied_Housing07_11 = A43AA115,
         Occupied_Housing08_12 = A43AA125,
         Occupied_Housing09_13 = A43AA135,
         Occupied_Housing10_14 = A43AA145,
         Occupied_Housing11_15 = A43AA155,
         Occupied_Housing12_16 = A43AA165,
         Occupied_Housing13_17 = A43AA175,
         Occupied_Housing14_18 = A43AA185,
         Occupied_Housing15_19 = A43AA195,
         Occupied_Housing16_20 = A43AA205,
         Occupied_Housing17_21 = A43AA215,
         Occupied_Housing18_22 = A43AA225,
         Occupied_Housing2020 = A43AA2020,
         Vacant_Housing2010 = A43AB2010,
         Vacant_Housing06_10 = A43AB105,
         Vacant_Housing07_11 = A43AB115,
         Vacant_Housing08_12 = A43AB125,
         Vacant_Housing09_13 = A43AB135,
         Vacant_Housing10_14 = A43AB145,
         Vacant_Housing11_15 = A43AB155,
         Vacant_Housing12_16 = A43AB165,
         Vacant_Housing13_17 = A43AB175,
         Vacant_Housing14_18 = A43AB185,
         Vacant_Housing15_19 = A43AB195,
         Vacant_Housing16_20 = A43AB205,
         Vacant_Housing17_21 = A43AB215,
         Vacant_Housing18_22 = A43AB225,
         Vacant_Housing2020 = A43AB2020,)

#Dropping margin of error rows to avoid mess
leon_county2 <- leon_county2 %>%
  select(-A43AA105M, -A43AA115M, -A43AA125M, -A43AA135M, -A43AA145M, -A43AA155M, -A43AA165M, -A43AA175M, -A43AA185M,
         -A43AA195M, -A43AA205M, -A43AA215M, -A43AA225M, -A43AB105M, -A43AB115M, -A43AB125M, -A43AB135M, -A43AB145M, 
         -A43AB155M, -A43AB165M, -A43AB175M, -A43AB185M, -A43AB195M, -A43AB205M, -A43AB215M, -A43AB225M)

#Save as new Dataset
write.csv(leon_county2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/leon_county2.csv", row.names = FALSE)

# Calculate total housing and vacant ratios for each time period

# For 2006-2010
leon_county2$Housing_Total_06_10 <- leon_county2$Occupied_Housing06_10 + leon_county2$Vacant_Housing06_10
leon_county2$Vacant_Ratio_06_10 <- leon_county2$Vacant_Housing06_10 / leon_county2$Housing_Total_06_10

# For 2007-2011
leon_county2$Housing_Total_07_11 <- leon_county2$Occupied_Housing07_11 + leon_county2$Vacant_Housing07_11
leon_county2$Vacant_Ratio_07_11 <- leon_county2$Vacant_Housing07_11 / leon_county2$Housing_Total_07_11

# For 2008-2012
leon_county2$Housing_Total_08_12 <- leon_county2$Occupied_Housing08_12 + leon_county2$Vacant_Housing08_12
leon_county2$Vacant_Ratio_08_12 <- leon_county2$Vacant_Housing08_12 / leon_county2$Housing_Total_08_12

# For 2009-2013
leon_county2$Housing_Total_09_13 <- leon_county2$Occupied_Housing09_13 + leon_county2$Vacant_Housing09_13
leon_county2$Vacant_Ratio_09_13 <- leon_county2$Vacant_Housing09_13 / leon_county2$Housing_Total_09_13

# For 2010-2014
leon_county2$Housing_Total_10_14 <- leon_county2$Occupied_Housing10_14 + leon_county2$Vacant_Housing10_14
leon_county2$Vacant_Ratio_10_14 <- leon_county2$Vacant_Housing10_14 / leon_county2$Housing_Total_10_14

# For 2011-2015
leon_county2$Housing_Total_11_15 <- leon_county2$Occupied_Housing11_15 + leon_county2$Vacant_Housing11_15
leon_county2$Vacant_Ratio_11_15 <- leon_county2$Vacant_Housing11_15 / leon_county2$Housing_Total_11_15

# For 2012-2016
leon_county2$Housing_Total_12_16 <- leon_county2$Occupied_Housing12_16 + leon_county2$Vacant_Housing12_16
leon_county2$Vacant_Ratio_12_16 <- leon_county2$Vacant_Housing12_16 / leon_county2$Housing_Total_12_16

# For 2013-2017
leon_county2$Housing_Total_13_17 <- leon_county2$Occupied_Housing13_17 + leon_county2$Vacant_Housing13_17
leon_county2$Vacant_Ratio_13_17 <- leon_county2$Vacant_Housing13_17 / leon_county2$Housing_Total_13_17

# For 2014-2018
leon_county2$Housing_Total_14_18 <- leon_county2$Occupied_Housing14_18 + leon_county2$Vacant_Housing14_18
leon_county2$Vacant_Ratio_14_18 <- leon_county2$Vacant_Housing14_18 / leon_county2$Housing_Total_14_18

# For 2015-2019
leon_county2$Housing_Total_15_19 <- leon_county2$Occupied_Housing15_19 + leon_county2$Vacant_Housing15_19
leon_county2$Vacant_Ratio_15_19 <- leon_county2$Vacant_Housing15_19 / leon_county2$Housing_Total_15_19

# For 2016-2020
leon_county2$Housing_Total_16_20 <- leon_county2$Occupied_Housing16_20 + leon_county2$Vacant_Housing16_20
leon_county2$Vacant_Ratio_16_20 <- leon_county2$Vacant_Housing16_20 / leon_county2$Housing_Total_16_20

# For 2017-2021
leon_county2$Housing_Total_17_21 <- leon_county2$Occupied_Housing17_21 + leon_county2$Vacant_Housing17_21
leon_county2$Vacant_Ratio_17_21 <- leon_county2$Vacant_Housing17_21 / leon_county2$Housing_Total_17_21

# For 2018-2022
leon_county2$Housing_Total_18_22 <- leon_county2$Occupied_Housing18_22 + leon_county2$Vacant_Housing18_22
leon_county2$Vacant_Ratio_18_22 <- leon_county2$Vacant_Housing18_22 / leon_county2$Housing_Total_18_22

# For 2010
leon_county2$Housing_Total_2010 <- leon_county2$Occupied_Housing2010 + leon_county2$Vacant_Housing2010
leon_county2$Vacant_Ratio_2010 <- leon_county2$Vacant_Housing2010 / leon_county2$Housing_Total_2010

# For 2020
leon_county2$Housing_Total_2020 <- leon_county2$Occupied_Housing2020 + leon_county2$Vacant_Housing2020
leon_county2$Vacant_Ratio_2020 <- leon_county2$Vacant_Housing2020 / leon_county2$Housing_Total_2020

# Create a data frame with time periods and vacant housing ratios for plotting
leon_county_plot_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Vacant_Ratio = c(leon_county2$Vacant_Ratio_06_10, 
                   leon_county2$Vacant_Ratio_07_11, 
                   leon_county2$Vacant_Ratio_08_12, 
                   leon_county2$Vacant_Ratio_09_13, 
                   leon_county2$Vacant_Ratio_10_14, 
                   leon_county2$Vacant_Ratio_11_15, 
                   leon_county2$Vacant_Ratio_12_16, 
                   leon_county2$Vacant_Ratio_13_17, 
                   leon_county2$Vacant_Ratio_14_18, 
                   leon_county2$Vacant_Ratio_15_19, 
                   leon_county2$Vacant_Ratio_16_20, 
                   leon_county2$Vacant_Ratio_17_21, 
                   leon_county2$Vacant_Ratio_18_22)
)

# Update the x-axis labels to "Years 06-10" and so on
x_labels <- c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
              "Years 18-22")

# Create the plot for Leon County
leon_plot <- ggplot(leon_county_plot_data, aes(x = Period, y = Vacant_Ratio, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Vacant Housing Ratios Over Time in Leon County",
       x = "Time Period",
       y = "Vacant Housing Ratio (%)") +
  scale_x_discrete(labels = x_labels) +  # Set the custom x-axis labels
  scale_y_continuous(labels = percent) +  # Format the y-axis as percentages
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
leon_plot

#Miami-Dade

miami_dade_county2 <- miami_dade_county2 %>%
  rename(Occupied_Housing2010 = A43AA2010,
         Occupied_Housing06_10 = A43AA105,
         Occupied_Housing07_11 = A43AA115,
         Occupied_Housing08_12 = A43AA125,
         Occupied_Housing09_13 = A43AA135,
         Occupied_Housing10_14 = A43AA145,
         Occupied_Housing11_15 = A43AA155,
         Occupied_Housing12_16 = A43AA165,
         Occupied_Housing13_17 = A43AA175,
         Occupied_Housing14_18 = A43AA185,
         Occupied_Housing15_19 = A43AA195,
         Occupied_Housing16_20 = A43AA205,
         Occupied_Housing17_21 = A43AA215,
         Occupied_Housing18_22 = A43AA225,
         Occupied_Housing2020 = A43AA2020,
         Vacant_Housing2010 = A43AB2010,
         Vacant_Housing06_10 = A43AB105,
         Vacant_Housing07_11 = A43AB115,
         Vacant_Housing08_12 = A43AB125,
         Vacant_Housing09_13 = A43AB135,
         Vacant_Housing10_14 = A43AB145,
         Vacant_Housing11_15 = A43AB155,
         Vacant_Housing12_16 = A43AB165,
         Vacant_Housing13_17 = A43AB175,
         Vacant_Housing14_18 = A43AB185,
         Vacant_Housing15_19 = A43AB195,
         Vacant_Housing16_20 = A43AB205,
         Vacant_Housing17_21 = A43AB215,
         Vacant_Housing18_22 = A43AB225,
         Vacant_Housing2020 = A43AB2020,)

#Dropping margin of error rows to avoid mess
miami_dade_county2 <- miami_dade_county2 %>%
  select(-A43AA105M, -A43AA115M, -A43AA125M, -A43AA135M, -A43AA145M, -A43AA155M, -A43AA165M, -A43AA175M, -A43AA185M,
         -A43AA195M, -A43AA205M, -A43AA215M, -A43AA225M, -A43AB105M, -A43AB115M, -A43AB125M, -A43AB135M, -A43AB145M, 
         -A43AB155M, -A43AB165M, -A43AB175M, -A43AB185M, -A43AB195M, -A43AB205M, -A43AB215M, -A43AB225M)

# Calculate total housing and vacant ratios for each time period

# For 2006-2010
miami_dade_county2$Housing_Total_06_10 <- miami_dade_county2$Occupied_Housing06_10 + miami_dade_county2$Vacant_Housing06_10
miami_dade_county2$Vacant_Ratio_06_10 <- miami_dade_county2$Vacant_Housing06_10 / miami_dade_county2$Housing_Total_06_10

# For 2007-2011
miami_dade_county2$Housing_Total_07_11 <- miami_dade_county2$Occupied_Housing07_11 + miami_dade_county2$Vacant_Housing07_11
miami_dade_county2$Vacant_Ratio_07_11 <- miami_dade_county2$Vacant_Housing07_11 / miami_dade_county2$Housing_Total_07_11

# For 2008-2012
miami_dade_county2$Housing_Total_08_12 <- miami_dade_county2$Occupied_Housing08_12 + miami_dade_county2$Vacant_Housing08_12
miami_dade_county2$Vacant_Ratio_08_12 <- miami_dade_county2$Vacant_Housing08_12 / miami_dade_county2$Housing_Total_08_12

# For 2009-2013
miami_dade_county2$Housing_Total_09_13 <- miami_dade_county2$Occupied_Housing09_13 + miami_dade_county2$Vacant_Housing09_13
miami_dade_county2$Vacant_Ratio_09_13 <- miami_dade_county2$Vacant_Housing09_13 / miami_dade_county2$Housing_Total_09_13

# For 2010-2014
miami_dade_county2$Housing_Total_10_14 <- miami_dade_county2$Occupied_Housing10_14 + miami_dade_county2$Vacant_Housing10_14
miami_dade_county2$Vacant_Ratio_10_14 <- miami_dade_county2$Vacant_Housing10_14 / miami_dade_county2$Housing_Total_10_14

# For 2011-2015
miami_dade_county2$Housing_Total_11_15 <- miami_dade_county2$Occupied_Housing11_15 + miami_dade_county2$Vacant_Housing11_15
miami_dade_county2$Vacant_Ratio_11_15 <- miami_dade_county2$Vacant_Housing11_15 / miami_dade_county2$Housing_Total_11_15

# For 2012-2016
miami_dade_county2$Housing_Total_12_16 <- miami_dade_county2$Occupied_Housing12_16 + miami_dade_county2$Vacant_Housing12_16
miami_dade_county2$Vacant_Ratio_12_16 <- miami_dade_county2$Vacant_Housing12_16 / miami_dade_county2$Housing_Total_12_16

# For 2013-2017
miami_dade_county2$Housing_Total_13_17 <- miami_dade_county2$Occupied_Housing13_17 + miami_dade_county2$Vacant_Housing13_17
miami_dade_county2$Vacant_Ratio_13_17 <- miami_dade_county2$Vacant_Housing13_17 / miami_dade_county2$Housing_Total_13_17

# For 2014-2018
miami_dade_county2$Housing_Total_14_18 <- miami_dade_county2$Occupied_Housing14_18 + miami_dade_county2$Vacant_Housing14_18
miami_dade_county2$Vacant_Ratio_14_18 <- miami_dade_county2$Vacant_Housing14_18 / miami_dade_county2$Housing_Total_14_18

# For 2015-2019
miami_dade_county2$Housing_Total_15_19 <- miami_dade_county2$Occupied_Housing15_19 + miami_dade_county2$Vacant_Housing15_19
miami_dade_county2$Vacant_Ratio_15_19 <- miami_dade_county2$Vacant_Housing15_19 / miami_dade_county2$Housing_Total_15_19

# For 2016-2020
miami_dade_county2$Housing_Total_16_20 <- miami_dade_county2$Occupied_Housing16_20 + miami_dade_county2$Vacant_Housing16_20
miami_dade_county2$Vacant_Ratio_16_20 <- miami_dade_county2$Vacant_Housing16_20 / miami_dade_county2$Housing_Total_16_20

# For 2017-2021
miami_dade_county2$Housing_Total_17_21 <- miami_dade_county2$Occupied_Housing17_21 + miami_dade_county2$Vacant_Housing17_21
miami_dade_county2$Vacant_Ratio_17_21 <- miami_dade_county2$Vacant_Housing17_21 / miami_dade_county2$Housing_Total_17_21

# For 2018-2022
miami_dade_county2$Housing_Total_18_22 <- miami_dade_county2$Occupied_Housing18_22 + miami_dade_county2$Vacant_Housing18_22
miami_dade_county2$Vacant_Ratio_18_22 <- miami_dade_county2$Vacant_Housing18_22 / miami_dade_county2$Housing_Total_18_22

# For 2010
miami_dade_county2$Housing_Total_2010 <- miami_dade_county2$Occupied_Housing2010 + miami_dade_county2$Vacant_Housing2010
miami_dade_county2$Vacant_Ratio_2010 <- miami_dade_county2$Vacant_Housing2010 / miami_dade_county2$Housing_Total_2010

# For 2020
miami_dade_county2$Housing_Total_2020 <- miami_dade_county2$Occupied_Housing2020 + miami_dade_county2$Vacant_Housing2020
miami_dade_county2$Vacant_Ratio_2020 <- miami_dade_county2$Vacant_Housing2020 / miami_dade_county2$Housing_Total_2020

# Create a data frame with time periods and vacant housing ratios for plotting
miami_dade_plot_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Vacant_Ratio = c(miami_dade_county2$Vacant_Ratio_06_10, 
                   miami_dade_county2$Vacant_Ratio_07_11, 
                   miami_dade_county2$Vacant_Ratio_08_12, 
                   miami_dade_county2$Vacant_Ratio_09_13, 
                   miami_dade_county2$Vacant_Ratio_10_14, 
                   miami_dade_county2$Vacant_Ratio_11_15, 
                   miami_dade_county2$Vacant_Ratio_12_16, 
                   miami_dade_county2$Vacant_Ratio_13_17, 
                   miami_dade_county2$Vacant_Ratio_14_18, 
                   miami_dade_county2$Vacant_Ratio_15_19, 
                   miami_dade_county2$Vacant_Ratio_16_20, 
                   miami_dade_county2$Vacant_Ratio_17_21, 
                   miami_dade_county2$Vacant_Ratio_18_22)
)

# Create the line graph using ggplot2
miami_plot <- ggplot(miami_dade_plot_data, aes(x = Period, y = Vacant_Ratio, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Vacant Housing Ratios Over Time in Miami-Dade County",
       x = "Time Period",
       y = "Vacant Housing Ratio (%)") +
  scale_x_discrete(labels = x_labels) +  # Set the custom x-axis labels
  scale_y_continuous(labels = percent) +  # Format the y-axis as percentages
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
miami_plot

# Display the two plots side by side
leon_plot + miami_plot


#Overlay both lines into one graph
combined_data <- data.frame(
  Period = rep(c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
                 "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"), 2),
  Vacant_Ratio = c(leon_county_plot_data$Vacant_Ratio, miami_dade_plot_data$Vacant_Ratio),
  County = rep(c("Leon County", "Miami-Dade County"), each = 13))  # Adding a county column


Vacancies = ggplot(combined_data, aes(x = Period, y = Vacant_Ratio, color = County, group = County)) +
  geom_line(size = 1.2) +    # Solid lines for both counties
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +    # Bigger points with border
  labs(title = "Vacant Housing Ratios in Leon County vs Miami-Dade County",
       subtitle = "Comparison across time periods",
       x = "Time Period",
       y = "Vacant Housing Ratio (%)",
       caption = "Source: IPUMS NHGIS") +
  scale_x_discrete(labels = x_labels) +  # Set the custom x-axis labels
  scale_y_continuous(labels = percent, limits = c(0.10, 0.175)) +  # Format y-axis as percentages, set limits
  scale_color_manual(values = c("Leon County" = "#782F40", "Miami-Dade County" = "#CEB888")) +  # FSU colors
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray"),  # Subtitle customization
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Move the legend to the right side
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.caption = element_text(size = 10, face = "italic", hjust = 1, color = "darkgray")  # Caption customization
  )

Vacancies
