setwd("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0003_csv")
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)
library(scales) #for percent formatting


#### First ####

#filtering for Leon County and Miami
LE <- read.csv("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0003_csv/nhgis0003_ts_nominal_county.csv")


#Drop all states except Florida
LE <- subset(LE, STATE == "Florida")

#Drop all counties except Leon or Miami-Dade
MIA <- subset(LE, COUNTY == "Miami-Dade County")

LE <- subset(LE, COUNTY == "Leon County")


###### Leon County ######
#Rename to differentiate
LE <- LE %>%
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
         Vacant_Housing2020 = A43AB2020,
         OwnerOccupied_2010 = B37AA2010,
         OwnerOccupied_06_10 = B37AA105,
         OwnerOccupied_07_11 = B37AA115,
         OwnerOccupied_08_12 = B37AA125,
         OwnerOccupied_09_13 = B37AA135,
         OwnerOccupied_10_14 = B37AA145,
         OwnerOccupied_11_15 = B37AA155,
         OwnerOccupied_12_16 = B37AA165,
         OwnerOccupied_13_17 = B37AA175,
         OwnerOccupied_14_18 = B37AA185,
         OwnerOccupied_15_19 = B37AA195,
         OwnerOccupied_16_20 = B37AA205,
         OwnerOccupied_17_21 = B37AA215,
         OwnerOccupied_18_22 = B37AA225,
         RenterOccupied_2010 = B37AB2010,
         RenterOccupied_06_10 = B37AB105,
         RenterOccupied_07_11 = B37AB115,
         RenterOccupied_08_12 = B37AB125,
         RenterOccupied_09_13 = B37AB135,
         RenterOccupied_10_14 = B37AB145,
         RenterOccupied_11_15 = B37AB155,
         RenterOccupied_12_16 = B37AB165,
         RenterOccupied_13_17 = B37AB175,
         RenterOccupied_14_18 = B37AB185,
         RenterOccupied_15_19 = B37AB195,
         RenterOccupied_16_20 = B37AB205,
         RenterOccupied_17_21 = B37AB215,
         RenterOccupied_18_22 = B37AB225)

LE <- LE %>%
  select(-A43AA105M, -A43AA115M, -A43AA125M, -A43AA135M, -A43AA145M, -A43AA155M, -A43AA165M, -A43AA175M, -A43AA185M,
         -A43AA195M, -A43AA205M, -A43AA215M, -A43AA225M, -A43AB105M, -A43AB115M, -A43AB125M, -A43AB135M, -A43AB145M, 
         -A43AB155M, -A43AB165M, -A43AB175M, -A43AB185M, -A43AB195M, -A43AB205M, -A43AB215M, -A43AB225M, -B37AA105M, -B37AA115M,
         -B37AA125M, -B37AA135M, -B37AA145M, -B37AA155M, -B37AA165M, -B37AA175M, -B37AA185M, -B37AA185M, -B37AA195M,
         -B37AA205M, -B37AA215M, -B37AA225M, -B37AB105M, -B37AB115M, -B37AB125M, -B37AB135M, -B37AB145M, -B37AB155M,
         -B37AB165M, -B37AB175M, -B37AB185M, -B37AB195M, -B37AB205M, -B37AB215M, -B37AB225M)


write.csv(LE, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/LE.csv", row.names = FALSE)

# Create new variables that add Occupied and Vacant Housing for each time period

# For 2006-2010
LE$Housing_Total_06_10 <- LE$Occupied_Housing06_10 + LE$Vacant_Housing06_10

# For 2007-2011
LE$Housing_Total_07_11 <- LE$Occupied_Housing07_11 + LE$Vacant_Housing07_11

# For 2008-2012
LE$Housing_Total_08_12 <- LE$Occupied_Housing08_12 + LE$Vacant_Housing08_12

# For 2009-2013
LE$Housing_Total_09_13 <- LE$Occupied_Housing09_13 + LE$Vacant_Housing09_13

# For 2010-2014
LE$Housing_Total_10_14 <- LE$Occupied_Housing10_14 + LE$Vacant_Housing10_14

# For 2011-2015
LE$Housing_Total_11_15 <- LE$Occupied_Housing11_15 + LE$Vacant_Housing11_15

# For 2012-2016
LE$Housing_Total_12_16 <- LE$Occupied_Housing12_16 + LE$Vacant_Housing12_16

# For 2013-2017
LE$Housing_Total_13_17 <- LE$Occupied_Housing13_17 + LE$Vacant_Housing13_17

# For 2014-2018
LE$Housing_Total_14_18 <- LE$Occupied_Housing14_18 + LE$Vacant_Housing14_18

# For 2015-2019
LE$Housing_Total_15_19 <- LE$Occupied_Housing15_19 + LE$Vacant_Housing15_19

# For 2016-2020
LE$Housing_Total_16_20 <- LE$Occupied_Housing16_20 + LE$Vacant_Housing16_20

# For 2017-2021
LE$Housing_Total_17_21 <- LE$Occupied_Housing17_21 + LE$Vacant_Housing17_21

# For 2018-2022
LE$Housing_Total_18_22 <- LE$Occupied_Housing18_22 + LE$Vacant_Housing18_22


# Create new variables that add OwnerOccupied and RenterOccupied Housing for each time period

# For 2006-2010
LE$Total_OwnerRenter_06_10 <- LE$OwnerOccupied_06_10 + LE$RenterOccupied_06_10

# For 2007-2011
LE$Total_OwnerRenter_07_11 <- LE$OwnerOccupied_07_11 + LE$RenterOccupied_07_11

# For 2008-2012
LE$Total_OwnerRenter_08_12 <- LE$OwnerOccupied_08_12 + LE$RenterOccupied_08_12

# For 2009-2013
LE$Total_OwnerRenter_09_13 <- LE$OwnerOccupied_09_13 + LE$RenterOccupied_09_13

# For 2010-2014
LE$Total_OwnerRenter_10_14 <- LE$OwnerOccupied_10_14 + LE$RenterOccupied_10_14

# For 2011-2015
LE$Total_OwnerRenter_11_15 <- LE$OwnerOccupied_11_15 + LE$RenterOccupied_11_15

# For 2012-2016
LE$Total_OwnerRenter_12_16 <- LE$OwnerOccupied_12_16 + LE$RenterOccupied_12_16

# For 2013-2017
LE$Total_OwnerRenter_13_17 <- LE$OwnerOccupied_13_17 + LE$RenterOccupied_13_17

# For 2014-2018
LE$Total_OwnerRenter_14_18 <- LE$OwnerOccupied_14_18 + LE$RenterOccupied_14_18

# For 2015-2019
LE$Total_OwnerRenter_15_19 <- LE$OwnerOccupied_15_19 + LE$RenterOccupied_15_19

# For 2016-2020
LE$Total_OwnerRenter_16_20 <- LE$OwnerOccupied_16_20 + LE$RenterOccupied_16_20

# For 2017-2021
LE$Total_OwnerRenter_17_21 <- LE$OwnerOccupied_17_21 + LE$RenterOccupied_17_21

# For 2018-2022
LE$Total_OwnerRenter_18_22 <- LE$OwnerOccupied_18_22 + LE$RenterOccupied_18_22


# Calculate Vacancy Ratio for each time period by dividing Vacant_Housing by Housing_Total

# For 2006-2010
LE$Vacancy_Ratio_06_10 <- LE$Vacant_Housing06_10 / LE$Housing_Total_06_10

# For 2007-2011
LE$Vacancy_Ratio_07_11 <- LE$Vacant_Housing07_11 / LE$Housing_Total_07_11

# For 2008-2012
LE$Vacancy_Ratio_08_12 <- LE$Vacant_Housing08_12 / LE$Housing_Total_08_12

# For 2009-2013
LE$Vacancy_Ratio_09_13 <- LE$Vacant_Housing09_13 / LE$Housing_Total_09_13

# For 2010-2014
LE$Vacancy_Ratio_10_14 <- LE$Vacant_Housing10_14 / LE$Housing_Total_10_14

# For 2011-2015
LE$Vacancy_Ratio_11_15 <- LE$Vacant_Housing11_15 / LE$Housing_Total_11_15

# For 2012-2016
LE$Vacancy_Ratio_12_16 <- LE$Vacant_Housing12_16 / LE$Housing_Total_12_16

# For 2013-2017
LE$Vacancy_Ratio_13_17 <- LE$Vacant_Housing13_17 / LE$Housing_Total_13_17

# For 2014-2018
LE$Vacancy_Ratio_14_18 <- LE$Vacant_Housing14_18 / LE$Housing_Total_14_18

# For 2015-2019
LE$Vacancy_Ratio_15_19 <- LE$Vacant_Housing15_19 / LE$Housing_Total_15_19

# For 2016-2020
LE$Vacancy_Ratio_16_20 <- LE$Vacant_Housing16_20 / LE$Housing_Total_16_20

# For 2017-2021
LE$Vacancy_Ratio_17_21 <- LE$Vacant_Housing17_21 / LE$Housing_Total_17_21

# For 2018-2022
LE$Vacancy_Ratio_18_22 <- LE$Vacant_Housing18_22 / LE$Housing_Total_18_22


# Calculate Owner and Renter Occupancy Ratios for each time period

# For 2006-2010
LE$Owner_Occupancy_Ratio_06_10 <- LE$OwnerOccupied_06_10 / LE$Occupied_Housing06_10
LE$Renter_Occupancy_Ratio_06_10 <- LE$RenterOccupied_06_10 / LE$Occupied_Housing06_10

# For 2007-2011
LE$Owner_Occupancy_Ratio_07_11 <- LE$OwnerOccupied_07_11 / LE$Occupied_Housing07_11
LE$Renter_Occupancy_Ratio_07_11 <- LE$RenterOccupied_07_11 / LE$Occupied_Housing07_11

# For 2008-2012
LE$Owner_Occupancy_Ratio_08_12 <- LE$OwnerOccupied_08_12 / LE$Occupied_Housing08_12
LE$Renter_Occupancy_Ratio_08_12 <- LE$RenterOccupied_08_12 / LE$Occupied_Housing08_12

# For 2009-2013
LE$Owner_Occupancy_Ratio_09_13 <- LE$OwnerOccupied_09_13 / LE$Occupied_Housing09_13
LE$Renter_Occupancy_Ratio_09_13 <- LE$RenterOccupied_09_13 / LE$Occupied_Housing09_13

# For 2010-2014
LE$Owner_Occupancy_Ratio_10_14 <- LE$OwnerOccupied_10_14 / LE$Occupied_Housing10_14
LE$Renter_Occupancy_Ratio_10_14 <- LE$RenterOccupied_10_14 / LE$Occupied_Housing10_14

# For 2011-2015
LE$Owner_Occupancy_Ratio_11_15 <- LE$OwnerOccupied_11_15 / LE$Occupied_Housing11_15
LE$Renter_Occupancy_Ratio_11_15 <- LE$RenterOccupied_11_15 / LE$Occupied_Housing11_15

# For 2012-2016
LE$Owner_Occupancy_Ratio_12_16 <- LE$OwnerOccupied_12_16 / LE$Occupied_Housing12_16
LE$Renter_Occupancy_Ratio_12_16 <- LE$RenterOccupied_12_16 / LE$Occupied_Housing12_16

# For 2013-2017
LE$Owner_Occupancy_Ratio_13_17 <- LE$OwnerOccupied_13_17 / LE$Occupied_Housing13_17
LE$Renter_Occupancy_Ratio_13_17 <- LE$RenterOccupied_13_17 / LE$Occupied_Housing13_17

# For 2014-2018
LE$Owner_Occupancy_Ratio_14_18 <- LE$OwnerOccupied_14_18 / LE$Occupied_Housing14_18
LE$Renter_Occupancy_Ratio_14_18 <- LE$RenterOccupied_14_18 / LE$Occupied_Housing14_18

# For 2015-2019
LE$Owner_Occupancy_Ratio_15_19 <- LE$OwnerOccupied_15_19 / LE$Occupied_Housing15_19
LE$Renter_Occupancy_Ratio_15_19 <- LE$RenterOccupied_15_19 / LE$Occupied_Housing15_19

# For 2016-2020
LE$Owner_Occupancy_Ratio_16_20 <- LE$OwnerOccupied_16_20 / LE$Occupied_Housing16_20
LE$Renter_Occupancy_Ratio_16_20 <- LE$RenterOccupied_16_20 / LE$Occupied_Housing16_20

# For 2017-2021
LE$Owner_Occupancy_Ratio_17_21 <- LE$OwnerOccupied_17_21 / LE$Occupied_Housing17_21
LE$Renter_Occupancy_Ratio_17_21 <- LE$RenterOccupied_17_21 / LE$Occupied_Housing17_21

# For 2018-2022
LE$Owner_Occupancy_Ratio_18_22 <- LE$OwnerOccupied_18_22 / LE$Occupied_Housing18_22
LE$Renter_Occupancy_Ratio_18_22 <- LE$RenterOccupied_18_22 / LE$Occupied_Housing18_22


# Create a data frame with time periods and vacancy ratios
vacancy_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Vacancy_Ratio = c(LE$Vacancy_Ratio_06_10, LE$Vacancy_Ratio_07_11, LE$Vacancy_Ratio_08_12, 
                    LE$Vacancy_Ratio_09_13, LE$Vacancy_Ratio_10_14, LE$Vacancy_Ratio_11_15, 
                    LE$Vacancy_Ratio_12_16, LE$Vacancy_Ratio_13_17, LE$Vacancy_Ratio_14_18, 
                    LE$Vacancy_Ratio_15_19, LE$Vacancy_Ratio_16_20, LE$Vacancy_Ratio_17_21, 
                    LE$Vacancy_Ratio_18_22)
)

# Create the vacancy ratio plot with FSU colors and percent y-axis
Leon_Vacancy = ggplot(vacancy_data, aes(x = Period, y = Vacancy_Ratio, group = 1)) + 
  geom_line(color = "#782F40", size = 1.2) +  # Garnet color for line
  geom_point(color = "#782F40", size = 4) +   # Garnet points
  labs(title = "Vacancy Ratio Over Time Leon County",
       x = "Time Period",
       y = "Vacancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = percent) +  # Format y-axis as percentage
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12)  # Larger y-axis labels
  )

# Create a data frame with time periods and both occupancy ratios
occupancy_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Owner_Occupancy_Ratio = c(LE$Owner_Occupancy_Ratio_06_10, LE$Owner_Occupancy_Ratio_07_11, 
                            LE$Owner_Occupancy_Ratio_08_12, LE$Owner_Occupancy_Ratio_09_13, 
                            LE$Owner_Occupancy_Ratio_10_14, LE$Owner_Occupancy_Ratio_11_15, 
                            LE$Owner_Occupancy_Ratio_12_16, LE$Owner_Occupancy_Ratio_13_17, 
                            LE$Owner_Occupancy_Ratio_14_18, LE$Owner_Occupancy_Ratio_15_19, 
                            LE$Owner_Occupancy_Ratio_16_20, LE$Owner_Occupancy_Ratio_17_21, 
                            LE$Owner_Occupancy_Ratio_18_22),
  Renter_Occupancy_Ratio = c(LE$Renter_Occupancy_Ratio_06_10, LE$Renter_Occupancy_Ratio_07_11, 
                             LE$Renter_Occupancy_Ratio_08_12, LE$Renter_Occupancy_Ratio_09_13, 
                             LE$Renter_Occupancy_Ratio_10_14, LE$Renter_Occupancy_Ratio_11_15, 
                             LE$Renter_Occupancy_Ratio_12_16, LE$Renter_Occupancy_Ratio_13_17, 
                             LE$Renter_Occupancy_Ratio_14_18, LE$Renter_Occupancy_Ratio_15_19, 
                             LE$Renter_Occupancy_Ratio_16_20, LE$Renter_Occupancy_Ratio_17_21, 
                             LE$Renter_Occupancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
library(reshape2)
occupancy_data_long <- melt(occupancy_data, id.vars = "Period", 
                            variable.name = "Occupancy_Type", 
                            value.name = "Ratio")

# Create the plot for owner and renter occupancy ratios with FSU colors and percent y-axis
Leon_Renter_v_Owner = ggplot(occupancy_data_long, aes(x = Period, y = Ratio, color = Occupancy_Type, group = Occupancy_Type)) +
  geom_line(size = 1.2) +    # Solid lines for both occupancy types
  geom_point(size = 4) +
  labs(title = "Owner vs Renter Occupancy Ratio Over Time Leon County",
       x = "Time Period",
       y = "Occupancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("Owner_Occupancy_Ratio" = "#782F40",  # Garnet for owner
                                "Renter_Occupancy_Ratio" = "#CEB888"), # Gold for renter
                     labels = c("Owner Occupancy Ratio", "Renter Occupancy Ratio")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )



##### Miami-Dade County ######

#Rename to differentiate
MIA <- MIA %>%
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
         Vacant_Housing2020 = A43AB2020,
         OwnerOccupied_2010 = B37AA2010,
         OwnerOccupied_06_10 = B37AA105,
         OwnerOccupied_07_11 = B37AA115,
         OwnerOccupied_08_12 = B37AA125,
         OwnerOccupied_09_13 = B37AA135,
         OwnerOccupied_10_14 = B37AA145,
         OwnerOccupied_11_15 = B37AA155,
         OwnerOccupied_12_16 = B37AA165,
         OwnerOccupied_13_17 = B37AA175,
         OwnerOccupied_14_18 = B37AA185,
         OwnerOccupied_15_19 = B37AA195,
         OwnerOccupied_16_20 = B37AA205,
         OwnerOccupied_17_21 = B37AA215,
         OwnerOccupied_18_22 = B37AA225,
         RenterOccupied_2010 = B37AB2010,
         RenterOccupied_06_10 = B37AB105,
         RenterOccupied_07_11 = B37AB115,
         RenterOccupied_08_12 = B37AB125,
         RenterOccupied_09_13 = B37AB135,
         RenterOccupied_10_14 = B37AB145,
         RenterOccupied_11_15 = B37AB155,
         RenterOccupied_12_16 = B37AB165,
         RenterOccupied_13_17 = B37AB175,
         RenterOccupied_14_18 = B37AB185,
         RenterOccupied_15_19 = B37AB195,
         RenterOccupied_16_20 = B37AB205,
         RenterOccupied_17_21 = B37AB215,
         RenterOccupied_18_22 = B37AB225)

MIA <- MIA %>%
  select(-A43AA105M, -A43AA115M, -A43AA125M, -A43AA135M, -A43AA145M, -A43AA155M, -A43AA165M, -A43AA175M, -A43AA185M,
         -A43AA195M, -A43AA205M, -A43AA215M, -A43AA225M, -A43AB105M, -A43AB115M, -A43AB125M, -A43AB135M, -A43AB145M, 
         -A43AB155M, -A43AB165M, -A43AB175M, -A43AB185M, -A43AB195M, -A43AB205M, -A43AB215M, -A43AB225M, -B37AA105M, -B37AA115M,
         -B37AA125M, -B37AA135M, -B37AA145M, -B37AA155M, -B37AA165M, -B37AA175M, -B37AA185M, -B37AA185M, -B37AA195M,
         -B37AA205M, -B37AA215M, -B37AA225M, -B37AB105M, -B37AB115M, -B37AB125M, -B37AB135M, -B37AB145M, -B37AB155M,
         -B37AB165M, -B37AB175M, -B37AB185M, -B37AB195M, -B37AB205M, -B37AB215M, -B37AB225M)


# Create new variables that add Occupied and Vacant Housing for each time period

# For 2006-2010
MIA$Housing_Total_06_10 <- MIA$Occupied_Housing06_10 + MIA$Vacant_Housing06_10

# For 2007-2011
MIA$Housing_Total_07_11 <- MIA$Occupied_Housing07_11 + MIA$Vacant_Housing07_11

# For 2008-2012
MIA$Housing_Total_08_12 <- MIA$Occupied_Housing08_12 + MIA$Vacant_Housing08_12

# For 2009-2013
MIA$Housing_Total_09_13 <- MIA$Occupied_Housing09_13 + MIA$Vacant_Housing09_13

# For 2010-2014
MIA$Housing_Total_10_14 <- MIA$Occupied_Housing10_14 + MIA$Vacant_Housing10_14

# For 2011-2015
MIA$Housing_Total_11_15 <- MIA$Occupied_Housing11_15 + MIA$Vacant_Housing11_15

# For 2012-2016
MIA$Housing_Total_12_16 <- MIA$Occupied_Housing12_16 + MIA$Vacant_Housing12_16

# For 2013-2017
MIA$Housing_Total_13_17 <- MIA$Occupied_Housing13_17 + MIA$Vacant_Housing13_17

# For 2014-2018
MIA$Housing_Total_14_18 <- MIA$Occupied_Housing14_18 + MIA$Vacant_Housing14_18

# For 2015-2019
MIA$Housing_Total_15_19 <- MIA$Occupied_Housing15_19 + MIA$Vacant_Housing15_19

# For 2016-2020
MIA$Housing_Total_16_20 <- MIA$Occupied_Housing16_20 + MIA$Vacant_Housing16_20

# For 2017-2021
MIA$Housing_Total_17_21 <- MIA$Occupied_Housing17_21 + MIA$Vacant_Housing17_21

# For 2018-2022
MIA$Housing_Total_18_22 <- MIA$Occupied_Housing18_22 + MIA$Vacant_Housing18_22


# Create new variables that add OwnerOccupied and RenterOccupied Housing for each time period

# For 2006-2010
MIA$Total_OwnerRenter_06_10 <- MIA$OwnerOccupied_06_10 + MIA$RenterOccupied_06_10

# For 2007-2011
MIA$Total_OwnerRenter_07_11 <- MIA$OwnerOccupied_07_11 + MIA$RenterOccupied_07_11

# For 2008-2012
MIA$Total_OwnerRenter_08_12 <- MIA$OwnerOccupied_08_12 + MIA$RenterOccupied_08_12

# For 2009-2013
MIA$Total_OwnerRenter_09_13 <- MIA$OwnerOccupied_09_13 + MIA$RenterOccupied_09_13

# For 2010-2014
MIA$Total_OwnerRenter_10_14 <- MIA$OwnerOccupied_10_14 + MIA$RenterOccupied_10_14

# For 2011-2015
MIA$Total_OwnerRenter_11_15 <- MIA$OwnerOccupied_11_15 + MIA$RenterOccupied_11_15

# For 2012-2016
MIA$Total_OwnerRenter_12_16 <- MIA$OwnerOccupied_12_16 + MIA$RenterOccupied_12_16

# For 2013-2017
MIA$Total_OwnerRenter_13_17 <- MIA$OwnerOccupied_13_17 + MIA$RenterOccupied_13_17

# For 2014-2018
MIA$Total_OwnerRenter_14_18 <- MIA$OwnerOccupied_14_18 + MIA$RenterOccupied_14_18

# For 2015-2019
MIA$Total_OwnerRenter_15_19 <- MIA$OwnerOccupied_15_19 + MIA$RenterOccupied_15_19

# For 2016-2020
MIA$Total_OwnerRenter_16_20 <- MIA$OwnerOccupied_16_20 + MIA$RenterOccupied_16_20

# For 2017-2021
MIA$Total_OwnerRenter_17_21 <- MIA$OwnerOccupied_17_21 + MIA$RenterOccupied_17_21

# For 2018-2022
MIA$Total_OwnerRenter_18_22 <- MIA$OwnerOccupied_18_22 + MIA$RenterOccupied_18_22


# Calculate Vacancy Ratio for each time period by dividing Vacant_Housing by Housing_Total

# For 2006-2010
MIA$Vacancy_Ratio_06_10 <- MIA$Vacant_Housing06_10 / MIA$Housing_Total_06_10

# For 2007-2011
MIA$Vacancy_Ratio_07_11 <- MIA$Vacant_Housing07_11 / MIA$Housing_Total_07_11

# For 2008-2012
MIA$Vacancy_Ratio_08_12 <- MIA$Vacant_Housing08_12 / MIA$Housing_Total_08_12

# For 2009-2013
MIA$Vacancy_Ratio_09_13 <- MIA$Vacant_Housing09_13 / MIA$Housing_Total_09_13

# For 2010-2014
MIA$Vacancy_Ratio_10_14 <- MIA$Vacant_Housing10_14 / MIA$Housing_Total_10_14

# For 2011-2015
MIA$Vacancy_Ratio_11_15 <- MIA$Vacant_Housing11_15 / MIA$Housing_Total_11_15

# For 2012-2016
MIA$Vacancy_Ratio_12_16 <- MIA$Vacant_Housing12_16 / MIA$Housing_Total_12_16

# For 2013-2017
MIA$Vacancy_Ratio_13_17 <- MIA$Vacant_Housing13_17 / MIA$Housing_Total_13_17

# For 2014-2018
MIA$Vacancy_Ratio_14_18 <- MIA$Vacant_Housing14_18 / MIA$Housing_Total_14_18

# For 2015-2019
MIA$Vacancy_Ratio_15_19 <- MIA$Vacant_Housing15_19 / MIA$Housing_Total_15_19

# For 2016-2020
MIA$Vacancy_Ratio_16_20 <- MIA$Vacant_Housing16_20 / MIA$Housing_Total_16_20

# For 2017-2021
MIA$Vacancy_Ratio_17_21 <- MIA$Vacant_Housing17_21 / MIA$Housing_Total_17_21

# For 2018-2022
MIA$Vacancy_Ratio_18_22 <- MIA$Vacant_Housing18_22 / MIA$Housing_Total_18_22


# Calculate Owner and Renter Occupancy Ratios for each time period

# For 2006-2010
MIA$Owner_Occupancy_Ratio_06_10 <- MIA$OwnerOccupied_06_10 / MIA$Occupied_Housing06_10
MIA$Renter_Occupancy_Ratio_06_10 <- MIA$RenterOccupied_06_10 / MIA$Occupied_Housing06_10

# For 2007-2011
MIA$Owner_Occupancy_Ratio_07_11 <- MIA$OwnerOccupied_07_11 / MIA$Occupied_Housing07_11
MIA$Renter_Occupancy_Ratio_07_11 <- MIA$RenterOccupied_07_11 / MIA$Occupied_Housing07_11

# For 2008-2012
MIA$Owner_Occupancy_Ratio_08_12 <- MIA$OwnerOccupied_08_12 / MIA$Occupied_Housing08_12
MIA$Renter_Occupancy_Ratio_08_12 <- MIA$RenterOccupied_08_12 / MIA$Occupied_Housing08_12

# For 2009-2013
MIA$Owner_Occupancy_Ratio_09_13 <- MIA$OwnerOccupied_09_13 / MIA$Occupied_Housing09_13
MIA$Renter_Occupancy_Ratio_09_13 <- MIA$RenterOccupied_09_13 / MIA$Occupied_Housing09_13


# For 2010-2014
MIA$Owner_Occupancy_Ratio_10_14 <- MIA$OwnerOccupied_10_14 / MIA$Occupied_Housing10_14
MIA$Renter_Occupancy_Ratio_10_14 <- MIA$RenterOccupied_10_14 / MIA$Occupied_Housing10_14

# For 2011-2015
MIA$Owner_Occupancy_Ratio_11_15 <- MIA$OwnerOccupied_11_15 / MIA$Occupied_Housing11_15
MIA$Renter_Occupancy_Ratio_11_15 <- MIA$RenterOccupied_11_15 / MIA$Occupied_Housing11_15

# For 2012-2016
MIA$Owner_Occupancy_Ratio_12_16 <- MIA$OwnerOccupied_12_16 / MIA$Occupied_Housing12_16
MIA$Renter_Occupancy_Ratio_12_16 <- MIA$RenterOccupied_12_16 / MIA$Occupied_Housing12_16

# For 2013-2017
MIA$Owner_Occupancy_Ratio_13_17 <- MIA$OwnerOccupied_13_17 / MIA$Occupied_Housing13_17
MIA$Renter_Occupancy_Ratio_13_17 <- MIA$RenterOccupied_13_17 / MIA$Occupied_Housing13_17

# For 2014-2018
MIA$Owner_Occupancy_Ratio_14_18 <- MIA$OwnerOccupied_14_18 / MIA$Occupied_Housing14_18
MIA$Renter_Occupancy_Ratio_14_18 <- MIA$RenterOccupied_14_18 / MIA$Occupied_Housing14_18

# For 2015-2019
MIA$Owner_Occupancy_Ratio_15_19 <- MIA$OwnerOccupied_15_19 / MIA$Occupied_Housing15_19
MIA$Renter_Occupancy_Ratio_15_19 <- MIA$RenterOccupied_15_19 / MIA$Occupied_Housing15_19

# For 2016-2020
MIA$Owner_Occupancy_Ratio_16_20 <- MIA$OwnerOccupied_16_20 / MIA$Occupied_Housing16_20
MIA$Renter_Occupancy_Ratio_16_20 <- MIA$RenterOccupied_16_20 / MIA$Occupied_Housing16_20

# For 2017-2021
MIA$Owner_Occupancy_Ratio_17_21 <- MIA$OwnerOccupied_17_21 / MIA$Occupied_Housing17_21
MIA$Renter_Occupancy_Ratio_17_21 <- MIA$RenterOccupied_17_21 / MIA$Occupied_Housing17_21

# For 2018-2022
MIA$Owner_Occupancy_Ratio_18_22 <- MIA$OwnerOccupied_18_22 / MIA$Occupied_Housing18_22
MIA$Renter_Occupancy_Ratio_18_22 <- MIA$RenterOccupied_18_22 / MIA$Occupied_Housing18_22


# Create a data frame with time periods and vacancy ratios
vacancy_data2 <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Vacancy_Ratio = c(MIA$Vacancy_Ratio_06_10, MIA$Vacancy_Ratio_07_11, MIA$Vacancy_Ratio_08_12, 
                    MIA$Vacancy_Ratio_09_13, MIA$Vacancy_Ratio_10_14, MIA$Vacancy_Ratio_11_15, 
                    MIA$Vacancy_Ratio_12_16, MIA$Vacancy_Ratio_13_17, MIA$Vacancy_Ratio_14_18, 
                    MIA$Vacancy_Ratio_15_19, MIA$Vacancy_Ratio_16_20, MIA$Vacancy_Ratio_17_21, 
                    MIA$Vacancy_Ratio_18_22)
)

# Create the vacancy ratio plot with FSU colors and percent y-axis
Miami_Vacancy = ggplot(vacancy_data2, aes(x = Period, y = Vacancy_Ratio, group = 1)) + 
  geom_line(color = "#782F40", size = 1.2) +  # Garnet color for line
  geom_point(color = "#782F40", size = 4) +   # Garnet points
  labs(title = "Vacancy Ratio Over Time in Miami-Dade County",
       x = "Time Period",
       y = "Vacancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12)  # Larger y-axis labels
  )


# Create a data frame with time periods and both occupancy ratios
occupancy_data2 <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Owner_Occupancy_Ratio = c(MIA$Owner_Occupancy_Ratio_06_10, MIA$Owner_Occupancy_Ratio_07_11, 
                            MIA$Owner_Occupancy_Ratio_08_12, MIA$Owner_Occupancy_Ratio_09_13, 
                            MIA$Owner_Occupancy_Ratio_10_14, MIA$Owner_Occupancy_Ratio_11_15, 
                            MIA$Owner_Occupancy_Ratio_12_16, MIA$Owner_Occupancy_Ratio_13_17, 
                            MIA$Owner_Occupancy_Ratio_14_18, MIA$Owner_Occupancy_Ratio_15_19, 
                            MIA$Owner_Occupancy_Ratio_16_20, MIA$Owner_Occupancy_Ratio_17_21, 
                            MIA$Owner_Occupancy_Ratio_18_22),
  Renter_Occupancy_Ratio = c(MIA$Renter_Occupancy_Ratio_06_10, MIA$Renter_Occupancy_Ratio_07_11, 
                             MIA$Renter_Occupancy_Ratio_08_12, MIA$Renter_Occupancy_Ratio_09_13, 
                             MIA$Renter_Occupancy_Ratio_10_14, MIA$Renter_Occupancy_Ratio_11_15, 
                             MIA$Renter_Occupancy_Ratio_12_16, MIA$Renter_Occupancy_Ratio_13_17, 
                             MIA$Renter_Occupancy_Ratio_14_18, MIA$Renter_Occupancy_Ratio_15_19, 
                             MIA$Renter_Occupancy_Ratio_16_20, MIA$Renter_Occupancy_Ratio_17_21, 
                             MIA$Renter_Occupancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
library(reshape2)
occupancy_data_long2 <- melt(occupancy_data2, id.vars = "Period", 
                            variable.name = "Occupancy_Type", 
                            value.name = "Ratio")

# Create the plot for owner and renter occupancy ratios with FSU colors and percent y-axis
Miami_Renter_v_Owner = ggplot(occupancy_data_long2, aes(x = Period, y = Ratio, color = Occupancy_Type, group = Occupancy_Type)) +
  geom_line(size = 1.2) +    # Solid lines for both occupancy types
  geom_point(size = 4) +
  labs(title = "Owner vs Renter Occupancy Ratio Over Time in Miami-Dade County",
       x = "Time Period",
       y = "Occupancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("Owner_Occupancy_Ratio" = "#782F40",  # Garnet for owner
                                "Renter_Occupancy_Ratio" = "#CEB888"), # Gold for renter
                     labels = c("Owner Occupancy Ratio", "Renter Occupancy Ratio")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )


##### Combined Graphs ######

# VACANCY RATIOS 
# Create a combined data frame with time periods and vacancy ratios for both counties
combined_vacancy_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Leon_Vacancy_Ratio = c(LE$Vacancy_Ratio_06_10, LE$Vacancy_Ratio_07_11, LE$Vacancy_Ratio_08_12, 
                         LE$Vacancy_Ratio_09_13, LE$Vacancy_Ratio_10_14, LE$Vacancy_Ratio_11_15, 
                         LE$Vacancy_Ratio_12_16, LE$Vacancy_Ratio_13_17, LE$Vacancy_Ratio_14_18, 
                         LE$Vacancy_Ratio_15_19, LE$Vacancy_Ratio_16_20, LE$Vacancy_Ratio_17_21, 
                         LE$Vacancy_Ratio_18_22),
  Miami_Vacancy_Ratio = c(MIA$Vacancy_Ratio_06_10, MIA$Vacancy_Ratio_07_11, MIA$Vacancy_Ratio_08_12, 
                          MIA$Vacancy_Ratio_09_13, MIA$Vacancy_Ratio_10_14, MIA$Vacancy_Ratio_11_15, 
                          MIA$Vacancy_Ratio_12_16, MIA$Vacancy_Ratio_13_17, MIA$Vacancy_Ratio_14_18, 
                          MIA$Vacancy_Ratio_15_19, MIA$Vacancy_Ratio_16_20, MIA$Vacancy_Ratio_17_21, 
                          MIA$Vacancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
combined_vacancy_long <- melt(combined_vacancy_data, id.vars = "Period", 
                              variable.name = "County", 
                              value.name = "Vacancy_Ratio")

# Create the plot with both counties' vacancy ratios
Both_Vanancy_Rates = ggplot(combined_vacancy_long, aes(x = Period, y = Vacancy_Ratio, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(title = "Vacancy Ratios Over Time: Leon County vs Miami-Dade County",
       x = "Time Period",
       y = "Vacancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("Leon_Vacancy_Ratio" = "#782F40",  # Garnet for Leon County
                                "Miami_Vacancy_Ratio" = "#CEB888"), # Gold for Miami-Dade County
                     labels = c("Leon County", "Miami-Dade County")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )


# RENTER OCCUPANCY
# Create a combined data frame with time periods and renter occupancy ratios for both counties
combined_renter_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Leon_Renter_Occupancy_Ratio = c(LE$Renter_Occupancy_Ratio_06_10, LE$Renter_Occupancy_Ratio_07_11, 
                                  LE$Renter_Occupancy_Ratio_08_12, LE$Renter_Occupancy_Ratio_09_13, 
                                  LE$Renter_Occupancy_Ratio_10_14, LE$Renter_Occupancy_Ratio_11_15, 
                                  LE$Renter_Occupancy_Ratio_12_16, LE$Renter_Occupancy_Ratio_13_17, 
                                  LE$Renter_Occupancy_Ratio_14_18, LE$Renter_Occupancy_Ratio_15_19, 
                                  LE$Renter_Occupancy_Ratio_16_20, LE$Renter_Occupancy_Ratio_17_21, 
                                  LE$Renter_Occupancy_Ratio_18_22),
  Miami_Renter_Occupancy_Ratio = c(MIA$Renter_Occupancy_Ratio_06_10, MIA$Renter_Occupancy_Ratio_07_11, 
                                   MIA$Renter_Occupancy_Ratio_08_12, MIA$Renter_Occupancy_Ratio_09_13, 
                                   MIA$Renter_Occupancy_Ratio_10_14, MIA$Renter_Occupancy_Ratio_11_15, 
                                   MIA$Renter_Occupancy_Ratio_12_16, MIA$Renter_Occupancy_Ratio_13_17, 
                                   MIA$Renter_Occupancy_Ratio_14_18, MIA$Renter_Occupancy_Ratio_15_19, 
                                   MIA$Renter_Occupancy_Ratio_16_20, MIA$Renter_Occupancy_Ratio_17_21, 
                                   MIA$Renter_Occupancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
combined_renter_long <- melt(combined_renter_data, id.vars = "Period", 
                             variable.name = "County", 
                             value.name = "Renter_Occupancy_Ratio")

# Create the plot with both counties' renter occupancy ratios
Both_Renter_Occupancy_Rate = ggplot(combined_renter_long, aes(x = Period, y = Renter_Occupancy_Ratio, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(title = "Renter Occupancy Ratios Over Time: Leon County vs Miami-Dade County",
       x = "Time Period",
       y = "Renter Occupancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("Leon_Renter_Occupancy_Ratio" = "#782F40",  # Garnet for Leon County
                                "Miami_Renter_Occupancy_Ratio" = "#CEB888"), # Gold for Miami-Dade County
                     labels = c("Leon County", "Miami-Dade County")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )


# OWNER OCCUPANCY
# Create a combined data frame with time periods and owner occupancy ratios for both counties
combined_owner_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Leon_Owner_Occupancy_Ratio = c(LE$Owner_Occupancy_Ratio_06_10, LE$Owner_Occupancy_Ratio_07_11, 
                                 LE$Owner_Occupancy_Ratio_08_12, LE$Owner_Occupancy_Ratio_09_13, 
                                 LE$Owner_Occupancy_Ratio_10_14, LE$Owner_Occupancy_Ratio_11_15, 
                                 LE$Owner_Occupancy_Ratio_12_16, LE$Owner_Occupancy_Ratio_13_17, 
                                 LE$Owner_Occupancy_Ratio_14_18, LE$Owner_Occupancy_Ratio_15_19, 
                                 LE$Owner_Occupancy_Ratio_16_20, LE$Owner_Occupancy_Ratio_17_21, 
                                 LE$Owner_Occupancy_Ratio_18_22),
  Miami_Owner_Occupancy_Ratio = c(MIA$Owner_Occupancy_Ratio_06_10, MIA$Owner_Occupancy_Ratio_07_11, 
                                  MIA$Owner_Occupancy_Ratio_08_12, MIA$Owner_Occupancy_Ratio_09_13, 
                                  MIA$Owner_Occupancy_Ratio_10_14, MIA$Owner_Occupancy_Ratio_11_15, 
                                  MIA$Owner_Occupancy_Ratio_12_16, MIA$Owner_Occupancy_Ratio_13_17, 
                                  MIA$Owner_Occupancy_Ratio_14_18, MIA$Owner_Occupancy_Ratio_15_19, 
                                  MIA$Owner_Occupancy_Ratio_16_20, MIA$Owner_Occupancy_Ratio_17_21, 
                                  MIA$Owner_Occupancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
combined_owner_long <- melt(combined_owner_data, id.vars = "Period", 
                            variable.name = "County", 
                            value.name = "Owner_Occupancy_Ratio")

# Create the plot with both counties' owner occupancy ratios
Both_Owner_Occupancy_Rate = ggplot(combined_owner_long, aes(x = Period, y = Owner_Occupancy_Ratio, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(title = "Owner Occupancy Ratios Over Time: Leon County vs Miami-Dade County",
       x = "Time Period",
       y = "Owner Occupancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("Leon_Owner_Occupancy_Ratio" = "#782F40",  # Garnet for Leon County
                                "Miami_Owner_Occupancy_Ratio" = "#CEB888"), # Gold for Miami-Dade County
                     labels = c("Leon County", "Miami-Dade County")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )

#BOTH RENTER AND OWNER OCCUPANCY RATES
# Create a combined data frame with time periods and both renter and owner occupancy ratios for both counties
combined_occupancy_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  LE_Owner_Occupancy_Ratio = c(LE$Owner_Occupancy_Ratio_06_10, LE$Owner_Occupancy_Ratio_07_11, 
                               LE$Owner_Occupancy_Ratio_08_12, LE$Owner_Occupancy_Ratio_09_13, 
                               LE$Owner_Occupancy_Ratio_10_14, LE$Owner_Occupancy_Ratio_11_15, 
                               LE$Owner_Occupancy_Ratio_12_16, LE$Owner_Occupancy_Ratio_13_17, 
                               LE$Owner_Occupancy_Ratio_14_18, LE$Owner_Occupancy_Ratio_15_19, 
                               LE$Owner_Occupancy_Ratio_16_20, LE$Owner_Occupancy_Ratio_17_21, 
                               LE$Owner_Occupancy_Ratio_18_22),
  LE_Renter_Occupancy_Ratio = c(LE$Renter_Occupancy_Ratio_06_10, LE$Renter_Occupancy_Ratio_07_11, 
                                LE$Renter_Occupancy_Ratio_08_12, LE$Renter_Occupancy_Ratio_09_13, 
                                LE$Renter_Occupancy_Ratio_10_14, LE$Renter_Occupancy_Ratio_11_15, 
                                LE$Renter_Occupancy_Ratio_12_16, LE$Renter_Occupancy_Ratio_13_17, 
                                LE$Renter_Occupancy_Ratio_14_18, LE$Renter_Occupancy_Ratio_15_19, 
                                LE$Renter_Occupancy_Ratio_16_20, LE$Renter_Occupancy_Ratio_17_21, 
                                LE$Renter_Occupancy_Ratio_18_22),
  MIA_Owner_Occupancy_Ratio = c(MIA$Owner_Occupancy_Ratio_06_10, MIA$Owner_Occupancy_Ratio_07_11, 
                                MIA$Owner_Occupancy_Ratio_08_12, MIA$Owner_Occupancy_Ratio_09_13, 
                                MIA$Owner_Occupancy_Ratio_10_14, MIA$Owner_Occupancy_Ratio_11_15, 
                                MIA$Owner_Occupancy_Ratio_12_16, MIA$Owner_Occupancy_Ratio_13_17, 
                                MIA$Owner_Occupancy_Ratio_14_18, MIA$Owner_Occupancy_Ratio_15_19, 
                                MIA$Owner_Occupancy_Ratio_16_20, MIA$Owner_Occupancy_Ratio_17_21, 
                                MIA$Owner_Occupancy_Ratio_18_22),
  MIA_Renter_Occupancy_Ratio = c(MIA$Renter_Occupancy_Ratio_06_10, MIA$Renter_Occupancy_Ratio_07_11, 
                                 MIA$Renter_Occupancy_Ratio_08_12, MIA$Renter_Occupancy_Ratio_09_13, 
                                 MIA$Renter_Occupancy_Ratio_10_14, MIA$Renter_Occupancy_Ratio_11_15, 
                                 MIA$Renter_Occupancy_Ratio_12_16, MIA$Renter_Occupancy_Ratio_13_17, 
                                 MIA$Renter_Occupancy_Ratio_14_18, MIA$Renter_Occupancy_Ratio_15_19, 
                                 MIA$Renter_Occupancy_Ratio_16_20, MIA$Renter_Occupancy_Ratio_17_21, 
                                 MIA$Renter_Occupancy_Ratio_18_22)
)

# Melt the data into long format for ggplot2
combined_occupancy_long <- melt(combined_occupancy_data, id.vars = "Period", 
                                variable.name = "County_Occupancy_Type", 
                                value.name = "Occupancy_Ratio")

# Create the plot with both counties' owner and renter occupancy ratios
Both_Rent_v_Owner_Rates = ggplot(combined_occupancy_long, aes(x = Period, y = Occupancy_Ratio, color = County_Occupancy_Type, group = County_Occupancy_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(title = "Owner and Renter Occupancy Ratios: LE vs MIA",
       x = "Time Period",
       y = "Occupancy Ratio (%)") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  scale_color_manual(values = c("LE_Owner_Occupancy_Ratio" = "#1f77b4",  # Muted Blue for LE Owner
                                "LE_Renter_Occupancy_Ratio" = "#ff7f0e",  # Orange for LE Renter
                                "MIA_Owner_Occupancy_Ratio" = "#2ca02c",  # Green for MIA Owner
                                "MIA_Renter_Occupancy_Ratio" = "#d62728"), # Red for MIA Renter
                     labels = c("LE Owner Occupancy", "LE Renter Occupancy", 
                                "MIA Owner Occupancy", "MIA Renter Occupancy")) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered and bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Angled x-axis labels with larger font
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    legend.position = "right",  # Keep the legend on the right side
    legend.title = element_blank()  # Remove legend title
  )

#Average vacancy rate for each county
# Compute the average vacancy rate for Leon County
LE$leon_avg_vacancy <- mean(c(LE$Vacancy_Ratio_06_10, LE$Vacancy_Ratio_07_11, LE$Vacancy_Ratio_08_12, 
                           LE$Vacancy_Ratio_09_13, LE$Vacancy_Ratio_10_14, LE$Vacancy_Ratio_11_15, 
                           LE$Vacancy_Ratio_12_16, LE$Vacancy_Ratio_13_17, LE$Vacancy_Ratio_14_18, 
                           LE$Vacancy_Ratio_15_19, LE$Vacancy_Ratio_16_20, LE$Vacancy_Ratio_17_21, 
                           LE$Vacancy_Ratio_18_22), na.rm = TRUE)  # Handle any NA values

# Compute the average vacancy rate for Miami-Dade County
MIA$miami_avg_vacancy <- mean(c(MIA$Vacancy_Ratio_06_10, MIA$Vacancy_Ratio_07_11, MIA$Vacancy_Ratio_08_12, 
                            MIA$Vacancy_Ratio_09_13, MIA$Vacancy_Ratio_10_14, MIA$Vacancy_Ratio_11_15, 
                            MIA$Vacancy_Ratio_12_16, MIA$Vacancy_Ratio_13_17, MIA$Vacancy_Ratio_14_18, 
                            MIA$Vacancy_Ratio_15_19, MIA$Vacancy_Ratio_16_20, MIA$Vacancy_Ratio_17_21, 
                            MIA$Vacancy_Ratio_18_22), na.rm = TRUE)  # Handle any NA values

# Calculate the difference between each year's vacancy rate and the average for Leon County
LE$Vacancy_Diff_06_10 <- LE$Vacancy_Ratio_06_10 - LE$leon_avg_vacancy
LE$Vacancy_Diff_07_11 <- LE$Vacancy_Ratio_07_11 - LE$leon_avg_vacancy
LE$Vacancy_Diff_08_12 <- LE$Vacancy_Ratio_08_12 - LE$leon_avg_vacancy
LE$Vacancy_Diff_09_13 <- LE$Vacancy_Ratio_09_13 - LE$leon_avg_vacancy
LE$Vacancy_Diff_10_14 <- LE$Vacancy_Ratio_10_14 - LE$leon_avg_vacancy
LE$Vacancy_Diff_11_15 <- LE$Vacancy_Ratio_11_15 - LE$leon_avg_vacancy
LE$Vacancy_Diff_12_16 <- LE$Vacancy_Ratio_12_16 - LE$leon_avg_vacancy
LE$Vacancy_Diff_13_17 <- LE$Vacancy_Ratio_13_17 - LE$leon_avg_vacancy
LE$Vacancy_Diff_14_18 <- LE$Vacancy_Ratio_14_18 - LE$leon_avg_vacancy
LE$Vacancy_Diff_15_19 <- LE$Vacancy_Ratio_15_19 - LE$leon_avg_vacancy
LE$Vacancy_Diff_16_20 <- LE$Vacancy_Ratio_16_20 - LE$leon_avg_vacancy
LE$Vacancy_Diff_17_21 <- LE$Vacancy_Ratio_17_21 - LE$leon_avg_vacancy
LE$Vacancy_Diff_18_22 <- LE$Vacancy_Ratio_18_22 - LE$leon_avg_vacancy

# Calculate the difference between each year's vacancy rate and the average for Miami-Dade County
MIA$Vacancy_Diff_06_10 <- MIA$Vacancy_Ratio_06_10 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_07_11 <- MIA$Vacancy_Ratio_07_11 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_08_12 <- MIA$Vacancy_Ratio_08_12 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_09_13 <- MIA$Vacancy_Ratio_09_13 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_10_14 <- MIA$Vacancy_Ratio_10_14 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_11_15 <- MIA$Vacancy_Ratio_11_15 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_12_16 <- MIA$Vacancy_Ratio_12_16 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_13_17 <- MIA$Vacancy_Ratio_13_17 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_14_18 <- MIA$Vacancy_Ratio_14_18 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_15_19 <- MIA$Vacancy_Ratio_15_19 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_16_20 <- MIA$Vacancy_Ratio_16_20 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_17_21 <- MIA$Vacancy_Ratio_17_21 - MIA$miami_avg_vacancy
MIA$Vacancy_Diff_18_22 <- MIA$Vacancy_Ratio_18_22 - MIA$miami_avg_vacancy


# Step 1: Create a combined data frame with time periods and vacancy rate differences for both counties
combined_vacancy_diff_data <- data.frame(
  Period = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", 
             "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  Leon_Vacancy_Diff = c(LE$Vacancy_Diff_06_10, LE$Vacancy_Diff_07_11, LE$Vacancy_Diff_08_12, 
                        LE$Vacancy_Diff_09_13, LE$Vacancy_Diff_10_14, LE$Vacancy_Diff_11_15, 
                        LE$Vacancy_Diff_12_16, LE$Vacancy_Diff_13_17, LE$Vacancy_Diff_14_18, 
                        LE$Vacancy_Diff_15_19, LE$Vacancy_Diff_16_20, LE$Vacancy_Diff_17_21, 
                        LE$Vacancy_Diff_18_22),
  Miami_Vacancy_Diff = c(MIA$Vacancy_Diff_06_10, MIA$Vacancy_Diff_07_11, MIA$Vacancy_Diff_08_12, 
                         MIA$Vacancy_Diff_09_13, MIA$Vacancy_Diff_10_14, MIA$Vacancy_Diff_11_15, 
                         MIA$Vacancy_Diff_12_16, MIA$Vacancy_Diff_13_17, MIA$Vacancy_Diff_14_18, 
                         MIA$Vacancy_Diff_15_19, MIA$Vacancy_Diff_16_20, MIA$Vacancy_Diff_17_21, 
                         MIA$Vacancy_Diff_18_22)
)

# Step 2: Melt the data into long format for ggplot2
combined_vacancy_diff_long <- melt(combined_vacancy_diff_data, id.vars = "Period", 
                                   variable.name = "County", 
                                   value.name = "Vacancy_Diff")

# Step 3: Create the plot with both counties' vacancy rate differences
Both_Average_Differences_Vancancy_Rates = ggplot(combined_vacancy_diff_long, aes(x = Period, y = Vacancy_Diff, color = County, group = County)) +
  geom_line(size = 1.5) +  # Use thicker lines for better visibility
  geom_point(size = 3) +  # Slightly smaller points for clarity
  scale_color_manual(values = c("Leon_Vacancy_Diff" = "#0072B2",  # Blue for Leon County
                                "Miami_Vacancy_Diff" = "#D55E00"),
                     labels = c("Leon Vacancy Difference", "Miami-Dade Vacancy Difference")) +  # Custom legend labels
  labs(title = "Vacancy Rate Differences from Average (Leon County vs. Miami-Dade County)",
       subtitle = "Comparing vacancy rate deviations from their respective averages over time",
       x = "Time Period",
       y = "Vacancy Rate Difference (%)",
       caption = "Source: IPUMS NHGIS") +
  scale_x_discrete(labels = c("Years 06-10", "Years 07-11", "Years 08-12", "Years 09-13", 
                              "Years 10-14", "Years 11-15", "Years 12-16", "Years 13-17", 
                              "Years 14-18", "Years 15-19", "Years 16-20", "Years 17-21", 
                              "Years 18-22")) +  # Updated x-axis labels
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  theme_minimal(base_size = 14) +  # Minimal theme with larger base font size for readability
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold and centered title
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),  # Subtitle for context
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Angled x-axis labels for better spacing
    axis.text.y = element_text(size = 10),  # Adjusted y-axis label size
    panel.grid.major = element_line(color = "gray80"),  # Light gray major gridlines
    panel.grid.minor = element_line(color = "gray90"),  # Lighter gray minor gridlines
    legend.position = "right",  # Keep legend on the right
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Increase legend text size
    plot.caption = element_text(hjust = 0, face = "italic", size = 10, margin = margin(t = 10))  # Add a caption
  )

Leon_Vacancy
Leon_Renter_v_Owner
Miami_Vacancy
Miami_Renter_v_Owner
Both_Rent_v_Owner_Rates
Both_Renter_Occupancy_Rate
Both_Owner_Occupancy_Rate
Both_Average_Differences_Vancancy_Rates

ggsave("Leon Vacancy Rate.png", plot = Leon_Vacancy, width = 6, height = 4)
ggsave("Leon Renter vs. Owner.png", plot = Leon_Renter_v_Owner, width = 6, height = 4)
ggsave("Miami Vacancy Rate.png", plot = Miami_Vacancy, width = 6, height = 4)
ggsave("Miami Renter vs. Owner.png", plot = Miami_Renter_v_Owner, width = 6, height = 4)
ggsave("Both Renter vs Owner.png", plot = Both_Rent_v_Owner_Rates, width = 6, height = 4)
ggsave("Both Renter Occupancy Rates.png", plot = Both_Renter_Occupancy_Rate, width = 6, height = 4)
ggsave("Both Owner Occupancy Rates.png", plot = Both_Owner_Occupancy_Rate, width = 6, height = 4)
ggsave("Both Differences in Average Vacancy Rates.png", plot = Both_Average_Differences_Vancancy_Rates, width = 6, height = 4)




##### Second #####
setwd("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0004_csv")
LE2 <- read_csv("nhgis0004_ts_nominal_county.csv")
LE2 <- subset(LE2, STATE == "Florida")
MIA2 <- subset(LE2, COUNTY == "Miami-Dade County")

LE2 <- subset(LE2, COUNTY == "Leon County")
#### LEON ####
LE2 <- LE2 %>%
  rename(ForRent06_10 = D11AA105,
         ForRent07_11 = D11AA115,
         ForRent08_12 = D11AA125,
         ForRent09_13 = D11AA135,
         ForRent10_14 = D11AA145,
         ForRent11_15 = D11AA155,
         ForRent12_16 = D11AA165,
         ForRent13_17 = D11AA175,
         ForRent14_18 = D11AA185,
         ForRent15_19 = D11AA195,
         ForRent16_20 = D11AA205,
         ForRent17_21 = D11AA215,
         ForRent18_22 = D11AA225,
         RentedNotOcc06_10 = D11AB105,
         RentedNotOcc07_11 = D11AB115,
         RentedNotOcc08_12 = D11AB125,
         RentedNotOcc09_13 = D11AB135,
         RentedNotOcc10_14 = D11AB145,
         RentedNotOcc11_15 = D11AB155,
         RentedNotOcc12_16 = D11AB165,
         RentedNotOcc13_17 = D11AB175,
         RentedNotOcc14_18 = D11AB185,
         RentedNotOcc15_19 = D11AB195,
         RentedNotOcc16_20 = D11AB205,
         RentedNotOcc17_21 = D11AB215,
         RentedNotOcc18_22 = D11AB225,
         ForSale06_10 = D11AC105,
         ForSale07_11 = D11AC115,
         ForSale08_12 = D11AC125,
         ForSale09_13 = D11AC135,
         ForSale10_14 = D11AC145,
         ForSale11_15 = D11AC155,
         ForSale12_16 = D11AC165,
         ForSale13_17 = D11AC175,
         ForSale14_18 = D11AC185,
         ForSale15_19 = D11AC195,
         ForSale16_20 = D11AC205,
         ForSale17_21 = D11AC215,
         ForSale18_22 = D11AC225,
         SoldNotOcc06_10 = D11AD105,
         SoldNotOcc07_11 = D11AD115,
         SoldNotOcc08_12 = D11AD125,
         SoldNotOcc09_13 = D11AD135,
         SoldNotOcc10_14 = D11AD145,
         SoldNotOcc11_15 = D11AD155,
         SoldNotOcc12_16 = D11AD165,
         SoldNotOcc13_17 = D11AD175,
         SoldNotOcc14_18 = D11AD185,
         SoldNotOcc15_19 = D11AD195,
         SoldNotOcc16_20 = D11AD205,
         SoldNotOcc17_21 = D11AD215,
         SoldNotOcc18_22 = D11AD225,
         OwnerOcc06_10 = B37AA105,
         OwnerOcc07_11 = B37AA115,
         OwnerOcc08_12 = B37AA125,
         OwnerOcc09_13 = B37AA135,
         OwnerOcc10_14 = B37AA145,
         OwnerOcc11_15 = B37AA155,
         OwnerOcc12_16 = B37AA165,
         OwnerOcc13_17 = B37AA175,
         OwnerOcc14_18 = B37AA185,
         OwnerOcc15_19 = B37AA195,
         OwnerOcc16_20 = B37AA205,
         OwnerOcc17_21 = B37AA215,
         OwnerOcc18_22 = B37AA225,
         RenterOcc06_10 = B37AB105,
         RenterOcc07_11 = B37AB115,
         RenterOcc08_12 = B37AB125,
         RenterOcc09_13 = B37AB135,
         RenterOcc10_14 = B37AB145,
         RenterOcc11_15 = B37AB155,
         RenterOcc12_16 = B37AB165,
         RenterOcc13_17 = B37AB175,
         RenterOcc14_18 = B37AB185,
         RenterOcc15_19 = B37AB195,
         RenterOcc16_20 = B37AB205,
         RenterOcc17_21 = B37AB215,
         RenterOcc18_22 = B37AB225)

LE2 <- LE2 %>%
  select(c(ForRent06_10, ForRent07_11, ForRent08_12, ForRent09_13, ForRent10_14, ForRent11_15, ForRent12_16, ForRent13_17, ForRent14_18, ForRent15_19, ForRent16_20, ForRent17_21, ForRent18_22,
           RentedNotOcc06_10,
           RentedNotOcc07_11,
           RentedNotOcc08_12,
           RentedNotOcc09_13,
           RentedNotOcc10_14,
           RentedNotOcc11_15,
           RentedNotOcc12_16,
           RentedNotOcc13_17,
           RentedNotOcc14_18,
           RentedNotOcc15_19,
           RentedNotOcc16_20,
           RentedNotOcc17_21,
           RentedNotOcc18_22,
           ForSale06_10,
           ForSale07_11,
           ForSale08_12,
           ForSale09_13,
           ForSale10_14,
           ForSale11_15,
           ForSale12_16,
           ForSale13_17,
           ForSale14_18,
           ForSale15_19,
           ForSale16_20,
           ForSale17_21,
           ForSale18_22,
           SoldNotOcc06_10,
           SoldNotOcc07_11,
           SoldNotOcc08_12,
           SoldNotOcc09_13,
           SoldNotOcc10_14,
           SoldNotOcc11_15,
           SoldNotOcc12_16,
           SoldNotOcc13_17,
           SoldNotOcc14_18,
           SoldNotOcc15_19,
           SoldNotOcc16_20,
           SoldNotOcc17_21,
           SoldNotOcc18_22,
           OwnerOcc06_10,
           OwnerOcc07_11,
           OwnerOcc08_12,
           OwnerOcc09_13,
           OwnerOcc10_14,
           OwnerOcc11_15,
           OwnerOcc12_16,
           OwnerOcc13_17,
           OwnerOcc14_18,
           OwnerOcc15_19,
           OwnerOcc16_20,
           OwnerOcc17_21,
           OwnerOcc18_22,
           RenterOcc06_10,
           RenterOcc07_11,
           RenterOcc08_12,
           RenterOcc09_13,
           RenterOcc10_14,
           RenterOcc11_15,
           RenterOcc12_16,
           RenterOcc13_17,
           RenterOcc14_18,
           RenterOcc15_19,
           RenterOcc16_20,
           RenterOcc17_21,
           RenterOcc18_22))

#write.csv(LE2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/LE2.csv", row.names = FALSE)

# Calculate the OwnerVacancyRate for each interval directly
LE2$OwnerVacRate06_10 <- LE2$ForSale06_10 / (LE2$ForSale06_10 + LE2$OwnerOcc06_10 + LE2$SoldNotOcc06_10)
LE2$OwnerVacRate07_11 <- LE2$ForSale07_11 / (LE2$ForSale07_11 + LE2$OwnerOcc07_11 + LE2$SoldNotOcc07_11)
LE2$OwnerVacRate08_12 <- LE2$ForSale08_12 / (LE2$ForSale08_12 + LE2$OwnerOcc08_12 + LE2$SoldNotOcc08_12)
LE2$OwnerVacRate09_13 <- LE2$ForSale09_13 / (LE2$ForSale09_13 + LE2$OwnerOcc09_13 + LE2$SoldNotOcc09_13)
LE2$OwnerVacRate10_14 <- LE2$ForSale10_14 / (LE2$ForSale10_14 + LE2$OwnerOcc10_14 + LE2$SoldNotOcc10_14)
LE2$OwnerVacRate11_15 <- LE2$ForSale11_15 / (LE2$ForSale11_15 + LE2$OwnerOcc11_15 + LE2$SoldNotOcc11_15)
LE2$OwnerVacRate12_16 <- LE2$ForSale12_16 / (LE2$ForSale12_16 + LE2$OwnerOcc12_16 + LE2$SoldNotOcc12_16)
LE2$OwnerVacRate13_17 <- LE2$ForSale13_17 / (LE2$ForSale13_17 + LE2$OwnerOcc13_17 + LE2$SoldNotOcc13_17)
LE2$OwnerVacRate14_18 <- LE2$ForSale14_18 / (LE2$ForSale14_18 + LE2$OwnerOcc14_18 + LE2$SoldNotOcc14_18)
LE2$OwnerVacRate15_19 <- LE2$ForSale15_19 / (LE2$ForSale15_19 + LE2$OwnerOcc15_19 + LE2$SoldNotOcc15_19)
LE2$OwnerVacRate16_20 <- LE2$ForSale16_20 / (LE2$ForSale16_20 + LE2$OwnerOcc16_20 + LE2$SoldNotOcc16_20)
LE2$OwnerVacRate17_21 <- LE2$ForSale17_21 / (LE2$ForSale17_21 + LE2$OwnerOcc17_21 + LE2$SoldNotOcc17_21)
LE2$OwnerVacRate18_22 <- LE2$ForSale18_22 / (LE2$ForSale18_22 + LE2$OwnerOcc18_22 + LE2$SoldNotOcc18_22)

# Calculate the RenterVacancyRate for each interval
LE2$RenterVacRate06_10 <- LE2$ForRent06_10 / (LE2$ForRent06_10 + LE2$RenterOcc06_10 + LE2$RentedNotOcc06_10)
LE2$RenterVacRate07_11 <- LE2$ForRent07_11 / (LE2$ForRent07_11 + LE2$RenterOcc07_11 + LE2$RentedNotOcc07_11)
LE2$RenterVacRate08_12 <- LE2$ForRent08_12 / (LE2$ForRent08_12 + LE2$RenterOcc08_12 + LE2$RentedNotOcc08_12)
LE2$RenterVacRate09_13 <- LE2$ForRent09_13 / (LE2$ForRent09_13 + LE2$RenterOcc09_13 + LE2$RentedNotOcc09_13)
LE2$RenterVacRate10_14 <- LE2$ForRent10_14 / (LE2$ForRent10_14 + LE2$RenterOcc10_14 + LE2$RentedNotOcc10_14)
LE2$RenterVacRate11_15 <- LE2$ForRent11_15 / (LE2$ForRent11_15 + LE2$RenterOcc11_15 + LE2$RentedNotOcc11_15)
LE2$RenterVacRate12_16 <- LE2$ForRent12_16 / (LE2$ForRent12_16 + LE2$RenterOcc12_16 + LE2$RentedNotOcc12_16)
LE2$RenterVacRate13_17 <- LE2$ForRent13_17 / (LE2$ForRent13_17 + LE2$RenterOcc13_17 + LE2$RentedNotOcc13_17)
LE2$RenterVacRate14_18 <- LE2$ForRent14_18 / (LE2$ForRent14_18 + LE2$RenterOcc14_18 + LE2$RentedNotOcc14_18)
LE2$RenterVacRate15_19 <- LE2$ForRent15_19 / (LE2$ForRent15_19 + LE2$RenterOcc15_19 + LE2$RentedNotOcc15_19)
LE2$RenterVacRate16_20 <- LE2$ForRent16_20 / (LE2$ForRent16_20 + LE2$RenterOcc16_20 + LE2$RentedNotOcc16_20)
LE2$RenterVacRate17_21 <- LE2$ForRent17_21 / (LE2$ForRent17_21 + LE2$RenterOcc17_21 + LE2$RentedNotOcc17_21)
LE2$RenterVacRate18_22 <- LE2$ForRent18_22 / (LE2$ForRent18_22 + LE2$RenterOcc18_22 + LE2$RentedNotOcc18_22)

# Create a data frame with the relevant information for plotting
plot_data <- data.frame(
  Interval = c('06_10', '07_11', '08_12', '09_13', '10_14', '11_15', 
               '12_16', '13_17', '14_18', '15_19', '16_20', '17_21', '18_22'),
  OwnerVacRate = c(LE2$OwnerVacRate06_10, LE2$OwnerVacRate07_11, LE2$OwnerVacRate08_12, 
                   LE2$OwnerVacRate09_13, LE2$OwnerVacRate10_14, LE2$OwnerVacRate11_15, 
                   LE2$OwnerVacRate12_16, LE2$OwnerVacRate13_17, LE2$OwnerVacRate14_18, 
                   LE2$OwnerVacRate15_19, LE2$OwnerVacRate16_20, LE2$OwnerVacRate17_21, 
                   LE2$OwnerVacRate18_22),
  RenterVacRate = c(LE2$RenterVacRate06_10, LE2$RenterVacRate07_11, LE2$RenterVacRate08_12, 
                    LE2$RenterVacRate09_13, LE2$RenterVacRate10_14, LE2$RenterVacRate11_15, 
                    LE2$RenterVacRate12_16, LE2$RenterVacRate13_17, LE2$RenterVacRate14_18, 
                    LE2$RenterVacRate15_19, LE2$RenterVacRate16_20, LE2$RenterVacRate17_21, 
                    LE2$RenterVacRate18_22)
)

# Reshape data for plotting both lines on the same graph
plot_data_melt <- melt(plot_data, id.vars = "Interval", 
                       variable.name = "Type", value.name = "Rate")

# FSU colors
fsu_colors <- c("OwnerVacRate" = "#782F40", "RenterVacRate" = "#CEB888")

# Create the line plot
Leon = ggplot(plot_data_melt, aes(x = Interval, y = Rate, group = Type, color = Type)) +
  geom_line(size = 1.5) +  # Thicker line for visibility
  geom_point(size = 3) +   # Larger points for clarity
  geom_text(aes(label = scales::percent(Rate, accuracy = 0.1)),  # Add values at each point
            vjust = -0.8, color = "black", size = 3.5) +  # Black color, adjusted vertical position
  scale_color_manual(values = fsu_colors, 
                     labels = c("Owner Vacancy Rate", "Renter Vacancy Rate")) +  # Custom legend labels
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  scale_x_discrete(labels = c("06_10" = "Years 06-10", "07_11" = "Years 07-11", 
                              "08_12" = "Years 08-12", "09_13" = "Years 09-13", 
                              "10_14" = "Years 10-14", "11_15" = "Years 11-15", 
                              "12_16" = "Years 12-16", "13_17" = "Years 13-17", 
                              "14_18" = "Years 14-18", "15_19" = "Years 15-19", 
                              "16_20" = "Years 16-20", "17_21" = "Years 17-21", 
                              "18_22" = "Years 18-22")) +  # Custom x-axis labels
  labs(title = "Owner and Renter Vacancy Rate Over Time for Leon County", 
       x = "Years", 
       y = "Vacancy Rate (%)", 
       color = "") +  # Empty color label to avoid legend title
  theme_minimal(base_size = 14) +  # Professional minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered and bold title
    legend.position = "bottom",  # Move legend to bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




#### MIAMI ####
MIA2 <- MIA2 %>%
  rename(ForRent06_10 = D11AA105,
         ForRent07_11 = D11AA115,
         ForRent08_12 = D11AA125,
         ForRent09_13 = D11AA135,
         ForRent10_14 = D11AA145,
         ForRent11_15 = D11AA155,
         ForRent12_16 = D11AA165,
         ForRent13_17 = D11AA175,
         ForRent14_18 = D11AA185,
         ForRent15_19 = D11AA195,
         ForRent16_20 = D11AA205,
         ForRent17_21 = D11AA215,
         ForRent18_22 = D11AA225,
         RentedNotOcc06_10 = D11AB105,
         RentedNotOcc07_11 = D11AB115,
         RentedNotOcc08_12 = D11AB125,
         RentedNotOcc09_13 = D11AB135,
         RentedNotOcc10_14 = D11AB145,
         RentedNotOcc11_15 = D11AB155,
         RentedNotOcc12_16 = D11AB165,
         RentedNotOcc13_17 = D11AB175,
         RentedNotOcc14_18 = D11AB185,
         RentedNotOcc15_19 = D11AB195,
         RentedNotOcc16_20 = D11AB205,
         RentedNotOcc17_21 = D11AB215,
         RentedNotOcc18_22 = D11AB225,
         ForSale06_10 = D11AC105,
         ForSale07_11 = D11AC115,
         ForSale08_12 = D11AC125,
         ForSale09_13 = D11AC135,
         ForSale10_14 = D11AC145,
         ForSale11_15 = D11AC155,
         ForSale12_16 = D11AC165,
         ForSale13_17 = D11AC175,
         ForSale14_18 = D11AC185,
         ForSale15_19 = D11AC195,
         ForSale16_20 = D11AC205,
         ForSale17_21 = D11AC215,
         ForSale18_22 = D11AC225,
         SoldNotOcc06_10 = D11AD105,
         SoldNotOcc07_11 = D11AD115,
         SoldNotOcc08_12 = D11AD125,
         SoldNotOcc09_13 = D11AD135,
         SoldNotOcc10_14 = D11AD145,
         SoldNotOcc11_15 = D11AD155,
         SoldNotOcc12_16 = D11AD165,
         SoldNotOcc13_17 = D11AD175,
         SoldNotOcc14_18 = D11AD185,
         SoldNotOcc15_19 = D11AD195,
         SoldNotOcc16_20 = D11AD205,
         SoldNotOcc17_21 = D11AD215,
         SoldNotOcc18_22 = D11AD225,
         OwnerOcc06_10 = B37AA105,
         OwnerOcc07_11 = B37AA115,
         OwnerOcc08_12 = B37AA125,
         OwnerOcc09_13 = B37AA135,
         OwnerOcc10_14 = B37AA145,
         OwnerOcc11_15 = B37AA155,
         OwnerOcc12_16 = B37AA165,
         OwnerOcc13_17 = B37AA175,
         OwnerOcc14_18 = B37AA185,
         OwnerOcc15_19 = B37AA195,
         OwnerOcc16_20 = B37AA205,
         OwnerOcc17_21 = B37AA215,
         OwnerOcc18_22 = B37AA225,
         RenterOcc06_10 = B37AB105,
         RenterOcc07_11 = B37AB115,
         RenterOcc08_12 = B37AB125,
         RenterOcc09_13 = B37AB135,
         RenterOcc10_14 = B37AB145,
         RenterOcc11_15 = B37AB155,
         RenterOcc12_16 = B37AB165,
         RenterOcc13_17 = B37AB175,
         RenterOcc14_18 = B37AB185,
         RenterOcc15_19 = B37AB195,
         RenterOcc16_20 = B37AB205,
         RenterOcc17_21 = B37AB215,
         RenterOcc18_22 = B37AB225)

MIA2 <- MIA2 %>%
  select(c(ForRent06_10, ForRent07_11, ForRent08_12, ForRent09_13, ForRent10_14, ForRent11_15, ForRent12_16, ForRent13_17, ForRent14_18, ForRent15_19, ForRent16_20, ForRent17_21, ForRent18_22,
           RentedNotOcc06_10,
           RentedNotOcc07_11,
           RentedNotOcc08_12,
           RentedNotOcc09_13,
           RentedNotOcc10_14,
           RentedNotOcc11_15,
           RentedNotOcc12_16,
           RentedNotOcc13_17,
           RentedNotOcc14_18,
           RentedNotOcc15_19,
           RentedNotOcc16_20,
           RentedNotOcc17_21,
           RentedNotOcc18_22,
           ForSale06_10,
           ForSale07_11,
           ForSale08_12,
           ForSale09_13,
           ForSale10_14,
           ForSale11_15,
           ForSale12_16,
           ForSale13_17,
           ForSale14_18,
           ForSale15_19,
           ForSale16_20,
           ForSale17_21,
           ForSale18_22,
           SoldNotOcc06_10,
           SoldNotOcc07_11,
           SoldNotOcc08_12,
           SoldNotOcc09_13,
           SoldNotOcc10_14,
           SoldNotOcc11_15,
           SoldNotOcc12_16,
           SoldNotOcc13_17,
           SoldNotOcc14_18,
           SoldNotOcc15_19,
           SoldNotOcc16_20,
           SoldNotOcc17_21,
           SoldNotOcc18_22,
           OwnerOcc06_10,
           OwnerOcc07_11,
           OwnerOcc08_12,
           OwnerOcc09_13,
           OwnerOcc10_14,
           OwnerOcc11_15,
           OwnerOcc12_16,
           OwnerOcc13_17,
           OwnerOcc14_18,
           OwnerOcc15_19,
           OwnerOcc16_20,
           OwnerOcc17_21,
           OwnerOcc18_22,
           RenterOcc06_10,
           RenterOcc07_11,
           RenterOcc08_12,
           RenterOcc09_13,
           RenterOcc10_14,
           RenterOcc11_15,
           RenterOcc12_16,
           RenterOcc13_17,
           RenterOcc14_18,
           RenterOcc15_19,
           RenterOcc16_20,
           RenterOcc17_21,
           RenterOcc18_22))

# Calculate OwnerVacancyRate for each interval
MIA2$OwnerVacRate06_10 <- MIA2$ForSale06_10 / (MIA2$ForSale06_10 + MIA2$OwnerOcc06_10 + MIA2$SoldNotOcc06_10)
MIA2$OwnerVacRate07_11 <- MIA2$ForSale07_11 / (MIA2$ForSale07_11 + MIA2$OwnerOcc07_11 + MIA2$SoldNotOcc07_11)
MIA2$OwnerVacRate08_12 <- MIA2$ForSale08_12 / (MIA2$ForSale08_12 + MIA2$OwnerOcc08_12 + MIA2$SoldNotOcc08_12)
MIA2$OwnerVacRate09_13 <- MIA2$ForSale09_13 / (MIA2$ForSale09_13 + MIA2$OwnerOcc09_13 + MIA2$SoldNotOcc09_13)
MIA2$OwnerVacRate10_14 <- MIA2$ForSale10_14 / (MIA2$ForSale10_14 + MIA2$OwnerOcc10_14 + MIA2$SoldNotOcc10_14)
MIA2$OwnerVacRate11_15 <- MIA2$ForSale11_15 / (MIA2$ForSale11_15 + MIA2$OwnerOcc11_15 + MIA2$SoldNotOcc11_15)
MIA2$OwnerVacRate12_16 <- MIA2$ForSale12_16 / (MIA2$ForSale12_16 + MIA2$OwnerOcc12_16 + MIA2$SoldNotOcc12_16)
MIA2$OwnerVacRate13_17 <- MIA2$ForSale13_17 / (MIA2$ForSale13_17 + MIA2$OwnerOcc13_17 + MIA2$SoldNotOcc13_17)
MIA2$OwnerVacRate14_18 <- MIA2$ForSale14_18 / (MIA2$ForSale14_18 + MIA2$OwnerOcc14_18 + MIA2$SoldNotOcc14_18)
MIA2$OwnerVacRate15_19 <- MIA2$ForSale15_19 / (MIA2$ForSale15_19 + MIA2$OwnerOcc15_19 + MIA2$SoldNotOcc15_19)
MIA2$OwnerVacRate16_20 <- MIA2$ForSale16_20 / (MIA2$ForSale16_20 + MIA2$OwnerOcc16_20 + MIA2$SoldNotOcc16_20)
MIA2$OwnerVacRate17_21 <- MIA2$ForSale17_21 / (MIA2$ForSale17_21 + MIA2$OwnerOcc17_21 + MIA2$SoldNotOcc17_21)
MIA2$OwnerVacRate18_22 <- MIA2$ForSale18_22 / (MIA2$ForSale18_22 + MIA2$OwnerOcc18_22 + MIA2$SoldNotOcc18_22)

# Calculate RenterVacancyRate for each interval
MIA2$RenterVacRate06_10 <- MIA2$ForRent06_10 / (MIA2$ForRent06_10 + MIA2$RenterOcc06_10 + MIA2$RentedNotOcc06_10)
MIA2$RenterVacRate07_11 <- MIA2$ForRent07_11 / (MIA2$ForRent07_11 + MIA2$RenterOcc07_11 + MIA2$RentedNotOcc07_11)
MIA2$RenterVacRate08_12 <- MIA2$ForRent08_12 / (MIA2$ForRent08_12 + MIA2$RenterOcc08_12 + MIA2$RentedNotOcc08_12)
MIA2$RenterVacRate09_13 <- MIA2$ForRent09_13 / (MIA2$ForRent09_13 + MIA2$RenterOcc09_13 + MIA2$RentedNotOcc09_13)
MIA2$RenterVacRate10_14 <- MIA2$ForRent10_14 / (MIA2$ForRent10_14 + MIA2$RenterOcc10_14 + MIA2$RentedNotOcc10_14)
MIA2$RenterVacRate11_15 <- MIA2$ForRent11_15 / (MIA2$ForRent11_15 + MIA2$RenterOcc11_15 + MIA2$RentedNotOcc11_15)
MIA2$RenterVacRate12_16 <- MIA2$ForRent12_16 / (MIA2$ForRent12_16 + MIA2$RenterOcc12_16 + MIA2$RentedNotOcc12_16)
MIA2$RenterVacRate13_17 <- MIA2$ForRent13_17 / (MIA2$ForRent13_17 + MIA2$RenterOcc13_17 + MIA2$RentedNotOcc13_17)
MIA2$RenterVacRate14_18 <- MIA2$ForRent14_18 / (MIA2$ForRent14_18 + MIA2$RenterOcc14_18 + MIA2$RentedNotOcc14_18)
MIA2$RenterVacRate15_19 <- MIA2$ForRent15_19 / (MIA2$ForRent15_19 + MIA2$RenterOcc15_19 + MIA2$RentedNotOcc15_19)
MIA2$RenterVacRate16_20 <- MIA2$ForRent16_20 / (MIA2$ForRent16_20 + MIA2$RenterOcc16_20 + MIA2$RentedNotOcc16_20)
MIA2$RenterVacRate17_21 <- MIA2$ForRent17_21 / (MIA2$ForRent17_21 + MIA2$RenterOcc17_21 + MIA2$RentedNotOcc17_21)
MIA2$RenterVacRate18_22 <- MIA2$ForRent18_22 / (MIA2$ForRent18_22 + MIA2$RenterOcc18_22 + MIA2$RentedNotOcc18_22)

# Create a data frame with the relevant information for plotting
plot_data2 <- data.frame(
  Interval = c('06_10', '07_11', '08_12', '09_13', '10_14', '11_15', 
               '12_16', '13_17', '14_18', '15_19', '16_20', '17_21', '18_22'),
  OwnerOccRate = c(MIA2$OwnerVacRate06_10, MIA2$OwnerVacRate07_11, MIA2$OwnerVacRate08_12, 
                   MIA2$OwnerVacRate09_13, MIA2$OwnerVacRate10_14, MIA2$OwnerVacRate11_15, 
                   MIA2$OwnerVacRate12_16, MIA2$OwnerVacRate13_17, MIA2$OwnerVacRate14_18, 
                   MIA2$OwnerVacRate15_19, MIA2$OwnerVacRate16_20, MIA2$OwnerVacRate17_21, 
                   MIA2$OwnerVacRate18_22),
  RenterOccRate = c(MIA2$RenterVacRate06_10, MIA2$RenterVacRate07_11, MIA2$RenterVacRate08_12, 
                    MIA2$RenterVacRate09_13, MIA2$RenterVacRate10_14, MIA2$RenterVacRate11_15, 
                    MIA2$RenterVacRate12_16, MIA2$RenterVacRate13_17, MIA2$RenterVacRate14_18, 
                    MIA2$RenterVacRate15_19, MIA2$RenterVacRate16_20, MIA2$RenterVacRate17_21, 
                    MIA2$RenterVacRate18_22)
)

# Reshape data for plotting both lines on the same graph
plot_data_melt2 <- melt(plot_data2, id.vars = "Interval", 
                       variable.name = "Type", value.name = "Rate")

# Create the line plot with custom x-axis labels and values at each point
MIA = ggplot(plot_data_melt2, aes(x = Interval, y = Rate, group = Type, color = Type)) +
  geom_line(size = 1.5) +  # Thicker line for visibility
  geom_point(size = 3) +   # Larger points for clarity
  geom_text(aes(label = scales::percent(Rate, accuracy = 0.1)),  # Add values at each point
            vjust = -0.8, color = "black", size = 3.5) +  # Black color, adjusted vertical position
  scale_color_manual(values = fsu_colors, 
                     labels = c("Owner Vacancy Rate", "Renter Vacancy Rate")) +  # Custom legend labels
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  scale_x_discrete(labels = c("06_10" = "Years 06-10", "07_11" = "Years 07-11", 
                              "08_12" = "Years 08-12", "09_13" = "Years 09-13", 
                              "10_14" = "Years 10-14", "11_15" = "Years 11-15", 
                              "12_16" = "Years 12-16", "13_17" = "Years 13-17", 
                              "14_18" = "Years 14-18", "15_19" = "Years 15-19", 
                              "16_20" = "Years 16-20", "17_21" = "Years 17-21", 
                              "18_22" = "Years 18-22")) +  # Custom x-axis labels
  labs(title = "Owner and Renter Vacancy Rate Over Time for Miami-Dade County", 
       x = "Years", 
       y = "Vacancy Rate (%)", 
       color = "") +  # Empty color label to avoid legend title
  theme_minimal(base_size = 14) +  # Professional minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered and bold title
    legend.position = "bottom",  # Move legend to bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

MIA
#BOTH
plot_data3 <- data.frame(
  Interval = rep(c('06_10', '07_11', '08_12', '09_13', '10_14', '11_15', 
                   '12_16', '13_17', '14_18', '15_19', '16_20', '17_21', '18_22'), 2),
  Dataset = c(rep("Miami-Dade", 13), rep("Leon", 13)),
  OwnerVacRate = c(MIA2$OwnerVacRate06_10, MIA2$OwnerVacRate07_11, MIA2$OwnerVacRate08_12, 
                   MIA2$OwnerVacRate09_13, MIA2$OwnerVacRate10_14, MIA2$OwnerVacRate11_15, 
                   MIA2$OwnerVacRate12_16, MIA2$OwnerVacRate13_17, MIA2$OwnerVacRate14_18, 
                   MIA2$OwnerVacRate15_19, MIA2$OwnerVacRate16_20, MIA2$OwnerVacRate17_21, 
                   MIA2$OwnerVacRate18_22,
                   LE2$OwnerVacRate06_10, LE2$OwnerVacRate07_11, LE2$OwnerVacRate08_12, 
                   LE2$OwnerVacRate09_13, LE2$OwnerVacRate10_14, LE2$OwnerVacRate11_15, 
                   LE2$OwnerVacRate12_16, LE2$OwnerVacRate13_17, LE2$OwnerVacRate14_18, 
                   LE2$OwnerVacRate15_19, LE2$OwnerVacRate16_20, LE2$OwnerVacRate17_21, 
                   LE2$OwnerVacRate18_22),
  RenterVacRate = c(MIA2$RenterVacRate06_10, MIA2$RenterVacRate07_11, MIA2$RenterVacRate08_12, 
                    MIA2$RenterVacRate09_13, MIA2$RenterVacRate10_14, MIA2$RenterVacRate11_15, 
                    MIA2$RenterVacRate12_16, MIA2$RenterVacRate13_17, MIA2$RenterVacRate14_18, 
                    MIA2$RenterVacRate15_19, MIA2$RenterVacRate16_20, MIA2$RenterVacRate17_21, 
                    MIA2$RenterVacRate18_22,
                    LE2$RenterVacRate06_10, LE2$RenterVacRate07_11, LE2$RenterVacRate08_12, 
                    LE2$RenterVacRate09_13, LE2$RenterVacRate10_14, LE2$RenterVacRate11_15, 
                    LE2$RenterVacRate12_16, LE2$RenterVacRate13_17, LE2$RenterVacRate14_18, 
                    LE2$RenterVacRate15_19, LE2$RenterVacRate16_20, LE2$RenterVacRate17_21, 
                    LE2$RenterVacRate18_22)
)

# Reshape the data so that we can plot Owner and Renter rates together
plot_data_melt3 <- melt(plot_data3, id.vars = c("Interval", "Dataset"), 
                       variable.name = "Type", value.name = "Rate")

# Combine Dataset and Type to differentiate each line
plot_data_melt3$LineType <- paste(plot_data_melt3$Dataset, plot_data_melt3$Type, sep = "_")

# Create the line plot with colors and legend
Both = ggplot(plot_data_melt3, aes(x = Interval, y = Rate, group = LineType, color = LineType)) +
  geom_line(size = 1.5) +  # Thicker line for visibility
  scale_color_manual(values = c("Miami-Dade_OwnerVacRate" = "#782F40", 
                                "Miami-Dade_RenterVacRate" = "#004B87", 
                                "Leon_OwnerVacRate" = "#CEB888", 
                                "Leon_RenterVacRate" = "#FFA500"),
                     labels = c("Miami-Dade Owner Vacancy Rate", 
                                "Miami-Dade Renter Vacancy Rate", 
                                "Leon Owner Vancancy Rate", 
                                "Leon Renter Vacancy Rate")) +  # Custom legend labels
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  scale_x_discrete(labels = c("06_10" = "Years 06-10", "07_11" = "Years 07-11", 
                              "08_12" = "Years 08-12", "09_13" = "Years 09-13", 
                              "10_14" = "Years 10-14", "11_15" = "Years 11-15", 
                              "12_16" = "Years 12-16", "13_17" = "Years 13-17", 
                              "14_18" = "Years 14-18", "15_19" = "Years 15-19", 
                              "16_20" = "Years 16-20", "17_21" = "Years 17-21", 
                              "18_22" = "Years 18-22")) +  # Custom x-axis labels
  labs(title = "Owner and Renter Vacancy Rates for Miami-Dade and Leon Over Time", 
       x = "Years", 
       y = "Occupancy Rate (%)", 
       color = "") +  # Empty color label to avoid legend title
  theme_minimal(base_size = 14) +  # Professional minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered and bold title
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10),  # Adjust legend text size for fitting
    legend.box = "horizontal",  # Arrange legend in horizontal layout
    legend.spacing.x = unit(0.5, 'cm'),  # Add spacing between legend items
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

Both
#Average Owner Vacancy Rate Leon
average_owner_Vac_rate_LE2 <- mean(c(
  mean(LE2$OwnerVacRate06_10, na.rm = TRUE),
  mean(LE2$OwnerVacRate07_11, na.rm = TRUE),
  mean(LE2$OwnerVacRate08_12, na.rm = TRUE),
  mean(LE2$OwnerVacRate09_13, na.rm = TRUE),
  mean(LE2$OwnerVacRate10_14, na.rm = TRUE),
  mean(LE2$OwnerVacRate11_15, na.rm = TRUE),
  mean(LE2$OwnerVacRate12_16, na.rm = TRUE),
  mean(LE2$OwnerVacRate13_17, na.rm = TRUE),
  mean(LE2$OwnerVacRate14_18, na.rm = TRUE),
  mean(LE2$OwnerVacRate15_19, na.rm = TRUE),
  mean(LE2$OwnerVacRate16_20, na.rm = TRUE),
  mean(LE2$OwnerVacRate17_21, na.rm = TRUE),
  mean(LE2$OwnerVacRate18_22, na.rm = TRUE)
), na.rm = TRUE)

#Average Renter Vacancy Rate Leon
average_renter_Vac_rate_LE2 <- mean(c(
  mean(LE2$RenterVacRate06_10, na.rm = TRUE),
  mean(LE2$RenterVacRate07_11, na.rm = TRUE),
  mean(LE2$RenterVacRate08_12, na.rm = TRUE),
  mean(LE2$RenterVacRate09_13, na.rm = TRUE),
  mean(LE2$RenterVacRate10_14, na.rm = TRUE),
  mean(LE2$RenterVacRate11_15, na.rm = TRUE),
  mean(LE2$RenterVacRate12_16, na.rm = TRUE),
  mean(LE2$RenterVacRate13_17, na.rm = TRUE),
  mean(LE2$RenterVacRate14_18, na.rm = TRUE),
  mean(LE2$RenterVacRate15_19, na.rm = TRUE),
  mean(LE2$RenterVacRate16_20, na.rm = TRUE),
  mean(LE2$RenterVacRate17_21, na.rm = TRUE),
  mean(LE2$RenterVacRate18_22, na.rm = TRUE)
), na.rm = TRUE)

#Average Owner Vacancy Rate Miami-Dade
average_owner_Vac_rate_MIA2 <- mean(c(
  mean(MIA2$OwnerVacRate06_10, na.rm = TRUE),
  mean(MIA2$OwnerVacRate07_11, na.rm = TRUE),
  mean(MIA2$OwnerVacRate08_12, na.rm = TRUE),
  mean(MIA2$OwnerVacRate09_13, na.rm = TRUE),
  mean(MIA2$OwnerVacRate10_14, na.rm = TRUE),
  mean(MIA2$OwnerVacRate11_15, na.rm = TRUE),
  mean(MIA2$OwnerVacRate12_16, na.rm = TRUE),
  mean(MIA2$OwnerVacRate13_17, na.rm = TRUE),
  mean(MIA2$OwnerVacRate14_18, na.rm = TRUE),
  mean(MIA2$OwnerVacRate15_19, na.rm = TRUE),
  mean(MIA2$OwnerVacRate16_20, na.rm = TRUE),
  mean(MIA2$OwnerVacRate17_21, na.rm = TRUE),
  mean(MIA2$OwnerVacRate18_22, na.rm = TRUE)
), na.rm = TRUE)

#Average Renter Vacancy Rate Miami-Dade
average_renter_Vac_rate_MIA2 <- mean(c(
  mean(MIA2$RenterVacRate06_10, na.rm = TRUE),
  mean(MIA2$RenterVacRate07_11, na.rm = TRUE),
  mean(MIA2$RenterVacRate08_12, na.rm = TRUE),
  mean(MIA2$RenterVacRate09_13, na.rm = TRUE),
  mean(MIA2$RenterVacRate10_14, na.rm = TRUE),
  mean(MIA2$RenterVacRate11_15, na.rm = TRUE),
  mean(MIA2$RenterVacRate12_16, na.rm = TRUE),
  mean(MIA2$RenterVacRate13_17, na.rm = TRUE),
  mean(MIA2$RenterVacRate14_18, na.rm = TRUE),
  mean(MIA2$RenterVacRate15_19, na.rm = TRUE),
  mean(MIA2$RenterVacRate16_20, na.rm = TRUE),
  mean(MIA2$RenterVacRate17_21, na.rm = TRUE),
  mean(MIA2$RenterVacRate18_22, na.rm = TRUE)
), na.rm = TRUE)

#Differences
# --- Step 1: Calculate the Differences from the Average ---

# Calculate renter Vacancy rate differences for LE2
LE2$RenterVacDiff06_10 <- LE2$RenterVacRate06_10 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff07_11 <- LE2$RenterVacRate07_11 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff08_12 <- LE2$RenterVacRate08_12 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff09_13 <- LE2$RenterVacRate09_13 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff10_14 <- LE2$RenterVacRate10_14 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff11_15 <- LE2$RenterVacRate11_15 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff12_16 <- LE2$RenterVacRate12_16 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff13_17 <- LE2$RenterVacRate13_17 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff14_18 <- LE2$RenterVacRate14_18 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff15_19 <- LE2$RenterVacRate15_19 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff16_20 <- LE2$RenterVacRate16_20 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff17_21 <- LE2$RenterVacRate17_21 - average_renter_Vac_rate_LE2
LE2$RenterVacDiff18_22 <- LE2$RenterVacRate18_22 - average_renter_Vac_rate_LE2

# Calculate renter Vacancy rate differences for MIA2
MIA2$RenterVacDiff06_10 <- MIA2$RenterVacRate06_10 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff07_11 <- MIA2$RenterVacRate07_11 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff08_12 <- MIA2$RenterVacRate08_12 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff09_13 <- MIA2$RenterVacRate09_13 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff10_14 <- MIA2$RenterVacRate10_14 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff11_15 <- MIA2$RenterVacRate11_15 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff12_16 <- MIA2$RenterVacRate12_16 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff13_17 <- MIA2$RenterVacRate13_17 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff14_18 <- MIA2$RenterVacRate14_18 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff15_19 <- MIA2$RenterVacRate15_19 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff16_20 <- MIA2$RenterVacRate16_20 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff17_21 <- MIA2$RenterVacRate17_21 - average_renter_Vac_rate_MIA2
MIA2$RenterVacDiff18_22 <- MIA2$RenterVacRate18_22 - average_renter_Vac_rate_MIA2

# Similarly calculate owner Vacancy rate differences for LE2
LE2$OwnerVacDiff06_10 <- LE2$OwnerVacRate06_10 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff07_11 <- LE2$OwnerVacRate07_11 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff08_12 <- LE2$OwnerVacRate08_12 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff09_13 <- LE2$OwnerVacRate09_13 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff10_14 <- LE2$OwnerVacRate10_14 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff11_15 <- LE2$OwnerVacRate11_15 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff12_16 <- LE2$OwnerVacRate12_16 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff13_17 <- LE2$OwnerVacRate13_17 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff14_18 <- LE2$OwnerVacRate14_18 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff15_19 <- LE2$OwnerVacRate15_19 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff16_20 <- LE2$OwnerVacRate16_20 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff17_21 <- LE2$OwnerVacRate17_21 - average_owner_Vac_rate_LE2
LE2$OwnerVacDiff18_22 <- LE2$OwnerVacRate18_22 - average_owner_Vac_rate_LE2

# And for MIA2
MIA2$OwnerVacDiff06_10 <- MIA2$OwnerVacRate06_10 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff07_11 <- MIA2$OwnerVacRate07_11 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff08_12 <- MIA2$OwnerVacRate08_12 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff09_13 <- MIA2$OwnerVacRate09_13 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff10_14 <- MIA2$OwnerVacRate10_14 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff11_15 <- MIA2$OwnerVacRate11_15 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff12_16 <- MIA2$OwnerVacRate12_16 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff13_17 <- MIA2$OwnerVacRate13_17 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff14_18 <- MIA2$OwnerVacRate14_18 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff15_19 <- MIA2$OwnerVacRate15_19 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff16_20 <- MIA2$OwnerVacRate16_20 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff17_21 <- MIA2$OwnerVacRate17_21 - average_owner_Vac_rate_MIA2
MIA2$OwnerVacDiff18_22 <- MIA2$OwnerVacRate18_22 - average_owner_Vac_rate_MIA2

# --- Step 1: Reshape the Data for Renter Occupancy Rate Differences ---

# Create a data frame for renter occupancy rate differences
renter_diff_df <- data.frame(
  Year = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  LE2_Diff = c(LE2$RenterVacDiff06_10, LE2$RenterVacDiff07_11, LE2$RenterVacDiff08_12, LE2$RenterVacDiff09_13, 
               LE2$RenterVacDiff10_14, LE2$RenterVacDiff11_15, LE2$RenterVacDiff12_16, LE2$RenterVacDiff13_17, 
               LE2$RenterVacDiff14_18, LE2$RenterVacDiff15_19, LE2$RenterVacDiff16_20, LE2$RenterVacDiff17_21, LE2$RenterVacDiff18_22),
  MIA2_Diff = c(MIA2$RenterVacDiff06_10, MIA2$RenterVacDiff07_11, MIA2$RenterVacDiff08_12, 
                MIA2$RenterVacDiff09_13, MIA2$RenterVacDiff10_14, MIA2$RenterVacDiff11_15, 
                MIA2$RenterVacDiff12_16, MIA2$RenterVacDiff13_17, MIA2$RenterVacDiff14_18, 
                MIA2$RenterVacDiff15_19, MIA2$RenterVacDiff16_20, MIA2$RenterVacDiff17_21, MIA2$RenterVacDiff18_22)
)

# Reshape the data for ggplot (long format)
renter_diff_melt <- melt(renter_diff_df, id.vars = "Year", variable.name = "Dataset", value.name = "Difference")

# Plot Renter Vacancy Rate Differences with percentage formatting and improved appearance
RenterDiff = ggplot(renter_diff_melt, aes(x = Year, y = Difference, color = Dataset, group = Dataset)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +   # Add points for clarity
  geom_text(aes(label = scales::percent(Difference, accuracy = 0.1)), vjust = -0.5, color = "black", size = 3) +  # Add values on points
  scale_color_manual(values = c("LE2_Diff" = "skyblue", "MIA2_Diff" = "#FFA500"), 
                     labels = c("Leon County Difference", "Miami-Dade Difference")) +  # Custom legend labels
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  scale_x_discrete(labels = c("06_10" = "Years 06-10", "07_11" = "Years 07-11", 
                              "08_12" = "Years 08-12", "09_13" = "Years 09-13", 
                              "10_14" = "Years 10-14", "11_15" = "Years 11-15", 
                              "12_16" = "Years 12-16", "13_17" = "Years 13-17", 
                              "14_18" = "Years 14-18", "15_19" = "Years 15-19", 
                              "16_20" = "Years 16-20", "17_21" = "Years 17-21", 
                              "18_22" = "Years 18-22")) +  # Custom x-axis labels
  labs(title = "Difference from Average Renter Vacancy Rate",
       y = "Difference from Average (%)",
       x = "Year Intervals",
       color = "Dataset") +
  theme_minimal(base_size = 14) +  # Professional minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered and bold title
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

# --- Step 3: Reshape the Data for Owner Occupancy Rate Differences ---

# Create a data frame for owner occupancy rate differences
owner_diff_df <- data.frame(
  Year = c("06_10", "07_11", "08_12", "09_13", "10_14", "11_15", "12_16", "13_17", "14_18", "15_19", "16_20", "17_21", "18_22"),
  LE2_Diff = c(LE2$OwnerVacDiff06_10, LE2$OwnerVacDiff07_11, LE2$OwnerVacDiff08_12, LE2$OwnerVacDiff09_13, 
               LE2$OwnerVacDiff10_14, LE2$OwnerVacDiff11_15, LE2$OwnerVacDiff12_16, LE2$OwnerVacDiff13_17, 
               LE2$OwnerVacDiff14_18, LE2$OwnerVacDiff15_19, LE2$OwnerVacDiff16_20, LE2$OwnerVacDiff17_21, LE2$OwnerVacDiff18_22),
  MIA2_Diff = c(MIA2$OwnerVacDiff06_10, MIA2$OwnerVacDiff07_11, MIA2$OwnerVacDiff08_12, 
                MIA2$OwnerVacDiff09_13, MIA2$OwnerVacDiff10_14, MIA2$OwnerVacDiff11_15, 
                MIA2$OwnerVacDiff12_16, MIA2$OwnerVacDiff13_17, MIA2$OwnerVacDiff14_18, 
                MIA2$OwnerVacDiff15_19, MIA2$OwnerVacDiff16_20, MIA2$OwnerVacDiff17_21, MIA2$OwnerVacDiff18_22)
)

# Reshape the data for ggplot (long format)
owner_diff_melt <- melt(owner_diff_df, id.vars = "Year", variable.name = "Dataset", value.name = "Difference")

# Plot Owner Vacancy Rate Differences with percentage formatting and improved appearance
OwnerDiff = ggplot(owner_diff_melt, aes(x = Year, y = Difference, color = Dataset, group = Dataset)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +   # Add points for clarity
  geom_text(aes(label = scales::percent(Difference, accuracy = 0.1)), vjust = -0.5, color = "black", size = 3) +  # Add values on points
  scale_color_manual(values = c("LE2_Diff" = "skyblue", "MIA2_Diff" = "#FFA500"), 
                     labels = c("Leon County Difference", "Miami-Dade Difference")) +  # Custom legend labels
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  scale_x_discrete(labels = c("06_10" = "Years 06-10", "07_11" = "Years 07-11", 
                              "08_12" = "Years 08-12", "09_13" = "Years 09-13", 
                              "10_14" = "Years 10-14", "11_15" = "Years 11-15", 
                              "12_16" = "Years 12-16", "13_17" = "Years 13-17", 
                              "14_18" = "Years 14-18", "15_19" = "Years 15-19", 
                              "16_20" = "Years 16-20", "17_21" = "Years 17-21", 
                              "18_22" = "Years 18-22")) +  # Custom x-axis labels
  labs(title = "Difference from Average Owner Vacancy Rate",
       y = "Difference from Average (%)",
       x = "Year Intervals",
       color = "Dataset") +
  theme_minimal(base_size = 14) +  # Professional minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered and bold title
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

ggsave("Owner/ Renter Vacancy Rate Leon.png", plot = Leon, width = 12, height = 15)
ggsave("Owner/ Renter Vacancy Rate Miami-Dade.png", plot = MIA, width = 12, height = 15)
ggsave("Owner/ Renter Vacancy Rate Both.png", plot = Both, width = 12, height = 15)
ggsave("Difference from Average Renter Vacancy Rate.png", plot = RenterDiff, width = 12, height = 12)
ggsave("Difference from Average Owner Vacancy Rate.png", plot = OwnerDiff, width = 12, height = 12)

Leon
MIA
Both
RenterDiff
OwnerDiff


#Calculate Housing Shortfall For Leon County
# Step 1: Define a function to calculate vacancy rate
calculate_vacancy_rate <- function(for_rent, renter_occ, rented_not_occ) {
  return(for_rent / (for_rent + renter_occ + rented_not_occ))
}

# Step 2: Calculate the vacancy rate for each five-year period
vacancy_rates <- LE2 %>%
  mutate(
    VacRate06_10 = calculate_vacancy_rate(ForRent06_10, RenterOcc06_10, RentedNotOcc06_10),
    VacRate07_11 = calculate_vacancy_rate(ForRent07_11, RenterOcc07_11, RentedNotOcc07_11),
    VacRate08_12 = calculate_vacancy_rate(ForRent08_12, RenterOcc08_12, RentedNotOcc08_12),
    VacRate09_13 = calculate_vacancy_rate(ForRent09_13, RenterOcc09_13, RentedNotOcc09_13),
    VacRate10_14 = calculate_vacancy_rate(ForRent10_14, RenterOcc10_14, RentedNotOcc10_14),
    VacRate11_15 = calculate_vacancy_rate(ForRent11_15, RenterOcc11_15, RentedNotOcc11_15),
    VacRate12_16 = calculate_vacancy_rate(ForRent12_16, RenterOcc12_16, RentedNotOcc12_16),
    VacRate13_17 = calculate_vacancy_rate(ForRent13_17, RenterOcc13_17, RentedNotOcc13_17),
    VacRate14_18 = calculate_vacancy_rate(ForRent14_18, RenterOcc14_18, RentedNotOcc14_18),
    VacRate15_19 = calculate_vacancy_rate(ForRent15_19, RenterOcc15_19, RentedNotOcc15_19),
    VacRate16_20 = calculate_vacancy_rate(ForRent16_20, RenterOcc16_20, RentedNotOcc16_20),
    VacRate17_21 = calculate_vacancy_rate(ForRent17_21, RenterOcc17_21, RentedNotOcc17_21),
    VacRate18_22 = calculate_vacancy_rate(ForRent18_22, RenterOcc18_22, RentedNotOcc18_22)
  )

# Step 3: Calculate the natural vacancy rate (average over all periods)
vacancy_rates <- vacancy_rates %>%
  mutate(
    NaturalVacRate = rowMeans(select(., starts_with("VacRate")), na.rm = TRUE)
  )

# Step 4: Calculate the actual housing shortage
# Multiply by the total number of occupied units (RenterOcc + OwnerOcc) to get actual shortage numbers
housing_shortage <- vacancy_rates %>%
  mutate(
    Shortage06_10 = (VacRate06_10 - NaturalVacRate) * (RenterOcc06_10 + OwnerOcc06_10),
    Shortage07_11 = (VacRate07_11 - NaturalVacRate) * (RenterOcc07_11 + OwnerOcc07_11),
    Shortage08_12 = (VacRate08_12 - NaturalVacRate) * (RenterOcc08_12 + OwnerOcc08_12),
    Shortage09_13 = (VacRate09_13 - NaturalVacRate) * (RenterOcc09_13 + OwnerOcc09_13),
    Shortage10_14 = (VacRate10_14 - NaturalVacRate) * (RenterOcc10_14 + OwnerOcc10_14),
    Shortage11_15 = (VacRate11_15 - NaturalVacRate) * (RenterOcc11_15 + OwnerOcc11_15),
    Shortage12_16 = (VacRate12_16 - NaturalVacRate) * (RenterOcc12_16 + OwnerOcc12_16),
    Shortage13_17 = (VacRate13_17 - NaturalVacRate) * (RenterOcc13_17 + OwnerOcc13_17),
    Shortage14_18 = (VacRate14_18 - NaturalVacRate) * (RenterOcc14_18 + OwnerOcc14_18),
    Shortage15_19 = (VacRate15_19 - NaturalVacRate) * (RenterOcc15_19 + OwnerOcc15_19),
    Shortage16_20 = (VacRate16_20 - NaturalVacRate) * (RenterOcc16_20 + OwnerOcc16_20),
    Shortage17_21 = (VacRate17_21 - NaturalVacRate) * (RenterOcc17_21 + OwnerOcc17_21),
    Shortage18_22 = (VacRate18_22 - NaturalVacRate) * (RenterOcc18_22 + OwnerOcc18_22)
  )

# Step 5: Create a table showing the actual housing shortage for each city and period
housing_shortage_table <- housing_shortage %>%
  select(starts_with("Shortage"))

# View the final table of actual housing shortages
print(housing_shortage_table)

library(gt)
# Create a nicely formatted table using gt
housing_shortage_table %>%
  gt() %>%
  tab_header(
    title = "Housing Shortage by Five-Year Period"
  ) %>%
  cols_label(
    Shortage06_10 = "Shortage 06-10",
    Shortage07_11 = "Shortage 07-11",
    Shortage08_12 = "Shortage 08-12",
    Shortage09_13 = "Shortage 09-13",
    Shortage10_14 = "Shortage 10-14",
    Shortage11_15 = "Shortage 11-15",
    Shortage12_16 = "Shortage 12-16",
    Shortage13_17 = "Shortage 13-17",
    Shortage14_18 = "Shortage 14-18",
    Shortage15_19 = "Shortage 15-19",
    Shortage16_20 = "Shortage 16-20",
    Shortage17_21 = "Shortage 17-21",
    Shortage18_22 = "Shortage 18-22"
  ) %>%
  fmt_number(
    columns = everything(),  # Format all numeric columns
    decimals = 0  # No decimal places for simplicity
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())  # Bold the column headers
  )

# Step 1: Transpose the housing shortage table for Leon County
transposed_shortage_table <- t(housing_shortage_table)

# Convert it back into a data frame for better handling
transposed_shortage_df <- as.data.frame(transposed_shortage_table)

# Rename the rownames to include the period (original column names)
transposed_shortage_df <- transposed_shortage_df %>%
  tibble::rownames_to_column(var = "Period")  # Use the column names (shortage periods) as rownames

# View the transposed data in a vertical format using kable
library(knitr)
kable(transposed_shortage_df, 
      caption = "Housing Shortage in Leon County by Five-Year Period", 
      col.names = c("Period", "Leon County"),  # Only one county now
      align = 'c')

library(ggpubr)

# Step 1: Manually create the correct period labels
periods <- c("06-10", "07-11", "08-12", "09-13", "10-14", 
             "11-15", "12-16", "13-17", "14-18", 
             "15-19", "16-20", "17-21", "18-22")

# Step 2: Create a data frame with proper periods and Leon County values
table_data <- data.frame(
  Period = paste("Shortage", periods, sep = " "),  # Create explicit period labels
  `Leon County` = round(as.numeric(transposed_shortage_df$V1), 2)  # Ensure numeric values are rounded
)

# Step 3: Create a professional table using ggpubr's ggtexttable with extra padding
table_plot <- ggtexttable(table_data, 
                          rows = NULL,  # No row names
                          theme = ttheme(
                            "minimal", 
                            base_size = 12, 
                            padding = unit(c(14, 10), "mm")  # Increase vertical padding (first value)
                          ))

# Step 4: Add more space between the title and the table
annotated_plot <- annotate_figure(table_plot, 
                                  top = text_grob("Housing Shortage in Leon County by Five-Year Period", 
                                                  face = "bold", size = 14, vjust = 2),  # Adjust vjust for more space
                                  fig.lab.face = "bold")

# Step 5: Increase the margin around the plot for better spacing
annotated_plot <- annotated_plot + 
  theme(plot.margin = margin(20, 10, 20, 10))  # Add margin to avoid overlap

# Display the plot
print(annotated_plot)

# Save as PDF
ggsave("Leon_County_Housing_Shortage.pdf", plot = annotated_plot, width = 8, height = 10)

# Create a new column in the data frame to customize the x-axis labels (replace underscore with hyphen)
transposed_shortage_df$Custom_Period <- gsub("_", "-", transposed_shortage_df$Period)
transposed_shortage_df$Custom_Period <- gsub("Shortage", "Years", transposed_shortage_df$Custom_Period)

# Create a line plot with custom x-axis labels and values on each point
Leon_Housing = ggplot(transposed_shortage_df, aes(x = Custom_Period, y = V1, group = 1)) +
  geom_line(color = "lightgreen", size = 1.2) +  # Line representing the housing shortage in blue
  geom_point(color = "black", size = 3) +  # Black points for each data point
  geom_text(aes(label = round(V1, 0)), vjust = -1, size = 3.5, color = "black") +  # Display values on each point
  
  # Custom theme for a more professional look
  theme_minimal() +  # Clean, minimalistic background
  ggtitle("Housing Shortage in Leon County by Five-Year Periods") +  # Add title
  xlab("Five-Year Period") +  # Label x-axis
  ylab("Housing Shortage (Units)") +  # Label y-axis
  
  # Customize x-axis labels to show "Years 06-10", "Years 07-11", etc.
  scale_x_discrete(labels = transposed_shortage_df$Custom_Period) +
  
  # Style customizations for title, axis labels, and text
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Y-axis label size
    axis.title.x = element_text(size = 14, face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 14, face = "bold"),  # Y-axis title styling
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title and bold it
    plot.margin = margin(10, 10, 10, 10)  # Add some margin for breathing space
  )

ggsave("Leon_County_HousingPlot.pdf", plot = Leon_Housing, width = 12, height = 8)

#write.csv(transposed_shortage_df, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/LeonTable.csv", row.names = FALSE)




#Calculate the Housing Shortfall for Miami-Dade
#Calculate Housing Shortfall For Leon County
# Step 1: Define a function to calculate vacancy rate
calculate_vacancy_rate2 <- function(for_rent, renter_occ, rented_not_occ) {
  return(for_rent / (for_rent + renter_occ + rented_not_occ))
}

# Step 2: Calculate the vacancy rate for each five-year period
vacancy_rates2 <- MIA2 %>%
  mutate(
    VacRate06_10 = calculate_vacancy_rate(ForRent06_10, RenterOcc06_10, RentedNotOcc06_10),
    VacRate07_11 = calculate_vacancy_rate(ForRent07_11, RenterOcc07_11, RentedNotOcc07_11),
    VacRate08_12 = calculate_vacancy_rate(ForRent08_12, RenterOcc08_12, RentedNotOcc08_12),
    VacRate09_13 = calculate_vacancy_rate(ForRent09_13, RenterOcc09_13, RentedNotOcc09_13),
    VacRate10_14 = calculate_vacancy_rate(ForRent10_14, RenterOcc10_14, RentedNotOcc10_14),
    VacRate11_15 = calculate_vacancy_rate(ForRent11_15, RenterOcc11_15, RentedNotOcc11_15),
    VacRate12_16 = calculate_vacancy_rate(ForRent12_16, RenterOcc12_16, RentedNotOcc12_16),
    VacRate13_17 = calculate_vacancy_rate(ForRent13_17, RenterOcc13_17, RentedNotOcc13_17),
    VacRate14_18 = calculate_vacancy_rate(ForRent14_18, RenterOcc14_18, RentedNotOcc14_18),
    VacRate15_19 = calculate_vacancy_rate(ForRent15_19, RenterOcc15_19, RentedNotOcc15_19),
    VacRate16_20 = calculate_vacancy_rate(ForRent16_20, RenterOcc16_20, RentedNotOcc16_20),
    VacRate17_21 = calculate_vacancy_rate(ForRent17_21, RenterOcc17_21, RentedNotOcc17_21),
    VacRate18_22 = calculate_vacancy_rate(ForRent18_22, RenterOcc18_22, RentedNotOcc18_22)
  )

# Step 3: Calculate the natural vacancy rate (average over all periods)
vacancy_rates2 <- vacancy_rates2 %>%
  mutate(
    NaturalVacRate = rowMeans(select(., starts_with("VacRate")), na.rm = TRUE)
  )

# Step 4: Calculate the actual housing shortage
# Multiply by the total number of occupied units (RenterOcc + OwnerOcc) to get actual shortage numbers
housing_shortage2 <- vacancy_rates2 %>%
  mutate(
    Shortage06_10 = (VacRate06_10 - NaturalVacRate) * (RenterOcc06_10 + OwnerOcc06_10),
    Shortage07_11 = (VacRate07_11 - NaturalVacRate) * (RenterOcc07_11 + OwnerOcc07_11),
    Shortage08_12 = (VacRate08_12 - NaturalVacRate) * (RenterOcc08_12 + OwnerOcc08_12),
    Shortage09_13 = (VacRate09_13 - NaturalVacRate) * (RenterOcc09_13 + OwnerOcc09_13),
    Shortage10_14 = (VacRate10_14 - NaturalVacRate) * (RenterOcc10_14 + OwnerOcc10_14),
    Shortage11_15 = (VacRate11_15 - NaturalVacRate) * (RenterOcc11_15 + OwnerOcc11_15),
    Shortage12_16 = (VacRate12_16 - NaturalVacRate) * (RenterOcc12_16 + OwnerOcc12_16),
    Shortage13_17 = (VacRate13_17 - NaturalVacRate) * (RenterOcc13_17 + OwnerOcc13_17),
    Shortage14_18 = (VacRate14_18 - NaturalVacRate) * (RenterOcc14_18 + OwnerOcc14_18),
    Shortage15_19 = (VacRate15_19 - NaturalVacRate) * (RenterOcc15_19 + OwnerOcc15_19),
    Shortage16_20 = (VacRate16_20 - NaturalVacRate) * (RenterOcc16_20 + OwnerOcc16_20),
    Shortage17_21 = (VacRate17_21 - NaturalVacRate) * (RenterOcc17_21 + OwnerOcc17_21),
    Shortage18_22 = (VacRate18_22 - NaturalVacRate) * (RenterOcc18_22 + OwnerOcc18_22)
  )

# Step 5: Create a table showing the actual housing shortage for each city and period
housing_shortage_table2 <- housing_shortage2 %>%
  select(starts_with("Shortage"))

# View the final table of actual housing shortages
print(housing_shortage_table2)

library(gt)
# Create a nicely formatted table using gt
housing_shortage_table2 %>%
  gt() %>%
  tab_header(
    title = "Housing Shortage by Five-Year Period"
  ) %>%
  cols_label(
    Shortage06_10 = "Shortage 06-10",
    Shortage07_11 = "Shortage 07-11",
    Shortage08_12 = "Shortage 08-12",
    Shortage09_13 = "Shortage 09-13",
    Shortage10_14 = "Shortage 10-14",
    Shortage11_15 = "Shortage 11-15",
    Shortage12_16 = "Shortage 12-16",
    Shortage13_17 = "Shortage 13-17",
    Shortage14_18 = "Shortage 14-18",
    Shortage15_19 = "Shortage 15-19",
    Shortage16_20 = "Shortage 16-20",
    Shortage17_21 = "Shortage 17-21",
    Shortage18_22 = "Shortage 18-22"
  ) %>%
  fmt_number(
    columns = everything(),  # Format all numeric columns
    decimals = 0  # No decimal places for simplicity
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())  # Bold the column headers
  )

# Step 1: Transpose the housing shortage table for Leon County
transposed_shortage_table2 <- t(housing_shortage_table2)

# Convert it back into a data frame for better handling
transposed_shortage_df2 <- as.data.frame(transposed_shortage_table2)

# Rename the rownames to include the period (original column names)
transposed_shortage_df2 <- transposed_shortage_df2 %>%
  tibble::rownames_to_column(var = "Period")  # Use the column names (shortage periods) as rownames

# View the transposed data in a vertical format using kable
library(knitr)
kable(transposed_shortage_df2, 
      caption = "Housing Shortage in Leon County by Five-Year Period", 
      col.names = c("Period", "Leon County"),  # Only one county now
      align = 'c')

library(ggpubr)

# Step 1: Manually create the correct period labels
periods2 <- c("06-10", "07-11", "08-12", "09-13", "10-14", 
             "11-15", "12-16", "13-17", "14-18", 
             "15-19", "16-20", "17-21", "18-22")

# Step 2: Create a data frame with proper periods and Leon County values
table_data2 <- data.frame(
  Period = paste("Shortage", periods, sep = " "),  # Create explicit period labels
  `Miami-Dade County` = round(as.numeric(transposed_shortage_df2$V1), 2)  # Ensure numeric values are rounded
)

# Step 3: Create a professional table using ggpubr's ggtexttable with extra padding
table_plot2 <- ggtexttable(table_data2, 
                          rows = NULL,  # No row names
                          theme = ttheme(
                            "minimal", 
                            base_size = 12, 
                            padding = unit(c(14, 10), "mm")  # Increase vertical padding (first value)
                          ))

# Step 4: Add more space between the title and the table
annotated_plot2 <- annotate_figure(table_plot2, 
                                  top = text_grob("Housing Shortage in Miami-Dade County by Five-Year Period", 
                                                  face = "bold", size = 14, vjust = 2),  # Adjust vjust for more space
                                  fig.lab.face = "bold")

# Step 5: Increase the margin around the plot for better spacing
annotated_plot2 <- annotated_plot2 + 
  theme(plot.margin = margin(20, 10, 20, 10))  # Add margin to avoid overlap

# Display the plot
print(annotated_plot2)

# Save as PDF
ggsave("Miami_County_Housing_Shortage.pdf", plot = annotated_plot2, width = 8, height = 10)

#write.csv(transposed_shortage_df2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/MIATable.csv", row.names = FALSE)

# Create a new column in the data frame to customize the x-axis labels (replace underscore with hyphen)
transposed_shortage_df2$Custom_Period <- gsub("_", "-", transposed_shortage_df2$Period)
transposed_shortage_df2$Custom_Period <- gsub("Shortage", "Years", transposed_shortage_df2$Custom_Period)

# Create a line plot with custom x-axis labels and values on each point
Miami_Housing = ggplot(transposed_shortage_df2, aes(x = Custom_Period, y = V1, group = 1)) +
  geom_line(color = "lightgreen", size = 1.2) +  # Line representing the housing shortage in blue
  geom_point(color = "black", size = 3) +  # Black points for each data point
  geom_text(aes(label = round(V1, 0)), vjust = -1, size = 3.5, color = "black") +  # Display values on each point
  
  # Custom theme for a more professional look
  theme_minimal() +  # Clean, minimalistic background
  ggtitle("Housing Shortage in Miami-Dade County by Five-Year Periods") +  # Add title
  xlab("Five-Year Period") +  # Label x-axis
  ylab("Housing Shortage (Units)") +  # Label y-axis
  
  # Customize x-axis labels to show "Years 06-10", "Years 07-11", etc.
  scale_x_discrete(labels = transposed_shortage_df2$Custom_Period) +
  
  # Style customizations for title, axis labels, and text
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Y-axis label size
    axis.title.x = element_text(size = 14, face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 14, face = "bold"),  # Y-axis title styling
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title and bold it
    plot.margin = margin(10, 10, 10, 10)  # Add some margin for breathing space
  )
ggsave("Miami_County_HousingPlot.pdf", plot = Miami_Housing, width = 12, height = 8)

#write.csv(transposed_shortage_df2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/MIATable.csv", row.names = FALSE)


#Both Housing Shortage Line Plot
# Step 1: Add a column to indicate the county for Leon and Miami datasets
transposed_shortage_df$County <- "Leon County"
transposed_shortage_df2$County <- "Miami-Dade County"

# Step 2: Combine the two datasets
combined_data <- bind_rows(
  transposed_shortage_df %>% select(Custom_Period, V1, County),
  transposed_shortage_df2 %>% select(Custom_Period, V1, County)
)

# Step 3: Create the line plot with both counties
Both_HousingShortage = ggplot(combined_data, aes(x = Custom_Period, y = V1, color = County, group = County)) +
  geom_line(size = 1.2) +  # Plot lines for both counties
  geom_point(size = 3) +  # Add points at each data point
  theme_minimal() +  # Minimalistic theme
  ggtitle("Housing Shortage in Leon County and Miami-Dade County by Five-Year Period") +  # Title
  xlab("Five-Year Period") +  # X-axis label
  ylab("Housing Shortage (Units)") +  # Y-axis label
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_color_manual(values = c("Leon County" = "#782F40", "Miami-Dade County" = "#CEB888")) +  # Assign colors
  geom_text(aes(label = round(V1, 0)), vjust = -1, size = 3, color = "black")  # Display values in black on the points

ggsave("Both Housing Shortages.pdf", plot = Both_HousingShortage, width = 12, height = 8)


#write.csv(LE2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/LE2.csv", row.names = FALSE)
#write.csv(MIA2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/MIA2.csv", row.names = FALSE)

#Calculating actual owner and renter shortages for Leon
NaturalRenterVacRate <- mean(c(LE2$RenterVacRate06_10, LE2$RenterVacRate07_11, LE2$RenterVacRate08_12,
                               LE2$RenterVacRate09_13, LE2$RenterVacRate10_14, LE2$RenterVacRate11_15,
                               LE2$RenterVacRate12_16, LE2$RenterVacRate13_17, LE2$RenterVacRate14_18,
                               LE2$RenterVacRate15_19, LE2$RenterVacRate16_20, LE2$RenterVacRate17_21,
                               LE2$RenterVacRate18_22), na.rm = TRUE)

NaturalOwnerVacRate <- mean(c(LE2$OwnerVacRate06_10, LE2$OwnerVacRate07_11, LE2$OwnerVacRate08_12,
                              LE2$OwnerVacRate09_13, LE2$OwnerVacRate10_14, LE2$OwnerVacRate11_15,
                              LE2$OwnerVacRate12_16, LE2$OwnerVacRate13_17, LE2$OwnerVacRate14_18,
                              LE2$OwnerVacRate15_19, LE2$OwnerVacRate16_20, LE2$OwnerVacRate17_21,
                              LE2$OwnerVacRate18_22), na.rm = TRUE)


LE2 <- LE2 %>%
  mutate(
    # Step 1: Calculate Renter Shortages for each period
    RenterShortage06_10 = ( (RenterOcc06_10 + RentedNotOcc06_10) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc06_10 + RentedNotOcc06_10 + ForRent06_10),
    RenterShortage07_11 = ( (RenterOcc07_11 + RentedNotOcc07_11) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc07_11 + RentedNotOcc07_11 + ForRent07_11),
    RenterShortage08_12 = ( (RenterOcc08_12 + RentedNotOcc08_12) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc08_12 + RentedNotOcc08_12 + ForRent08_12),
    RenterShortage09_13 = ( (RenterOcc09_13 + RentedNotOcc09_13) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc09_13 + RentedNotOcc09_13 + ForRent09_13),
    RenterShortage10_14 = ( (RenterOcc10_14 + RentedNotOcc10_14) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc10_14 + RentedNotOcc10_14 + ForRent10_14),
    RenterShortage11_15 = ( (RenterOcc11_15 + RentedNotOcc11_15) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc11_15 + RentedNotOcc11_15 + ForRent11_15),
    RenterShortage12_16 = ( (RenterOcc12_16 + RentedNotOcc12_16) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc12_16 + RentedNotOcc12_16 + ForRent12_16),
    RenterShortage13_17 = ( (RenterOcc13_17 + RentedNotOcc13_17) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc13_17 + RentedNotOcc13_17 + ForRent13_17),
    RenterShortage14_18 = ( (RenterOcc14_18 + RentedNotOcc14_18) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc14_18 + RentedNotOcc14_18 + ForRent14_18),
    RenterShortage15_19 = ( (RenterOcc15_19 + RentedNotOcc15_19) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc15_19 + RentedNotOcc15_19 + ForRent15_19),
    RenterShortage16_20 = ( (RenterOcc16_20 + RentedNotOcc16_20) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc16_20 + RentedNotOcc16_20 + ForRent16_20),
    RenterShortage17_21 = ( (RenterOcc17_21 + RentedNotOcc17_21) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc17_21 + RentedNotOcc17_21 + ForRent17_21),
    RenterShortage18_22 = ( (RenterOcc18_22 + RentedNotOcc18_22) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc18_22 + RentedNotOcc18_22 + ForRent18_22),
    
    # Step 2: Calculate Owner Shortages for each period
    OwnerShortage06_10 = ( (OwnerOcc06_10 + SoldNotOcc06_10) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc06_10 + SoldNotOcc06_10 + ForSale06_10),
    OwnerShortage07_11 = ( (OwnerOcc07_11 + SoldNotOcc07_11) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc07_11 + SoldNotOcc07_11 + ForSale07_11),
    OwnerShortage08_12 = ( (OwnerOcc08_12 + SoldNotOcc08_12) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc08_12 + SoldNotOcc08_12 + ForSale08_12),
    OwnerShortage09_13 = ( (OwnerOcc09_13 + SoldNotOcc09_13) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc09_13 + SoldNotOcc09_13 + ForSale09_13),
    OwnerShortage10_14 = ( (OwnerOcc10_14 + SoldNotOcc10_14) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc10_14 + SoldNotOcc10_14 + ForSale10_14),
    OwnerShortage11_15 = ( (OwnerOcc11_15 + SoldNotOcc11_15) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc11_15 + SoldNotOcc11_15 + ForSale11_15),
    OwnerShortage12_16 = ( (OwnerOcc12_16 + SoldNotOcc12_16) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc12_16 + SoldNotOcc12_16 + ForSale12_16),
    OwnerShortage13_17 = ( (OwnerOcc13_17 + SoldNotOcc13_17) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc13_17 + SoldNotOcc13_17 + ForSale13_17),
    OwnerShortage14_18 = ( (OwnerOcc14_18 + SoldNotOcc14_18) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc14_18 + SoldNotOcc14_18 + ForSale14_18),
    OwnerShortage15_19 = ( (OwnerOcc15_19 + SoldNotOcc15_19) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc15_19 + SoldNotOcc15_19 + ForSale15_19),
    OwnerShortage16_20 = ( (OwnerOcc16_20 + SoldNotOcc16_20) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc16_20 + SoldNotOcc16_20 + ForSale16_20),
    OwnerShortage17_21 = ( (OwnerOcc17_21 + SoldNotOcc17_21) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc17_21 + SoldNotOcc17_21 + ForSale17_21),
    OwnerShortage18_22 = ( (OwnerOcc18_22 + SoldNotOcc18_22) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc18_22 + SoldNotOcc18_22 + ForSale18_22),
    
    # Step 3: Calculate Cumulative Shortages for each period
    CumulativeShortage06_10 = RenterShortage06_10 + OwnerShortage06_10,
    CumulativeShortage07_11 = RenterShortage07_11 + OwnerShortage07_11,
    CumulativeShortage08_12 = RenterShortage08_12 + OwnerShortage08_12,
    CumulativeShortage09_13 = RenterShortage09_13 + OwnerShortage09_13,
    CumulativeShortage10_14 = RenterShortage10_14 + OwnerShortage10_14,
    CumulativeShortage11_15 = RenterShortage11_15 + OwnerShortage11_15,
    CumulativeShortage12_16 = RenterShortage12_16 + OwnerShortage12_16,
    CumulativeShortage13_17 = RenterShortage13_17 + OwnerShortage13_17,
    CumulativeShortage14_18 = RenterShortage14_18 + OwnerShortage14_18,
    CumulativeShortage15_19 = RenterShortage15_19 + OwnerShortage15_19,
    CumulativeShortage16_20 = RenterShortage16_20 + OwnerShortage16_20,
    CumulativeShortage17_21 = RenterShortage17_21 + OwnerShortage17_21,
    CumulativeShortage18_22 = RenterShortage18_22 + OwnerShortage18_22
  )
    
    
# The resulting columns now contain the housing shortages or surpluses for renters and owners across all periods.
# These columns are directly added to the existing dataset 'LE2', allowing for further analysis or visualization.
library(writexl)

# Select the relevant columns for exporting (shortages and cumulative shortages)
shortage_table <- data.frame(
  Period = c("Shortage 2006-2010", "Shortage 2007-2011", "Shortage 2008-2012", 
             "Shortage 2009-2013", "Shortage 2010-2014", "Shortage 2011-2015", 
             "Shortage 2012-2016", "Shortage 2013-2017", "Shortage 2014-2018", 
             "Shortage 2015-2019", "Shortage 2016-2020", "Shortage 2017-2021", 
             "Shortage 2018-2022"),
  
  `Owner Housing Shortage` = c(LE2$OwnerShortage06_10, LE2$OwnerShortage07_11, LE2$OwnerShortage08_12, 
                               LE2$OwnerShortage09_13, LE2$OwnerShortage10_14, LE2$OwnerShortage11_15, 
                               LE2$OwnerShortage12_16, LE2$OwnerShortage13_17, LE2$OwnerShortage14_18, 
                               LE2$OwnerShortage15_19, LE2$OwnerShortage16_20, LE2$OwnerShortage17_21, 
                               LE2$OwnerShortage18_22),
  
  `Renter Housing Shortage` = c(LE2$RenterShortage06_10, LE2$RenterShortage07_11, LE2$RenterShortage08_12, 
                                LE2$RenterShortage09_13, LE2$RenterShortage10_14, LE2$RenterShortage11_15, 
                                LE2$RenterShortage12_16, LE2$RenterShortage13_17, LE2$RenterShortage14_18, 
                                LE2$RenterShortage15_19, LE2$RenterShortage16_20, LE2$RenterShortage17_21, 
                                LE2$RenterShortage18_22),
  
  `Cumulative Housing Shortage` = c(LE2$CumulativeShortage06_10, LE2$CumulativeShortage07_11, LE2$CumulativeShortage08_12, 
                                    LE2$CumulativeShortage09_13, LE2$CumulativeShortage10_14, LE2$CumulativeShortage11_15, 
                                    LE2$CumulativeShortage12_16, LE2$CumulativeShortage13_17, LE2$CumulativeShortage14_18, 
                                    LE2$CumulativeShortage15_19, LE2$CumulativeShortage16_20, LE2$CumulativeShortage17_21, 
                                    LE2$CumulativeShortage18_22)
)


# Export the new data frame to an Excel file
write_xlsx(shortage_table, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/R Code/housing_shortages_detailed.xlsx")



#Miami Actual Renter and Housing Shortage
MIA2 <- MIA2 %>%
  mutate(
    # Step 1: Calculate Renter Shortages for each period
    RenterShortage06_10 = ( (RenterOcc06_10 + RentedNotOcc06_10) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc06_10 + RentedNotOcc06_10 + ForRent06_10),
    RenterShortage07_11 = ( (RenterOcc07_11 + RentedNotOcc07_11) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc07_11 + RentedNotOcc07_11 + ForRent07_11),
    RenterShortage08_12 = ( (RenterOcc08_12 + RentedNotOcc08_12) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc08_12 + RentedNotOcc08_12 + ForRent08_12),
    RenterShortage09_13 = ( (RenterOcc09_13 + RentedNotOcc09_13) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc09_13 + RentedNotOcc09_13 + ForRent09_13),
    RenterShortage10_14 = ( (RenterOcc10_14 + RentedNotOcc10_14) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc10_14 + RentedNotOcc10_14 + ForRent10_14),
    RenterShortage11_15 = ( (RenterOcc11_15 + RentedNotOcc11_15) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc11_15 + RentedNotOcc11_15 + ForRent11_15),
    RenterShortage12_16 = ( (RenterOcc12_16 + RentedNotOcc12_16) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc12_16 + RentedNotOcc12_16 + ForRent12_16),
    RenterShortage13_17 = ( (RenterOcc13_17 + RentedNotOcc13_17) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc13_17 + RentedNotOcc13_17 + ForRent13_17),
    RenterShortage14_18 = ( (RenterOcc14_18 + RentedNotOcc14_18) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc14_18 + RentedNotOcc14_18 + ForRent14_18),
    RenterShortage15_19 = ( (RenterOcc15_19 + RentedNotOcc15_19) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc15_19 + RentedNotOcc15_19 + ForRent15_19),
    RenterShortage16_20 = ( (RenterOcc16_20 + RentedNotOcc16_20) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc16_20 + RentedNotOcc16_20 + ForRent16_20),
    RenterShortage17_21 = ( (RenterOcc17_21 + RentedNotOcc17_21) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc17_21 + RentedNotOcc17_21 + ForRent17_21),
    RenterShortage18_22 = ( (RenterOcc18_22 + RentedNotOcc18_22) / (1 - NaturalRenterVacRate) ) - 
      (RenterOcc18_22 + RentedNotOcc18_22 + ForRent18_22),
    
    # Step 2: Calculate Owner Shortages for each period
    OwnerShortage06_10 = ( (OwnerOcc06_10 + SoldNotOcc06_10) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc06_10 + SoldNotOcc06_10 + ForSale06_10),
    OwnerShortage07_11 = ( (OwnerOcc07_11 + SoldNotOcc07_11) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc07_11 + SoldNotOcc07_11 + ForSale07_11),
    OwnerShortage08_12 = ( (OwnerOcc08_12 + SoldNotOcc08_12) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc08_12 + SoldNotOcc08_12 + ForSale08_12),
    OwnerShortage09_13 = ( (OwnerOcc09_13 + SoldNotOcc09_13) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc09_13 + SoldNotOcc09_13 + ForSale09_13),
    OwnerShortage10_14 = ( (OwnerOcc10_14 + SoldNotOcc10_14) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc10_14 + SoldNotOcc10_14 + ForSale10_14),
    OwnerShortage11_15 = ( (OwnerOcc11_15 + SoldNotOcc11_15) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc11_15 + SoldNotOcc11_15 + ForSale11_15),
    OwnerShortage12_16 = ( (OwnerOcc12_16 + SoldNotOcc12_16) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc12_16 + SoldNotOcc12_16 + ForSale12_16),
    OwnerShortage13_17 = ( (OwnerOcc13_17 + SoldNotOcc13_17) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc13_17 + SoldNotOcc13_17 + ForSale13_17),
    OwnerShortage14_18 = ( (OwnerOcc14_18 + SoldNotOcc14_18) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc14_18 + SoldNotOcc14_18 + ForSale14_18),
    OwnerShortage15_19 = ( (OwnerOcc15_19 + SoldNotOcc15_19) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc15_19 + SoldNotOcc15_19 + ForSale15_19),
    OwnerShortage16_20 = ( (OwnerOcc16_20 + SoldNotOcc16_20) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc16_20 + SoldNotOcc16_20 + ForSale16_20),
    OwnerShortage17_21 = ( (OwnerOcc17_21 + SoldNotOcc17_21) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc17_21 + SoldNotOcc17_21 + ForSale17_21),
    OwnerShortage18_22 = ( (OwnerOcc18_22 + SoldNotOcc18_22) / (1 - NaturalOwnerVacRate) ) - 
      (OwnerOcc18_22 + SoldNotOcc18_22 + ForSale18_22),
    
    # Step 3: Calculate Cumulative Shortages for each period
    CumulativeShortage06_10 = RenterShortage06_10 + OwnerShortage06_10,
    CumulativeShortage07_11 = RenterShortage07_11 + OwnerShortage07_11,
    CumulativeShortage08_12 = RenterShortage08_12 + OwnerShortage08_12,
    CumulativeShortage09_13 = RenterShortage09_13 + OwnerShortage09_13,
    CumulativeShortage10_14 = RenterShortage10_14 + OwnerShortage10_14,
    CumulativeShortage11_15 = RenterShortage11_15 + OwnerShortage11_15,
    CumulativeShortage12_16 = RenterShortage12_16 + OwnerShortage12_16,
    CumulativeShortage13_17 = RenterShortage13_17 + OwnerShortage13_17,
    CumulativeShortage14_18 = RenterShortage14_18 + OwnerShortage14_18,
    CumulativeShortage15_19 = RenterShortage15_19 + OwnerShortage15_19,
    CumulativeShortage16_20 = RenterShortage16_20 + OwnerShortage16_20,
    CumulativeShortage17_21 = RenterShortage17_21 + OwnerShortage17_21,
    CumulativeShortage18_22 = RenterShortage18_22 + OwnerShortage18_22
  )


# Select the relevant columns for exporting (shortages and cumulative shortages)
shortage_table2 <- data.frame(
  Period = c("Shortage 2006-2010", "Shortage 2007-2011", "Shortage 2008-2012", 
             "Shortage 2009-2013", "Shortage 2010-2014", "Shortage 2011-2015", 
             "Shortage 2012-2016", "Shortage 2013-2017", "Shortage 2014-2018", 
             "Shortage 2015-2019", "Shortage 2016-2020", "Shortage 2017-2021", 
             "Shortage 2018-2022"),
  
  `Owner Housing Shortage` = c(MIA2$OwnerShortage06_10, MIA2$OwnerShortage07_11, MIA2$OwnerShortage08_12, 
                               MIA2$OwnerShortage09_13, MIA2$OwnerShortage10_14, MIA2$OwnerShortage11_15, 
                               MIA2$OwnerShortage12_16, MIA2$OwnerShortage13_17, MIA2$OwnerShortage14_18, 
                               MIA2$OwnerShortage15_19, MIA2$OwnerShortage16_20, MIA2$OwnerShortage17_21, 
                               MIA2$OwnerShortage18_22),
  
  `Renter Housing Shortage` = c(MIA2$RenterShortage06_10, MIA2$RenterShortage07_11, MIA2$RenterShortage08_12, 
                                MIA2$RenterShortage09_13, MIA2$RenterShortage10_14, MIA2$RenterShortage11_15, 
                                MIA2$RenterShortage12_16, MIA2$RenterShortage13_17, MIA2$RenterShortage14_18, 
                                MIA2$RenterShortage15_19, MIA2$RenterShortage16_20, MIA2$RenterShortage17_21, 
                                MIA2$RenterShortage18_22),
  
  `Cumulative Housing Shortage` = c(MIA2$CumulativeShortage06_10, MIA2$CumulativeShortage07_11, MIA2$CumulativeShortage08_12, 
                                    MIA2$CumulativeShortage09_13, MIA2$CumulativeShortage10_14, MIA2$CumulativeShortage11_15, 
                                    MIA2$CumulativeShortage12_16, MIA2$CumulativeShortage13_17, MIA2$CumulativeShortage14_18, 
                                    MIA2$CumulativeShortage15_19, MIA2$CumulativeShortage16_20, MIA2$CumulativeShortage17_21, 
                                    MIA2$CumulativeShortage18_22)
)


# Export the new data frame to an Excel file
write_xlsx(shortage_table2, "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/R Code/housing_shortages_detailedMIA.xlsx")

# Step 1: Create a combined data frame with owner and renter shortages for both counties
plot_data <- data.frame(
  Period = c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", 
             "2012-2016", "2013-2017", "2014-2018", "2015-2019", "2016-2020", "2017-2021", "2018-2022"),
  
  # Renter and Owner shortages for Leon County (LE2)
  LeonCountyRenter = c(LE2$RenterShortage06_10, LE2$RenterShortage07_11, LE2$RenterShortage08_12, 
                       LE2$RenterShortage09_13, LE2$RenterShortage10_14, LE2$RenterShortage11_15, 
                       LE2$RenterShortage12_16, LE2$RenterShortage13_17, LE2$RenterShortage14_18, 
                       LE2$RenterShortage15_19, LE2$RenterShortage16_20, LE2$RenterShortage17_21, 
                       LE2$RenterShortage18_22),
  
  LeonCountyOwner = c(LE2$OwnerShortage06_10, LE2$OwnerShortage07_11, LE2$OwnerShortage08_12, 
                      LE2$OwnerShortage09_13, LE2$OwnerShortage10_14, LE2$OwnerShortage11_15, 
                      LE2$OwnerShortage12_16, LE2$OwnerShortage13_17, LE2$OwnerShortage14_18, 
                      LE2$OwnerShortage15_19, LE2$OwnerShortage16_20, LE2$OwnerShortage17_21, 
                      LE2$OwnerShortage18_22),
  
  # Renter and Owner shortages for Miami-Dade County (MIA2)
  MiamiDadeRenter = c(MIA2$RenterShortage06_10, MIA2$RenterShortage07_11, MIA2$RenterShortage08_12, 
                      MIA2$RenterShortage09_13, MIA2$RenterShortage10_14, MIA2$RenterShortage11_15, 
                      MIA2$RenterShortage12_16, MIA2$RenterShortage13_17, MIA2$RenterShortage14_18, 
                      MIA2$RenterShortage15_19, MIA2$RenterShortage16_20, MIA2$RenterShortage17_21, 
                      MIA2$RenterShortage18_22),
  
  MiamiDadeOwner = c(MIA2$OwnerShortage06_10, MIA2$OwnerShortage07_11, MIA2$OwnerShortage08_12, 
                     MIA2$OwnerShortage09_13, MIA2$OwnerShortage10_14, MIA2$OwnerShortage11_15, 
                     MIA2$OwnerShortage12_16, MIA2$OwnerShortage13_17, MIA2$OwnerShortage14_18, 
                     MIA2$OwnerShortage15_19, MIA2$OwnerShortage16_20, MIA2$OwnerShortage17_21, 
                     MIA2$OwnerShortage18_22)
)

# Step 2: Convert the data into long format for easier plotting with ggplot2
plot_data_long <- reshape2::melt(plot_data, id.vars = "Period", variable.name = "Type", value.name = "Shortage")

# Step 3: Plot the line graph using ggplot2, with dots for Leon County and triangles for Miami-Dade County
plot <- ggplot(plot_data_long, aes(x = factor(Period), y = Shortage, color = Type, group = Type)) + 
  geom_line(size = 1) + 
  
  # Add points for each county with different shapes
  geom_point(data = subset(plot_data_long, grepl("LeonCounty", Type)), shape = 16, size = 3) +  # Dots for Leon County
  geom_point(data = subset(plot_data_long, grepl("MiamiDade", Type)), shape = 17, size = 3) +  # Triangles for Miami-Dade County
  
  # Add labels and title
  labs(title = "Renter and Owner Housing Shortages: Leon County vs Miami-Dade County",
       x = "Time Period",
       y = "Housing Shortage",
       color = "Shortage Type") +
  
  # Rotate x-axis labels for better readability (45 degrees)
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
  
  # Make the plot look more presentable
  theme_minimal()

plot
# Step 4: Save the plot as a PDF
ggsave("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/R Code/housing_shortages_comparison.pdf", plot = plot, width = 14, height = 8)


