setwd("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Keith Research/Data Sets/nhgis0004_csv")
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)
library(scales) #for percent formatting
library(writexl)

Florida <- read_csv("nhgis0004_ts_nominal_county.csv")

#Drop all states except Florida
Florida <- subset(Florida, STATE == "Florida")

Florida <- Florida %>%
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

Florida <- Florida %>%
  select(c(COUNTY, ForRent06_10, ForRent07_11, ForRent08_12, ForRent09_13, ForRent10_14, ForRent11_15, ForRent12_16, ForRent13_17, ForRent14_18, ForRent15_19, ForRent16_20, ForRent17_21, ForRent18_22,
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

#write.csv(Florida, "Florida.csv", row.names = FALSE)

# Step 1: Calculate renter and owner vacancy rates for each county and each period
calculate_vacancy_rates <- function(data) {
  data %>%
    mutate(
      # Calculate renter vacancy rates for each time period
      RenterVacRate06_10 = ForRent06_10 / (ForRent06_10 + RenterOcc06_10 + RentedNotOcc06_10),
      RenterVacRate07_11 = ForRent07_11 / (ForRent07_11 + RenterOcc07_11 + RentedNotOcc07_11),
      RenterVacRate08_12 = ForRent08_12 / (ForRent08_12 + RenterOcc08_12 + RentedNotOcc08_12),
      RenterVacRate09_13 = ForRent09_13 / (ForRent09_13 + RenterOcc09_13 + RentedNotOcc09_13),
      RenterVacRate10_14 = ForRent10_14 / (ForRent10_14 + RenterOcc10_14 + RentedNotOcc10_14),
      RenterVacRate11_15 = ForRent11_15 / (ForRent11_15 + RenterOcc11_15 + RentedNotOcc11_15),
      RenterVacRate12_16 = ForRent12_16 / (ForRent12_16 + RenterOcc12_16 + RentedNotOcc12_16),
      RenterVacRate13_17 = ForRent13_17 / (ForRent13_17 + RenterOcc13_17 + RentedNotOcc13_17),
      RenterVacRate14_18 = ForRent14_18 / (ForRent14_18 + RenterOcc14_18 + RentedNotOcc14_18),
      RenterVacRate15_19 = ForRent15_19 / (ForRent15_19 + RenterOcc15_19 + RentedNotOcc15_19),
      RenterVacRate16_20 = ForRent16_20 / (ForRent16_20 + RenterOcc16_20 + RentedNotOcc16_20),
      RenterVacRate17_21 = ForRent17_21 / (ForRent17_21 + RenterOcc17_21 + RentedNotOcc17_21),
      RenterVacRate18_22 = ForRent18_22 / (ForRent18_22 + RenterOcc18_22 + RentedNotOcc18_22),
      
      # Calculate owner vacancy rates for each time period
      OwnerVacRate06_10 = ForSale06_10 / (ForSale06_10 + OwnerOcc06_10 + SoldNotOcc06_10),
      OwnerVacRate07_11 = ForSale07_11 / (ForSale07_11 + OwnerOcc07_11 + SoldNotOcc07_11),
      OwnerVacRate08_12 = ForSale08_12 / (ForSale08_12 + OwnerOcc08_12 + SoldNotOcc08_12),
      OwnerVacRate09_13 = ForSale09_13 / (ForSale09_13 + OwnerOcc09_13 + SoldNotOcc09_13),
      OwnerVacRate10_14 = ForSale10_14 / (ForSale10_14 + OwnerOcc10_14 + SoldNotOcc10_14),
      OwnerVacRate11_15 = ForSale11_15 / (ForSale11_15 + OwnerOcc11_15 + SoldNotOcc11_15),
      OwnerVacRate12_16 = ForSale12_16 / (ForSale12_16 + OwnerOcc12_16 + SoldNotOcc12_16),
      OwnerVacRate13_17 = ForSale13_17 / (ForSale13_17 + OwnerOcc13_17 + SoldNotOcc13_17),
      OwnerVacRate14_18 = ForSale14_18 / (ForSale14_18 + OwnerOcc14_18 + SoldNotOcc14_18),
      OwnerVacRate15_19 = ForSale15_19 / (ForSale15_19 + OwnerOcc15_19 + SoldNotOcc15_19),
      OwnerVacRate16_20 = ForSale16_20 / (ForSale16_20 + OwnerOcc16_20 + SoldNotOcc16_20),
      OwnerVacRate17_21 = ForSale17_21 / (ForSale17_21 + OwnerOcc17_21 + SoldNotOcc17_21),
      OwnerVacRate18_22 = ForSale18_22 / (ForSale18_22 + OwnerOcc18_22 + SoldNotOcc18_22)
    )
}

# Step 2: Calculate natural vacancy rates for each county (average across all periods)
calculate_natural_vacancy_rates <- function(data) {
  data %>%
    mutate(
      # Natural Renter Vacancy Rate (average of renter vacancy rates across all periods)
      NaturalRenterVacRate = rowMeans(select(data, starts_with("RenterVacRate")), na.rm = TRUE),
      
      # Natural Owner Vacancy Rate (average of owner vacancy rates across all periods)
      NaturalOwnerVacRate = rowMeans(select(data, starts_with("OwnerVacRate")), na.rm = TRUE)
    )
}

# Step 3: Apply the vacancy rate calculations to the Florida dataset
Florida_with_vacancy_rates <- Florida %>%
  calculate_vacancy_rates() %>%
  calculate_natural_vacancy_rates()

# Step 4: Calculate the renter and owner housing shortages based on the natural vacancy rates
calculate_shortages <- function(data) {
  data %>%
    mutate(
      # Renter Housing Shortages
      RenterShortage06_10 = ((RenterOcc06_10 + RentedNotOcc06_10) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc06_10 + RentedNotOcc06_10 + ForRent06_10),
      RenterShortage07_11 = ((RenterOcc07_11 + RentedNotOcc07_11) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc07_11 + RentedNotOcc07_11 + ForRent07_11),
      RenterShortage08_12 = ((RenterOcc08_12 + RentedNotOcc08_12) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc08_12 + RentedNotOcc08_12 + ForRent08_12),
      RenterShortage09_13 = ((RenterOcc09_13 + RentedNotOcc09_13) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc09_13 + RentedNotOcc09_13 + ForRent09_13),
      RenterShortage10_14 = ((RenterOcc10_14 + RentedNotOcc10_14) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc10_14 + RentedNotOcc10_14 + ForRent10_14),
      RenterShortage11_15 = ((RenterOcc11_15 + RentedNotOcc11_15) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc11_15 + RentedNotOcc11_15 + ForRent11_15),
      RenterShortage12_16 = ((RenterOcc12_16 + RentedNotOcc12_16) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc12_16 + RentedNotOcc12_16 + ForRent12_16),
      RenterShortage13_17 = ((RenterOcc13_17 + RentedNotOcc13_17) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc13_17 + RentedNotOcc13_17 + ForRent13_17),
      RenterShortage14_18 = ((RenterOcc14_18 + RentedNotOcc14_18) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc14_18 + RentedNotOcc14_18 + ForRent14_18),
      RenterShortage15_19 = ((RenterOcc15_19 + RentedNotOcc15_19) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc15_19 + RentedNotOcc15_19 + ForRent15_19),
      RenterShortage16_20 = ((RenterOcc16_20 + RentedNotOcc16_20) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc16_20 + RentedNotOcc16_20 + ForRent16_20),
      RenterShortage17_21 = ((RenterOcc17_21 + RentedNotOcc17_21) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc17_21 + RentedNotOcc17_21 + ForRent17_21),
      RenterShortage18_22 = ((RenterOcc18_22 + RentedNotOcc18_22) / (1 - NaturalRenterVacRate)) - 
        (RenterOcc18_22 + RentedNotOcc18_22 + ForRent18_22),
      
      # Owner Housing Shortages
      OwnerShortage06_10 = ((OwnerOcc06_10 + SoldNotOcc06_10) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc06_10 + SoldNotOcc06_10 + ForSale06_10),
      OwnerShortage07_11 = ((OwnerOcc07_11 + SoldNotOcc07_11) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc07_11 + SoldNotOcc07_11 + ForSale07_11),
      OwnerShortage08_12 = ((OwnerOcc08_12 + SoldNotOcc08_12) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc08_12 + SoldNotOcc08_12 + ForSale08_12),
      OwnerShortage09_13 = ((OwnerOcc09_13 + SoldNotOcc09_13) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc09_13 + SoldNotOcc09_13 + ForSale09_13),
      OwnerShortage10_14 = ((OwnerOcc10_14 + SoldNotOcc10_14) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc10_14 + SoldNotOcc10_14 + ForSale10_14),
      OwnerShortage11_15 = ((OwnerOcc11_15 + SoldNotOcc11_15) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc11_15 + SoldNotOcc11_15 + ForSale11_15),
      OwnerShortage12_16 = ((OwnerOcc12_16 + SoldNotOcc12_16) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc12_16 + SoldNotOcc12_16 + ForSale12_16),
      OwnerShortage13_17 = ((OwnerOcc13_17 + SoldNotOcc13_17) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc13_17 + SoldNotOcc13_17 + ForSale13_17),
      OwnerShortage14_18 = ((OwnerOcc14_18 + SoldNotOcc14_18) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc14_18 + SoldNotOcc14_18 + ForSale14_18),
      OwnerShortage15_19 = ((OwnerOcc15_19 + SoldNotOcc15_19) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc15_19 + SoldNotOcc15_19 + ForSale15_19),
      OwnerShortage16_20 = ((OwnerOcc16_20 + SoldNotOcc16_20) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc16_20 + SoldNotOcc16_20 + ForSale16_20),
      OwnerShortage17_21 = ((OwnerOcc17_21 + SoldNotOcc17_21) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc17_21 + SoldNotOcc17_21 + ForSale17_21),
      OwnerShortage18_22 = ((OwnerOcc18_22 + SoldNotOcc18_22) / (1 - NaturalOwnerVacRate)) - 
        (OwnerOcc18_22 + SoldNotOcc18_22 + ForSale18_22),
      
      # Cumulative Housing Shortages (Renter + Owner)
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
}

# Step 5: Apply the shortage calculations to the dataset
Florida_with_shortages <- Florida_with_vacancy_rates %>%
  calculate_shortages()

# Step 6: Export the final results to Excel
write.csv(Florida_with_shortages, "Florida Shortages.csv", row.names = FALSE)



library(openxlsx)

# Step 1: Prepare tables for each county with the specified columns
prepare_county_table <- function(data) {
  # Create a table for each county
  county_table <- data.frame(
    Period = c("2006-2010", "2007-2011", "2008-2012", "2009-2013",
               "2010-2014", "2011-2015", "2012-2016", "2013-2017", 
               "2014-2018", "2015-2019", "2016-2020", "2017-2021", 
               "2018-2022"),
    Owner.Housing.Shortage = c(data$OwnerShortage06_10, data$OwnerShortage07_11, data$OwnerShortage08_12, 
                               data$OwnerShortage09_13, data$OwnerShortage10_14, data$OwnerShortage11_15, 
                               data$OwnerShortage12_16, data$OwnerShortage13_17, data$OwnerShortage14_18, 
                               data$OwnerShortage15_19, data$OwnerShortage16_20, data$OwnerShortage17_21, 
                               data$OwnerShortage18_22),
    Renter.Housing.Shortage = c(data$RenterShortage06_10, data$RenterShortage07_11, data$RenterShortage08_12, 
                                data$RenterShortage09_13, data$RenterShortage10_14, data$RenterShortage11_15, 
                                data$RenterShortage12_16, data$RenterShortage13_17, data$RenterShortage14_18, 
                                data$RenterShortage15_19, data$RenterShortage16_20, data$RenterShortage17_21, 
                                data$RenterShortage18_22),
    Cumulative.Housing.Shortage = c(data$CumulativeShortage06_10, data$CumulativeShortage07_11, 
                                    data$CumulativeShortage08_12, data$CumulativeShortage09_13, 
                                    data$CumulativeShortage10_14, data$CumulativeShortage11_15, 
                                    data$CumulativeShortage12_16, data$CumulativeShortage13_17, 
                                    data$CumulativeShortage14_18, data$CumulativeShortage15_19, 
                                    data$CumulativeShortage16_20, data$CumulativeShortage17_21, 
                                    data$CumulativeShortage18_22)
  )
  
  return(county_table)
}

# Step 2: Create a line plot for renter and owner housing shortages
create_line_plot <- function(county_table, county_name) {
  # Reshape the data to long format for ggplot
  county_table_long <- county_table %>%
    pivot_longer(cols = c(Owner.Housing.Shortage, Renter.Housing.Shortage), 
                 names_to = "Housing.Type", values_to = "Shortage")
  
  # Create the plot
  ggplot(county_table_long, aes(x = Period, y = Shortage, group = Housing.Type, color = Housing.Type)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = paste(county_name, "- Renter and Owner Housing Shortage"),
         x = "Time Period",
         y = "Housing Shortage",
         color = "Housing Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Owner.Housing.Shortage" = "blue", "Renter.Housing.Shortage" = "red"))
}

# Step 3: Apply the functions to each county and create the plots and tables
county_tables <- Florida_with_shortages %>%
  group_by(COUNTY) %>%
  group_split() %>%
  lapply(prepare_county_table)

# Get the names of the counties
county_names <- unique(Florida_with_shortages$COUNTY)

# Create a workbook
wb <- createWorkbook()

for (i in 1:length(county_tables)) {
  # Add worksheet for each county
  addWorksheet(wb, sheetName = county_names[i])
  
  # Write the data to the worksheet
  writeData(wb, county_names[i], county_tables[[i]])
  
  # Create the plot
  plot <- create_line_plot(county_tables[[i]], county_names[i])
  
  # Save the plot as an image in the sheet
  plot_image_file <- paste0(tempfile(), ".png")  # Save plot temporarily as a PNG
  ggsave(plot_image_file, plot, width = 7, height = 5, dpi = 300)
  
  # Insert the plot into the worksheet
  insertImage(wb, sheet = county_names[i], file = plot_image_file, startCol = 8, startRow = 3, width = 7, height = 5)
}

# Save the workbook with all counties' sheets
saveWorkbook(wb, "Florida_Housing_Shortages_with_Plots.xlsx", overwrite = TRUE)




# Step 1: Calculate total housing shortage for owners and renters for all counties
calculate_total_shortage <- function(data) {
  total_shortage <- data %>%
    summarize(
      TotalOwnerShortage06_10 = sum(OwnerShortage06_10, na.rm = TRUE),
      TotalOwnerShortage07_11 = sum(OwnerShortage07_11, na.rm = TRUE),
      TotalOwnerShortage08_12 = sum(OwnerShortage08_12, na.rm = TRUE),
      TotalOwnerShortage09_13 = sum(OwnerShortage09_13, na.rm = TRUE),
      TotalOwnerShortage10_14 = sum(OwnerShortage10_14, na.rm = TRUE),
      TotalOwnerShortage11_15 = sum(OwnerShortage11_15, na.rm = TRUE),
      TotalOwnerShortage12_16 = sum(OwnerShortage12_16, na.rm = TRUE),
      TotalOwnerShortage13_17 = sum(OwnerShortage13_17, na.rm = TRUE),
      TotalOwnerShortage14_18 = sum(OwnerShortage14_18, na.rm = TRUE),
      TotalOwnerShortage15_19 = sum(OwnerShortage15_19, na.rm = TRUE),
      TotalOwnerShortage16_20 = sum(OwnerShortage16_20, na.rm = TRUE),
      TotalOwnerShortage17_21 = sum(OwnerShortage17_21, na.rm = TRUE),
      TotalOwnerShortage18_22 = sum(OwnerShortage18_22, na.rm = TRUE),
      
      TotalRenterShortage06_10 = sum(RenterShortage06_10, na.rm = TRUE),
      TotalRenterShortage07_11 = sum(RenterShortage07_11, na.rm = TRUE),
      TotalRenterShortage08_12 = sum(RenterShortage08_12, na.rm = TRUE),
      TotalRenterShortage09_13 = sum(RenterShortage09_13, na.rm = TRUE),
      TotalRenterShortage10_14 = sum(RenterShortage10_14, na.rm = TRUE),
      TotalRenterShortage11_15 = sum(RenterShortage11_15, na.rm = TRUE),
      TotalRenterShortage12_16 = sum(RenterShortage12_16, na.rm = TRUE),
      TotalRenterShortage13_17 = sum(RenterShortage13_17, na.rm = TRUE),
      TotalRenterShortage14_18 = sum(RenterShortage14_18, na.rm = TRUE),
      TotalRenterShortage15_19 = sum(RenterShortage15_19, na.rm = TRUE),
      TotalRenterShortage16_20 = sum(RenterShortage16_20, na.rm = TRUE),
      TotalRenterShortage17_21 = sum(RenterShortage17_21, na.rm = TRUE),
      TotalRenterShortage18_22 = sum(RenterShortage18_22, na.rm = TRUE)
    )
  
  # Create the total shortage table in long format
  total_shortage_long <- tibble(
    Period = c("2006-2010", "2007-2011", "2008-2012", "2009-2013",
               "2010-2014", "2011-2015", "2012-2016", "2013-2017", 
               "2014-2018", "2015-2019", "2016-2020", "2017-2021", 
               "2018-2022"),
    `Owner Housing Shortage` = c(total_shortage$TotalOwnerShortage06_10, total_shortage$TotalOwnerShortage07_11, 
                                 total_shortage$TotalOwnerShortage08_12, total_shortage$TotalOwnerShortage09_13, 
                                 total_shortage$TotalOwnerShortage10_14, total_shortage$TotalOwnerShortage11_15, 
                                 total_shortage$TotalOwnerShortage12_16, total_shortage$TotalOwnerShortage13_17, 
                                 total_shortage$TotalOwnerShortage14_18, total_shortage$TotalOwnerShortage15_19, 
                                 total_shortage$TotalOwnerShortage16_20, total_shortage$TotalOwnerShortage17_21, 
                                 total_shortage$TotalOwnerShortage18_22),
    `Renter Housing Shortage` = c(total_shortage$TotalRenterShortage06_10, total_shortage$TotalRenterShortage07_11, 
                                  total_shortage$TotalRenterShortage08_12, total_shortage$TotalRenterShortage09_13, 
                                  total_shortage$TotalRenterShortage10_14, total_shortage$TotalRenterShortage11_15, 
                                  total_shortage$TotalRenterShortage12_16, total_shortage$TotalRenterShortage13_17, 
                                  total_shortage$TotalRenterShortage14_18, total_shortage$TotalRenterShortage15_19, 
                                  total_shortage$TotalRenterShortage16_20, total_shortage$TotalRenterShortage17_21, 
                                  total_shortage$TotalRenterShortage18_22),
    `Cumulative Housing Shortage` = `Owner Housing Shortage` + `Renter Housing Shortage`
  )
  
  return(total_shortage_long)
}

# Step 2: Create a line plot for the total owner and renter housing shortages
create_total_line_plot <- function(total_table) {
  # Reshape the data to long format for ggplot
  total_table_long <- total_table %>%
    pivot_longer(cols = c(`Owner Housing Shortage`, `Renter Housing Shortage`), 
                 names_to = "Housing Type", values_to = "Shortage")
  
  # Create the plot
  ggplot(total_table_long, aes(x = Period, y = Shortage, group = `Housing Type`, color = `Housing Type`)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Total Renter and Owner Housing Shortage Across All Counties",
         x = "Time Period",
         y = "Housing Shortage",
         color = "Housing Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Owner Housing Shortage" = "blue", "Renter Housing Shortage" = "red"))
}

# Step 3: Calculate total shortages for all counties
total_shortage_table <- calculate_total_shortage(Florida_with_shortages)

# Step 4: Create the total housing shortage line plot
total_shortage_plot <- create_total_line_plot(total_shortage_table)

# Step 5: Write the total shortage table and plot to an Excel file
wb <- createWorkbook()

# Add worksheet for total housing shortage
addWorksheet(wb, sheetName = "Total Housing Shortage")

# Write the data to the worksheet
writeData(wb, "Total Housing Shortage", total_shortage_table)

# Save the plot as an image and insert into the worksheet
plot_image_file <- paste0(tempfile(), ".png")  # Save plot temporarily as a PNG
ggsave(plot_image_file, total_shortage_plot, width = 7, height = 5, dpi = 300)

# Insert the plot into the worksheet
insertImage(wb, sheet = "Total Housing Shortage", file = plot_image_file, startCol = 8, startRow = 3, width = 7, height = 5)

# Save the workbook
saveWorkbook(wb, "Total_Housing_Shortages_All_Counties.xlsx", overwrite = TRUE)


#write.csv(Florida_with_shortages, "Florida Shortages.csv", row.names = FALSE)





# Step 1: Calculate renter and owner housing shortages for each period
calculate_housing_shortages <- function(data) {
  data %>%
    mutate(
      # Renter Vacancy Difference and Housing Shortage Calculation
      RenterVacDiff06_10 = NaturalRenterVacRate - RenterVacRate06_10,
      RenterShortage06_10 = RenterVacDiff06_10 * (RenterOcc06_10 + RentedNotOcc06_10 + ForRent06_10),
      
      RenterVacDiff07_11 = NaturalRenterVacRate - RenterVacRate07_11,
      RenterShortage07_11 = RenterVacDiff07_11 * (RenterOcc07_11 + RentedNotOcc07_11 + ForRent07_11),
      
      RenterVacDiff08_12 = NaturalRenterVacRate - RenterVacRate08_12,
      RenterShortage08_12 = RenterVacDiff08_12 * (RenterOcc08_12 + RentedNotOcc08_12 + ForRent08_12),
      
      RenterVacDiff09_13 = NaturalRenterVacRate - RenterVacRate09_13,
      RenterShortage09_13 = RenterVacDiff09_13 * (RenterOcc09_13 + RentedNotOcc09_13 + ForRent09_13),
      
      RenterVacDiff10_14 = NaturalRenterVacRate - RenterVacRate10_14,
      RenterShortage10_14 = RenterVacDiff10_14 * (RenterOcc10_14 + RentedNotOcc10_14 + ForRent10_14),
      
      RenterVacDiff11_15 = NaturalRenterVacRate - RenterVacRate11_15,
      RenterShortage11_15 = RenterVacDiff11_15 * (RenterOcc11_15 + RentedNotOcc11_15 + ForRent11_15),
      
      RenterVacDiff12_16 = NaturalRenterVacRate - RenterVacRate12_16,
      RenterShortage12_16 = RenterVacDiff12_16 * (RenterOcc12_16 + RentedNotOcc12_16 + ForRent12_16),
      
      RenterVacDiff13_17 = NaturalRenterVacRate - RenterVacRate13_17,
      RenterShortage13_17 = RenterVacDiff13_17 * (RenterOcc13_17 + RentedNotOcc13_17 + ForRent13_17),
      
      RenterVacDiff14_18 = NaturalRenterVacRate - RenterVacRate14_18,
      RenterShortage14_18 = RenterVacDiff14_18 * (RenterOcc14_18 + RentedNotOcc14_18 + ForRent14_18),
      
      RenterVacDiff15_19 = NaturalRenterVacRate - RenterVacRate15_19,
      RenterShortage15_19 = RenterVacDiff15_19 * (RenterOcc15_19 + RentedNotOcc15_19 + ForRent15_19),
      
      RenterVacDiff16_20 = NaturalRenterVacRate - RenterVacRate16_20,
      RenterShortage16_20 = RenterVacDiff16_20 * (RenterOcc16_20 + RentedNotOcc16_20 + ForRent16_20),
      
      RenterVacDiff17_21 = NaturalRenterVacRate - RenterVacRate17_21,
      RenterShortage17_21 = RenterVacDiff17_21 * (RenterOcc17_21 + RentedNotOcc17_21 + ForRent17_21),
      
      RenterVacDiff18_22 = NaturalRenterVacRate - RenterVacRate18_22,
      RenterShortage18_22 = RenterVacDiff18_22 * (RenterOcc18_22 + RentedNotOcc18_22 + ForRent18_22),
      
      # Owner Vacancy Difference and Housing Shortage Calculation
      OwnerVacDiff06_10 = NaturalOwnerVacRate - OwnerVacRate06_10,
      OwnerShortage06_10 = OwnerVacDiff06_10 * (OwnerOcc06_10 + SoldNotOcc06_10 + ForSale06_10),
      
      OwnerVacDiff07_11 = NaturalOwnerVacRate - OwnerVacRate07_11,
      OwnerShortage07_11 = OwnerVacDiff07_11 * (OwnerOcc07_11 + SoldNotOcc07_11 + ForSale07_11),
      
      OwnerVacDiff08_12 = NaturalOwnerVacRate - OwnerVacRate08_12,
      OwnerShortage08_12 = OwnerVacDiff08_12 * (OwnerOcc08_12 + SoldNotOcc08_12 + ForSale08_12),
      
      OwnerVacDiff09_13 = NaturalOwnerVacRate - OwnerVacRate09_13,
      OwnerShortage09_13 = OwnerVacDiff09_13 * (OwnerOcc09_13 + SoldNotOcc09_13 + ForSale09_13),
      
      OwnerVacDiff10_14 = NaturalOwnerVacRate - OwnerVacRate10_14,
      OwnerShortage10_14 = OwnerVacDiff10_14 * (OwnerOcc10_14 + SoldNotOcc10_14 + ForSale10_14),
      
      OwnerVacDiff11_15 = NaturalOwnerVacRate - OwnerVacRate11_15,
      OwnerShortage11_15 = OwnerVacDiff11_15 * (OwnerOcc11_15 + SoldNotOcc11_15 + ForSale11_15),
      
      OwnerVacDiff12_16 = NaturalOwnerVacRate - OwnerVacRate12_16,
      OwnerShortage12_16 = OwnerVacDiff12_16 * (OwnerOcc12_16 + SoldNotOcc12_16 + ForSale12_16),
      
      OwnerVacDiff13_17 = NaturalOwnerVacRate - OwnerVacRate13_17,
      OwnerShortage13_17 = OwnerVacDiff13_17 * (OwnerOcc13_17 + SoldNotOcc13_17 + ForSale13_17),
      
      OwnerVacDiff14_18 = NaturalOwnerVacRate - OwnerVacRate14_18,
      OwnerShortage14_18 = OwnerVacDiff14_18 * (OwnerOcc14_18 + SoldNotOcc14_18 + ForSale14_18),
      
      OwnerVacDiff15_19 = NaturalOwnerVacRate - OwnerVacRate15_19,
      OwnerShortage15_19 = OwnerVacDiff15_19 * (OwnerOcc15_19 + SoldNotOcc15_19 + ForSale15_19),
      
      OwnerVacDiff16_20 = NaturalOwnerVacRate - OwnerVacRate16_20,
      OwnerShortage16_20 = OwnerVacDiff16_20 * (OwnerOcc16_20 + SoldNotOcc16_20 + ForSale16_20),
      
      OwnerVacDiff17_21 = NaturalOwnerVacRate - OwnerVacRate17_21,
      OwnerShortage17_21 = OwnerVacDiff17_21 * (OwnerOcc17_21 + SoldNotOcc17_21 + ForSale17_21),
      
      OwnerVacDiff18_22 = NaturalOwnerVacRate - OwnerVacRate18_22,
      OwnerShortage18_22 = OwnerVacDiff18_22 * (OwnerOcc18_22 + SoldNotOcc18_22 + ForSale18_22),
      
      # Cumulative Housing Shortage
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
}


# Helper function to ensure unique sheet names
make_unique_sheet_name <- function(wb, sheet_name) {
  count <- 1
  new_name <- sheet_name
  while (new_name %in% names(wb)) {
    new_name <- paste0(sheet_name, "_", count)
    count <- count + 1
  }
  return(new_name)
}

# Step 1: Prepare tables for each county with the specified columns
prepare_county_table <- function(data) {
  # Create a table for each county
  county_table <- data.frame(
    Period = c("2006-2010", "2007-2011", "2008-2012", "2009-2013",
               "2010-2014", "2011-2015", "2012-2016", "2013-2017", 
               "2014-2018", "2015-2019", "2016-2020", "2017-2021", 
               "2018-2022"),
    Owner.Housing.Shortage = c(data$OwnerShortage06_10, data$OwnerShortage07_11, data$OwnerShortage08_12, 
                               data$OwnerShortage09_13, data$OwnerShortage10_14, data$OwnerShortage11_15, 
                               data$OwnerShortage12_16, data$OwnerShortage13_17, data$OwnerShortage14_18, 
                               data$OwnerShortage15_19, data$OwnerShortage16_20, data$OwnerShortage17_21, 
                               data$OwnerShortage18_22),
    Renter.Housing.Shortage = c(data$RenterShortage06_10, data$RenterShortage07_11, data$RenterShortage08_12, 
                                data$RenterShortage09_13, data$RenterShortage10_14, data$RenterShortage11_15, 
                                data$RenterShortage12_16, data$RenterShortage13_17, data$RenterShortage14_18, 
                                data$RenterShortage15_19, data$RenterShortage16_20, data$RenterShortage17_21, 
                                data$RenterShortage18_22),
    Cumulative.Housing.Shortage = c(data$CumulativeShortage06_10, data$CumulativeShortage07_11, 
                                    data$CumulativeShortage08_12, data$CumulativeShortage09_13, 
                                    data$CumulativeShortage10_14, data$CumulativeShortage11_15, 
                                    data$CumulativeShortage12_16, data$CumulativeShortage13_17, 
                                    data$CumulativeShortage14_18, data$CumulativeShortage15_19, 
                                    data$CumulativeShortage16_20, data$CumulativeShortage17_21, 
                                    data$CumulativeShortage18_22)
  )
  
  return(county_table)
}

# Step 2: Create a line plot for renter and owner housing shortages
create_line_plot <- function(county_table, county_name) {
  # Reshape the data to long format for ggplot
  county_table_long <- county_table %>%
    pivot_longer(cols = c(Owner.Housing.Shortage, Renter.Housing.Shortage), 
                 names_to = "Housing.Type", values_to = "Shortage")
  
  # Create the plot
  ggplot(county_table_long, aes(x = Period, y = Shortage, group = Housing.Type, color = Housing.Type)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = paste(county_name, "- Renter and Owner Housing Shortage"),
         x = "Time Period",
         y = "Housing Shortage",
         color = "Housing Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Owner.Housing.Shortage" = "blue", "Renter.Housing.Shortage" = "red"))
}

# Step 3: Apply the functions to each county and create the plots and tables
county_tables <- Florida_with_housing_shortages %>%
  group_by(COUNTY) %>%
  group_split() %>%
  lapply(prepare_county_table)

# Get the names of the counties
county_names <- unique(Florida_with_housing_shortages$COUNTY)

# Create a workbook
wb <- createWorkbook()

for (i in 1:length(county_tables)) {
  # Add worksheet for each county
  addWorksheet(wb, sheetName = county_names[i])
  
  # Write the data to the worksheet
  writeData(wb, county_names[i], county_tables[[i]])
  
  # Create the plot
  plot <- create_line_plot(county_tables[[i]], county_names[i])
  
  # Save the plot as an image in the sheet
  plot_image_file <- paste0(tempfile(), ".png")  # Save plot temporarily as a PNG
  ggsave(plot_image_file, plot, width = 7, height = 5, dpi = 300)
  
  # Insert the plot into the worksheet
  insertImage(wb, sheet = county_names[i], file = plot_image_file, startCol = 8, startRow = 3, width = 7, height = 5)
}

# Save the workbook with all counties' sheets
saveWorkbook(wb, "Florida_Housing_Shortages_with_Plots.xlsx", overwrite = TRUE)


#TOTAL HOUSING SHORTAGE FOR FLORIDA

# Step 1: Calculate total housing shortage for owners and renters for all counties
calculate_total_shortage <- function(data) {
  total_shortage <- data %>%
    summarize(
      TotalOwnerShortage06_10 = sum(OwnerShortage06_10, na.rm = TRUE),
      TotalOwnerShortage07_11 = sum(OwnerShortage07_11, na.rm = TRUE),
      TotalOwnerShortage08_12 = sum(OwnerShortage08_12, na.rm = TRUE),
      TotalOwnerShortage09_13 = sum(OwnerShortage09_13, na.rm = TRUE),
      TotalOwnerShortage10_14 = sum(OwnerShortage10_14, na.rm = TRUE),
      TotalOwnerShortage11_15 = sum(OwnerShortage11_15, na.rm = TRUE),
      TotalOwnerShortage12_16 = sum(OwnerShortage12_16, na.rm = TRUE),
      TotalOwnerShortage13_17 = sum(OwnerShortage13_17, na.rm = TRUE),
      TotalOwnerShortage14_18 = sum(OwnerShortage14_18, na.rm = TRUE),
      TotalOwnerShortage15_19 = sum(OwnerShortage15_19, na.rm = TRUE),
      TotalOwnerShortage16_20 = sum(OwnerShortage16_20, na.rm = TRUE),
      TotalOwnerShortage17_21 = sum(OwnerShortage17_21, na.rm = TRUE),
      TotalOwnerShortage18_22 = sum(OwnerShortage18_22, na.rm = TRUE),
      
      TotalRenterShortage06_10 = sum(RenterShortage06_10, na.rm = TRUE),
      TotalRenterShortage07_11 = sum(RenterShortage07_11, na.rm = TRUE),
      TotalRenterShortage08_12 = sum(RenterShortage08_12, na.rm = TRUE),
      TotalRenterShortage09_13 = sum(RenterShortage09_13, na.rm = TRUE),
      TotalRenterShortage10_14 = sum(RenterShortage10_14, na.rm = TRUE),
      TotalRenterShortage11_15 = sum(RenterShortage11_15, na.rm = TRUE),
      TotalRenterShortage12_16 = sum(RenterShortage12_16, na.rm = TRUE),
      TotalRenterShortage13_17 = sum(RenterShortage13_17, na.rm = TRUE),
      TotalRenterShortage14_18 = sum(RenterShortage14_18, na.rm = TRUE),
      TotalRenterShortage15_19 = sum(RenterShortage15_19, na.rm = TRUE),
      TotalRenterShortage16_20 = sum(RenterShortage16_20, na.rm = TRUE),
      TotalRenterShortage17_21 = sum(RenterShortage17_21, na.rm = TRUE),
      TotalRenterShortage18_22 = sum(RenterShortage18_22, na.rm = TRUE)
    )
  
  # Create the total shortage table in long format
  total_shortage_long <- tibble(
    Period = c("2006-2010", "2007-2011", "2008-2012", "2009-2013",
               "2010-2014", "2011-2015", "2012-2016", "2013-2017", 
               "2014-2018", "2015-2019", "2016-2020", "2017-2021", 
               "2018-2022"),
    `Owner Housing Shortage` = c(total_shortage$TotalOwnerShortage06_10, total_shortage$TotalOwnerShortage07_11, 
                                 total_shortage$TotalOwnerShortage08_12, total_shortage$TotalOwnerShortage09_13, 
                                 total_shortage$TotalOwnerShortage10_14, total_shortage$TotalOwnerShortage11_15, 
                                 total_shortage$TotalOwnerShortage12_16, total_shortage$TotalOwnerShortage13_17, 
                                 total_shortage$TotalOwnerShortage14_18, total_shortage$TotalOwnerShortage15_19, 
                                 total_shortage$TotalOwnerShortage16_20, total_shortage$TotalOwnerShortage17_21, 
                                 total_shortage$TotalOwnerShortage18_22),
    `Renter Housing Shortage` = c(total_shortage$TotalRenterShortage06_10, total_shortage$TotalRenterShortage07_11, 
                                  total_shortage$TotalRenterShortage08_12, total_shortage$TotalRenterShortage09_13, 
                                  total_shortage$TotalRenterShortage10_14, total_shortage$TotalRenterShortage11_15, 
                                  total_shortage$TotalRenterShortage12_16, total_shortage$TotalRenterShortage13_17, 
                                  total_shortage$TotalRenterShortage14_18, total_shortage$TotalRenterShortage15_19, 
                                  total_shortage$TotalRenterShortage16_20, total_shortage$TotalRenterShortage17_21, 
                                  total_shortage$TotalRenterShortage18_22),
    `Cumulative Housing Shortage` = `Owner Housing Shortage` + `Renter Housing Shortage`
  )
  
  return(total_shortage_long)
}

# Step 2: Create a line plot for the total owner and renter housing shortages
create_total_line_plot <- function(total_table) {
  # Reshape the data to long format for ggplot
  total_table_long <- total_table %>%
    pivot_longer(cols = c(`Owner Housing Shortage`, `Renter Housing Shortage`), 
                 names_to = "Housing Type", values_to = "Shortage")
  
  # Create the plot
  ggplot(total_table_long, aes(x = Period, y = Shortage, group = `Housing Type`, color = `Housing Type`)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Total Renter and Owner Housing Shortage Across All Counties",
         x = "Time Period",
         y = "Housing Shortage",
         color = "Housing Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Owner Housing Shortage" = "blue", "Renter Housing Shortage" = "red"))
}

# Step 3: Calculate total shortages for all counties
total_shortage_table <- calculate_total_shortage(Florida_with_housing_shortages)

# Step 4: Create the total housing shortage line plot
total_shortage_plot <- create_total_line_plot(total_shortage_table)

# Step 5: Write the total shortage table and plot to an Excel file
wb1 <- createWorkbook()

# Add worksheet for total housing shortage
addWorksheet(wb1, sheetName = "Total Housing Shortage")

# Write the data to the worksheet
writeData(wb1, "Total Housing Shortage", total_shortage_table)

# Save the plot as an image and insert into the worksheet
plot_image_file <- paste0(tempfile(), ".png")  # Save plot temporarily as a PNG
ggsave(plot_image_file, total_shortage_plot, width = 7, height = 5, dpi = 300)

# Insert the plot into the worksheet
insertImage(wb1, sheet = "Total Housing Shortage", file = plot_image_file, startCol = 8, startRow = 3, width = 7, height = 5)

# Save the workbook
saveWorkbook(wb1, "Total_Housing_Shortages_All_Counties.xlsx", overwrite = TRUE)
