# ============================================================
# Florida Housing Shortage Analysis — K&S Methodology
# DeVoe L. Moore Center, Florida State University
#
# Covers 15 ACS 5-year periods: 2006-2010 through 2020-2024
# To add a future period, add one row to `periods` below.
# ============================================================

setwd("/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Data Sets/nhgis0013_csv")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

# ============================================================
# PERIOD DEFINITIONS
# label  : human-readable period label used in all outputs
# suffix : NHGIS column suffix (increments by 10 per period)
# tag    : short internal label used for column naming
#
# To add a new period, append one row here. Nothing else changes.
# ============================================================
periods <- data.frame(
  label  = c("2006-2010","2007-2011","2008-2012","2009-2013","2010-2014",
             "2011-2015","2012-2016","2013-2017","2014-2018","2015-2019",
             "2016-2020","2017-2021","2018-2022","2019-2023","2020-2024"),
  suffix = c("105","115","125","135","145","155","165","175","185","195",
             "205","215","225","235","245"),
  tag    = c("06_10","07_11","08_12","09_13","10_14","11_15","12_16","13_17",
             "14_18","15_19","16_20","17_21","18_22","19_23","20_24"),
  stringsAsFactors = FALSE
)

# ============================================================
# LOAD & FILTER TO FLORIDA
# ============================================================
Florida_raw <- read_csv("nhgis0013_ts_nominal_county.csv", show_col_types = FALSE)
Florida_raw <- filter(Florida_raw, STATE == "Florida")

# ============================================================
# PROGRAMMATIC RENAMING
# Builds a named vector: c(NewColName = "OldNHGISCode", ...)
# covering all 6 variables x 15 periods = 90 columns at once.
# ============================================================
rename_vec <- c(
  setNames(paste0("D11AA", periods$suffix), paste0("ForRent_",      periods$tag)),
  setNames(paste0("D11AB", periods$suffix), paste0("RentedNotOcc_", periods$tag)),
  setNames(paste0("D11AC", periods$suffix), paste0("ForSale_",      periods$tag)),
  setNames(paste0("D11AD", periods$suffix), paste0("SoldNotOcc_",   periods$tag)),
  setNames(paste0("B37AA", periods$suffix), paste0("OwnerOcc_",     periods$tag)),
  setNames(paste0("B37AB", periods$suffix), paste0("RenterOcc_",    periods$tag))
)

Florida_wide <- Florida_raw %>%
  rename(all_of(rename_vec)) %>%
  select(
    COUNTY, STATEFP, COUNTYFP, GISJOIN,  # keep FIPS for CBSA crosswalk later
    starts_with("ForRent_"),
    starts_with("RentedNotOcc_"),
    starts_with("ForSale_"),
    starts_with("SoldNotOcc_"),
    starts_with("OwnerOcc_"),
    starts_with("RenterOcc_")
  )

# ============================================================
# PIVOT TO LONG FORMAT
# One row per county per period (67 counties x 15 periods = 1005 rows).
# All calculations happen once on this structure — no repeated formulas.
# ============================================================
Florida_long <- Florida_wide %>%
  pivot_longer(
    cols          = -c(COUNTY, STATEFP, COUNTYFP, GISJOIN),
    names_to      = c("variable", "tag"),
    names_pattern = "^(.+)_([0-9]{2}_[0-9]{2})$"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  left_join(periods[, c("tag", "label")], by = "tag") %>%
  rename(Period = label) %>%
  mutate(Period = factor(Period, levels = periods$label))  # chronological order

# ============================================================
# STEP 1: VACANCY RATES (per county, per period)
# RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc)
# OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
# ============================================================
Florida_long <- Florida_long %>%
  mutate(
    RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
    OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
  )

# ============================================================
# STEP 2: NATURAL VACANCY RATES (long-run average per county)
# Automatically averages across however many periods exist (n=15).
# ============================================================
natural_vac <- Florida_long %>%
  group_by(COUNTY) %>%
  summarize(
    NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
    NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
    .groups = "drop"
  )

Florida_long <- left_join(Florida_long, natural_vac, by = "COUNTY")

# ============================================================
# STEP 3: VACANCY DIFFERENCES & HOUSING SHORTAGES
# VacDiff > 0 → shortage (market tighter than long-run average)
# VacDiff < 0 → surplus
# Shortage (units) = VacDiff x Total Market Size
# ============================================================
Florida_long <- Florida_long %>%
  mutate(
    RenterVacDiff      = NaturalRenterVacRate - RenterVacRate,
    OwnerVacDiff       = NaturalOwnerVacRate  - OwnerVacRate,
    RenterShortage     = RenterVacDiff * (RenterOcc + RentedNotOcc + ForRent),
    OwnerShortage      = OwnerVacDiff  * (OwnerOcc  + SoldNotOcc   + ForSale),
    CumulativeShortage = RenterShortage + OwnerShortage
  )

# ============================================================
# EXPORT 1: Full long-format CSV
# Use STATEFP + COUNTYFP to join Census CBSA delineation file.
# ============================================================
write_csv(Florida_long, "../../Updated_Years/Florida_Housing_Shortages_Long.csv")
message("Exported: Florida_Housing_Shortages_Long.csv")

# ============================================================
# HELPER: Excel sheet names must be <= 31 characters & unique
# ============================================================
make_sheet_name <- function(wb, name) {
  name <- substr(name, 1, 31)
  base <- name; i <- 1
  while (name %in% names(wb)) {
    name <- paste0(substr(base, 1, 28), "_", i)
    i <- i + 1
  }
  name
}

# ============================================================
# PLOT FUNCTION: Renter vs owner shortage line chart per county
# ============================================================
make_county_plot <- function(df, cty) {
  df %>%
    select(Period, RenterShortage, OwnerShortage) %>%
    pivot_longer(c(RenterShortage, OwnerShortage),
                 names_to = "Type", values_to = "Shortage") %>%
    mutate(Type = ifelse(Type == "OwnerShortage",
                         "Owner Housing Shortage", "Renter Housing Shortage")) %>%
    ggplot(aes(x = Period, y = Shortage, group = Type, color = Type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = c(
      "Owner Housing Shortage"  = "#2166ac",
      "Renter Housing Shortage" = "#d6604d"
    )) +
    labs(
      title = paste(cty, "— Housing Shortage by Tenure"),
      x     = "ACS 5-Year Period",
      y     = "Units (positive = shortage, negative = surplus)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# ============================================================
# EXPORT 2: Per-county Excel workbook
# One sheet per county — table of results + line chart
# ============================================================
message("Building per-county Excel workbook...")
wb1 <- createWorkbook()
pct_style <- createStyle(numFmt = "0.0000")

for (cty in sort(unique(Florida_long$COUNTY))) {
  
  cdf <- Florida_long %>%
    filter(COUNTY == cty) %>%
    arrange(Period) %>%
    select(Period,
           NaturalRenterVacRate, NaturalOwnerVacRate,
           RenterVacRate, OwnerVacRate,
           RenterVacDiff, OwnerVacDiff,
           RenterShortage, OwnerShortage, CumulativeShortage)
  
  sht <- make_sheet_name(wb1, cty)
  addWorksheet(wb1, sht)
  writeData(wb1, sht, cdf)
  
  # Format vacancy rate columns as 4 decimal places
  rate_cols <- which(names(cdf) %in% c("NaturalRenterVacRate", "NaturalOwnerVacRate",
                                       "RenterVacRate", "OwnerVacRate",
                                       "RenterVacDiff", "OwnerVacDiff"))
  for (col in rate_cols)
    addStyle(wb1, sht, pct_style, rows = 2:(nrow(cdf) + 1), cols = col)
  
  p  <- make_county_plot(cdf, cty)
  pf <- paste0(tempfile(), ".png")
  ggsave(pf, p, width = 8, height = 5, dpi = 150)
  insertImage(wb1, sht, pf, startCol = 12, startRow = 2, width = 8, height = 5)
}

saveWorkbook(wb1, "../../Updated_Years/Florida_Housing_Shortages_by_County.xlsx", overwrite = TRUE)
message("Exported: Florida_Housing_Shortages_by_County.xlsx")

# ============================================================
# EXPORT 3: Total Florida summary workbook
# All 67 counties summed, all 15 periods
# ============================================================
total <- Florida_long %>%
  group_by(Period) %>%
  summarize(
    `Owner Housing Shortage`      = round(sum(OwnerShortage,      na.rm = TRUE)),
    `Renter Housing Shortage`     = round(sum(RenterShortage,     na.rm = TRUE)),
    `Cumulative Housing Shortage` = round(sum(CumulativeShortage, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Period)

total_plot <- total %>%
  pivot_longer(c(`Owner Housing Shortage`, `Renter Housing Shortage`),
               names_to = "Type", values_to = "Shortage") %>%
  ggplot(aes(x = Period, y = Shortage, group = Type, color = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(
    "Owner Housing Shortage"  = "#2166ac",
    "Renter Housing Shortage" = "#d6604d"
  )) +
  labs(
    title = "Total Housing Shortage — Florida (All 67 Counties)",
    x     = "ACS 5-Year Period",
    y     = "Units (positive = shortage, negative = surplus)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

wb2 <- createWorkbook()
addWorksheet(wb2, "Total Housing Shortage")
writeData(wb2, "Total Housing Shortage", total)

pf2 <- paste0(tempfile(), ".png")
ggsave(pf2, total_plot, width = 8, height = 5, dpi = 150)
insertImage(wb2, "Total Housing Shortage", pf2,
            startCol = 6, startRow = 3, width = 8, height = 5)

saveWorkbook(wb2, "../../Updated_Years/Total_Housing_Shortages_Florida.xlsx", overwrite = TRUE)
message("Exported: ../../Total_Housing_Shortages_Florida.xlsx") 

message("All done!")
