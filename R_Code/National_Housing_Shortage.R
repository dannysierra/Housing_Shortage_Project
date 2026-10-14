# National housing shortage estimates, K&S methodology.
# DeVoe L. Moore Center, Florida State University
#
# Covers six geographic levels over 15 ACS 5-year periods (2006-2010 through
# 2020-2024). State, division, and county get Excel workbooks with charts;
# tract, place, and county subdivision are CSV only, since 30k-70k units each
# is well past what a workbook can hold.

#
# Adding a future period means appending one row to `periods` and nothing else.
#
# County is the slow section (20-60 min) because it renders a chart for every
# one of the 3,100+ counties. Tract/place/sub are 5-15 min each, mostly I/O.

data_dir <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Data Sets/nhgis0014_csv"
out_dir  <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results"

setwd(data_dir)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

out_cty_dir <- file.path(out_dir, "County_by_State")
dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_cty_dir, showWarnings = FALSE, recursive = TRUE)


# Setup -------------------------------------------------------------------

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

# NHGIS uses the same column codes at every geographic level, so one rename
# vector covers all six files.
rename_vec <- c(
  setNames(paste0("D11AA", periods$suffix), paste0("ForRent_",      periods$tag)),
  setNames(paste0("D11AB", periods$suffix), paste0("RentedNotOcc_", periods$tag)),
  setNames(paste0("D11AC", periods$suffix), paste0("ForSale_",      periods$tag)),
  setNames(paste0("D11AD", periods$suffix), paste0("SoldNotOcc_",   periods$tag)),
  setNames(paste0("B37AA", periods$suffix), paste0("OwnerOcc_",     periods$tag)),
  setNames(paste0("B37AB", periods$suffix), paste0("RenterOcc_",    periods$tag))
)

output_cols <- c("Period",
                 "NaturalRenterVacRate", "NaturalOwnerVacRate",
                 "RenterVacRate",        "OwnerVacRate",
                 "RenterVacDiff",        "OwnerVacDiff",
                 "RenterShortage",       "OwnerShortage",
                 "CumulativeShortage")

# Columns shown as decimals rather than whole numbers in the Excel output.
rate_cols <- c("NaturalRenterVacRate", "NaturalOwnerVacRate",
               "RenterVacRate", "OwnerVacRate",
               "RenterVacDiff", "OwnerVacDiff")


# Helpers -----------------------------------------------------------------

# Read one NHGIS file and reshape it to one row per unit per period.
load_and_pivot <- function(filename, id_cols) {
  read_csv(filename, show_col_types = FALSE) %>%
    rename(all_of(rename_vec)) %>%
    select(all_of(id_cols),
           starts_with("ForRent_"),
           starts_with("RentedNotOcc_"),
           starts_with("ForSale_"),
           starts_with("SoldNotOcc_"),
           starts_with("OwnerOcc_"),
           starts_with("RenterOcc_")) %>%
    pivot_longer(
      cols          = -all_of(id_cols),
      names_to      = c("variable", "tag"),
      names_pattern = "^(.+)_([0-9]{2}_[0-9]{2})$"
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    left_join(periods[, c("tag", "label")], by = "tag") %>%
    rename(Period = label) %>%
    mutate(Period = factor(Period, levels = periods$label))
}

# K&S: compare each period's vacancy rate to the unit's own long-run average.
# A rate below the long-run average implies a shortage; above implies a surplus.
#
# group_by_cols must uniquely identify a geographic unit across all 15 periods —
# names won't do it (half the states have a Washington County), so pass the code.
run_ks <- function(long_df, group_by_cols) {
  long_df <- long_df %>%
    mutate(
      RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
      OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
    )

  nat <- long_df %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarize(
      NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
      NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
      .groups = "drop"
    )

  long_df %>%
    left_join(nat, by = group_by_cols) %>%
    mutate(
      RenterVacDiff      = NaturalRenterVacRate - RenterVacRate,
      OwnerVacDiff       = NaturalOwnerVacRate  - OwnerVacRate,
      RenterShortage     = RenterVacDiff * (RenterOcc + RentedNotOcc + ForRent),
      OwnerShortage      = OwnerVacDiff  * (OwnerOcc  + SoldNotOcc   + ForSale),
      CumulativeShortage = RenterShortage + OwnerShortage
    )
}

# Excel caps sheet names at 31 characters and requires them to be unique, so
# truncate and append a counter if the short name is already taken.
make_sheet_name <- function(wb, name) {
  name <- substr(name, 1, 31)
  base <- name
  i <- 1
  while (name %in% names(wb)) {
    name <- paste0(substr(base, 1, 28), "_", i)
    i <- i + 1
  }
  name
}

make_plot <- function(df, title_text) {
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
    scale_color_manual(values = c("Owner Housing Shortage"  = "#2166ac",
                                  "Renter Housing Shortage" = "#d6604d")) +
    labs(title = title_text,
         x     = "ACS 5-Year Period",
         y     = "Units (positive = shortage, negative = surplus)",
         color = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x     = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# One sheet per unit: the results table, with the chart parked in column 12.
write_unit_sheet <- function(wb, sheet_label, unit_df) {
  tbl <- unit_df %>%
    arrange(Period) %>%
    select(all_of(output_cols))

  sht <- make_sheet_name(wb, sheet_label)
  addWorksheet(wb, sht)
  writeData(wb, sht, tbl)

  pct_style <- createStyle(numFmt = "0.0000")
  for (col in which(names(tbl) %in% rate_cols)) {
    addStyle(wb, sht, pct_style, rows = 2:(nrow(tbl) + 1), cols = col)
  }

  pf <- paste0(tempfile(), ".png")
  ggsave(pf, make_plot(tbl, sheet_label), width = 8, height = 5, dpi = 150)
  insertImage(wb, sht, pf, startCol = 12, startRow = 2, width = 8, height = 5)
}


# State -------------------------------------------------------------------

state_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_state.csv",
  id_cols  = c("GISJOIN", "STATE", "STATEFP", "STATENH")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(state_long, file.path(out_dir, "National_State_Housing_Shortages_Long.csv"))

wb_state <- createWorkbook()
for (st in sort(unique(state_long$STATE))) {
  write_unit_sheet(wb_state, st, filter(state_long, STATE == st))
}
saveWorkbook(wb_state,
             file.path(out_dir, "National_State_Housing_Shortages.xlsx"),
             overwrite = TRUE)


# Division ----------------------------------------------------------------

# The division file has no STATEFP — DIVISION is the unit of observation.
division_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_division.csv",
  id_cols  = c("GISJOIN", "DIVISION", "DIVISIONA")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(division_long, file.path(out_dir, "National_Division_Housing_Shortages_Long.csv"))

wb_div <- createWorkbook()
for (div in sort(unique(division_long$DIVISION))) {
  write_unit_sheet(wb_div, div, filter(division_long, DIVISION == div))
}
saveWorkbook(wb_div,
             file.path(out_dir, "National_Division_Housing_Shortages.xlsx"),
             overwrite = TRUE)


# County ------------------------------------------------------------------

# GISJOIN (state + county FIPS) is the unit ID, not COUNTY — county names
# repeat across states.
county_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_county.csv",
  id_cols  = c("GISJOIN", "STATE", "STATEFP", "STATENH",
               "COUNTY", "COUNTYFP", "COUNTYNH")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(county_long, file.path(out_dir, "National_County_Housing_Shortages_Long.csv"))

# One workbook per state, since a single national workbook would be unusable.
# Comment out this loop to get the CSV alone and skip the hour of chart rendering.
state_list <- sort(unique(county_long$STATE))

for (st in state_list) {
  cat(match(st, state_list), "/", length(state_list), st, "\n")

  st_data <- filter(county_long, STATE == st)
  wb_cty  <- createWorkbook()
  for (cty in sort(unique(st_data$COUNTY))) {
    write_unit_sheet(wb_cty, cty, filter(st_data, COUNTY == cty))
  }

  st_clean <- gsub("[^A-Za-z0-9]", "_", st)
  saveWorkbook(wb_cty,
               file.path(out_cty_dir, paste0("County_Shortages_", st_clean, ".xlsx")),
               overwrite = TRUE)
}


# Tract -------------------------------------------------------------------

# ~73,000 tracts x 15 periods is roughly 1.1M rows, so CSV only. Join to a
# shapefile for mapping.
#
# Caveat worth carrying into any writeup: ACS vacancy-status estimates at the
# tract level have wide margins of error, and the natural vacancy rate here is
# an average of 15 noisy rates. Report MOEs if these go in a publication.
#
# NHGISCODE rather than GISJOIN — GISJOIN changes when tract boundaries are
# redrawn, which would split one tract into two "units" mid-panel.
tract_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_tract.csv",
  id_cols  = c("NHGISCODE", "STATE", "STATEFP",
               "COUNTY", "COUNTYFP", "TRACTA")
) %>%
  run_ks(group_by_cols = "NHGISCODE")

write_csv(tract_long, file.path(out_dir, "National_Tract_Housing_Shortages_Long.csv"))

rm(tract_long); gc()


# Place -------------------------------------------------------------------

# ~30,000 cities, towns, and CDPs. Place names repeat across states, so
# NHGISCODE is again the unit ID.
place_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_place.csv",
  id_cols  = c("NHGISCODE", "STATE", "STATEFP", "PLACE", "PLACEA")
) %>%
  run_ks(group_by_cols = "NHGISCODE")

write_csv(place_long, file.path(out_dir, "National_Place_Housing_Shortages_Long.csv"))

rm(place_long); gc()


# County subdivision ------------------------------------------------------

# ~35,000 subdivisions. Mostly useful in New England, where townships are the
# real unit of local government; elsewhere the boundaries are largely nominal.
#
# This one can't use load_and_pivot: some subdivisions appear under more than
# one county assignment, which collides in pivot_wider. Collapsing to one row
# per NHGISCODE first (summing raw counts) resolves it, but drops the readable
# name columns, so they get stashed and rejoined afterward.
sub_raw <- read_csv("nhgis0014_ts_nominal_cty_sub.csv", show_col_types = FALSE)

sub_meta <- sub_raw %>%
  select(NHGISCODE, STATE, STATEFP, COUNTY, COUNTYFP, CTY_SUB) %>%
  distinct(NHGISCODE, .keep_all = TRUE)

sub_long <- sub_raw %>%
  rename(all_of(rename_vec)) %>%
  select(NHGISCODE,
         starts_with("ForRent_"), starts_with("RentedNotOcc_"),
         starts_with("ForSale_"), starts_with("SoldNotOcc_"),
         starts_with("OwnerOcc_"), starts_with("RenterOcc_")) %>%
  group_by(NHGISCODE) %>%
  summarize(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols          = -NHGISCODE,
    names_to      = c("variable", "tag"),
    names_pattern = "^(.+)_([0-9]{2}_[0-9]{2})$"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  left_join(periods[, c("tag", "label")], by = "tag") %>%
  rename(Period = label) %>%
  mutate(Period = factor(Period, levels = periods$label)) %>%
  left_join(sub_meta, by = "NHGISCODE") %>%
  run_ks(group_by_cols = "NHGISCODE")

write_csv(sub_long, file.path(out_dir, "National_CountySub_Housing_Shortages_Long.csv"))

rm(sub_long); gc()

cat("Done. Results in", out_dir, "\n")
