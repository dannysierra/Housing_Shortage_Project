# ==================================================================
# National Housing Shortage Analysis — K&S Methodology
# DeVoe L. Moore Center, Florida State University
#
# Geographic Levels Covered:
#   1. State      → Excel (1 sheet/state) + CSV
#   2. Division   → Excel (1 sheet/division) + CSV
#   3. County     → 1 Excel per state in County_by_State/ + CSV
#   4. Tract      → CSV only (73,000+ units; Excel not feasible)
#   5. Place      → CSV only (30,000+ units)
#   6. County Sub → CSV only (35,000+ units)
#
# Covers 15 ACS 5-year periods: 2006-2010 through 2020-2024
# To add a future period, append one row to `periods` below.
#
# Expected runtimes (approximate):
#   State / Division : < 1 minute
#   County           : 20–60 minutes (plots for all 3,100+ counties)
#   Tract            : 5–15 minutes (large file, CSV only)
#   Place / Sub      : 5–10 minutes each (CSV only)
# ==================================================================

# ── !! UPDATE THESE TWO PATHS BEFORE RUNNING !! ─────────────────
data_dir <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Data Sets/nhgis0014_csv"
out_dir  <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results"
# ────────────────────────────────────────────────────────────────

setwd(data_dir)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

# Create output folders
out_cty_dir <- file.path(out_dir, "County_by_State")
dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_cty_dir, showWarnings = FALSE, recursive = TRUE)

# ==================================================================
# 0A. PERIOD DEFINITIONS
# Append one row here to add a future period. Nothing else changes.
# ==================================================================
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

# ==================================================================
# 0B. SHARED RENAME VECTOR
# NHGIS column codes are identical across all geographic levels.
# ==================================================================
rename_vec <- c(
  setNames(paste0("D11AA", periods$suffix), paste0("ForRent_",      periods$tag)),
  setNames(paste0("D11AB", periods$suffix), paste0("RentedNotOcc_", periods$tag)),
  setNames(paste0("D11AC", periods$suffix), paste0("ForSale_",      periods$tag)),
  setNames(paste0("D11AD", periods$suffix), paste0("SoldNotOcc_",   periods$tag)),
  setNames(paste0("B37AA", periods$suffix), paste0("OwnerOcc_",     periods$tag)),
  setNames(paste0("B37AB", periods$suffix), paste0("RenterOcc_",    periods$tag))
)

# ==================================================================
# 0C. SHARED UTILITY FUNCTIONS
# ==================================================================

# Load a raw NHGIS file, rename columns, keep only needed columns,
# and pivot to long format (one row per unit per period).
load_and_pivot <- function(filename, id_cols) {
  message("  Loading: ", filename)
  raw <- read_csv(filename, show_col_types = FALSE)
  message("  Rows: ", nrow(raw), " | Pivoting to long format...")
  raw %>%
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

# Run K&S calculations on a long-format data frame.
# group_by_cols: character vector of column(s) that uniquely
# identify each geographic unit across all 15 periods.
run_ks <- function(long_df, group_by_cols) {
  # Step 1: Observed vacancy rates
  long_df <- long_df %>%
    mutate(
      RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
      OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
    )
  # Step 2: Natural vacancy rates (long-run average per unit)
  nat <- long_df %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarize(
      NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
      NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
      .groups = "drop"
    )
  # Step 3: Shortage calculations
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

# Excel sheet name: max 31 characters, unique within workbook.
make_sheet_name <- function(wb, name) {
  name <- substr(name, 1, 31)
  base <- name; i <- 1
  while (name %in% names(wb)) {
    name <- paste0(substr(base, 1, 28), "_", i); i <- i + 1
  }
  name
}

# Standard renter vs. owner shortage line chart.
make_plot <- function(df, title_text) {
  df %>%
    select(Period, RenterShortage, OwnerShortage) %>%
    pivot_longer(c(RenterShortage, OwnerShortage),
                 names_to = "Type", values_to = "Shortage") %>%
    mutate(Type = ifelse(Type == "OwnerShortage",
                         "Owner Housing Shortage", "Renter Housing Shortage")) %>%
    ggplot(aes(x = Period, y = Shortage, group = Type, color = Type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_line(linewidth = 1) + geom_point(size = 3) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = c("Owner Housing Shortage"  = "#2166ac",
                                  "Renter Housing Shortage" = "#d6604d")) +
    labs(title  = title_text,
         x      = "ACS 5-Year Period",
         y      = "Units (positive = shortage, negative = surplus)",
         color  = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# Write one sheet to a workbook: results table + line chart.
write_unit_sheet <- function(wb, sheet_label, unit_df) {
  output_cols <- c("Period",
                   "NaturalRenterVacRate", "NaturalOwnerVacRate",
                   "RenterVacRate",        "OwnerVacRate",
                   "RenterVacDiff",        "OwnerVacDiff",
                   "RenterShortage",       "OwnerShortage",
                   "CumulativeShortage")
  tbl  <- unit_df %>% arrange(Period) %>% select(all_of(output_cols))
  sht  <- make_sheet_name(wb, sheet_label)
  addWorksheet(wb, sht)
  writeData(wb, sht, tbl)
  pct_style <- createStyle(numFmt = "0.0000")
  rate_cols <- which(names(tbl) %in% c("NaturalRenterVacRate","NaturalOwnerVacRate",
                                       "RenterVacRate","OwnerVacRate",
                                       "RenterVacDiff","OwnerVacDiff"))
  for (col in rate_cols)
    addStyle(wb, sht, pct_style, rows = 2:(nrow(tbl) + 1), cols = col)
  p  <- make_plot(tbl, sheet_label)
  pf <- paste0(tempfile(), ".png")
  ggsave(pf, p, width = 8, height = 5, dpi = 150)
  insertImage(wb, sht, pf, startCol = 12, startRow = 2, width = 8, height = 5)
}

# ==================================================================
# SECTION 1: STATE LEVEL
# Output: National_State_Housing_Shortages_Long.csv
#         National_State_Housing_Shortages.xlsx  (1 sheet / state)
# ==================================================================
message("\n════════════════════════════════════════")
message("SECTION 1: STATE LEVEL")
message("════════════════════════════════════════")

state_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_state.csv",
  id_cols  = c("GISJOIN", "STATE", "STATEFP", "STATENH")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(state_long,
          file.path(out_dir, "National_State_Housing_Shortages_Long.csv"))
message("Exported: National_State_Housing_Shortages_Long.csv")

wb_state <- createWorkbook()
for (st in sort(unique(state_long$STATE))) {
  write_unit_sheet(wb_state,
                   sheet_label = st,
                   unit_df     = filter(state_long, STATE == st))
}
saveWorkbook(wb_state,
             file.path(out_dir, "National_State_Housing_Shortages.xlsx"),
             overwrite = TRUE)
message("Exported: National_State_Housing_Shortages.xlsx")

# ==================================================================
# SECTION 2: DIVISION LEVEL
# Note: Division file has no STATEFP — DIVISION is the unit.
# Output: National_Division_Housing_Shortages_Long.csv
#         National_Division_Housing_Shortages.xlsx  (1 sheet / division)
# ==================================================================
message("\n════════════════════════════════════════")
message("SECTION 2: DIVISION LEVEL")
message("════════════════════════════════════════")

division_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_division.csv",
  id_cols  = c("GISJOIN", "DIVISION", "DIVISIONA")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(division_long,
          file.path(out_dir, "National_Division_Housing_Shortages_Long.csv"))
message("Exported: National_Division_Housing_Shortages_Long.csv")

wb_div <- createWorkbook()
for (div in sort(unique(division_long$DIVISION))) {
  write_unit_sheet(wb_div,
                   sheet_label = div,
                   unit_df     = filter(division_long, DIVISION == div))
}
saveWorkbook(wb_div,
             file.path(out_dir, "National_Division_Housing_Shortages.xlsx"),
             overwrite = TRUE)
message("Exported: National_Division_Housing_Shortages.xlsx")

# ==================================================================
# SECTION 3: COUNTY LEVEL
# ~3,100+ counties nationally. One Excel workbook per state saved
# to County_by_State/ subfolder. Plus a national long CSV.
#
# NOTE: County names are NOT unique nationally (e.g., many states
# have "Washington County"). GISJOIN (encodes state + county FIPS)
# is used as the unique unit identifier for natural vacancy rate.
#
# To skip the per-state Excel workbooks and produce only the CSV,
# comment out the "Create one Excel per state" block below.
# ==================================================================
message("\n════════════════════════════════════════")
message("SECTION 3: COUNTY LEVEL")
message("(One Excel workbook per state — this section takes the longest)")
message("════════════════════════════════════════")

county_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_county.csv",
  id_cols  = c("GISJOIN", "STATE", "STATEFP", "STATENH",
               "COUNTY", "COUNTYFP", "COUNTYNH")
) %>%
  run_ks(group_by_cols = "GISJOIN")

write_csv(county_long,
          file.path(out_dir, "National_County_Housing_Shortages_Long.csv"))
message("Exported: National_County_Housing_Shortages_Long.csv")

# ── Create one Excel per state ────────────────────────────────────
state_list <- sort(unique(county_long$STATE))
message("Building ", length(state_list), " state Excel workbooks...")

for (st in state_list) {
  message("  ", st)
  st_data <- filter(county_long, STATE == st)
  wb_cty  <- createWorkbook()
  for (cty in sort(unique(st_data$COUNTY))) {
    write_unit_sheet(wb_cty,
                     sheet_label = cty,
                     unit_df     = filter(st_data, COUNTY == cty))
  }
  st_clean <- gsub("[^A-Za-z0-9]", "_", st)   # safe filename
  saveWorkbook(wb_cty,
               file.path(out_cty_dir, paste0("County_Shortages_", st_clean, ".xlsx")),
               overwrite = TRUE)
}
message("Exported all county workbooks to County_by_State/")

# ==================================================================
# SECTION 4: TRACT LEVEL  (CSV only)
#
# ~73,000+ tracts × 15 periods ≈ 1.1M rows after pivoting.
# Per-unit Excel is not feasible at this scale.
# The long CSV can be joined to shapefiles for mapping.
#
# METHODOLOGICAL NOTE: ACS tract-level estimates have large margins
# of error for vacancy-status variables. Natural vacancy rates here
# are sensitive to sampling noise. Interpret with caution and report
# margins of error when using tract-level results in publications.
#
# NHGISCODE is used as the stable unit identifier (preferred over
# GISJOIN, which changes when tract boundaries are redrawn).
# ==================================================================
message("\n════════════════════════════════════════")
message("SECTION 4: TRACT LEVEL (CSV only)")
message("Large file — may take several minutes to load and process")
message("════════════════════════════════════════")

tract_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_tract.csv",
  id_cols  = c("NHGISCODE", "STATE", "STATEFP",
               "COUNTY", "COUNTYFP", "TRACTA")
) %>%
  run_ks(group_by_cols = "NHGISCODE")

write_csv(tract_long,
          file.path(out_dir, "National_Tract_Housing_Shortages_Long.csv"))
message("Exported: National_Tract_Housing_Shortages_Long.csv")

# Free memory before next section
rm(tract_long); gc()

# ==================================================================
# SECTION 5: PLACE LEVEL  (CSV only)
#
# ~30,000+ places (cities, towns, CDPs) nationally.
# Place names are not unique across states; NHGISCODE is the
# stable unit identifier for natural vacancy rate calculation.
# ==================================================================
message("\n════════════════════════════════════════")
message("SECTION 5: PLACE LEVEL (CSV only)")
message("════════════════════════════════════════")

place_long <- load_and_pivot(
  filename = "nhgis0014_ts_nominal_place.csv",
  id_cols  = c("NHGISCODE", "STATE", "STATEFP", "PLACE", "PLACEA")
) %>%
  run_ks(group_by_cols = "NHGISCODE")

write_csv(place_long,
          file.path(out_dir, "National_Place_Housing_Shortages_Long.csv"))
message("Exported: National_Place_Housing_Shortages_Long.csv")

rm(place_long); gc()

# ==================================================================
# SECTION 6: COUNTY SUBDIVISION LEVEL  (CSV only)
#
# ~35,000+ county subdivisions nationally.
# Primarily relevant for New England states where townships are the
# primary unit of local government. Analytical value varies widely
# by region.
# ==================================================================
message("\n── Section 6: County Subdivision Level (CSV only) ──")

message("  Loading: nhgis0014_ts_nominal_cty_sub.csv")
sub_raw <- read_csv("nhgis0014_ts_nominal_cty_sub.csv", show_col_types = FALSE)
message("  Rows: ", nrow(sub_raw), " | Pivoting to long format...")

# Save human-readable metadata separately before pivoting
sub_meta <- sub_raw %>%
  select(NHGISCODE, STATE, STATEFP, COUNTY, COUNTYFP, CTY_SUB) %>%
  distinct(NHGISCODE, .keep_all = TRUE)

# Pivot using NHGISCODE only — avoids duplicate collisions from
# subdivisions that appear under multiple county assignments.
# Any genuine duplicates are resolved by summing raw counts.
sub_long <- sub_raw %>%
  rename(all_of(rename_vec)) %>%
  select(NHGISCODE,
         starts_with("ForRent_"), starts_with("RentedNotOcc_"),
         starts_with("ForSale_"),  starts_with("SoldNotOcc_"),
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
  left_join(sub_meta, by = "NHGISCODE") %>%   # re-attach readable names
  run_ks(group_by_cols = "NHGISCODE")

write_csv(sub_long,
          file.path(out_dir, "National_CountySub_Housing_Shortages_Long.csv"))
message("Exported: National_CountySub_Housing_Shortages_Long.csv")

rm(sub_long); gc()

# ==================================================================
# DONE
# ==================================================================
message("\n════════════════════════════════════════")
message("All sections complete.")
message("Results saved to: ", out_dir)
message("Output file summary:")
message("  National_State_Housing_Shortages_Long.csv")
message("  National_State_Housing_Shortages.xlsx")
message("  National_Division_Housing_Shortages_Long.csv")
message("  National_Division_Housing_Shortages.xlsx")
message("  National_County_Housing_Shortages_Long.csv")
message("  County_by_State/County_Shortages_[StateName].xlsx  (one per state)")
message("  National_Tract_Housing_Shortages_Long.csv")
message("  National_Place_Housing_Shortages_Long.csv")
message("  National_CountySub_Housing_Shortages_Long.csv")
message("════════════════════════════════════════")
