# ==================================================================
# National CBSA Housing Shortage Analysis — K&S Methodology
# DeVoe L. Moore Center, Florida State University
#
# This script picks up from the national county output:
#   National_County_Housing_Shortages_Long.csv
#
# It joins the NBER 2023 CBSA delineation crosswalk, aggregates
# raw housing counts to the CBSA level, and runs the full K&S
# methodology separately for Metropolitan and Micropolitan areas.
#
# BEFORE RUNNING:
#   1. Download the crosswalk CSV from:
#      https://data.nber.org/cbsa-csa-fips-county-crosswalk/2023/cbsa2fipsxw_2023.csv
#   2. Save it to your data folder
#   3. Update the three paths below
#
# Outputs:
#   National_MSA_Housing_Shortages_Long.csv
#   National_MSA_Housing_Shortages.xlsx      (1 sheet per MSA)
#   National_Micro_Housing_Shortages_Long.csv
#   National_Micro_Housing_Shortages.xlsx    (1 sheet per Micro)
#   National_CBSA_Summary.xlsx               (ranked summary tables)
#
# ~384 MSAs and ~543 Micropolitans nationally (2023 delineation)
# ==================================================================

# ── !! UPDATE THESE PATHS BEFORE RUNNING !! ──────────────────────

# Folder where National_County_Housing_Shortages_Long.csv lives
county_csv_path <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results/National_County_Housing_Shortages_Long.csv"

# The NBER crosswalk CSV you downloaded
crosswalk_path <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Data Sets/nhgis0014_csv/cbsa2fipsxw_2023.csv"

# Output folder (same as your other national results)
out_dir <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results"

# ── PLOT OPTION ───────────────────────────────────────────────────
# Set to TRUE to include line charts in the per-CBSA Excel sheets.
# FALSE produces the same tables without charts — much faster.
# With ~380 MSAs + ~540 Micros, plots add roughly 30-45 minutes.
INCLUDE_PLOTS <- FALSE

# ─────────────────────────────────────────────────────────────────

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

periods_order <- c("2006-2010","2007-2011","2008-2012","2009-2013","2010-2014",
                   "2011-2015","2012-2016","2013-2017","2014-2018","2015-2019",
                   "2016-2020","2017-2021","2018-2022","2019-2023","2020-2024")

# ==================================================================
# STEP 1: LOAD THE NATIONAL COUNTY LONG CSV
# ==================================================================
message("Loading national county data...")
county_long <- read_csv(county_csv_path, show_col_types = FALSE)
message("  Rows: ", nrow(county_long), " | Counties: ",
        n_distinct(county_long$GISJOIN))

# Build 5-digit county FIPS for crosswalk join
county_long <- county_long %>%
  mutate(CountyFIPS = paste0(
    formatC(as.integer(STATEFP),  width = 2, flag = "0"),
    formatC(as.integer(COUNTYFP), width = 3, flag = "0")
  ))

# ==================================================================
# STEP 2: LOAD AND VALIDATE THE NBER CROSSWALK
# ==================================================================
message("Loading NBER CBSA crosswalk...")
xwalk_raw <- read_csv(crosswalk_path, show_col_types = FALSE)

message("  Crosswalk columns detected:")
print(names(xwalk_raw))

# ── Standardise column names to lowercase for robustness ─────────
names(xwalk_raw) <- tolower(names(xwalk_raw))

# ── Build clean crosswalk: one row per county ─────────────────────
# The Metro/Micro column is truncated to "metropolitanmicropolitanstatis"
# by R when reading the NBER file. county_fips is the pre-built
# 5-digit FIPS — no need to construct it from separate state/county codes.
crosswalk <- xwalk_raw %>%
  select(
    CBSA_Code  = cbsacode,
    CBSA_Name  = cbsatitle,
    CBSA_Type  = metropolitanmicropolitanstatis,
    CountyFIPS = county_fips
  ) %>%
  mutate(
    CountyFIPS = formatC(as.integer(CountyFIPS), width = 5, flag = "0"),
    CBSA_Type  = case_when(
      grepl("Metropolitan Statistical Area", CBSA_Type, ignore.case = TRUE) ~ "Metropolitan",
      grepl("Micropolitan Statistical Area", CBSA_Type, ignore.case = TRUE) ~ "Micropolitan",
      TRUE ~ CBSA_Type
    )
  ) %>%
  distinct()

message("  CBSA crosswalk: ", nrow(crosswalk), " county-CBSA rows")
message("  Metro CBSAs: ",
        n_distinct(filter(crosswalk, CBSA_Type == "Metropolitan")$CBSA_Name))
message("  Micro CBSAs: ",
        n_distinct(filter(crosswalk, CBSA_Type == "Micropolitan")$CBSA_Name))

# ==================================================================
# STEP 3: JOIN CROSSWALK TO COUNTY DATA
# Counties not in any CBSA (non-core rural counties) are dropped.
# ==================================================================
message("Joining crosswalk to county data...")

county_cbsa <- county_long %>%
  inner_join(crosswalk, by = "CountyFIPS")

# Diagnostic: how many counties matched?
matched   <- n_distinct(county_cbsa$CountyFIPS)
total_cty <- n_distinct(county_long$CountyFIPS)
message("  Counties matched to a CBSA: ", matched, " of ", total_cty,
        " (", total_cty - matched, " non-core, excluded)")

# ==================================================================
# STEP 4: AGGREGATE RAW COUNTS TO CBSA LEVEL
#
# CRITICAL: always sum raw counts BEFORE calculating rates.
# Never average rates across counties — that biases the result
# toward smaller counties and away from larger ones.
# ==================================================================
message("Aggregating raw counts to CBSA level...")

cbsa_raw <- county_cbsa %>%
  group_by(CBSA_Code, CBSA_Name, CBSA_Type, Period) %>%
  summarize(
    ForRent      = sum(ForRent,      na.rm = TRUE),
    RentedNotOcc = sum(RentedNotOcc, na.rm = TRUE),
    ForSale      = sum(ForSale,      na.rm = TRUE),
    SoldNotOcc   = sum(SoldNotOcc,   na.rm = TRUE),
    OwnerOcc     = sum(OwnerOcc,     na.rm = TRUE),
    RenterOcc    = sum(RenterOcc,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Period = factor(Period, levels = periods_order))

message("  Total CBSA-period rows: ", nrow(cbsa_raw))

# ==================================================================
# STEP 5: K&S CALCULATIONS AT CBSA LEVEL
#
# Natural vacancy rate = long-run average of each CBSA's own
# observed vacancy rates across all 15 periods.
# ==================================================================
message("Running K&S calculations...")

cbsa_long <- cbsa_raw %>%
  # Step 1: Observed vacancy rates
  mutate(
    RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
    OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
  )

# Step 2: Natural vacancy rates (per CBSA, across all 15 periods)
nat <- cbsa_long %>%
  group_by(CBSA_Code) %>%
  summarize(
    NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
    NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
    .groups = "drop"
  )

cbsa_long <- cbsa_long %>%
  left_join(nat, by = "CBSA_Code") %>%
  # Step 3: Shortages
  mutate(
    RenterVacDiff      = NaturalRenterVacRate - RenterVacRate,
    OwnerVacDiff       = NaturalOwnerVacRate  - OwnerVacRate,
    RenterShortage     = RenterVacDiff * (RenterOcc + RentedNotOcc + ForRent),
    OwnerShortage      = OwnerVacDiff  * (OwnerOcc  + SoldNotOcc   + ForSale),
    CumulativeShortage = RenterShortage + OwnerShortage
  )

# Split into Metro and Micro
msa_long   <- filter(cbsa_long, CBSA_Type == "Metropolitan")
micro_long <- filter(cbsa_long, CBSA_Type == "Micropolitan")

message("  MSA rows:   ", nrow(msa_long),   " | MSAs:   ", n_distinct(msa_long$CBSA_Name))
message("  Micro rows: ", nrow(micro_long), " | Micros: ", n_distinct(micro_long$CBSA_Name))

# ==================================================================
# SHARED HELPER FUNCTIONS
# ==================================================================

make_sheet_name <- function(wb, name) {
  name <- substr(name, 1, 31)
  base <- name; i <- 1
  while (name %in% names(wb)) {
    name <- paste0(substr(base, 1, 28), "_", i); i <- i + 1
  }
  name
}

make_cbsa_plot <- function(df, title_text) {
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
    labs(title = title_text, x = "ACS 5-Year Period",
         y = "Units (positive = shortage, negative = surplus)", color = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

output_cols <- c("Period",
                 "NaturalRenterVacRate", "NaturalOwnerVacRate",
                 "RenterVacRate",        "OwnerVacRate",
                 "RenterVacDiff",        "OwnerVacDiff",
                 "RenterShortage",       "OwnerShortage",
                 "CumulativeShortage")

write_cbsa_workbook <- function(long_df, type_label, filename) {
  message("Building ", type_label, " workbook (", filename, ")...")
  wb       <- createWorkbook()
  pct_sty  <- createStyle(numFmt = "0.0000")
  cbsa_list <- sort(unique(long_df$CBSA_Name))
  total     <- length(cbsa_list)
  
  for (i in seq_along(cbsa_list)) {
    cbsa <- cbsa_list[i]
    if (i %% 50 == 0 || i == total)
      message("  Sheet ", i, " / ", total, ": ", cbsa)
    
    tbl <- long_df %>%
      filter(CBSA_Name == cbsa) %>%
      arrange(Period) %>%
      select(all_of(output_cols))
    
    sht <- make_sheet_name(wb, cbsa)
    addWorksheet(wb, sht)
    writeData(wb, sht, tbl)
    
    rate_cols <- which(names(tbl) %in%
                         c("NaturalRenterVacRate","NaturalOwnerVacRate",
                           "RenterVacRate","OwnerVacRate","RenterVacDiff","OwnerVacDiff"))
    for (col in rate_cols)
      addStyle(wb, sht, pct_sty, rows = 2:(nrow(tbl) + 1), cols = col)
    
    if (INCLUDE_PLOTS) {
      p  <- make_cbsa_plot(tbl, cbsa)
      pf <- paste0(tempfile(), ".png")
      ggsave(pf, p, width = 8, height = 5, dpi = 150)
      insertImage(wb, sht, pf, startCol = 12, startRow = 2,
                  width = 8, height = 5)
    }
  }
  saveWorkbook(wb, file.path(out_dir, filename), overwrite = TRUE)
  message("  Exported: ", filename)
}

# ==================================================================
# STEP 6: EXPORT MSA OUTPUTS
# ==================================================================
message("\n── Metropolitan Statistical Areas ──────────────────")

write_csv(msa_long, file.path(out_dir, "National_MSA_Housing_Shortages_Long.csv"))
message("Exported: National_MSA_Housing_Shortages_Long.csv")

write_cbsa_workbook(msa_long, "MSA", "National_MSA_Housing_Shortages.xlsx")

# ==================================================================
# STEP 7: EXPORT MICRO OUTPUTS
# ==================================================================
message("\n── Micropolitan Statistical Areas ──────────────────")

write_csv(micro_long, file.path(out_dir, "National_Micro_Housing_Shortages_Long.csv"))
message("Exported: National_Micro_Housing_Shortages_Long.csv")

write_cbsa_workbook(micro_long, "Micro", "National_Micro_Housing_Shortages.xlsx")

# ==================================================================
# STEP 8: NATIONAL CBSA SUMMARY WORKBOOK
# Most recent period, ranked by CumulativeShortage descending.
# Useful for quickly identifying the most/least affected markets.
# ==================================================================
message("\n── Building summary workbook ────────────────────────")

most_recent <- max(as.character(cbsa_long$Period))

make_summary_table <- function(long_df, type_label) {
  long_df %>%
    filter(as.character(Period) == most_recent) %>%
    arrange(desc(CumulativeShortage)) %>%
    select(
      CBSA_Code, CBSA_Name,
      RenterShortage, OwnerShortage, CumulativeShortage,
      NaturalRenterVacRate, NaturalOwnerVacRate,
      RenterVacRate, OwnerVacRate
    ) %>%
    mutate(Rank = row_number(), .before = CBSA_Code)
}

msa_summary   <- make_summary_table(msa_long,   "Metropolitan")
micro_summary <- make_summary_table(micro_long,  "Micropolitan")

wb_summary <- createWorkbook()

addWorksheet(wb_summary, "MSA Summary")
writeData(wb_summary, "MSA Summary", msa_summary)

addWorksheet(wb_summary, "Micro Summary")
writeData(wb_summary, "Micro Summary", micro_summary)

# Format vacancy rate columns
pct_sty <- createStyle(numFmt = "0.0000")
rate_col_names <- c("NaturalRenterVacRate","NaturalOwnerVacRate",
                    "RenterVacRate","OwnerVacRate")

for (sht in c("MSA Summary", "Micro Summary")) {
  tbl       <- if (sht == "MSA Summary") msa_summary else micro_summary
  rate_cols <- which(names(tbl) %in% rate_col_names)
  for (col in rate_cols)
    addStyle(wb_summary, sht, pct_sty, rows = 2:(nrow(tbl) + 1), cols = col)
}

saveWorkbook(wb_summary,
             file.path(out_dir, "National_CBSA_Summary.xlsx"),
             overwrite = TRUE)
message("Exported: National_CBSA_Summary.xlsx")

# ==================================================================
# DONE
# ==================================================================
message("\n════════════════════════════════════════")
message("CBSA analysis complete. Outputs:")
message("  National_MSA_Housing_Shortages_Long.csv")
message("  National_MSA_Housing_Shortages.xlsx      (1 sheet per MSA)")
message("  National_Micro_Housing_Shortages_Long.csv")
message("  National_Micro_Housing_Shortages.xlsx    (1 sheet per Micro)")
message("  National_CBSA_Summary.xlsx               (ranked, most recent period)")
message("════════════════════════════════════════")
