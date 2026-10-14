# CBSA-level housing shortages, K&S methodology.
# DeVoe L. Moore Center, Florida State University
#
# Picks up where National_Housing_Shortage.R leaves off: reads the county long
# CSV, joins the NBER 2023 CBSA delineation crosswalk, aggregates to CBSA, and
# runs K&S separately for metros (~384) and micros (~543).
#
# Needs the crosswalk CSV, which isn't in the NHGIS extract:
#   https://data.nber.org/cbsa-csa-fips-county-crosswalk/2023/cbsa2fipsxw_2023.csv
#
# Writes long CSVs and per-CBSA workbooks for each type, plus a ranked summary.

county_csv_path <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results/National_County_Housing_Shortages_Long.csv"
crosswalk_path  <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Data Sets/nhgis0014_csv/cbsa2fipsxw_2023.csv"
out_dir         <- "/Users/danielsierra/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/National_Housing_Shortage_Results"

# Charts in the per-CBSA sheets add roughly 30-45 minutes across ~920 CBSAs.
# FALSE gives the same tables without them.
INCLUDE_PLOTS <- TRUE


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

output_cols <- c("Period",
                 "NaturalRenterVacRate", "NaturalOwnerVacRate",
                 "RenterVacRate",        "OwnerVacRate",
                 "RenterVacDiff",        "OwnerVacDiff",
                 "RenterShortage",       "OwnerShortage",
                 "CumulativeShortage")

rate_cols <- c("NaturalRenterVacRate", "NaturalOwnerVacRate",
               "RenterVacRate", "OwnerVacRate",
               "RenterVacDiff", "OwnerVacDiff")


# Load and join -----------------------------------------------------------

county_long <- read_csv(county_csv_path, show_col_types = FALSE) %>%
  mutate(CountyFIPS = paste0(
    formatC(as.integer(STATEFP),  width = 2, flag = "0"),
    formatC(as.integer(COUNTYFP), width = 3, flag = "0")
  ))

# Two things about the NBER file: names are lowercased on read for safety, and
# the Metro/Micro column arrives truncated to "metropolitanmicropolitanstatis".
# county_fips is already 5 digits, so there's nothing to assemble.
xwalk_raw <- read_csv(crosswalk_path, show_col_types = FALSE)
names(xwalk_raw) <- tolower(names(xwalk_raw))

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

# inner_join drops non-core counties, which is intended — they belong to no CBSA.
# Worth eyeballing the match count the first time you run this against a new
# crosswalk vintage; a sudden drop usually means a FIPS formatting mismatch.
county_cbsa <- county_long %>%
  inner_join(crosswalk, by = "CountyFIPS")

cat("Counties matched to a CBSA:",
    n_distinct(county_cbsa$CountyFIPS), "of", n_distinct(county_long$CountyFIPS), "\n")


# K&S at the CBSA level ---------------------------------------------------

# Sum raw counts first, then compute rates. Averaging county rates would weight
# a 10,000-unit county the same as a 500,000-unit one.
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

cbsa_long <- cbsa_raw %>%
  mutate(
    RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
    OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
  )

# Natural rate is each CBSA's own average across the 15 periods — recomputed
# here rather than inherited from the counties, since the aggregate vacancy
# rate isn't the mean of its parts.
nat <- cbsa_long %>%
  group_by(CBSA_Code) %>%
  summarize(
    NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
    NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
    .groups = "drop"
  )

cbsa_long <- cbsa_long %>%
  left_join(nat, by = "CBSA_Code") %>%
  mutate(
    RenterVacDiff      = NaturalRenterVacRate - RenterVacRate,
    OwnerVacDiff       = NaturalOwnerVacRate  - OwnerVacRate,
    RenterShortage     = RenterVacDiff * (RenterOcc + RentedNotOcc + ForRent),
    OwnerShortage      = OwnerVacDiff  * (OwnerOcc  + SoldNotOcc   + ForSale),
    CumulativeShortage = RenterShortage + OwnerShortage
  )

msa_long   <- filter(cbsa_long, CBSA_Type == "Metropolitan")
micro_long <- filter(cbsa_long, CBSA_Type == "Micropolitan")

cat("MSAs:", n_distinct(msa_long$CBSA_Name),
    "| Micros:", n_distinct(micro_long$CBSA_Name), "\n")


# Helpers -----------------------------------------------------------------

# Excel caps sheet names at 31 characters and requires them to be unique.
# CBSA titles run long ("Minneapolis-St. Paul-Bloomington, MN-WI"), so
# collisions after truncation are common here.
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

make_cbsa_plot <- function(df, title_text) {
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

write_cbsa_workbook <- function(long_df, filename) {
  wb        <- createWorkbook()
  pct_sty   <- createStyle(numFmt = "0.0000")
  cbsa_list <- sort(unique(long_df$CBSA_Name))

  for (i in seq_along(cbsa_list)) {
    cbsa <- cbsa_list[i]
    if (i %% 50 == 0) cat(" ", i, "/", length(cbsa_list), "\n")

    tbl <- long_df %>%
      filter(CBSA_Name == cbsa) %>%
      arrange(Period) %>%
      select(all_of(output_cols))

    sht <- make_sheet_name(wb, cbsa)
    addWorksheet(wb, sht)
    writeData(wb, sht, tbl)

    for (col in which(names(tbl) %in% rate_cols)) {
      addStyle(wb, sht, pct_sty, rows = 2:(nrow(tbl) + 1), cols = col)
    }

    if (INCLUDE_PLOTS) {
      pf <- paste0(tempfile(), ".png")
      ggsave(pf, make_cbsa_plot(tbl, cbsa), width = 8, height = 5, dpi = 150)
      insertImage(wb, sht, pf, startCol = 12, startRow = 2, width = 8, height = 5)
    }
  }

  saveWorkbook(wb, file.path(out_dir, filename), overwrite = TRUE)
}


# Metro and micro outputs -------------------------------------------------

write_csv(msa_long, file.path(out_dir, "National_MSA_Housing_Shortages_Long.csv"))
cat("Building MSA workbook\n")
write_cbsa_workbook(msa_long, "National_MSA_Housing_Shortages.xlsx")

write_csv(micro_long, file.path(out_dir, "National_Micro_Housing_Shortages_Long.csv"))
cat("Building micro workbook\n")
write_cbsa_workbook(micro_long, "National_Micro_Housing_Shortages.xlsx")


# Ranked summary ----------------------------------------------------------

# Most recent period only, sorted worst shortage first — the quickest way to
# see which markets are tightest.
most_recent <- max(as.character(cbsa_long$Period))

make_summary_table <- function(long_df) {
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

msa_summary   <- make_summary_table(msa_long)
micro_summary <- make_summary_table(micro_long)

wb_summary <- createWorkbook()
pct_sty    <- createStyle(numFmt = "0.0000")

for (sht in c("MSA Summary", "Micro Summary")) {
  tbl <- if (sht == "MSA Summary") msa_summary else micro_summary
  addWorksheet(wb_summary, sht)
  writeData(wb_summary, sht, tbl)
  for (col in which(names(tbl) %in% rate_cols)) {
    addStyle(wb_summary, sht, pct_sty, rows = 2:(nrow(tbl) + 1), cols = col)
  }
}

saveWorkbook(wb_summary, file.path(out_dir, "National_CBSA_Summary.xlsx"),
             overwrite = TRUE)

cat("Done. Results in", out_dir, "\n")
