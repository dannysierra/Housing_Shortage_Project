
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

setwd("~/Library/CloudStorage/OneDrive-FloridaStateUniversity/Florida_Housing_Data_Project/Updated_Years")

# ============================================================
# FLORIDA COUNTY → CBSA CROSSWALK (2023 OMB Delineation)
# Source: OMB Bulletin No. 23-01, July 21, 2023
# Verified against Wikipedia "Florida statistical areas"
# 22 MSAs | 6 Micros | 15 Non-Core  (67 counties total)
# ============================================================
fl_crosswalk <- data.frame(stringsAsFactors = FALSE,
  CountyFIPS = c(
    # ── 22 Metropolitan Statistical Areas ──────────────────
    "12086","12011","12099",           # Miami-Fort Lauderdale-West Palm Beach
    "12111","12085",                   # Port St. Lucie
    "12061",                           # Sebastian-Vero Beach
    "12095","12117","12097","12069",   # Orlando-Kissimmee-Sanford
    "12105",                           # Lakeland-Winter Haven
    "12127","12035",                   # Deltona-Daytona Beach-Ormond Beach
    "12119",                           # Wildwood-The Villages
    "12057","12103","12101","12053",   # Tampa-St. Petersburg-Clearwater
    "12031","12109","12019","12089","12003", # Jacksonville
    "12071",                           # Cape Coral-Fort Myers
    "12021",                           # Naples-Marco Island
    "12115","12081",                   # North Port-Bradenton-Sarasota
    "12015",                           # Punta Gorda
    "12009",                           # Palm Bay-Melbourne-Titusville
    "12033","12113",                   # Pensacola-Ferry Pass-Brent
    "12001","12075","12041",           # Gainesville
    "12083",                           # Ocala
    "12073","12039","12129","12065",   # Tallahassee
    "12091","12131",                   # Crestview-Fort Walton Beach-Destin
    "12005","12133",                   # Panama City-Panama City Beach
    "12017",                           # Homosassa Springs
    "12055",                           # Sebring
    # ── 6 Micropolitan Statistical Areas ───────────────────
    "12087",                           # Key West-Key Largo
    "12093",                           # Okeechobee
    "12107",                           # Palatka
    "12051","12043",                   # Clewiston
    "12027",                           # Arcadia
    "12023",                           # Lake City
    # ── Non-Core (15 counties) ─────────────────────────────
    "12007","12013","12029","12037","12045",
    "12047","12049","12059","12063","12067",
    "12077","12079","12121","12123","12125"
  ),
  CBSA_Name = c(
    "Miami-Fort Lauderdale-West Palm Beach","Miami-Fort Lauderdale-West Palm Beach","Miami-Fort Lauderdale-West Palm Beach",
    "Port St. Lucie","Port St. Lucie",
    "Sebastian-Vero Beach",
    "Orlando-Kissimmee-Sanford","Orlando-Kissimmee-Sanford","Orlando-Kissimmee-Sanford","Orlando-Kissimmee-Sanford",
    "Lakeland-Winter Haven",
    "Deltona-Daytona Beach-Ormond Beach","Deltona-Daytona Beach-Ormond Beach",
    "Wildwood-The Villages",
    "Tampa-St. Petersburg-Clearwater","Tampa-St. Petersburg-Clearwater","Tampa-St. Petersburg-Clearwater","Tampa-St. Petersburg-Clearwater",
    "Jacksonville","Jacksonville","Jacksonville","Jacksonville","Jacksonville",
    "Cape Coral-Fort Myers",
    "Naples-Marco Island",
    "North Port-Bradenton-Sarasota","North Port-Bradenton-Sarasota",
    "Punta Gorda",
    "Palm Bay-Melbourne-Titusville",
    "Pensacola-Ferry Pass-Brent","Pensacola-Ferry Pass-Brent",
    "Gainesville","Gainesville","Gainesville",
    "Ocala",
    "Tallahassee","Tallahassee","Tallahassee","Tallahassee",
    "Crestview-Fort Walton Beach-Destin","Crestview-Fort Walton Beach-Destin",
    "Panama City-Panama City Beach","Panama City-Panama City Beach",
    "Homosassa Springs",
    "Sebring",
    "Key West-Key Largo",
    "Okeechobee",
    "Palatka",
    "Clewiston","Clewiston",
    "Arcadia",
    "Lake City",
    rep("Non-Core", 15)
  ),
  CBSA_Type = c(
    rep("Metropolitan", 45),   # 45 counties in 22 MSAs
    rep("Micropolitan", 7),    # 7 counties in 6 μSAs
    rep("Non-Core", 15)        # 15 non-core counties
  )
)

cat("Crosswalk rows:", nrow(fl_crosswalk), "(should be 67)\n")
cat("Type breakdown:\n")
print(table(fl_crosswalk$CBSA_Type))

# ── Verify no duplicates ───────────────────────────────────
stopifnot(nrow(fl_crosswalk) == 67)
stopifnot(!anyDuplicated(fl_crosswalk$CountyFIPS))

# ============================================================
# LOAD LONG-FORMAT COUNTY DATA (from previous analysis)
# ============================================================
long <- read_csv("Florida_Housing_Shortages_Long.csv",
                 show_col_types = FALSE)

# Build 5-digit FIPS to join on
long <- long %>%
  mutate(CountyFIPS = paste0(
    formatC(as.integer(STATEFP),  width=2, flag="0"),
    formatC(as.integer(COUNTYFP), width=3, flag="0")
  ))

# ── Join crosswalk ─────────────────────────────────────────
long_cbsa <- long %>%
  left_join(fl_crosswalk, by = "CountyFIPS")

# Check all counties matched
unmatched <- filter(long_cbsa, is.na(CBSA_Name))
if (nrow(unmatched) > 0) {
  cat("WARNING: unmatched counties:\n")
  print(unique(unmatched$COUNTY))
} else {
  cat("All counties matched successfully.\n")
}

# ============================================================
# AGGREGATE RAW COUNTS TO CBSA LEVEL
# Critical: sum raw counts BEFORE calculating rates —
# never average rates directly across counties.
# Non-Core counties are excluded from CBSA aggregation.
# ============================================================
periods_order <- c("2006-2010","2007-2011","2008-2012","2009-2013","2010-2014",
                   "2011-2015","2012-2016","2013-2017","2014-2018","2015-2019",
                   "2016-2020","2017-2021","2018-2022","2019-2023","2020-2024")

cbsa_raw <- long_cbsa %>%
  filter(CBSA_Type != "Non-Core") %>%
  group_by(CBSA_Name, CBSA_Type, Period) %>%
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

cat("\nCBSAs in aggregated data:", n_distinct(cbsa_raw$CBSA_Name), "\n")

# ============================================================
# K&S CALCULATIONS AT CBSA LEVEL
# Same methodology as county — natural rate is long-run
# average of the CBSA's own observed rates (not county averages)
# ============================================================

# Step 1: Vacancy rates
cbsa_long <- cbsa_raw %>%
  mutate(
    RenterVacRate = ForRent / (ForRent + RenterOcc + RentedNotOcc),
    OwnerVacRate  = ForSale / (ForSale + OwnerOcc  + SoldNotOcc)
  )

# Step 2: Natural vacancy rates (average across all 15 periods per CBSA)
nat_cbsa <- cbsa_long %>%
  group_by(CBSA_Name) %>%
  summarize(
    NaturalRenterVacRate = mean(RenterVacRate, na.rm = TRUE),
    NaturalOwnerVacRate  = mean(OwnerVacRate,  na.rm = TRUE),
    .groups = "drop"
  )
cbsa_long <- left_join(cbsa_long, nat_cbsa, by = "CBSA_Name")

# Step 3: Shortages
cbsa_long <- cbsa_long %>%
  mutate(
    RenterVacDiff      = NaturalRenterVacRate - RenterVacRate,
    OwnerVacDiff       = NaturalOwnerVacRate  - OwnerVacRate,
    RenterShortage     = RenterVacDiff * (RenterOcc + RentedNotOcc + ForRent),
    OwnerShortage      = OwnerVacDiff  * (OwnerOcc  + SoldNotOcc   + ForSale),
    CumulativeShortage = RenterShortage + OwnerShortage
  )

# ── Quick check: most recent period ───────────────────────
cat("\n── CBSA Housing Shortage — Most Recent Period (2020-2024) ──\n")
print(
  cbsa_long %>%
    filter(Period == "2020-2024") %>%
    arrange(desc(CumulativeShortage)) %>%
    select(CBSA_Name, CBSA_Type, RenterShortage, OwnerShortage, CumulativeShortage) %>%
    mutate(across(where(is.numeric), round)) %>%
    as.data.frame(),
  row.names = FALSE
)

# ── Export long CSV ────────────────────────────────────────
write_csv(cbsa_long, "../Metro_Results/Florida_CBSA_Housing_Shortages_Long.csv")
cat("\nExported: Florida_CBSA_Housing_Shortages_Long.csv\n")


# ============================================================
# HELPER: unique Excel sheet names (max 31 chars)
# ============================================================
make_sheet_name <- function(wb, name) {
  name <- substr(name, 1, 31)
  base <- name; i <- 1
  while (name %in% names(wb)) {
    name <- paste0(substr(base,1,28),"_",i); i <- i+1
  }
  name
}

# ============================================================
# PLOT FUNCTION: CBSA line chart
# ============================================================
make_cbsa_plot <- function(df, cbsa_name, cbsa_type) {
  df %>%
    select(Period, RenterShortage, OwnerShortage) %>%
    pivot_longer(c(RenterShortage, OwnerShortage),
                 names_to="Type", values_to="Shortage") %>%
    mutate(Type = ifelse(Type=="OwnerShortage",
                         "Owner Housing Shortage","Renter Housing Shortage")) %>%
    ggplot(aes(x=Period, y=Shortage, group=Type, color=Type)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60") +
    geom_line(linewidth=1) + geom_point(size=3) +
    scale_y_continuous(labels=comma) +
    scale_color_manual(values=c("Owner Housing Shortage"="#2166ac",
                                "Renter Housing Shortage"="#d6604d")) +
    labs(title=paste0(cbsa_name, "\n(", cbsa_type, ")"),
         x="ACS 5-Year Period",
         y="Units (positive=shortage, negative=surplus)", color=NULL) +
    theme_minimal(base_size=11) +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.position="bottom")
}

# ============================================================
# EXCEL OUTPUT 1: Metro CBSAs (one sheet per MSA)
# ============================================================
metro_cbsas <- sort(unique(filter(cbsa_long, CBSA_Type=="Metropolitan")$CBSA_Name))
wb_metro <- createWorkbook()
pct_style <- createStyle(numFmt="0.0000")

for (cbsa in metro_cbsas) {
  cdf <- cbsa_long %>%
    filter(CBSA_Name == cbsa) %>% arrange(Period) %>%
    select(Period, NaturalRenterVacRate, NaturalOwnerVacRate,
           RenterVacRate, OwnerVacRate,
           RenterVacDiff, OwnerVacDiff,
           RenterShortage, OwnerShortage, CumulativeShortage)

  sht <- make_sheet_name(wb_metro, cbsa)
  addWorksheet(wb_metro, sht)
  writeData(wb_metro, sht, cdf)

  rate_cols <- which(names(cdf) %in% c("NaturalRenterVacRate","NaturalOwnerVacRate",
                                        "RenterVacRate","OwnerVacRate",
                                        "RenterVacDiff","OwnerVacDiff"))
  for (col in rate_cols)
    addStyle(wb_metro, sht, pct_style, rows=2:(nrow(cdf)+1), cols=col)

  p <- make_cbsa_plot(cdf, cbsa, "Metropolitan Statistical Area")
  pf <- paste0(tempfile(),".png")
  ggsave(pf, p, width=8, height=5, dpi=150)
  insertImage(wb_metro, sht, pf, startCol=12, startRow=2, width=8, height=5)
}
saveWorkbook(wb_metro, "../Metro_Results/Florida_MSA_Housing_Shortages.xlsx", overwrite = TRUE)
cat("Exported: Florida_MSA_Housing_Shortages.xlsx\n")

# ============================================================
# EXCEL OUTPUT 2: Micro CBSAs (one sheet per μSA)
# ============================================================
micro_cbsas <- sort(unique(filter(cbsa_long, CBSA_Type=="Micropolitan")$CBSA_Name))
wb_micro <- createWorkbook()

for (cbsa in micro_cbsas) {
  cdf <- cbsa_long %>%
    filter(CBSA_Name == cbsa) %>% arrange(Period) %>%
    select(Period, NaturalRenterVacRate, NaturalOwnerVacRate,
           RenterVacRate, OwnerVacRate,
           RenterVacDiff, OwnerVacDiff,
           RenterShortage, OwnerShortage, CumulativeShortage)

  sht <- make_sheet_name(wb_micro, cbsa)
  addWorksheet(wb_micro, sht)
  writeData(wb_micro, sht, cdf)

  rate_cols <- which(names(cdf) %in% c("NaturalRenterVacRate","NaturalOwnerVacRate",
                                        "RenterVacRate","OwnerVacRate",
                                        "RenterVacDiff","OwnerVacDiff"))
  for (col in rate_cols)
    addStyle(wb_micro, sht, pct_style, rows=2:(nrow(cdf)+1), cols=col)

  p <- make_cbsa_plot(cdf, cbsa, "Micropolitan Statistical Area")
  pf <- paste0(tempfile(),".png")
  ggsave(pf, p, width=8, height=5, dpi=150)
  insertImage(wb_micro, sht, pf, startCol=12, startRow=2, width=8, height=5)
}
saveWorkbook(wb_micro,
  "../Metro_Results/Florida_Micro_Housing_Shortages.xlsx", overwrite=TRUE)
cat("Exported: Florida_Micro_Housing_Shortages.xlsx\n")

cat("All CBSA outputs done.\n")
