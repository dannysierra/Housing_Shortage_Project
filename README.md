# Housing Shortage Analysis — K&S Methodology

**DeVoe L. Moore Center, Florida State University**

This repository contains the R code used to estimate housing shortages and surpluses at multiple geographic levels using the Ihlanfeldt and Sierra (K&S) methodology. The analysis spans 15 rolling ACS 5-year periods from 2006–2010 through 2020–2024 and is implemented at both the Florida and national level.

---

## Background

Florida is facing an unprecedented housing affordability crisis. While causes are complex, economists broadly agree that a significant driver is the inability of housing supply to keep pace with demand — a problem intensified by rapid in-migration from other states. This project represents a systematic attempt to quantify housing shortages at the state, regional, and local level using a methodology that explicitly accounts for local market conditions.

The analysis uses the **Ihlanfeldt and Sierra (K&S) model**, developed by Florida State University economists Keith Ihlanfeldt and Daniel Sierra. Unlike widely cited national models such as those from Freddie Mac and Rosen Consulting Group (RCG) — which are designed to capture broad national trends and cannot provide localized insights — the K&S model uses **county-specific historical vacancy rates as local equilibrium benchmarks**. This makes it sensitive to the unique dynamics of individual markets, including seasonal demand in vacation areas and supply pressures in dense urban centers.

---

## Methodology

The K&S model estimates housing shortages by comparing observed vacancy rates to a county's own long-run "natural" vacancy rate — the average vacancy rate over all available periods, which serves as a proxy for a balanced local market.

**Key variables** (sourced from ACS 5-year estimates via NHGIS):

| Variable | Description |
|---|---|
| `ForRent` | Units vacant and available for rent |
| `RenterOcc` | Renter-occupied units |
| `RentedNotOcc` | Units rented but not yet occupied |
| `ForSale` | Units vacant and available for sale |
| `OwnerOcc` | Owner-occupied units |
| `SoldNotOcc` | Units sold but not yet occupied |

**Calculations:**

```
RenterVacRate  =  ForRent / (ForRent + RenterOcc + RentedNotOcc)
OwnerVacRate   =  ForSale / (ForSale + OwnerOcc  + SoldNotOcc)

NaturalRenterVacRate  =  average(RenterVacRate) across all periods
NaturalOwnerVacRate   =  average(OwnerVacRate)  across all periods

RenterVacDiff  =  NaturalRenterVacRate − RenterVacRate
OwnerVacDiff   =  NaturalOwnerVacRate  − OwnerVacRate

RenterShortage  =  RenterVacDiff × Total Rental Market Size
OwnerShortage   =  OwnerVacDiff  × Total Owner Market Size

CumulativeShortage  =  RenterShortage + OwnerShortage
```

A positive vacancy rate difference — and therefore a positive shortage figure — indicates the observed vacancy rate is below the historical natural rate, signaling a market tighter than its own long-run equilibrium. A negative figure indicates a surplus.

---

## Data Sources

**Housing data:** IPUMS NHGIS Time Series Tables, Nominal Geographic Integration
- **D11** — Vacant Housing Units by Vacancy Status (provides ForRent, RentedNotOcc, ForSale, SoldNotOcc)
- **B37** — Occupied Housing Units by Tenure (provides OwnerOcc, RenterOcc)
- Periods: ACS 5-year estimates, 2006–2010 through 2020–2024 (n = 15)
- Citation: Schroeder et al. IPUMS NHGIS, Version 20.0. Minneapolis, MN: IPUMS, 2025. http://doi.org/10.18128/D050.V20.0

**CBSA delineation file:** NBER Census CBSA to FIPS County Crosswalk, July 2023
- Source: https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk
- Based on OMB Bulletin No. 23-01 (July 21, 2023)

---

## Repository Structure

```
├── Florida_Housing_Updated.R          # Florida county-level analysis
├── Florida_CBSA_Analysis.R            # Florida Metro and Micro analysis
├── National_Housing_Shortage_Analysis.R   # National analysis (all geographic levels)
├── National_CBSA_Analysis.R           # National Metro and Micro analysis
└── README.md
```

---

## Scripts

### 1. `Florida_Housing_Updated.R`
Runs the K&S analysis for all 67 Florida counties across all 15 ACS periods. Reads from the NHGIS county-level time series extract. Produces a long-format CSV, a per-county Excel workbook (one sheet per county with results table and line chart), and a total Florida summary workbook.

### 2. `Florida_CBSA_Analysis.R`
Aggregates Florida county-level raw counts to the CBSA level and re-runs K&S calculations. Uses a hardcoded Florida county-to-CBSA crosswalk based on the 2023 OMB delineation (22 Metropolitan Statistical Areas, 6 Micropolitan Statistical Areas). Produces separate Metro and Micro Excel workbooks. Reads from the Florida county long CSV produced by Script 1.

### 3. `National_Housing_Shortage_Analysis.R`
Runs the K&S analysis nationally across six geographic levels using NHGIS national extracts:

| Level | Units | Output |
|---|---|---|
| State | ~51 | CSV + Excel (1 sheet/state) |
| Division | 9 | CSV + Excel (1 sheet/division) |
| County | ~3,100 | CSV + 1 Excel per state in `County_by_State/` subfolder |
| Census Tract | ~73,000 | CSV only |
| Place | ~30,000 | CSV only |
| County Subdivision | ~38,000 | CSV only |

Sub-county levels (tract, place, subdivision) produce CSV outputs only. ACS estimates at these levels carry higher margins of error; vacancy-status variables in particular should be interpreted cautiously for small geographies.

### 4. `National_CBSA_Analysis.R`
Joins the national county long CSV to the NBER 2023 CBSA delineation crosswalk, aggregates raw counts to the CBSA level, and runs K&S calculations separately for Metropolitan (~384) and Micropolitan (~543) Statistical Areas. Produces long CSVs, per-CBSA Excel workbooks, and a ranked national summary workbook showing all CBSAs sorted by cumulative shortage for the most recent period.

---

## Running Order

Scripts must be run in this order due to file dependencies:

```
1. Florida_Housing_Updated.R
2. Florida_CBSA_Analysis.R          ← depends on output of (1)
3. National_Housing_Shortage_Analysis.R
4. National_CBSA_Analysis.R         ← depends on output of (3)
```

Scripts 1–2 and Scripts 3–4 are independent of each other and can be run as separate workflows.

---

## Requirements

**R packages:**
```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "openxlsx"))
```

**Before running**, update the `data_dir`, `out_dir`, and related path variables at the top of each script to match your local directory structure.

**For Script 4**, download the NBER CBSA crosswalk before running:
```
https://data.nber.org/cbsa-csa-fips-county-crosswalk/2023/cbsa2fipsxw_2023.csv
```

---

## Output Files

All results are written to the output directory defined in each script. Key outputs:

| File | Description |
|---|---|
| `Florida_Housing_Shortages_Long.csv` | Florida county results, long format |
| `Florida_Housing_Shortages_by_County.xlsx` | Per-county Excel with charts |
| `Total_Housing_Shortages_Florida.xlsx` | Statewide Florida summary |
| `Florida_MSA_Housing_Shortages.xlsx` | Florida Metro results |
| `Florida_Micro_Housing_Shortages.xlsx` | Florida Micro results |
| `National_State_Housing_Shortages_Long.csv` | State-level results, long format |
| `National_State_Housing_Shortages.xlsx` | Per-state Excel with charts |
| `National_Division_Housing_Shortages_Long.csv` | Division-level results |
| `National_County_Housing_Shortages_Long.csv` | National county results (~47,000 rows) |
| `County_by_State/*.xlsx` | Per-state county Excel workbooks |
| `National_Tract_Housing_Shortages_Long.csv` | Tract-level results (~1.1M rows) |
| `National_Place_Housing_Shortages_Long.csv` | Place-level results (~450,000 rows) |
| `National_CountySub_Housing_Shortages_Long.csv` | Subdivision results (~570,000 rows) |
| `National_MSA_Housing_Shortages_Long.csv` | National Metro results |
| `National_MSA_Housing_Shortages.xlsx` | Per-MSA Excel workbook |
| `National_Micro_Housing_Shortages_Long.csv` | National Micro results |
| `National_Micro_Housing_Shortages.xlsx` | Per-Micro Excel workbook |
| `National_CBSA_Summary.xlsx` | All CBSAs ranked by shortage, most recent period |

---

## Adding Future ACS Periods

The code is designed so that adding a new ACS period requires changing exactly one place in each script — the `periods` data frame near the top. For example, to add the 2021–2025 period when it becomes available:

```r
periods <- data.frame(
  label  = c(..., "2021-2025"),
  suffix = c(..., "255"),        # NHGIS suffix increments by 10
  tag    = c(..., "21_25"),
  stringsAsFactors = FALSE
)
```

All downstream calculations, natural vacancy rate averages, and outputs update automatically.

---

## Authors and Affiliation

**Daniel Sierra** — DeVoe L. Moore Center, Florida State University

Methodology developed by Keith Ihlanfeldt and Daniel Sierra, adapted from the National Association of Home Builders (NAHB, 2022) national shortage estimation model.

---

## Citation

If using this code or its outputs, please cite:

> Sierra, D. (2025). *Housing Shortage Analysis Using the K&S Methodology*. DeVoe L. Moore Center, Florida State University.

Cite the underlying data:

> Schroeder, J., Van Riper, D., Manson, S., Knowles, K., Kugler, T., Roberts, F., and Ruggles, S. IPUMS National Historical Geographic Information System: Version 20.0 [dataset]. Minneapolis, MN: IPUMS, 2025. http://doi.org/10.18128/D050.V20.0
