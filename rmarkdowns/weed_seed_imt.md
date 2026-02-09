Weed seed production
================

- [Setup](#setup)
- [Packages](#packages)
- [Data import](#data-import)
  - [Subsets](#subsets)
- [Model testing](#model-testing)
- [Exploratory](#exploratory)
- [Selection](#selection)
  - [Ecobean](#ecobean)
  - [Ny-only](#ny-only)
- [Summary tables](#summary-tables)
  - [Ecobean](#ecobean-1)
  - [Ny-only](#ny-only-1)
- [Figures](#figures)
  - [Ecobean](#ecobean-2)
  - [Ny-only](#ny-only-2)

# Setup

# Packages

``` r
# Packages ------------------------------------------------------------
library(tidyverse)    # includes dplyr, ggplot2, readr, tibble, etc.
library(janitor)
library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(multcomp)
library(car)
library(kableExtra)
library(here)
library(conflicted)
library(lme4)
library(purrr)
library(tibble)
library(stringr)
library(WrensBookshelf)

# Handle conflicts ----------------------------------------------------
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::recode)

# Global theme --------------------------------------------------------
theme_set(theme_bw(base_size = 12))

# Treatment level order (use everywhere) ------------------------------
mow_levels <- c(
  "No mowing",
  "Early mowing",
  "Late mowing",
  "As-needed mowing"
)

# One consistent CVD-safe color palette for all figures ---------------
fill_cols <- WB_brewer(
  name = "LittleBlueHouseBesideTheSea1",
  n    = length(mow_levels),
  type = "discrete"
) |>
  setNames(mow_levels)


# x-axis label helpers ------------------------------------------------

# Break on spaces (if you ever want every word on its own line)
label_break_spaces <- function(x) {
  stringr::str_replace_all(x, " ", "\n")
}

# Break after the comma: "Rolled,\nno control", etc.
label_break_comma <- function(x) {
  stringr::str_replace_all(x, ", ", ",\n")
}

# Break after comma AND split "high-residue cultivation"
# -> "Rolled,\nhigh-residue\ncultivation"
label_break_comma_cult <- function(x) {
  x |>
    stringr::str_replace("high-residue cultivation",
                         "high-residue\ncultivation") |>
    stringr::str_replace_all(", ", ",\n")
}

# Helper: tidy emmeans output regardless of CI column names -----------
# (works directly on an emmeans object)
# trt_var = name of treatment column to relevel (e.g., "weed_trt", "treatment")

tidy_emm <- function(emm, trt_var = NULL, ref_levels = NULL) {
  emm_df <- as.data.frame(emm)

  lcl_col <- intersect(c("lower.CL", "asymp.LCL"), names(emm_df))[1]
  ucl_col <- intersect(c("upper.CL", "asymp.UCL"), names(emm_df))[1]

  if (is.na(lcl_col) || is.na(ucl_col)) {
    stop("Could not find CI columns in emmeans output.")
  }

  out <- emm_df |>
    dplyr::mutate(
      ci_low  = .data[[lcl_col]],
      ci_high = .data[[ucl_col]]
    )

  if (!is.null(trt_var) && !is.null(ref_levels) && trt_var %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        !!trt_var := factor(.data[[trt_var]], levels = ref_levels)
      )
  }

  out
}
```

# Data import

``` r
# ---------------------------------------------------------------
# Weed seed data: read + clean + per-plot seasonal totals
# ---------------------------------------------------------------

# Trap area (100 mm diameter; 0.10 m)
trap_area_m2 <- pi * (0.10 / 2)^2   # ≈ 0.00785 m²

# Read + clean ----------------------------------------------------
seeds_all <- read_excel(
  here("..", "data", "raw", "combined_weed_seeds.xlsx"),
  na = c("na", "Na", "NA")
) |>
  clean_names() |>
  mutate(
    year      = factor(year),
    location  = factor(location),
    site_year = factor(site_year),
    block     = factor(block),
    plot      = factor(plot),
    week      = as.integer(week),

    # Recode treatment to mowing labels consistent with mow_levels
    weed_trt = dplyr::recode(
      treatment,
      "NWC" = "No mowing",
      "EWC" = "Early mowing",
      "LWC" = "Late mowing",
      "AWC" = "As-needed mowing",
      .default = NA_character_
    ),
    weed_trt = factor(weed_trt, levels = mow_levels),

    seed_number         = as.numeric(seed_number),
    seeds_per_m2_sample = seed_number / trap_area_m2
  ) |>
  dplyr::select(
    sample_id, year, location, site_year, block, plot, week,
    treatment, weed_trt, seed_number, seeds_per_m2_sample
  )

# Optional quick sanity check (helps catch unexpected treatment codes)
seeds_all |>
  count(treatment, weed_trt)
```

    ## # A tibble: 4 × 3
    ##   treatment weed_trt             n
    ##   <chr>     <fct>            <int>
    ## 1 AWC       As-needed mowing   420
    ## 2 EWC       Early mowing       419
    ## 3 LWC       Late mowing        420
    ## 4 NWC       No mowing          420

``` r
# ---- Plot-season totals normalized by total area sampled -------------
plot_totals <- seeds_all |>
  group_by(location, site_year, block, plot, weed_trt) |>
  summarise(
    seeds_total       = sum(seed_number, na.rm = TRUE),
    # count only samples with data
    n_samples         = sum(!is.na(seed_number)),
    # weeks that actually have at least one non-missing sample
    n_weeks_collected = n_distinct(week[!is.na(seed_number)]),
    .groups           = "drop"
  ) |>
  mutate(
    area_sampled_m2       = n_samples * trap_area_m2,
    seeds_per_m2_season   = if_else(
      area_sampled_m2 > 0,
      seeds_total / area_sampled_m2,
      NA_real_
    ),
    seeds_per_m2_per_week = if_else(
      n_weeks_collected > 0,
      seeds_per_m2_season / n_weeks_collected,
      NA_real_
    )
  )

# quick peek
glimpse(seeds_all)
```

    ## Rows: 1,679
    ## Columns: 11
    ## $ sample_id           <chr> "VT_B1_P101_A", "VT_B1_P101_B", "VT_B1_P101_C", "V…
    ## $ year                <fct> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 20…
    ## $ location            <fct> VT, VT, VT, VT, VT, VT, VT, VT, VT, VT, VT, VT, VT…
    ## $ site_year           <fct> VT_2023, VT_2023, VT_2023, VT_2023, VT_2023, VT_20…
    ## $ block               <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,…
    ## $ plot                <fct> 101, 101, 101, 102, 102, 102, 103, 103, 103, 104, …
    ## $ week                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ treatment           <chr> "LWC", "LWC", "LWC", "NWC", "NWC", "NWC", "EWC", "…
    ## $ weed_trt            <fct> Late mowing, Late mowing, Late mowing, No mowing, …
    ## $ seed_number         <dbl> 2, 2, 7, 4, 1, 0, 3, 0, 24, 8, 7, 1, 0, 0, 0, 3, 2…
    ## $ seeds_per_m2_sample <dbl> 254.6479, 254.6479, 891.2677, 509.2958, 127.3240, …

``` r
head(plot_totals)
```

    ## # A tibble: 6 × 11
    ##   location site_year block plot  weed_trt         seeds_total n_samples
    ##   <fct>    <fct>     <fct> <fct> <fct>                  <dbl>     <int>
    ## 1 CU       CU_2023   1     101   Early mowing              34        15
    ## 2 CU       CU_2023   1     102   Late mowing                3        15
    ## 3 CU       CU_2023   1     103   As-needed mowing           2        15
    ## 4 CU       CU_2023   1     104   No mowing                  5        15
    ## 5 CU       CU_2023   2     201   As-needed mowing          34        15
    ## 6 CU       CU_2023   2     202   No mowing                168        15
    ## # ℹ 4 more variables: n_weeks_collected <int>, area_sampled_m2 <dbl>,
    ## #   seeds_per_m2_season <dbl>, seeds_per_m2_per_week <dbl>

## Subsets

``` r
# Ecobean = all five locations in the IMT network ----------------------
seed_plot_totals_eco <- plot_totals |>
  filter(location %in% c("CU", "FH", "ME", "VT", "WI")) |>
  mutate(
    year = readr::parse_number(as.character(site_year)),
    year = factor(year, levels = sort(unique(year)))
  ) |>
  droplevels()

# New York only = Cornell + Farm Hub -----------------------------------
seed_plot_totals_ny <- plot_totals |>
  filter(location %in% c("CU", "FH")) |>
  mutate(
    year = readr::parse_number(as.character(site_year)),
    year = factor(year, levels = sort(unique(year)))
  ) |>
  droplevels()

# Optional: quick sanity check of site-years in each subset ------------
seed_plot_totals_eco |>
  count(site_year) |>
  kable(caption = "Ecobean subset: weed seed plot totals by site-year") |>
  kable_styling(full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean subset: weed seed plot totals by site-year
</caption>

<thead>

<tr>

<th style="text-align:left;">

site_year
</th>

<th style="text-align:right;">

n
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

</tbody>

</table>

``` r
seed_plot_totals_ny |>
  count(site_year) |>
  kable(caption = "NY-only subset: weed seed plot totals by site-year") |>
  kable_styling(full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only subset: weed seed plot totals by site-year
</caption>

<thead>

<tr>

<th style="text-align:left;">

site_year
</th>

<th style="text-align:right;">

n
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:right;">

16
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:right;">

16
</td>

</tr>

</tbody>

</table>

# Model testing

# Exploratory

``` r
## Exploratory ------------------------------------------------------------

## 1a) Ecobean (all five locations): seasonal weed seed density (seeds m^-2)

# Summary table by site-year × mowing ----------------------------------
seed_plot_totals_eco |>
  group_by(site_year, weed_trt) |>
  summarise(
    n       = n(),
    mean    = mean(seeds_per_m2_season, na.rm = TRUE),
    median  = median(seeds_per_m2_season, na.rm = TRUE),
    sd      = sd(seeds_per_m2_season, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(site_year, weed_trt) |>
  kable(
    digits  = 1,
    caption = "Ecobean (all locations): seasonal weed seed density (seeds m\u207b\u00b2) by site-year \u00d7 mowing treatment"
  ) |>
  kable_styling(
    full_width        = FALSE,
    bootstrap_options = c("striped", "hover")
  )
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean (all locations): seasonal weed seed density (seeds m⁻²) by
site-year × mowing treatment
</caption>

<thead>

<tr>

<th style="text-align:left;">

site_year
</th>

<th style="text-align:left;">

weed_trt
</th>

<th style="text-align:right;">

n
</th>

<th style="text-align:right;">

mean
</th>

<th style="text-align:right;">

median
</th>

<th style="text-align:right;">

sd
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

623.9
</td>

<td style="text-align:right;">

513.5
</td>

<td style="text-align:right;">

579.3
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

492.3
</td>

<td style="text-align:right;">

517.8
</td>

<td style="text-align:right;">

307.8
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

55.2
</td>

<td style="text-align:right;">

50.9
</td>

<td style="text-align:right;">

40.7
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

379.8
</td>

<td style="text-align:right;">

343.8
</td>

<td style="text-align:right;">

331.5
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:right;">

72.2
</td>

<td style="text-align:right;">

144.2
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:right;">

63.7
</td>

<td style="text-align:right;">

164.2
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

116.7
</td>

<td style="text-align:right;">

97.6
</td>

<td style="text-align:right;">

121.5
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

106.1
</td>

<td style="text-align:right;">

84.9
</td>

<td style="text-align:right;">

117.9
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

171.9
</td>

<td style="text-align:right;">

161.3
</td>

<td style="text-align:right;">

96.6
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

163.4
</td>

<td style="text-align:right;">

152.8
</td>

<td style="text-align:right;">

51.1
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

142.2
</td>

<td style="text-align:right;">

114.6
</td>

<td style="text-align:right;">

92.3
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

178.3
</td>

<td style="text-align:right;">

182.5
</td>

<td style="text-align:right;">

86.8
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

3497.2
</td>

<td style="text-align:right;">

3497.2
</td>

<td style="text-align:right;">

2425.5
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

2724.7
</td>

<td style="text-align:right;">

2601.7
</td>

<td style="text-align:right;">

1895.1
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

1808.0
</td>

<td style="text-align:right;">

1757.1
</td>

<td style="text-align:right;">

1886.5
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

1525.8
</td>

<td style="text-align:right;">

1434.5
</td>

<td style="text-align:right;">

1403.1
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

2376.7
</td>

<td style="text-align:right;">

2461.6
</td>

<td style="text-align:right;">

1043.1
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

774.6
</td>

<td style="text-align:right;">

424.4
</td>

<td style="text-align:right;">

932.7
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

1103.5
</td>

<td style="text-align:right;">

445.6
</td>

<td style="text-align:right;">

1566.3
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

583.6
</td>

<td style="text-align:right;">

742.7
</td>

<td style="text-align:right;">

333.1
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

3927.9
</td>

<td style="text-align:right;">

2767.2
</td>

<td style="text-align:right;">

2742.2
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

4093.5
</td>

<td style="text-align:right;">

3077.0
</td>

<td style="text-align:right;">

2778.4
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

1846.2
</td>

<td style="text-align:right;">

1540.6
</td>

<td style="text-align:right;">

1893.7
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

4783.1
</td>

<td style="text-align:right;">

4795.9
</td>

<td style="text-align:right;">

1220.7
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

587.8
</td>

<td style="text-align:right;">

403.2
</td>

<td style="text-align:right;">

648.8
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

914.6
</td>

<td style="text-align:right;">

687.5
</td>

<td style="text-align:right;">

911.6
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

696.0
</td>

<td style="text-align:right;">

263.1
</td>

<td style="text-align:right;">

956.2
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

1137.4
</td>

<td style="text-align:right;">

785.2
</td>

<td style="text-align:right;">

1332.0
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

82.2
</td>

<td style="text-align:right;">

69.0
</td>

<td style="text-align:right;">

62.1
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

15.9
</td>

<td style="text-align:right;">

15.9
</td>

<td style="text-align:right;">

13.7
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

53.1
</td>

<td style="text-align:right;">

53.1
</td>

<td style="text-align:right;">

46.7
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

4
</td>

<td style="text-align:right;">

63.7
</td>

<td style="text-align:right;">

63.7
</td>

<td style="text-align:right;">

58.1
</td>

</tr>

</tbody>

</table>

``` r
# Boxplot: rows = site-year, columns = location ------------------------
seed_box_eco <- seed_plot_totals_eco |>
  mutate(location = factor(location, levels = c("CU", "FH", "ME", "VT", "WI"))) |>
  ggplot(aes(x = weed_trt, y = seeds_per_m2_season, fill = weed_trt)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, color = "black") +
  geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.6, color = "grey30") +
  facet_grid(year ~ location) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = NULL,
    y = "Seasonal weed seed density (seeds m\u207b\u00b2)",
    title = "Ecobean: seasonal weed seed density by mowing treatment"
  ) +
  theme(axis.text.x = element_text(size = 9),
        strip.text  = element_text(face = "bold"))

seed_box_eco
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
## 1b) New York only (Cornell + Farm Hub): seasonal weed seed density ----

# Summary table by site-year × mowing ----------------------------------
seed_box_ny <- seed_plot_totals_ny |>
  mutate(location = factor(location, levels = c("CU", "FH"))) |>
  ggplot(aes(x = weed_trt, y = seeds_per_m2_season, fill = weed_trt)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, color = "black") +
  geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.6, color = "grey30") +
  facet_grid(year ~ location) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = NULL,
    y = "Seasonal weed seed density (seeds m\u207b\u00b2)",
    title = "New York (Cornell + Farm Hub): seasonal weed seed density by mowing treatment"
  ) +
  theme(axis.text.x = element_text(size = 9),
        strip.text  = element_text(face = "bold"))

seed_box_ny
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Figure directories for weed seed figures ------------------------------
fig_dir_seed_eco <- here("analysis", "figs", "weed-seeds", "ecobean")
fig_dir_seed_ny  <- here("analysis", "figs", "weed-seeds", "ny-only")

dir.create(fig_dir_seed_eco, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir_seed_ny,  showWarnings = FALSE, recursive = TRUE)

# Save figures ---------------------------------------------------------
ggsave(
  filename = file.path(
    fig_dir_seed_eco,
    "fig_weed_seed_density_box_ecobean_seeds_m2.png"
  ),
  plot   = seed_box_eco,
  width  = 12,
  height = 6,
  dpi    = 300
)

ggsave(
  filename = file.path(
    fig_dir_seed_ny,
    "fig_weed_seed_density_box_ny_seeds_m2.png"
  ),
  plot   = seed_box_ny,
  width  = 12,
  height = 6,
  dpi    = 300
)
```

# Selection

## Ecobean

``` r
#### Ecobean seed rain: additive vs interaction (Tweedie log) -----------

options(contrasts = c("contr.sum", "contr.poly"))

seed_totals_eco <- seed_plot_totals_eco |>
  filter(!is.na(seeds_per_m2_season))

# Additive: weed_trt + site_year ---------------------------------------
seed_eco_add <- glmmTMB(
  seeds_per_m2_season ~ weed_trt + site_year +
    (1 | site_year:block),
  family = tweedie(link = "log"),
  data   = seed_totals_eco
)

# Interaction: weed_trt * site_year ------------------------------------
seed_eco_int <- glmmTMB(
  seeds_per_m2_season ~ weed_trt * site_year +
    (1 | site_year:block),
  family = tweedie(link = "log"),
  data   = seed_totals_eco
)

# LRT + AIC comparison --------------------------------------------------
lrt_seed_eco <- anova(seed_eco_add, seed_eco_int)

AIC_add_seed_eco  <- AIC(seed_eco_add)
AIC_int_seed_eco  <- AIC(seed_eco_int)
deltaAIC_seed_eco <- AIC_add_seed_eco - AIC_int_seed_eco  # >0 => interaction better
p_int_seed_eco    <- lrt_seed_eco$`Pr(>Chisq)`[2]

# Classify evidence (same thresholds you used for yield) ----------------
p_strong_seed_eco    <- 0.01
p_none_seed_eco      <- 0.20
dAIC_strong_seed_eco <- 4

interaction_class_seed_eco <- dplyr::case_when(
  p_int_seed_eco < p_strong_seed_eco |
    deltaAIC_seed_eco >= dAIC_strong_seed_eco ~ "interaction",
  deltaAIC_seed_eco <= -dAIC_strong_seed_eco  ~ "additive",
  p_int_seed_eco > p_none_seed_eco &
    abs(deltaAIC_seed_eco) < 2                ~ "additive",
  TRUE                                        ~ "gray_zone"
)

primary_model_name_seed_eco <- dplyr::case_when(
  interaction_class_seed_eco == "interaction" ~
    "Interaction: weed_trt * site_year",
  TRUE ~
    "Additive: weed_trt + site_year"
)

interaction_class_seed_eco
```

    ## [1] "additive"

``` r
deltaAIC_seed_eco
```

    ## [1] -12.38528

``` r
p_int_seed_eco
```

    ## [1] 0.100008

``` r
# Final model for emmeans / plots --------------------------------------
seed_glmm_eco <- if (interaction_class_seed_eco == "interaction") {
  seed_eco_int
} else {
  seed_eco_add
}

# AIC table for reporting ----------------------------------------------
aic_seed_eco_out <- tibble::tibble(
  model = c("Additive: weed_trt + site_year",
            "Interaction: weed_trt * site_year"),
  AIC = c(AIC_add_seed_eco, AIC_int_seed_eco)
) |>
  dplyr::mutate(
    deltaAIC = AIC - min(AIC),
    Selected = dplyr::if_else(model == primary_model_name_seed_eco, "Yes", ""),
    Evidence_for_interaction = interaction_class_seed_eco
  )

kable(
  aic_seed_eco_out,
  digits  = 2,
  caption = "Ecobean seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): additive vs interaction (Tweedie GLMM, AIC + LRT)"
) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean seed rain (seeds m⁻² season⁻¹): additive vs interaction (Tweedie
GLMM, AIC + LRT)
</caption>

<thead>

<tr>

<th style="text-align:left;">

model
</th>

<th style="text-align:right;">

AIC
</th>

<th style="text-align:right;">

deltaAIC
</th>

<th style="text-align:left;">

Selected
</th>

<th style="text-align:left;">

Evidence_for_interaction
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Additive: weed_trt + site_year
</td>

<td style="text-align:right;">

1855.84
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:left;">

Yes
</td>

<td style="text-align:left;">

additive
</td>

</tr>

<tr>

<td style="text-align:left;">

Interaction: weed_trt \* site_year
</td>

<td style="text-align:right;">

1868.23
</td>

<td style="text-align:right;">

12.39
</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

additive
</td>

</tr>

</tbody>

</table>

``` r
# Diagnostics + Type-III tests -----------------------------------------
set.seed(123)
res_seed_eco <- DHARMa::simulateResiduals(seed_glmm_eco)
plot(res_seed_eco)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
DHARMa::testDispersion(seed_glmm_eco)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted vs.
    ##  simulated
    ## 
    ## data:  simulationOutput
    ## dispersion = 0.75953, p-value = 0.848
    ## alternative hypothesis: two.sided

``` r
DHARMa::testZeroInflation(seed_glmm_eco)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

    ## 
    ##  DHARMa zero-inflation test via comparison to expected zeros with
    ##  simulation under H0 = fitted model
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 0.90123, p-value = 1
    ## alternative hypothesis: two.sided

``` r
car::Anova(seed_glmm_eco, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: seeds_per_m2_season
    ##                 Chisq Df Pr(>Chisq)    
    ## (Intercept) 2992.4992  1    < 2e-16 ***
    ## weed_trt       9.6683  3    0.02161 *  
    ## site_year    159.6021  7    < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Ny-only

``` r
#### NY-only seed rain: additive vs interaction (Tweedie log) -----------

options(contrasts = c("contr.sum", "contr.poly"))

seed_totals_ny <- seed_plot_totals_ny |>
  filter(!is.na(seeds_per_m2_season))

# Additive: weed_trt + site_year ---------------------------------------
seed_ny_add <- glmmTMB(
  seeds_per_m2_season ~ weed_trt + site_year +
    (1 | site_year:block),
  family = tweedie(link = "log"),
  data   = seed_totals_ny
)

# Interaction: weed_trt * site_year ------------------------------------
seed_ny_int <- glmmTMB(
  seeds_per_m2_season ~ weed_trt * site_year +
    (1 | site_year:block),
  family = tweedie(link = "log"),
  data   = seed_totals_ny
)

# LRT + AIC comparison --------------------------------------------------
lrt_seed_ny <- anova(seed_ny_add, seed_ny_int)

AIC_add_seed_ny  <- AIC(seed_ny_add)
AIC_int_seed_ny  <- AIC(seed_ny_int)
deltaAIC_seed_ny <- AIC_add_seed_ny - AIC_int_seed_ny  # >0 => interaction better
p_int_seed_ny    <- lrt_seed_ny$`Pr(>Chisq)`[2]

# Classify evidence -----------------------------------------------------
p_strong_seed_ny    <- 0.01
p_none_seed_ny      <- 0.20
dAIC_strong_seed_ny <- 4

interaction_class_seed_ny <- dplyr::case_when(
  p_int_seed_ny < p_strong_seed_ny |
    deltaAIC_seed_ny >= dAIC_strong_seed_ny ~ "interaction",
  deltaAIC_seed_ny <= -dAIC_strong_seed_ny  ~ "additive",
  p_int_seed_ny > p_none_seed_ny &
    abs(deltaAIC_seed_ny) < 2               ~ "additive",
  TRUE                                       ~ "gray_zone"
)

primary_model_name_seed_ny <- dplyr::case_when(
  interaction_class_seed_ny == "interaction" ~
    "Interaction: weed_trt * site_year",
  TRUE ~
    "Additive: weed_trt + site_year"
)

interaction_class_seed_ny
```

    ## [1] "additive"

``` r
deltaAIC_seed_ny
```

    ## [1] -4.018591

``` r
p_int_seed_ny
```

    ## [1] 0.1229881

``` r
# Final model for emmeans / plots --------------------------------------
seed_glmm_ny <- if (interaction_class_seed_ny == "interaction") {
  seed_ny_int
} else {
  seed_ny_add
}

# AIC table for reporting ----------------------------------------------
aic_seed_ny_out <- tibble::tibble(
  model = c("Additive: weed_trt + site_year",
            "Interaction: weed_trt * site_year"),
  AIC = c(AIC_add_seed_ny, AIC_int_seed_ny)
) |>
  dplyr::mutate(
    deltaAIC = AIC - min(AIC),
    Selected = dplyr::if_else(model == primary_model_name_seed_ny, "Yes", ""),
    Evidence_for_interaction = interaction_class_seed_ny
  )

kable(
  aic_seed_ny_out,
  digits  = 2,
  caption = "NY-only seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): additive vs interaction (Tweedie GLMM, AIC + LRT)"
) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only seed rain (seeds m⁻² season⁻¹): additive vs interaction (Tweedie
GLMM, AIC + LRT)
</caption>

<thead>

<tr>

<th style="text-align:left;">

model
</th>

<th style="text-align:right;">

AIC
</th>

<th style="text-align:right;">

deltaAIC
</th>

<th style="text-align:left;">

Selected
</th>

<th style="text-align:left;">

Evidence_for_interaction
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Additive: weed_trt + site_year
</td>

<td style="text-align:right;">

885.69
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:left;">

Yes
</td>

<td style="text-align:left;">

additive
</td>

</tr>

<tr>

<td style="text-align:left;">

Interaction: weed_trt \* site_year
</td>

<td style="text-align:right;">

889.71
</td>

<td style="text-align:right;">

4.02
</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

additive
</td>

</tr>

</tbody>

</table>

``` r
# Diagnostics + Type-III tests -----------------------------------------
set.seed(123)
res_seed_ny <- DHARMa::simulateResiduals(seed_glmm_ny)
plot(res_seed_ny)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
DHARMa::testDispersion(seed_glmm_ny)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted vs.
    ##  simulated
    ## 
    ## data:  simulationOutput
    ## dispersion = 0.64796, p-value = 0.848
    ## alternative hypothesis: two.sided

``` r
DHARMa::testZeroInflation(seed_glmm_ny)
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

    ## 
    ##  DHARMa zero-inflation test via comparison to expected zeros with
    ##  simulation under H0 = fitted model
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 0.85763, p-value = 1
    ## alternative hypothesis: two.sided

``` r
car::Anova(seed_glmm_ny, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: seeds_per_m2_season
    ##               Chisq Df Pr(>Chisq)    
    ## (Intercept) 848.278  1  < 2.2e-16 ***
    ## weed_trt      9.972  3    0.01881 *  
    ## site_year    35.762  3  8.409e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Summary tables

## Ecobean

``` r
## Ecobean: weed seed rain (seeds m^-2 season^-1) summary tables ---------

# Directory for all Ecobean weed-seed tables ----------------------------
tab_dir_seed_eco <- here("analysis", "tables", "weed-seeds", "Ecobean")
dir.create(tab_dir_seed_eco, showWarnings = FALSE, recursive = TRUE)


## 1) P-value summary (mowing, site-year, + LRT) -------------------------

anova_seed_eco <- car::Anova(seed_glmm_eco, type = 3)

anova_seed_eco_df <- anova_seed_eco |>
  as.data.frame() |>
  tibble::rownames_to_column("Effect")

p_trt_eco  <- anova_seed_eco_df$`Pr(>Chisq)`[anova_seed_eco_df$Effect == "weed_trt"]
p_site_eco <- anova_seed_eco_df$`Pr(>Chisq)`[anova_seed_eco_df$Effect == "site_year"]

pvals_seed_eco <- tibble::tibble(
  Effect = c("Mowing (weed_trt)", "Site-year"),
  p_raw  = c(p_trt_eco, p_site_eco)
)

# Add mowing × site-year row from LRT, even though final model is often additive
pvals_seed_eco <- pvals_seed_eco |>
  dplyr::bind_rows(
    tibble::tibble(
      Effect = "Mowing × Site-year (weed_trt × site_year)",
      p_raw  = p_int_seed_eco
    )
  ) |>
  dplyr::mutate(
    `P-value` = dplyr::case_when(
      p_raw < 0.001 ~ "<0.001",
      p_raw < 0.01  ~ "<0.01",
      TRUE          ~ sprintf("%.3f", p_raw)
    )
  ) |>
  dplyr::select(Effect, `P-value`)

# Save ANOVA/LRT p-value summary ----------------------------------------
readr::write_csv(
  pvals_seed_eco,
  file.path(tab_dir_seed_eco, "tab_eco-weed-seeds_Anova_pvals.csv")
)

pvals_seed_eco |>
  kable(
    caption   = "Ecobean weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): ANOVA and LRT p-values",
    col.names = c("Effect", "P-value")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean weed seed rain (seeds m⁻² season⁻¹): ANOVA and LRT p-values
</caption>

<thead>

<tr>

<th style="text-align:left;">

Effect
</th>

<th style="text-align:left;">

P-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mowing (weed_trt)
</td>

<td style="text-align:left;">

0.022
</td>

</tr>

<tr>

<td style="text-align:left;">

Site-year
</td>

<td style="text-align:left;">

\<0.001
</td>

</tr>

<tr>

<td style="text-align:left;">

Mowing × Site-year (weed_trt × site_year)
</td>

<td style="text-align:left;">

0.100
</td>

</tr>

</tbody>

</table>

``` r
## 2) Site-year means (model + raw) -------------------------------------

emm_sy_seed_eco <- emmeans(
  seed_glmm_eco,
  ~ site_year,
  type = "response"
)

emm_sy_seed_eco_tmp <- tidy_emm(emm_sy_seed_eco)

emm_sy_seed_eco_df <- emm_sy_seed_eco_tmp |>
  dplyr::mutate(
    site_year  = as.factor(site_year),
    model_mean = if ("response" %in% names(emm_sy_seed_eco_tmp)) response else emmean
  ) |>
  dplyr::select(site_year, model_mean, SE, ci_low, ci_high)

cld_sy_seed_eco <- multcomp::cld(
  emm_sy_seed_eco,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    sy_CLD    = stringr::str_trim(.group)
  ) |>
  dplyr::select(site_year, sy_CLD)

raw_sy_seed_eco <- seed_plot_totals_eco |>
  dplyr::group_by(site_year) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(site_year = as.factor(site_year))

sy_summary_seed_eco <- emm_sy_seed_eco_df |>
  dplyr::left_join(cld_sy_seed_eco, by = "site_year") |>
  dplyr::left_join(raw_sy_seed_eco, by = "site_year") |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    SE         = round(SE, 1),
    ci_low     = round(ci_low, 1),
    ci_high    = round(ci_high, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = sy_CLD
  ) |>
  dplyr::arrange(site_year)

readr::write_csv(
  sy_summary_seed_eco,
  file.path(tab_dir_seed_eco, "tab_eco-weed-seeds_site-year_means_CLD.csv")
)

sy_summary_seed_eco |>
  kable(
    caption   = "Ecobean weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): site-year means with CLDs",
    col.names = c("Site-year", "Model mean", "SE", "Lower CI", "Upper CI", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean weed seed rain (seeds m⁻² season⁻¹): site-year means with CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Site-year
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

Lower CI
</th>

<th style="text-align:right;">

Upper CI
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:right;">

337.1
</td>

<td style="text-align:right;">

103.9
</td>

<td style="text-align:right;">

184.3
</td>

<td style="text-align:right;">

616.6
</td>

<td style="text-align:left;">

d
</td>

<td style="text-align:right;">

387.8
</td>

<td style="text-align:left;">

d
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:right;">

101.3
</td>

<td style="text-align:right;">

36.1
</td>

<td style="text-align:right;">

50.4
</td>

<td style="text-align:right;">

203.6
</td>

<td style="text-align:left;">

ef
</td>

<td style="text-align:right;">

115.1
</td>

<td style="text-align:left;">

ef
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:right;">

164.8
</td>

<td style="text-align:right;">

53.6
</td>

<td style="text-align:right;">

87.2
</td>

<td style="text-align:right;">

311.6
</td>

<td style="text-align:left;">

de
</td>

<td style="text-align:right;">

163.9
</td>

<td style="text-align:left;">

de
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:right;">

1967.5
</td>

<td style="text-align:right;">

535.1
</td>

<td style="text-align:right;">

1154.6
</td>

<td style="text-align:right;">

3352.8
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

2388.9
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:right;">

1141.1
</td>

<td style="text-align:right;">

313.5
</td>

<td style="text-align:right;">

666.0
</td>

<td style="text-align:right;">

1955.0
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

1209.6
</td>

<td style="text-align:left;">

bc
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:right;">

3321.0
</td>

<td style="text-align:right;">

848.1
</td>

<td style="text-align:right;">

2013.2
</td>

<td style="text-align:right;">

5478.3
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

3662.7
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:right;">

788.1
</td>

<td style="text-align:right;">

224.5
</td>

<td style="text-align:right;">

451.0
</td>

<td style="text-align:right;">

1377.4
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

834.0
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:right;">

50.7
</td>

<td style="text-align:right;">

19.0
</td>

<td style="text-align:right;">

24.3
</td>

<td style="text-align:right;">

105.8
</td>

<td style="text-align:left;">

f
</td>

<td style="text-align:right;">

53.7
</td>

<td style="text-align:left;">

f
</td>

</tr>

</tbody>

</table>

``` r
## 3) Mowing means (model + raw) ----------------------------------------

emm_trt_seed_eco <- emmeans(
  seed_glmm_eco,
  ~ weed_trt,
  type = "response"
)

emm_trt_seed_eco_tmp <- tidy_emm(
  emm_trt_seed_eco,
  trt_var    = "weed_trt",
  ref_levels = mow_levels
)

emm_trt_seed_eco_df <- emm_trt_seed_eco_tmp |>
  dplyr::mutate(
    weed_trt   = factor(weed_trt, levels = mow_levels),
    model_mean = if ("response" %in% names(emm_trt_seed_eco_tmp)) response else emmean
  ) |>
  dplyr::select(weed_trt, model_mean, SE, ci_low, ci_high)

cld_trt_seed_eco <- multcomp::cld(
  emm_trt_seed_eco,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    trt_CLD  = stringr::str_trim(.group)
  ) |>
  dplyr::select(weed_trt, trt_CLD)

raw_trt_seed_eco <- seed_plot_totals_eco |>
  dplyr::group_by(weed_trt) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(weed_trt = factor(weed_trt, levels = mow_levels))

trt_summary_seed_eco <- emm_trt_seed_eco_df |>
  dplyr::left_join(cld_trt_seed_eco, by = "weed_trt") |>
  dplyr::left_join(raw_trt_seed_eco, by = "weed_trt") |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    SE         = round(SE, 1),
    ci_low     = round(ci_low, 1),
    ci_high    = round(ci_high, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = trt_CLD
  ) |>
  dplyr::arrange(weed_trt)

# Save treatment summary -------------------------------------------------
readr::write_csv(
  trt_summary_seed_eco,
  file.path(tab_dir_seed_eco, "tab_eco-weed-seeds_mowing_means_CLD.csv")
)

trt_summary_seed_eco |>
  kable(
    caption   = "Ecobean weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): mowing-treatment means with CLDs",
    col.names = c("Treatment", "Model mean", "SE", "Lower CI", "Upper CI", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean weed seed rain (seeds m⁻² season⁻¹): mowing-treatment means with
CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Treatment
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

Lower CI
</th>

<th style="text-align:right;">

Upper CI
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

607.5
</td>

<td style="text-align:right;">

99.1
</td>

<td style="text-align:right;">

441.3
</td>

<td style="text-align:right;">

836.3
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

1423.3
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

469.0
</td>

<td style="text-align:right;">

78.8
</td>

<td style="text-align:right;">

337.4
</td>

<td style="text-align:right;">

652.0
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

1162.2
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

315.2
</td>

<td style="text-align:right;">

56.5
</td>

<td style="text-align:right;">

221.8
</td>

<td style="text-align:right;">

448.0
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

727.6
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

455.9
</td>

<td style="text-align:right;">

77.1
</td>

<td style="text-align:right;">

327.3
</td>

<td style="text-align:right;">

635.0
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

1094.7
</td>

<td style="text-align:left;">

ab
</td>

</tr>

</tbody>

</table>

``` r
## 4) Site-year × mowing means (model + raw) ----------------------------

emm_int_seed_eco <- emmeans(
  seed_glmm_eco,
  ~ weed_trt | site_year,
  type = "response"
)

emm_int_seed_eco_tmp <- as_tibble(emm_int_seed_eco)

emm_int_seed_eco_df <- emm_int_seed_eco_tmp |>
  dplyr::mutate(
    site_year  = as.factor(site_year),
    weed_trt   = factor(weed_trt, levels = mow_levels),
    model_mean = if ("response" %in% names(emm_int_seed_eco_tmp)) response else emmean
  ) |>
  dplyr::select(site_year, weed_trt, model_mean)

cld_int_seed_eco <- multcomp::cld(
  emm_int_seed_eco,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    weed_trt  = factor(weed_trt, levels = mow_levels),
    int_CLD   = stringr::str_trim(.group)
  ) |>
  dplyr::select(site_year, weed_trt, int_CLD)

raw_int_seed_eco <- seed_plot_totals_eco |>
  dplyr::group_by(site_year, weed_trt) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    weed_trt  = factor(weed_trt, levels = mow_levels)
  )

int_summary_seed_eco <- emm_int_seed_eco_df |>
  dplyr::left_join(cld_int_seed_eco, by = c("site_year", "weed_trt")) |>
  dplyr::left_join(raw_int_seed_eco, by = c("site_year", "weed_trt")) |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = int_CLD
  ) |>
  dplyr::arrange(site_year, weed_trt)

# Save interaction summary ----------------------------------------------
readr::write_csv(
  int_summary_seed_eco,
  file.path(tab_dir_seed_eco, "tab_eco-weed-seeds_site-year_treatment_means_CLD.csv")
)

int_summary_seed_eco |>
  kable(
    caption   = "Ecobean weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): site-year \u00d7 treatment means with CLDs",
    col.names = c("Site-year", "Treatment", "Model mean", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Ecobean weed seed rain (seeds m⁻² season⁻¹): site-year × treatment means
with CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Site-year
</th>

<th style="text-align:left;">

Treatment
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

455.3
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

623.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

351.5
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

492.3
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

236.2
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

55.2
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

341.7
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

379.8
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

136.8
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

105.6
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

71.0
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

116.7
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

102.6
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

106.1
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

222.6
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

171.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

171.9
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

163.4
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

115.5
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

142.2
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

167.0
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

178.3
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

2657.0
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

3497.2
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

2051.5
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

2724.7
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

1378.6
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

1808.0
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

1994.1
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

1525.8
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

1541.0
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

2376.7
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

1189.8
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

774.6
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

799.5
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

1103.5
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

ME_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

1156.5
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

583.6
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

4484.9
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

3927.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

3462.7
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

4093.5
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

2327.0
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

1846.2
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

3365.9
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

4783.1
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

1064.4
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

587.8
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

821.8
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

914.6
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

552.2
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

696.0
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

VT_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

798.8
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

1137.4
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

68.5
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

82.2
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

52.9
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

15.9
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

35.5
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

53.1
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

WI_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

51.4
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

63.7
</td>

<td style="text-align:left;">

ab
</td>

</tr>

</tbody>

</table>

``` r
## 5) Model-info row for global seed summary ----------------------------

family_structure_seed_eco <- "Tweedie GLMM (log link)"

model_info_seed_eco <- tibble::tibble(
  response_label    = "Weed seed rain (seeds m^-2 season^-1), Ecobean model-predicted means*",
  family_structure  = family_structure_seed_eco,
  fixed_effects     = primary_model_name_seed_eco,
  random_effects    = "(1 | site_year:block)",
  AIC_additive      = round(AIC_add_seed_eco, 2),
  AIC_interaction   = round(AIC_int_seed_eco, 2),
  deltaAIC_add_int  = round(deltaAIC_seed_eco, 2),
  LRT_p_int_raw     = p_int_seed_eco,
  LRT_p_int_label   = dplyr::case_when(
    p_int_seed_eco < 0.001 ~ "<0.001",
    p_int_seed_eco < 0.01  ~ "<0.01",
    TRUE                   ~ sprintf("%.3f", p_int_seed_eco)
  ),
  interaction_class = interaction_class_seed_eco
)

readr::write_csv(
  model_info_seed_eco,
  file.path(tab_dir_seed_eco, "tab_eco-weed-seeds_model-info.csv")
)


## 6) OPTIONAL: combine into one Ecobean weed-seed workbook -------------

if (requireNamespace("writexl", quietly = TRUE)) {
  seed_tables_eco <- list(
    Anova_pvals         = pvals_seed_eco,
    SiteYear_means_CLD  = sy_summary_seed_eco,
    Mowing_means_CLD    = trt_summary_seed_eco,
    SiteYear_trt_means  = int_summary_seed_eco,
    Model_info          = model_info_seed_eco
  )
  
  writexl::write_xlsx(
    seed_tables_eco,
    path = file.path(tab_dir_seed_eco, "Ecobean_weed-seeds_all-tables.xlsx")
  )
} else {
  message("Package 'writexl' not installed; skipping Ecobean weed-seeds Excel export.")
}
```

## Ny-only

``` r
## NY-only: weed seed rain (seeds m^-2 season^-1) summary tables ---------

# Directory for all NY-only weed-seed tables ----------------------------
tab_dir_seed_ny <- here("analysis", "tables", "weed-seeds", "NY-only")
dir.create(tab_dir_seed_ny, showWarnings = FALSE, recursive = TRUE)


## 1) P-value summary (mowing, site-year, + LRT) -------------------------

anova_seed_ny <- car::Anova(seed_glmm_ny, type = 3)

anova_seed_ny_df <- anova_seed_ny |>
  as.data.frame() |>
  tibble::rownames_to_column("Effect")

p_trt_ny  <- anova_seed_ny_df$`Pr(>Chisq)`[anova_seed_ny_df$Effect == "weed_trt"]
p_site_ny <- anova_seed_ny_df$`Pr(>Chisq)`[anova_seed_ny_df$Effect == "site_year"]

pvals_seed_ny <- tibble::tibble(
  Effect = c("Mowing (weed_trt)", "Site-year"),
  p_raw  = c(p_trt_ny, p_site_ny)
)

# Add mowing × site-year row from LRT, even though final model is often additive
pvals_seed_ny <- pvals_seed_ny |>
  dplyr::bind_rows(
    tibble::tibble(
      Effect = "Mowing × Site-year (weed_trt × site_year)",
      p_raw  = p_int_seed_ny
    )
  ) |>
  dplyr::mutate(
    `P-value` = dplyr::case_when(
      p_raw < 0.001 ~ "<0.001",
      p_raw < 0.01  ~ "<0.01",
      TRUE          ~ sprintf("%.3f", p_raw)
    )
  ) |>
  dplyr::select(Effect, `P-value`)

# Save ANOVA/LRT p-value summary ----------------------------------------
readr::write_csv(
  pvals_seed_ny,
  file.path(tab_dir_seed_ny, "tab_ny-weed-seeds_Anova_pvals.csv")
)

pvals_seed_ny |>
  kable(
    caption   = "NY-only weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): ANOVA and LRT p-values",
    col.names = c("Effect", "P-value")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only weed seed rain (seeds m⁻² season⁻¹): ANOVA and LRT p-values
</caption>

<thead>

<tr>

<th style="text-align:left;">

Effect
</th>

<th style="text-align:left;">

P-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mowing (weed_trt)
</td>

<td style="text-align:left;">

0.019
</td>

</tr>

<tr>

<td style="text-align:left;">

Site-year
</td>

<td style="text-align:left;">

\<0.001
</td>

</tr>

<tr>

<td style="text-align:left;">

Mowing × Site-year (weed_trt × site_year)
</td>

<td style="text-align:left;">

0.123
</td>

</tr>

</tbody>

</table>

``` r
## 2) Site-year means (model + raw) -------------------------------------

emm_sy_seed_ny <- emmeans(
  seed_glmm_ny,
  ~ site_year,
  type = "response"
)

emm_sy_seed_ny_tmp <- tidy_emm(emm_sy_seed_ny)

emm_sy_seed_ny_df <- emm_sy_seed_ny_tmp |>
  dplyr::mutate(
    site_year  = as.factor(site_year),
    model_mean = if ("response" %in% names(emm_sy_seed_ny_tmp)) response else emmean
  ) |>
  dplyr::select(site_year, model_mean, SE, ci_low, ci_high)

cld_sy_seed_ny <- multcomp::cld(
  emm_sy_seed_ny,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    sy_CLD    = stringr::str_trim(.group)
  ) |>
  dplyr::select(site_year, sy_CLD)

raw_sy_seed_ny <- seed_plot_totals_ny |>
  dplyr::group_by(site_year) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(site_year = as.factor(site_year))

sy_summary_seed_ny <- emm_sy_seed_ny_df |>
  dplyr::left_join(cld_sy_seed_ny, by = "site_year") |>
  dplyr::left_join(raw_sy_seed_ny, by = "site_year") |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    SE         = round(SE, 1),
    ci_low     = round(ci_low, 1),
    ci_high    = round(ci_high, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = sy_CLD
  ) |>
  dplyr::arrange(site_year)

readr::write_csv(
  sy_summary_seed_ny,
  file.path(tab_dir_seed_ny, "tab_ny-weed-seeds_site-year_means_CLD.csv")
)

sy_summary_seed_ny |>
  kable(
    caption   = "NY-only weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): site-year means with CLDs",
    col.names = c("Site-year", "Model mean", "SE", "Lower CI", "Upper CI", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only weed seed rain (seeds m⁻² season⁻¹): site-year means with CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Site-year
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

Lower CI
</th>

<th style="text-align:right;">

Upper CI
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:right;">

313.7
</td>

<td style="text-align:right;">

119.2
</td>

<td style="text-align:right;">

149.0
</td>

<td style="text-align:right;">

660.6
</td>

<td style="text-align:left;">

b
</td>

<td style="text-align:right;">

387.8
</td>

<td style="text-align:left;">

b
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:right;">

86.8
</td>

<td style="text-align:right;">

36.8
</td>

<td style="text-align:right;">

37.9
</td>

<td style="text-align:right;">

199.1
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

115.1
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:right;">

163.1
</td>

<td style="text-align:right;">

63.6
</td>

<td style="text-align:right;">

76.0
</td>

<td style="text-align:right;">

350.2
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

163.9
</td>

<td style="text-align:left;">

bc
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:right;">

1758.5
</td>

<td style="text-align:right;">

625.7
</td>

<td style="text-align:right;">

875.5
</td>

<td style="text-align:right;">

3532.0
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

2388.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

</tbody>

</table>

``` r
## 3) Mowing means (model + raw) ----------------------------------------

emm_trt_seed_ny <- emmeans(
  seed_glmm_ny,
  ~ weed_trt,
  type = "response"
)

emm_trt_seed_ny_tmp <- tidy_emm(
  emm_trt_seed_ny,
  trt_var    = "weed_trt",
  ref_levels = mow_levels
)

emm_trt_seed_ny_df <- emm_trt_seed_ny_tmp |>
  dplyr::mutate(
    weed_trt   = factor(weed_trt, levels = mow_levels),
    model_mean = if ("response" %in% names(emm_trt_seed_ny_tmp)) response else emmean
  ) |>
  dplyr::select(weed_trt, model_mean, SE, ci_low, ci_high)

cld_trt_seed_ny <- multcomp::cld(
  emm_trt_seed_ny,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    trt_CLD  = stringr::str_trim(.group)
  ) |>
  dplyr::select(weed_trt, trt_CLD)

raw_trt_seed_ny <- seed_plot_totals_ny |>
  dplyr::group_by(weed_trt) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(weed_trt = factor(weed_trt, levels = mow_levels))

trt_summary_seed_ny <- emm_trt_seed_ny_df |>
  dplyr::left_join(cld_trt_seed_ny, by = "weed_trt") |>
  dplyr::left_join(raw_trt_seed_ny, by = "weed_trt") |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    SE         = round(SE, 1),
    ci_low     = round(ci_low, 1),
    ci_high    = round(ci_high, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = trt_CLD
  ) |>
  dplyr::arrange(weed_trt)

# Save treatment summary -------------------------------------------------
readr::write_csv(
  trt_summary_seed_ny,
  file.path(tab_dir_seed_ny, "tab_ny-weed-seeds_mowing_means_CLD.csv")
)

trt_summary_seed_ny |>
  kable(
    caption   = "NY-only weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): mowing-treatment means with CLDs",
    col.names = c("Treatment", "Model mean", "SE", "Lower CI", "Upper CI", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only weed seed rain (seeds m⁻² season⁻¹): mowing-treatment means with
CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Treatment
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

Lower CI
</th>

<th style="text-align:right;">

Upper CI
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

418.8
</td>

<td style="text-align:right;">

100.8
</td>

<td style="text-align:right;">

261.3
</td>

<td style="text-align:right;">

671.2
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

1102.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

357.2
</td>

<td style="text-align:right;">

86.4
</td>

<td style="text-align:right;">

222.4
</td>

<td style="text-align:right;">

573.7
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

874.8
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

203.9
</td>

<td style="text-align:right;">

52.6
</td>

<td style="text-align:right;">

123.0
</td>

<td style="text-align:right;">

338.1
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

530.5
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

256.2
</td>

<td style="text-align:right;">

64.5
</td>

<td style="text-align:right;">

156.5
</td>

<td style="text-align:right;">

419.5
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

547.5
</td>

<td style="text-align:left;">

bc
</td>

</tr>

</tbody>

</table>

``` r
## 4) Site-year × mowing means (model + raw) ----------------------------

emm_int_seed_ny <- emmeans(
  seed_glmm_ny,
  ~ weed_trt | site_year,
  type = "response"
)

emm_int_seed_ny_tmp <- as_tibble(emm_int_seed_ny)

emm_int_seed_ny_df <- emm_int_seed_ny_tmp |>
  dplyr::mutate(
    site_year  = as.factor(site_year),
    weed_trt   = factor(weed_trt, levels = mow_levels),
    model_mean = if ("response" %in% names(emm_int_seed_ny_tmp)) response else emmean
  ) |>
  dplyr::select(site_year, weed_trt, model_mean)

cld_int_seed_ny <- multcomp::cld(
  emm_int_seed_ny,
  adjust   = "none",
  Letters  = letters,
  sort     = TRUE,
  reversed = TRUE
) |>
  as_tibble() |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    weed_trt  = factor(weed_trt, levels = mow_levels),
    int_CLD   = stringr::str_trim(.group)
  ) |>
  dplyr::select(site_year, weed_trt, int_CLD)

raw_int_seed_ny <- seed_plot_totals_ny |>
  dplyr::group_by(site_year, weed_trt) |>
  dplyr::summarise(
    raw_mean = mean(seeds_per_m2_season, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  dplyr::mutate(
    site_year = as.factor(site_year),
    weed_trt  = factor(weed_trt, levels = mow_levels)
  )

int_summary_seed_ny <- emm_int_seed_ny_df |>
  dplyr::left_join(cld_int_seed_ny, by = c("site_year", "weed_trt")) |>
  dplyr::left_join(raw_int_seed_ny, by = c("site_year", "weed_trt")) |>
  dplyr::mutate(
    model_mean = round(model_mean, 1),
    raw_mean   = round(raw_mean, 1),
    raw_CLD    = int_CLD
  ) |>
  dplyr::arrange(site_year, weed_trt)

# Save interaction summary ----------------------------------------------
readr::write_csv(
  int_summary_seed_ny,
  file.path(tab_dir_seed_ny, "tab_ny-weed-seeds_site-year_treatment_means_CLD.csv")
)

int_summary_seed_ny |>
  kable(
    caption   = "NY-only weed seed rain (seeds m\u207b\u00b2 season\u207b\u00b9): site-year \u00d7 treatment means with CLDs",
    col.names = c("Site-year", "Treatment", "Model mean", "Model CLD", "Raw mean", "Raw CLD")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

NY-only weed seed rain (seeds m⁻² season⁻¹): site-year × treatment means
with CLDs
</caption>

<thead>

<tr>

<th style="text-align:left;">

Site-year
</th>

<th style="text-align:left;">

Treatment
</th>

<th style="text-align:right;">

Model mean
</th>

<th style="text-align:left;">

Model CLD
</th>

<th style="text-align:right;">

Raw mean
</th>

<th style="text-align:left;">

Raw CLD
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

441.9
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

623.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

376.9
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

492.3
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

215.2
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

55.2
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

270.4
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

379.8
</td>

<td style="text-align:left;">

bc
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

122.3
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

104.3
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

118.8
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

59.6
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

116.7
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

CU_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

74.8
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

106.1
</td>

<td style="text-align:left;">

bc
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

229.7
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

171.9
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

195.9
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

163.4
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

111.9
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

142.2
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2023
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

140.6
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

178.3
</td>

<td style="text-align:left;">

bc
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

No mowing
</td>

<td style="text-align:right;">

2476.8
</td>

<td style="text-align:left;">

a
</td>

<td style="text-align:right;">

3497.2
</td>

<td style="text-align:left;">

a
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Early mowing
</td>

<td style="text-align:right;">

2112.5
</td>

<td style="text-align:left;">

ab
</td>

<td style="text-align:right;">

2724.7
</td>

<td style="text-align:left;">

ab
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

Late mowing
</td>

<td style="text-align:right;">

1206.0
</td>

<td style="text-align:left;">

c
</td>

<td style="text-align:right;">

1808.0
</td>

<td style="text-align:left;">

c
</td>

</tr>

<tr>

<td style="text-align:left;">

FH_2024
</td>

<td style="text-align:left;">

As-needed mowing
</td>

<td style="text-align:right;">

1515.5
</td>

<td style="text-align:left;">

bc
</td>

<td style="text-align:right;">

1525.8
</td>

<td style="text-align:left;">

bc
</td>

</tr>

</tbody>

</table>

``` r
## 5) Model-info row for global seed summary ----------------------------

family_structure_seed_ny <- "Tweedie GLMM (log link)"

model_info_seed_ny <- tibble::tibble(
  response_label    = "Weed seed rain (seeds m^-2 season^-1), NY-only model-predicted means*",
  family_structure  = family_structure_seed_ny,
  fixed_effects     = primary_model_name_seed_ny,
  random_effects    = "(1 | site_year:block)",
  AIC_additive      = round(AIC_add_seed_ny, 2),
  AIC_interaction   = round(AIC_int_seed_ny, 2),
  deltaAIC_add_int  = round(deltaAIC_seed_ny, 2),
  LRT_p_int_raw     = p_int_seed_ny,
  LRT_p_int_label   = dplyr::case_when(
    p_int_seed_ny < 0.001 ~ "<0.001",
    p_int_seed_ny < 0.01  ~ "<0.01",
    TRUE                  ~ sprintf("%.3f", p_int_seed_ny)
  ),
  interaction_class = interaction_class_seed_ny
)

readr::write_csv(
  model_info_seed_ny,
  file.path(tab_dir_seed_ny, "tab_ny-weed-seeds_model-info.csv")
)


## 6) OPTIONAL: combine into one NY-only weed-seed workbook -------------

if (requireNamespace("writexl", quietly = TRUE)) {
  seed_tables_ny <- list(
    Anova_pvals         = pvals_seed_ny,
    SiteYear_means_CLD  = sy_summary_seed_ny,
    Mowing_means_CLD    = trt_summary_seed_ny,
    SiteYear_trt_means  = int_summary_seed_ny,
    Model_info          = model_info_seed_ny
  )
  
  writexl::write_xlsx(
    seed_tables_ny,
    path = file.path(tab_dir_seed_ny, "NY-only_weed-seeds_all-tables.xlsx")
  )
} else {
  message("Package 'writexl' not installed; skipping NY-only weed-seeds Excel export.")
}
```

# Figures

## Ecobean

``` r
## Ecobean weed seeds: figures (m^-2 and ft^-2; model + raw; with CLDs) --

# Unit conversion: seeds per m^2 -> seeds per ft^2
m2_to_ft2 <- 10.76391041671
seeds_m2_to_ft2 <- 1 / m2_to_ft2  # multiply seeds/m^2 by this to get seeds/ft^2

fig_dir_seed_eco <- here("analysis", "figs", "weed-seeds", "Ecobean")
dir.create(fig_dir_seed_eco, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1) MODEL-PREDICTED (seeds m^-2)
# -----------------------------

plot_df_seed_eco_model_m2 <- trt_summary_seed_eco |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    mean     = model_mean,
    ymin     = pmax(mean - SE, 0),
    ymax     = mean + SE,
    cld      = trt_CLD,
    cld_y    = ymax * 1.05
  )

fig_eco_seed_model_m2 <- ggplot(
  plot_df_seed_eco_model_m2,
  aes(x = weed_trt, y = mean, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.14) +
  geom_text(aes(y = cld_y, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~m^{-2}*")"),
    title   = "Ecobean: weed seed rain by mowing treatment",
    caption = "Bars show Tweedie GLMM-predicted marginal means (seeds m\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_eco_seed_model_m2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_eco, "fig_eco_seed_total_model_seeds_m2.png"),
  plot     = fig_eco_seed_model_m2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)

# -----------------------------
# 1a) MODEL-PREDICTED (seeds ft^-2)
# -----------------------------

plot_df_seed_eco_model_ft2 <- plot_df_seed_eco_model_m2 |>
  dplyr::mutate(
    mean_ft2  = mean * seeds_m2_to_ft2,
    ymin_ft2  = ymin * seeds_m2_to_ft2,
    ymax_ft2  = ymax * seeds_m2_to_ft2,
    cld_y_ft2 = ymax_ft2 * 1.05
  )

fig_eco_seed_model_ft2 <- ggplot(
  plot_df_seed_eco_model_ft2,
  aes(x = weed_trt, y = mean_ft2, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin_ft2, ymax = ymax_ft2), width = 0.14) +
  geom_text(aes(y = cld_y_ft2, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~ft^{-2}*")"),
    title   = "Ecobean: weed seed rain by mowing treatment",
    caption = "Bars show Tweedie GLMM-predicted marginal means (seeds ft\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_eco_seed_model_ft2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_eco, "fig_eco_seed_total_model_seeds_ft2.png"),
  plot     = fig_eco_seed_model_ft2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)

# -----------------------------
# 2) RAW MEANS (seeds m^-2)
# -----------------------------

plot_df_seed_eco_raw_m2 <- trt_summary_seed_eco |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    mean     = raw_mean,
    se       = SE,  # note: raw_mean SE not stored in table; see note below
    ymin     = NA_real_,
    ymax     = NA_real_,
    cld      = raw_CLD
  )

# If your trt_summary_seed_eco does NOT include raw SE,
# compute raw means + SE directly from seed_plot_totals_eco:
raw_eco_seed_fig_m2 <- seed_plot_totals_eco |>
  dplyr::group_by(weed_trt) |>
  dplyr::summarise(
    n    = dplyr::n(),
    mean = mean(seeds_per_m2_season, na.rm = TRUE),
    sd   = stats::sd(seeds_per_m2_season, na.rm = TRUE),
    se   = sd / sqrt(n),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    ymin     = pmax(mean - se, 0),
    ymax     = mean + se
  ) |>
  dplyr::left_join(
    trt_summary_seed_eco |>
      dplyr::select(weed_trt, raw_CLD) |>
      dplyr::mutate(weed_trt = factor(weed_trt, levels = mow_levels)),
    by = "weed_trt"
  ) |>
  dplyr::mutate(
    cld   = raw_CLD,
    cld_y = ymax * 1.05
  )

fig_eco_seed_raw_m2 <- ggplot(
  raw_eco_seed_fig_m2,
  aes(x = weed_trt, y = mean, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.14) +
  geom_text(aes(y = cld_y, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~m^{-2}*")"),
    title   = "Ecobean: weed seed rain by mowing treatment",
    caption = "Bars show raw treatment means (seeds m\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_eco_seed_raw_m2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_eco, "fig_eco_seed_total_raw_seeds_m2.png"),
  plot     = fig_eco_seed_raw_m2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)

# -----------------------------
# 2a) RAW MEANS (seeds ft^-2)
# -----------------------------

raw_eco_seed_fig_ft2 <- raw_eco_seed_fig_m2 |>
  dplyr::mutate(
    mean_ft2  = mean * seeds_m2_to_ft2,
    ymin_ft2  = ymin * seeds_m2_to_ft2,
    ymax_ft2  = ymax * seeds_m2_to_ft2,
    cld_y_ft2 = ymax_ft2 * 1.05
  )

fig_eco_seed_raw_ft2 <- ggplot(
  raw_eco_seed_fig_ft2,
  aes(x = weed_trt, y = mean_ft2, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin_ft2, ymax = ymax_ft2), width = 0.14) +
  geom_text(aes(y = cld_y_ft2, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~ft^{-2}*")"),
    title   = "Ecobean: weed seed rain by mowing treatment",
    caption = "Bars show raw treatment means (seeds ft\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_eco_seed_raw_ft2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_eco, "fig_eco_seed_total_raw_seeds_ft2.png"),
  plot     = fig_eco_seed_raw_ft2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)
```

## Ny-only

``` r
## NY-only weed seeds: figures (m^-2 and ft^-2; model + raw; with CLDs) --

# Unit conversion: seeds per m^2 -> seeds per ft^2
m2_to_ft2 <- 10.76391041671
seeds_m2_to_ft2 <- 1 / m2_to_ft2  # multiply seeds/m^2 by this to get seeds/ft^2

fig_dir_seed_ny <- here("analysis", "figs", "weed-seeds", "NY-only")
dir.create(fig_dir_seed_ny, showWarnings = FALSE, recursive = TRUE)

# 1) MODEL-PREDICTED MEANS (seeds m^-2 season^-1) -----------------------
# uses trt_summary_seed_ny from the NY-only summary-tables chunk

plot_df_seed_ny_model_m2 <- trt_summary_seed_ny |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    mean     = model_mean,
    ymin     = pmax(mean - SE, 0),
    ymax     = mean + SE,
    cld      = trt_CLD,
    cld_y    = ymax * 1.05
  )

fig_ny_seed_total_model_m2 <- ggplot(
  plot_df_seed_ny_model_m2,
  aes(x = weed_trt, y = mean, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.14) +
  geom_text(aes(y = cld_y, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~m^{-2}*")"),
    title   = "New York (Cornell + Farm Hub): weed seed rain by mowing treatment",
    caption = "Bars show Tweedie GLMM-predicted marginal means (seeds m\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_ny_seed_total_model_m2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_ny, "fig_ny_seed_total_model_seeds_m2.png"),
  plot     = fig_ny_seed_total_model_m2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)


# 1a) MODEL-PREDICTED MEANS (seeds ft^-2 season^-1) ---------------------

plot_df_seed_ny_model_ft2 <- plot_df_seed_ny_model_m2 |>
  dplyr::mutate(
    mean_ft2  = mean * seeds_m2_to_ft2,
    ymin_ft2  = ymin * seeds_m2_to_ft2,
    ymax_ft2  = ymax * seeds_m2_to_ft2,
    cld_y_ft2 = ymax_ft2 * 1.05
  )

fig_ny_seed_total_model_ft2 <- ggplot(
  plot_df_seed_ny_model_ft2,
  aes(x = weed_trt, y = mean_ft2, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin_ft2, ymax = ymax_ft2), width = 0.14) +
  geom_text(aes(y = cld_y_ft2, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~ft^{-2}*")"),
    title   = "New York (Cornell + Farm Hub): weed seed rain by mowing treatment",
    caption = "Bars show Tweedie GLMM-predicted marginal means (seeds ft\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_ny_seed_total_model_ft2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_ny, "fig_ny_seed_total_model_seeds_ft2.png"),
  plot     = fig_ny_seed_total_model_ft2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)


# 2) RAW MEANS (seeds m^-2 season^-1) -----------------------------------

raw_ny_seed_fig_m2 <- seed_plot_totals_ny |>
  dplyr::group_by(weed_trt) |>
  dplyr::summarise(
    n    = dplyr::n(),
    mean = mean(seeds_per_m2_season, na.rm = TRUE),
    sd   = stats::sd(seeds_per_m2_season, na.rm = TRUE),
    se   = sd / sqrt(n),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    weed_trt = factor(weed_trt, levels = mow_levels),
    ymin     = pmax(mean - se, 0),
    ymax     = mean + se
  ) |>
  dplyr::left_join(
    trt_summary_seed_ny |>
      dplyr::select(weed_trt, raw_CLD) |>
      dplyr::mutate(weed_trt = factor(weed_trt, levels = mow_levels)),
    by = "weed_trt"
  ) |>
  dplyr::mutate(
    cld   = raw_CLD,
    cld_y = ymax * 1.05
  )

fig_ny_seed_total_raw_m2 <- ggplot(
  raw_ny_seed_fig_m2,
  aes(x = weed_trt, y = mean, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.14) +
  geom_text(aes(y = cld_y, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~m^{-2}*")"),
    title   = "New York (Cornell + Farm Hub): weed seed rain by mowing treatment",
    caption = "Bars show raw treatment means (seeds m\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings from the Tweedie GLMM (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_ny_seed_total_raw_m2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_ny, "fig_ny_seed_total_raw_seeds_m2.png"),
  plot     = fig_ny_seed_total_raw_m2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)


# 3) RAW MEANS (seeds ft^-2 season^-1) ----------------------------------

raw_ny_seed_fig_ft2 <- raw_ny_seed_fig_m2 |>
  dplyr::mutate(
    mean_ft2  = mean * seeds_m2_to_ft2,
    se_ft2    = se   * seeds_m2_to_ft2,
    ymin_ft2  = ymin * seeds_m2_to_ft2,
    ymax_ft2  = ymax * seeds_m2_to_ft2,
    cld_y_ft2 = ymax_ft2 * 1.05
  )

fig_ny_seed_total_raw_ft2 <- ggplot(
  raw_ny_seed_fig_ft2,
  aes(x = weed_trt, y = mean_ft2, fill = weed_trt)
) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ymin_ft2, ymax = ymax_ft2), width = 0.14) +
  geom_text(aes(y = cld_y_ft2, label = cld), size = 6) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_x_discrete(labels = label_break_spaces) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x       = NULL,
    y       = expression(Weed~seed~rain~"(seeds"~ft^{-2}*")"),
    title   = "New York (Cornell + Farm Hub): weed seed rain by mowing treatment",
    caption = "Bars show raw treatment means (seeds ft\u207b\u00b2 season\u207b\u00b9) \u00b1 SE; letters indicate Fisher\u2019s LSD groupings from the Tweedie GLMM (P > 0.05)."
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x  = element_text(lineheight = 0.95, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )

fig_ny_seed_total_raw_ft2
```

![](weed_seed_imt_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
ggsave(
  filename = file.path(fig_dir_seed_ny, "fig_ny_seed_total_raw_seeds_ft2.png"),
  plot     = fig_ny_seed_total_raw_ft2,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)
```

``` r
# NOTE (future pub): consider modeling seeds_total with offset(log(area_sampled_m2))
# and NB2 as a sensitivity analysis; current approach uses Tweedie on density.
```
