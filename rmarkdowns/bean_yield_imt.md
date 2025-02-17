IMT Bean yield
================

# Load libraries

``` r
#Set work directory
setwd("/Users/ey239/Github/IMT/rmarkdowns")

#Load packages 
library(tidyverse) ##install.packages("tidyverse")
library(knitr)
library(patchwork) ##install.packages("patchwork")
library(skimr)     ##install.packages("skimr")
library(readxl)
library(janitor) ##install.packages("janitor")

library(kableExtra) ##install.packages("kableExtra")
library(webshot) ##install.packages("webshot")
webshot::install_phantomjs()
library(viridis) ##install.packages("viridis")
library(lme4) ##install.packages("lme4")
library(lmerTest) ##install.packages("lmerTest")
library(emmeans) ##install.packages("emmeans")
library(rstatix) ##install.packages("rstatix")
#library(Matrix) ##install.packages("Matrix")
library(multcomp) ##install.packages("multcomp")
library(multcompView) ##install.packages("multcompView")
library(ggResidpanel) ##install.packages("ggResidpanel")
#library(car)
#library(TMB)  ##install.packages("TMB")
library(glmmTMB)  ##install.packages("glmmTMB")
library(DHARMa)  ##install.packages("DHARMa")
library(performance) ##install.packages("performance")
library(WrensBookshelf)##install.packages("WrensBookshelf")
#Load Functions
MeanPlusSe<-function(x) mean(x)+plotrix::std.error(x)

find_logw0=function(x){c=trunc(log(min(x[x>0],na.rm=T)))
d=exp(c)
return(d)}
```

<br>

# Load and clean data

## Load data

``` r
combined_raw <- read_excel("~/Github/IMT/raw-data/combined_raw.xlsx")
kable(head(combined_raw))
```

| id | year | location | site_year | treatment | block | plot | microplot | bean_emergence | bean_biomass | intrarow_weed_biomass | interrow_weed_biomass | weed_biomass | bean_population | bean_yield |
|:---|---:|:---|:---|:---|---:|---:|:---|:---|---:|:---|:---|---:|:---|:---|
| WI_B1_P101 | 2023 | WI | WI_2023 | NWC | 1 | 101 | M | 23 | 233.3 | 0 | 2 | 1.84 | 38 | 295.66000000000003 |
| WI_B1_P101_SW | 2023 | WI | WI_2023 | NWC | 1 | 101 | SW | 24 | 61.7 | 31.66 | 155.9 | 187.56 | 28 | 88.81 |
| WI_B1_P101_WF | 2023 | WI | WI_2023 | NWC | 1 | 101 | WF | 14 | 210.3 | 0 | 0 | 0.00 | 38 | 273.51 |
| WI_B1_P102 | 2023 | WI | WI_2023 | LWC | 1 | 102 | M | 14 | 248.4 | 0 | 0 | 0.00 | 27 | 321.16000000000003 |
| WI_B1_P102_SW | 2023 | WI | WI_2023 | LWC | 1 | 102 | SW | 16 | 65.6 | 28.76 | 50 | 78.76 | 32 | 97.45 |
| WI_B1_P103 | 2023 | WI | WI_2023 | AWC | 1 | 103 | M | 17 | 279.3 | 0 | 0.21 | 0.21 | 34 | 302.72000000000003 |

\##Clean data

``` r
#Standardaze column names, convert to factors, check for outliers of variable**
clean_combined <- clean_names(combined_raw) |>  
  rename(mowing = treatment, weeds = microplot) |> 
  mutate(across(c(year, location, site_year, mowing, block, plot, weeds), as.factor),
         bean_yield = as.numeric(as.character(bean_yield))) |>  # Convert to numeric
  mutate(is_outlier = bean_yield < (quantile(bean_yield, 0.25, na.rm = TRUE) - 1.5 * IQR(bean_yield, na.rm = TRUE)) |
                     bean_yield > (quantile(bean_yield, 0.75, na.rm = TRUE) + 1.5 * IQR(bean_yield, na.rm = TRUE))) |> 
  mutate(is_outlier = replace_na(is_outlier, FALSE))  # Ensure logical values
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `bean_yield = as.numeric(as.character(bean_yield))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
bean_yield_clean <- clean_combined |>  
  filter(weeds %in% c("SW", "M")) |> # Keep only SW and M microplots
  filter(location %in% c("FH")) |>
  filter(!is.na(bean_yield)) |>  # Remove NA values
  filter(is_outlier == FALSE) |>  # Remove outliers
  mutate(
    bean_yield_adj_bu_acre = (((bean_yield / 454) / (16.4 / 43560)) / 60) * ((100 - 0.00001) / (100 - 14)),
    bean_yield_adj_lbs_acre = ((bean_yield / 454) / (16.4 / 43560)) * ((100 - 0.00001) / (100 - 14)),
    bean_yield_adj_kg_ha = ((bean_yield / 454) / (16.4 / 43560)) * 1.12085 * ((100 - 0.00001) / (100 - 14))
  )
kable(bean_yield_clean)
```

| id | year | location | site_year | mowing | block | plot | weeds | bean_emergence | bean_biomass | intrarow_weed_biomass | interrow_weed_biomass | weed_biomass | bean_population | bean_yield | is_outlier | bean_yield_adj_bu_acre | bean_yield_adj_lbs_acre | bean_yield_adj_kg_ha |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|:---|---:|:---|---:|:---|---:|---:|---:|
| FH_B1_P101 | 2023 | FH | FH_2023 | EWC | 1 | 101 | M | 21 | 87.18 | 0.75 | 16.5 | 17.25 | 22 | 367.79 | FALSE | 41.700220 | 2502.0132 | 2804.3815 |
| FH_B1_P101_SW | 2023 | FH | FH_2023 | EWC | 1 | 101 | SW | 24 | 40.32 | 3.54 | 37.65 | 41.19 | 23 | 218.00 | FALSE | 24.716952 | 1483.0171 | 1662.2398 |
| FH_B1_P102 | 2023 | FH | FH_2023 | LWC | 1 | 102 | M | 24 | 72.37 | 6.47 | 12.71 | 19.18 | 26 | 268.00 | FALSE | 30.385978 | 1823.1587 | 2043.4874 |
| FH_B1_P102_SW | 2023 | FH | FH_2023 | LWC | 1 | 102 | SW | 24 | 63.33 | 7.54 | 11.33 | 18.87 | 25 | 177.30 | FALSE | 20.102365 | 1206.1419 | 1351.9042 |
| FH_B1_P103 | 2023 | FH | FH_2023 | AWC | 1 | 103 | M | 23 | 148.62 | 4.22 | 0 | 4.22 | 22 | 290.00 | FALSE | 32.880349 | 1972.8210 | 2211.2364 |
| FH_B1_P103_SW | 2023 | FH | FH_2023 | AWC | 1 | 103 | SW | 26 | 84.27 | 2.9 | 8.1300000000000008 | 11.03 | 20 | 236.00 | FALSE | 26.757801 | 1605.4681 | 1799.4889 |
| FH_B1_P104 | 2023 | FH | FH_2023 | NWC | 1 | 104 | M | 25 | 189.99 | 0 | 5.45 | 5.45 | 24 | 321.38 | FALSE | 36.438230 | 2186.2938 | 2450.5074 |
| FH_B1_P104_SW | 2023 | FH | FH_2023 | NWC | 1 | 104 | SW | 24 | 67.15 | 2.04 | 8.4499999999999993 | 10.49 | 25 | 274.25 | FALSE | 31.094606 | 1865.6764 | 2091.1434 |
| FH_B1_P201 | 2023 | FH | FH_2023 | AWC | 2 | 201 | M | 25 | 112.48 | 0.9 | 1.33 | 2.23 | 27 | 308.62 | FALSE | 34.991495 | 2099.4897 | 2353.2130 |
| FH_B1_P201_SW | 2023 | FH | FH_2023 | AWC | 2 | 201 | SW | 29 | 108.85 | 2.85 | 3.46 | 6.31 | 28 | 382.00 | FALSE | 43.311357 | 2598.6814 | 2912.7321 |
| FH_B1_P202 | 2023 | FH | FH_2023 | NWC | 2 | 202 | M | 25 | 63.74 | 0 | 0 | 0.00 | 23 | 265.00 | FALSE | 30.045837 | 1802.7502 | 2020.6125 |
| FH_B1_P202_SW | 2023 | FH | FH_2023 | NWC | 2 | 202 | SW | 26 | 109.70 | 5.56 | 37.28 | 42.84 | 25 | 291.00 | FALSE | 32.993730 | 1979.6238 | 2218.8613 |
| FH_B1_P203 | 2023 | FH | FH_2023 | EWC | 2 | 203 | M | 30 | 170.66 | 0 | 0 | 0.00 | 27 | 436.00 | FALSE | 49.433904 | 2966.0343 | 3324.4795 |
| FH_B1_P203_SW | 2023 | FH | FH_2023 | EWC | 2 | 203 | SW | 26 | 100.61 | 0.42 | 50.46 | 50.88 | 22 | 342.20 | FALSE | 38.798812 | 2327.9287 | 2609.2589 |
| FH_B1_P204 | 2023 | FH | FH_2023 | LWC | 2 | 204 | M | 32 | 109.51 | 0 | 2.34 | 2.34 | 22 | 368.00 | FALSE | 41.724030 | 2503.4418 | 2805.9827 |
| FH_B1_P204_SW | 2023 | FH | FH_2023 | LWC | 2 | 204 | SW | 28 | 88.85 | 2.42 | 8.4600000000000009 | 10.88 | 11 | 316.00 | FALSE | 35.828243 | 2149.6946 | 2409.4852 |
| FH_B1_P301 | 2023 | FH | FH_2023 | NWC | 3 | 301 | M | 23 | 140.47 | 0 | 8.35 | 8.35 | 24 | 425.00 | FALSE | 48.186719 | 2891.2031 | 3240.6050 |
| FH_B1_P301_SW | 2023 | FH | FH_2023 | NWC | 3 | 301 | SW | 33 | 60.15 | 14.99 | 40.22 | 55.21 | 22 | 311.00 | FALSE | 35.261340 | 2115.6804 | 2371.3604 |
| FH_B1_P302 | 2023 | FH | FH_2023 | EWC | 3 | 302 | M | 23 | 121.94 | 2.44 | 29.69 | 32.13 | 23 | 343.28 | FALSE | 38.921263 | 2335.2758 | 2617.4939 |
| FH_B1_P302_SW | 2023 | FH | FH_2023 | EWC | 3 | 302 | SW | 28 | 123.37 | NA | 16.89 | 16.89 | 25 | 372.91 | FALSE | 42.280728 | 2536.8437 | 2843.4212 |
| FH_B1_P303 | 2023 | FH | FH_2023 | LWC | 3 | 303 | M | 28 | 121.44 | 0.31 | 3.92 | 4.23 | 28 | 437.00 | FALSE | 49.547285 | 2972.8371 | 3332.1045 |
| FH_B1_P303_SW | 2023 | FH | FH_2023 | LWC | 3 | 303 | SW | 31 | 131.51 | 0 | 3.94 | 3.94 | 24 | 419.55 | FALSE | 47.568795 | 2854.1277 | 3199.0490 |
| FH_B1_P304 | 2023 | FH | FH_2023 | AWC | 3 | 304 | M | 27 | 148.91 | 0 | 0 | 0.00 | 25 | 443.74 | FALSE | 50.311470 | 3018.6882 | 3383.4966 |
| FH_B1_P304_SW | 2023 | FH | FH_2023 | AWC | 3 | 304 | SW | 29 | 60.13 | 9.8800000000000008 | 0 | 9.88 | 21 | 262.99 | FALSE | 29.817942 | 1789.0765 | 2005.2864 |
| FH_B1_P401 | 2023 | FH | FH_2023 | LWC | 4 | 401 | M | 28 | 141.74 | 0 | 0 | 0.00 | 21 | 387.33 | FALSE | 43.915675 | 2634.9405 | 2953.3730 |
| FH_B1_P401_SW | 2023 | FH | FH_2023 | LWC | 4 | 401 | SW | 28 | 138.35 | 8.1 | 0 | 8.10 | 24 | 367.62 | FALSE | 41.680945 | 2500.8567 | 2803.0852 |
| FH_B1_P402 | 2023 | FH | FH_2023 | AWC | 4 | 402 | M | 30 | 102.87 | 0 | 0 | 0.00 | 25 | 326.00 | FALSE | 36.962048 | 2217.7229 | 2485.7347 |
| FH_B1_P402_SW | 2023 | FH | FH_2023 | AWC | 4 | 402 | SW | 29 | 69.97 | 1.31 | 6.29 | 7.60 | 28 | 393.92 | FALSE | 44.662852 | 2679.7711 | 3003.6215 |
| FH_B1_P403 | 2023 | FH | FH_2023 | NWC | 4 | 403 | M | 23 | 78.64 | 0 | 2.79 | 2.79 | 23 | 285.00 | FALSE | 32.313447 | 1938.8068 | 2173.1116 |
| FH_B1_P403_SW | 2023 | FH | FH_2023 | NWC | 4 | 403 | SW | 23 | 97.12 | 1.38 | 11.09 | 12.47 | 18 | 413.90 | FALSE | 46.928195 | 2815.6917 | 3155.9681 |
| FH_B1_P404 | 2023 | FH | FH_2023 | EWC | 4 | 404 | M | 25 | 97.21 | 0.49 | 0.56999999999999995 | 1.06 | 31 | 337.63 | FALSE | 38.280663 | 2296.8398 | 2574.4129 |
| FH_B1_P404_SW | 2023 | FH | FH_2023 | EWC | 4 | 404 | SW | 28 | 82.50 | 3.21 | 22.11 | 25.32 | 22 | 284.30 | FALSE | 32.234080 | 1934.0448 | 2167.7741 |
| FH_B1_P101 | 2024 | FH | FH_2024 | EWC | 1 | 101 | M | 32 | 120.37 | 10.08 | 68.430000000000007 | 78.51 | 18 | 139.85 | FALSE | 15.856265 | 951.3759 | 1066.3497 |
| FH_B1_P102 | 2024 | FH | FH_2024 | LWC | 1 | 102 | M | 31 | 193.20 | 0 | 10.130000000000001 | 10.13 | 23 | 192.95 | FALSE | 21.876770 | 1312.6062 | 1471.2347 |
| FH_B1_P102_SW | 2024 | FH | FH_2024 | LWC | 1 | 102 | SW | 21 | 75.42 | 5.49 | 58.71 | 64.20 | 24 | 217.43 | FALSE | 24.652325 | 1479.1395 | 1657.8935 |
| FH_B1_P103 | 2024 | FH | FH_2024 | AWC | 1 | 103 | M | 20 | 185.88 | 0.05 | 14.08 | 14.13 | 21 | 276.42 | FALSE | 31.340642 | 1880.4385 | 2107.6895 |
| FH_B1_P103_SW | 2024 | FH | FH_2024 | AWC | 1 | 103 | SW | 24 | 85.61 | 41.91 | 38.229999999999997 | 80.14 | 17 | 73.44 | FALSE | 8.326665 | 499.5999 | 559.9765 |
| FH_B1_P104_SW | 2024 | FH | FH_2024 | NWC | 1 | 104 | SW | 27 | 80.31 | 9.09 | 130.19999999999999 | 139.29 | 20.5 | 157.39 | FALSE | 17.844959 | 1070.6976 | 1200.0914 |
| FH_B1_P201 | 2024 | FH | FH_2024 | AWC | 2 | 201 | M | 30 | 179.00 | 0 | 1.8 | 1.80 | 25 | 216.82 | FALSE | 24.583163 | 1474.9898 | 1653.2423 |
| FH_B1_P201_SW | 2024 | FH | FH_2024 | AWC | 2 | 201 | SW | 29 | 108.09 | 42.64 | 12.16 | 54.80 | 23 | 194.05 | FALSE | 22.001489 | 1320.0893 | 1479.6221 |
| FH_B1_P202 | 2024 | FH | FH_2024 | NWC | 2 | 202 | M | 27 | 105.23 | 11.11 | 60 | 71.11 | 16 | 215.58 | FALSE | 24.442571 | 1466.5543 | 1643.7874 |
| FH_B1_P202_SW | 2024 | FH | FH_2024 | NWC | 2 | 202 | SW | 28 | 96.10 | 106.53 | 106.53 | 213.06 | 21 | 137.70 | FALSE | 15.612497 | 936.7498 | 1049.9560 |
| FH_B1_P203_SW | 2024 | FH | FH_2024 | EWC | 2 | 203 | SW | 24 | 103.03 | 31.09 | 88.9 | 119.99 | 28 | 210.98 | FALSE | 23.921021 | 1435.2613 | 1608.7126 |
| FH_B1_P204 | 2024 | FH | FH_2024 | LWC | 2 | 204 | M | 33 | 130.06 | 0 | 14.31 | 14.31 | 24 | 249.07 | FALSE | 28.239685 | 1694.3811 | 1899.1470 |
| FH_B1_P204_SW | 2024 | FH | FH_2024 | LWC | 2 | 204 | SW | 26 | 103.73 | 56.24 | 2.2200000000000002 | 58.46 | 20 | 155.42 | FALSE | 17.621600 | 1057.2960 | 1185.0702 |
| FH_B1_P301 | 2024 | FH | FH_2024 | NWC | 3 | 301 | M | 23 | 186.51 | 0 | 180.1 | 180.10 | 17 | 292.85 | FALSE | 33.203484 | 1992.2090 | 2232.9675 |
| FH_B1_P301_SW | 2024 | FH | FH_2024 | NWC | 3 | 301 | SW | 21 | 122.54 | 18.670000000000002 | 99.54 | 118.21 | 20 | 156.55 | FALSE | 17.749720 | 1064.9832 | 1193.6864 |
| FH_B1_P302 | 2024 | FH | FH_2024 | EWC | 3 | 302 | M | 29 | 130.39 | 0 | 52.65 | 52.65 | 27 | 363.11 | FALSE | 41.169599 | 2470.1759 | 2768.6967 |
| FH_B1_P302_SW | 2024 | FH | FH_2024 | EWC | 3 | 302 | SW | 20 | 134.20 | 3.76 | 94.85 | 98.61 | 26 | 223.24 | FALSE | 25.311066 | 1518.6640 | 1702.1945 |
| FH_B1_P304 | 2024 | FH | FH_2024 | AWC | 3 | 304 | M | 29 | 219.83 | 0 | 0.05 | 0.05 | 24 | 260.26 | FALSE | 29.508413 | 1770.5048 | 1984.4703 |
| FH_B1_P304_SW | 2024 | FH | FH_2024 | AWC | 3 | 304 | SW | 26 | 71.56 | 36.39 | 33.03 | 69.42 | 21 | 148.23 | FALSE | 16.806394 | 1008.3836 | 1130.2468 |
| FH_B1_P401 | 2024 | FH | FH_2024 | LWC | 4 | 401 | M | 29 | 154.23 | 16.09 | 34.47 | 50.56 | 24 | 328.71 | FALSE | 37.269309 | 2236.1585 | 2506.3983 |
| FH_B1_P401_SW | 2024 | FH | FH_2024 | LWC | 4 | 401 | SW | 21 | 197.69 | 26.45 | 0 | 26.45 | 28 | 269.58 | FALSE | 30.565119 | 1833.9072 | 2055.5348 |
| FH_B1_P402 | 2024 | FH | FH_2024 | AWC | 4 | 402 | M | 27 | 152.50 | 0 | 5.46 | 5.46 | 20 | 259.56 | FALSE | 29.429046 | 1765.7428 | 1979.1328 |
| FH_B1_P402_SW | 2024 | FH | FH_2024 | AWC | 4 | 402 | SW | 24 | 160.32 | 4.8899999999999997 | 18.75 | 23.64 | 17 | 171.35 | FALSE | 19.427751 | 1165.6651 | 1306.5357 |
| FH_B1_P403 | 2024 | FH | FH_2024 | NWC | 4 | 403 | M | 23 | 185.37 | 0 | 95.34 | 95.34 | 24 | 242.45 | FALSE | 27.489106 | 1649.3464 | 1848.6699 |
| FH_B1_P403_SW | 2024 | FH | FH_2024 | NWC | 4 | 403 | SW | 19 | 119.05 | 11.54 | 121.29 | 132.83 | 20 | 183.22 | FALSE | 20.773578 | 1246.4147 | 1397.0439 |
| FH_B1_P404 | 2024 | FH | FH_2024 | EWC | 4 | 404 | M | 30 | 194.11 | 0 | 45.72 | 45.72 | 24 | 277.00 | FALSE | 31.406403 | 1884.3842 | 2112.1120 |

# Model testing

## Lmer

Block is random Tyler is under the impression that block should always
be random and that post-hoc comparisons should use TUKEY rather the
Fischer. Fisher is bogus apparently.

``` r
# This is better for providing generalizatins and reccomendations. 

random <- lmer(bean_yield_adj_kg_ha ~ mowing*weeds + (1|year) + (1|block) + (1|block:mowing), data = bean_yield_clean)


#tyler whats up with uri being random? location*year takes into accoutn if we are concerned about only these years and locations, everything is relative to the ten means from this experiment

#random_uri <- lmer(bean_yield_adj_kg_ha  ~ mowing*weeds + location*year + (1|site_year:block)+  (1|site_year:block:mowing)  , data =  bean_yield_clean)


resid_panel(random)
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
simulateResiduals(random,plot = TRUE) # Residuals and normality look good
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.672 0.384 0.288 0.176 0.436 0.452 0.624 0.608 0.516 0.9 0.448 0.668 0.88 0.828 0.696 0.668 0.928 0.736 0.588 0.892 ...

``` r
check_model(random)
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
#tyler whats up with uri being random?
#random_uri <- lmer(bean_yield_adj_kg_ha  ~ mowing*weeds + site*year + (1|site_year) + (1|site_year:block)+  (1|site_year:block:mowing)  , data =  bean_yield_clean)
```

## Joint test (anova)

``` r
 random |> 
  joint_tests() |> 
  kable()  
```

|     | model term   | df1 |   df2 | F.ratio |   p.value |
|:----|:-------------|----:|------:|--------:|----------:|
| 1   | mowing       |   3 |  7.72 |   1.166 | 0.3828906 |
| 3   | weeds        |   1 | 38.54 |  13.911 | 0.0006157 |
| 2   | mowing:weeds |   3 | 37.20 |   0.261 | 0.8531380 |

#### Anova table

``` r
options(contrasts = c("contr.sum", "contr.poly"))
Anova(random, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: bean_yield_adj_kg_ha
    ##                Chisq Df Pr(>Chisq)    
    ## (Intercept)  21.6267  1  3.312e-06 ***
    ## mowing        2.3060  3    0.51138    
    ## weeds         5.9713  1    0.01454 *  
    ## mowing:weeds  0.7887  3    0.85216    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Fisher compact letter display

### Weed-control (No significant)

``` r
cld_mowing_fisher <-cld(emmeans(random, ~  mowing , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_mowing_fisher
```

    ##  mowing emmean  SE   df lower.CL upper.CL .group
    ##  LWC      2237 480 1.33    -1232     5707  a    
    ##  EWC      2157 480 1.34    -1273     5587  a    
    ##  AWC      2022 477 1.31    -1520     5564  a    
    ##  NWC      1976 478 1.32    -1532     5483  a    
    ## 
    ## Results are averaged over the levels of: weeds 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

### Weed-control by site_year (No significant)

### weeds (significant)

``` r
cld_weeds_fisher <-cld(emmeans(random, ~  weeds , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_weeds_fisher
```

    ##  weeds emmean  SE   df lower.CL upper.CL .group
    ##  M       2299 472 1.25    -1489     6086  a    
    ##  SW      1897 472 1.25    -1886     5681   b   
    ## 
    ## Results are averaged over the levels of: mowing 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

\#Figures \## Weed-control (not sifnificant) \###All sites

``` r
bean_yield_clean |> 
  left_join(cld_mowing_fisher) |> 
  ggplot(aes(x = factor(mowing, levels = c("NWC", "EWC", "LWC", "AWC")), y = bean_yield_adj_kg_ha, fill = mowing)) +
  stat_summary(geom = "bar", fun = "mean", width = 0.7) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  #stat_summary(geom="text", fun = "MeanPlusSe", aes(label= trimws(.group)),size=6.5,vjust=-0.5) +
  #geom_bar(stat="identity", position=position_dodge()) + 
  #geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=.2,
                 #position=position_dodge(.9))+
#geom_text(aes(label = trimws(.group), y = response + (SE + 30)), size = 7) +
  labs(
    x = "",
       y = expression(paste("Dry bean yield (", kg~ha^{-1}, " at 13% moisture)")),
    #title = str_c("Influence of interrow weed control on weed biomass"),
    subtitle = expression(italic("Not signficant"))) +
  
  scale_x_discrete(labels = c("No\nmowing", "Early\nmowing", "Late\nmowing", "As-needed\nmowing")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, end = 0.9, begin = 0.1) +
   theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 24),  # Increase font size of axis titles
    axis.text = element_text(size = 24),   # Increase font size of axis labels
    plot.title = element_text(size = 24, face = "bold"),  # Increase font size of title
    plot.subtitle = element_text(size = 24, face = "italic")  # Increase font size of subtitle
  
  )
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("bean_yield_mowing_kgha.png", width = 10, height = 8, dpi = 300)
```

### Single site

``` r
bean_yield_clean |> 
  left_join(cld_mowing_fisher) |> 
  ggplot(aes(x = factor(mowing, levels = c("NWC", "EWC", "LWC", "AWC")), 
             y = bean_yield_adj_kg_ha, fill = factor(year))) +  # Fill by year
  stat_summary(geom = "bar", fun = "mean", width = 0.6, position = position_dodge(width = 0.7)) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2, position = position_dodge(width = 0.7)) +
  labs(
    x = "",
    y = expression(paste("Dry bean yield (", kg~ha^{-1}, " at 13% moisture)")),
    fill = "Year",  # Legend title
    #subtitle = expression(italic("Not significant"))
  ) +
  scale_x_discrete(labels = c("No\nmowing", "Early\nmowing", "Late\nmowing", "As-needed\nmowing")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_WB_d(name = "BlueberriesForSal", direction = 1) +  # Use wren's bookshelf colors
  theme_bw() +
  theme(
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic")
  )
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("bean_yield_mowing_kgha_FH.png", width = 10, height = 8, dpi = 300)
```

## Weeds (S)

``` r
bean_yield_clean |> 
  left_join(cld_weeds_fisher) |> 
  ggplot(aes(x = weeds, y = bean_yield_adj_kg_ha, fill = weeds)) +  # Fill added
  stat_summary(geom = "bar", fun = "mean", width = 0.6, position = position_dodge(width = 0.7)) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2, position = position_dodge(width = 0.7)) +
  stat_summary(geom = "text", fun = "MeanPlusSe", aes(label = trimws(.group)), 
               size = 6.5, vjust = -0.5, position = position_dodge(width = 0.7)) +
  labs(
    x = "",
    y = expression(paste("Dry bean yield (", kg~ha^{-1}, " at 13% moisture)")),
    fill = "Weed Treatment"
  ) +
  scale_x_discrete(labels = c("Ambient weeds", "Surrogate weeds")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_WB_d(name = "BlueberriesForSal", direction = 1) +  # Ensure correct function use
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic")
  )
```

![](bean_yield_imt_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave("bean_yield_weeds_kgha_FH.png", width = 25, height = 10, dpi = 300)
```
