bean_yield_2023
================

# **Library**

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

#library(kableExtra) ##install.packages("kableExtra")
#library(viridis) ##install.packages("viridis")
#library(lme4) ##install.packages("lme4")
#library(lmerTest) ##install.packages("lmerTest")
#library(emmeans) ##install.packages("emmeans")
#library(rstatix) ##install.packages("rstatix")
#library(Matrix) ##install.packages("Matrix")
#library(multcomp) ##install.packages("multcomp")
#library(multcompView) ##install.packages("multcompView")
#library(ggResidpanel) ##install.packages("ggResidpanel")
#library(car)
#library(TMB)  ##install.packages("TMB")
#library(glmmTMB)  ##install.packages("glmmTMB")
#library(DHARMa)  ##install.packages("DHARMa")

#Load Functions
MeanPlusSe<-function(x) mean(x)+plotrix::std.error(x)

find_logw0=function(x){c=trunc(log(min(x[x>0],na.rm=T)))
d=exp(c)
return(d)}
```

<br>

# **Load and Clean Data**

## **Farm hub**

``` r
fh_raw_2023 <- read_excel("~/Github/IMT/raw-data/farmhub_raw_2023.xlsx") #load file

fh_clean_2023 <- clean_names(fh_raw_2023) |>  #change column names and convert to factors
  rename ('mowing'= trt,'weeds'= microplot) |> 
  mutate(across(c(mowing, block, plot, weeds), as.factor))

fh_yield_2023_grams_meter <- fh_clean_2023 |>
  filter(weeds %in% c("SW", "M")) |>
  select(c(1:6, beanyd))
kable(head(fh_yield_2023_grams_meter))
```

| id            | loc | mowing | block | plot | weeds | beanyd |
|:--------------|:----|:-------|:------|:-----|:------|-------:|
| FH_B1_P101    | FH  | EWC    | 1     | 101  | M     | 367.79 |
| FH_B1_P101_SW | FH  | EWC    | 1     | 101  | SW    | 218.00 |
| FH_B1_P102    | FH  | LWC    | 1     | 102  | M     | 268.00 |
| FH_B1_P102_SW | FH  | LWC    | 1     | 102  | SW    | 177.30 |
| FH_B1_P103    | FH  | AWC    | 1     | 103  | M     | 290.00 |
| FH_B1_P103_SW | FH  | AWC    | 1     | 103  | SW    | 236.00 |

``` r
fh_yield_2023_adj_bush_acre <- fh_clean_2023 |>
  filter(weeds %in% c("SW", "M")) |>
  select(c(1:6, beanyd)) |>
  mutate(beanyd = (((beanyd/454)/(16.4/43560))/60)) |> 
  mutate(beanyd = (beanyd * ((100-0.00001)/(100-14))))
kable(head(fh_yield_2023_adj_bush_acre)) 
```

| id            | loc | mowing | block | plot | weeds |   beanyd |
|:--------------|:----|:-------|:------|:-----|:------|---------:|
| FH_B1_P101    | FH  | EWC    | 1     | 101  | M     | 41.70022 |
| FH_B1_P101_SW | FH  | EWC    | 1     | 101  | SW    | 24.71695 |
| FH_B1_P102    | FH  | LWC    | 1     | 102  | M     | 30.38598 |
| FH_B1_P102_SW | FH  | LWC    | 1     | 102  | SW    | 20.10237 |
| FH_B1_P103    | FH  | AWC    | 1     | 103  | M     | 32.88035 |
| FH_B1_P103_SW | FH  | AWC    | 1     | 103  | SW    | 26.75780 |

``` r
fh_yield_2023_adj_lbs_acre <- fh_clean_2023 |>
  filter(weeds %in% c("SW", "M")) |>
  select(c(1:6, beanyd)) |>
  mutate(beanyd = ((beanyd/454)/(16.4/43560))) |> 
  mutate(beanyd = (beanyd * ((100-0.00001)/(100-14))))
kable(head(fh_yield_2023_adj_lbs_acre)) 
```

| id            | loc | mowing | block | plot | weeds |   beanyd |
|:--------------|:----|:-------|:------|:-----|:------|---------:|
| FH_B1_P101    | FH  | EWC    | 1     | 101  | M     | 2502.013 |
| FH_B1_P101_SW | FH  | EWC    | 1     | 101  | SW    | 1483.017 |
| FH_B1_P102    | FH  | LWC    | 1     | 102  | M     | 1823.159 |
| FH_B1_P102_SW | FH  | LWC    | 1     | 102  | SW    | 1206.142 |
| FH_B1_P103    | FH  | AWC    | 1     | 103  | M     | 1972.821 |
| FH_B1_P103_SW | FH  | AWC    | 1     | 103  | SW    | 1605.468 |

<br>

# **Load and Clean Data**

## **Musgrave**
