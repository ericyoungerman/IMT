interrow_weed_biomass_2023
================

# **Load libraries**

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

### **Load individual datasets**

``` r
fh_raw_2023 <- read_excel("~/Github/IMT/raw-data/farmhub_raw_2023.xlsx")
kable(head(fh_raw_2023))
```

| ID | LOC | TRT | BLOCK | PLOT | MICROPLOT | EMERG | BBM | INTRAWBM | INTERWBM | WBM | DEN | BEANYD |
|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| FH_B1_P101 | FH | EWC | 1 | 101 | M | 21 | 87.18 | 0.75 | 16.50 | 17.25 | 44 | 367.79 |
| FH_B1_P101_SW | FH | EWC | 1 | 101 | SW | 24 | 40.32 | 3.54 | 37.65 | 41.19 | 45 | 218.00 |
| FH_B1_P102 | FH | LWC | 1 | 102 | M | 24 | 72.37 | 6.47 | 12.71 | 19.18 | 52 | 268.00 |
| FH_B1_P102_SW | FH | LWC | 1 | 102 | SW | 24 | 63.33 | 7.54 | 11.33 | 18.87 | 49 | 177.30 |
| FH_B1_P103 | FH | AWC | 1 | 103 | M | 23 | 148.62 | 4.22 | 0.00 | 4.22 | 43 | 290.00 |
| FH_B1_P103_SW | FH | AWC | 1 | 103 | SW | 26 | 84.27 | 2.90 | 8.13 | 11.03 | 39 | 236.00 |

``` r
cu_raw_2023 <- read_excel("~/Github/IMT/raw-data/cornell_raw_2023.xlsx") |> 
   rename(WBM = TOTWBM, INTRAWBM = INTRABM, INTERWBM = INTERBM)
kable(head(cu_raw_2023))
```

| ID | LOC | TRT | BLOCK | PLOT | MICROPLOT | EMERG | BBM | INTRAWBM | INTERWBM | WBM | BEANDEN | BEANYD |
|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| CU_B1_P101 | CU | EWC | 1 | 101 | M | 22 | 91.99 | 9.39 | 20.64 | 30.03 | 19.0 | 222 |
| CU_B1_P101_SW | CU | EWC | 1 | 101 | SW | 19 | 71.80 | 22.33 | 19.62 | 41.95 | 15.5 | 299 |
| CU_B1_P102 | CU | LWC | 1 | 102 | M | 22 | 83.84 | 7.23 | 14.75 | 21.98 | 22.0 | 472 |
| CU_B1_P102_SW | CU | LWC | 1 | 102 | SW | 26 | 81.23 | 13.07 | 9.65 | 22.72 | 16.5 | 307 |
| CU_B1_P103 | CU | AWC | 1 | 103 | M | 21 | 109.21 | 6.67 | 11.28 | 17.95 | 24.5 | 456 |
| CU_B1_P103_SW | CU | AWC | 1 | 103 | SW | 28 | 152.43 | 10.51 | 3.54 | 14.05 | 21.5 | 446 |

``` r
#me_raw_2023 <- read_excel("~/Github/IMT/raw-data/maine_raw_2023.xlsx", 
                          #na = c("na")) # convert na to NA
#kable(head(me_raw_2023))

vt_raw_2023 <- read_excel("~/Github/IMT/raw-data/vermont_raw_2023.xlsx")
kable(head(vt_raw_2023))
```

| ID | LOC | TRT | BLOCK | PLOT | MICROPLOT | EMERG | BBM | INTRAWBM | INTERWBM | WBM | DEN | BEANYD |
|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| VT_B1_P101 | VT | LWC | 1 | 101 | M | 31 | 192.1 | 0.0 | 0.1 | 0.1 | 42 | 353.808 |
| VT_B1_P101_SW | VT | LWC | 1 | 101 | SW | 34 | 115.6 | 0.9 | 0.2 | 1.1 | 39 | 127.008 |
| VT_B1_P102 | VT | NWC | 1 | 102 | M | 23 | 234.9 | 0.0 | 0.9 | 0.9 | 64 | 317.520 |
| VT_B1_P102_SW | VT | NWC | 1 | 102 | SW | 30 | 177.3 | 0.0 | 21.5 | 21.5 | 65 | 285.768 |
| VT_B1_P102_WF | VT | NWC | 1 | 102 | WF | 24 | 316.1 | 0.0 | 0.0 | 0.0 | 40 | 535.248 |
| VT_B1_P103 | VT | EWC | 1 | 103 | M | 34 | 187.8 | 0.0 | 2.7 | 2.7 | 66 | 512.568 |

``` r
wi_raw_2023 <- read_excel("~/Github/IMT/raw-data/wisconsin_raw_2023.xlsx")
kable(head(wi_raw_2023))
```

| ID | LOC | TRT | BLOCK | PLOT | MICROPLOT | EMERG | BBM | INTRAWBM | INTERWBM | WBM | DEN | BEANYD |
|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| WI_B1_P101 | WI | NWC | 1 | 101 | M | 23 | 233.3 | 0.00 | 2.00 | 1.84 | 38 | 295.66 |
| WI_B1_P101_SW | WI | NWC | 1 | 101 | SW | 24 | 61.7 | 31.66 | 155.90 | 187.56 | 28 | 88.81 |
| WI_B1_P101_WF | WI | NWC | 1 | 101 | WF | 14 | 210.3 | 0.00 | 0.00 | 0.00 | 38 | 273.51 |
| WI_B1_P102 | WI | LWC | 1 | 102 | M | 14 | 248.4 | 0.00 | 0.00 | 0.00 | 27 | 321.16 |
| WI_B1_P102_SW | WI | LWC | 1 | 102 | SW | 16 | 65.6 | 28.76 | 50.00 | 78.76 | 32 | 97.45 |
| WI_B1_P103 | WI | AWC | 1 | 103 | M | 17 | 279.3 | 0.00 | 0.21 | 0.21 | 34 | 302.72 |

### **Combine all datasets and clean**

``` r
# Main did not divided wbm into inter and intra row and so is excluded
#Standardaze column names, convert to factors, check for outliers of variable**
all_raw_2023 <- bind_rows(fh_raw_2023, cu_raw_2023, vt_raw_2023, wi_raw_2023)

#Standardaze column names, convert to factors, check for outliers of variable**
all_clean_2023 <- clean_names(all_raw_2023) |>  
  rename ('mowing'= trt,'weeds'= microplot) |> 
  mutate(across(c(mowing, block, plot, weeds, loc), as.factor)) |> 
  mutate(is_outlier = interwbm < (quantile(interwbm, 0.25) - 1.5 * IQR(interwbm)) |
                       interwbm > (quantile(interwbm, 0.75) + 1.5 * IQR(interwbm)))

#select and convert data for wbm analysis
  
all_interwbm_clean_2023 <-all_clean_2023 |>              
  #filter(is_outlier == FALSE) |>              # Keep rows where beanyd is not an outlier
  filter(weeds %in% c("SW", "M")) |>       # Keep rows where SW and M are microplots
  mutate(interwbm_grams_meter = (interwbm * 2)) |> 
  mutate(interwbm_kg_ha = ((interwbm/0.5) *(10000))/(1000)) |>
  mutate(interwbm_lbs_ac = (((interwbm/0.5) *(10000))/(1000))* 0.892179)
kable(head(all_interwbm_clean_2023)) 
```

| id | loc | mowing | block | plot | weeds | emerg | bbm | intrawbm | interwbm | wbm | den | beanyd | beanden | is_outlier | interwbm_grams_meter | interwbm_kg_ha | interwbm_lbs_ac |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|
| FH_B1_P101 | FH | EWC | 1 | 101 | M | 21 | 87.18 | 0.75 | 16.50 | 17.25 | 44 | 367.79 | NA | FALSE | 33.00 | 330.0 | 294.4191 |
| FH_B1_P101_SW | FH | EWC | 1 | 101 | SW | 24 | 40.32 | 3.54 | 37.65 | 41.19 | 45 | 218.00 | NA | FALSE | 75.30 | 753.0 | 671.8108 |
| FH_B1_P102 | FH | LWC | 1 | 102 | M | 24 | 72.37 | 6.47 | 12.71 | 19.18 | 52 | 268.00 | NA | FALSE | 25.42 | 254.2 | 226.7919 |
| FH_B1_P102_SW | FH | LWC | 1 | 102 | SW | 24 | 63.33 | 7.54 | 11.33 | 18.87 | 49 | 177.30 | NA | FALSE | 22.66 | 226.6 | 202.1678 |
| FH_B1_P103 | FH | AWC | 1 | 103 | M | 23 | 148.62 | 4.22 | 0.00 | 4.22 | 43 | 290.00 | NA | FALSE | 0.00 | 0.0 | 0.0000 |
| FH_B1_P103_SW | FH | AWC | 1 | 103 | SW | 26 | 84.27 | 2.90 | 8.13 | 11.03 | 39 | 236.00 | NA | FALSE | 16.26 | 162.6 | 145.0683 |

<br>

# **Assumptions**

## **Levene’s test**

``` r
levene_test((interwbm_kg_ha) ~ loc, data = all_interwbm_clean_2023)
```

    ## # A tibble: 1 × 4
    ##     df1   df2 statistic      p
    ##   <int> <int>     <dbl>  <dbl>
    ## 1     3   124      2.94 0.0358

# **Model Testing**

### **location random**

``` r
#data should be log transformed due to zeroes

all_interwbm_random_2023 <- lmer(interwbm_kg_ha ~  mowing*weeds + (1|loc) + (1|loc:block)+  (1|loc:block:mowing)  , data = all_interwbm_clean_2023)

resid_panel(all_interwbm_random_2023)
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
log_all_interwbm_random_2023  <- lmer(log(interwbm_kg_ha+1) ~  mowing*weeds + (1|loc) + (1|loc:block)+  (1|loc:block:mowing)  , data = all_interwbm_clean_2023)

resid_panel(log_all_interwbm_random_2023)
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### **location fixed**

``` r
all_interwbm_fixed_2023 <- lmer(interwbm_kg_ha ~ loc*mowing*weeds + (1|loc:block)+  (1|loc:block:mowing), data = all_interwbm_clean_2023)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
resid_panel(all_interwbm_fixed_2023)
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
log_all_interwbm_fixed_2023 <- lmer(log(interwbm_kg_ha+1) ~ loc*mowing*weeds + (1|loc:block)+  (1|loc:block:mowing), data = all_interwbm_clean_2023)

resid_panel(log_all_interwbm_fixed_2023)
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->
\#\*\*Anova

``` r
 log_all_interwbm_fixed_2023 |> 
  joint_tests() |> 
  kable()  
```

|     | model term       | df1 | df2 | F.ratio |   p.value |
|:----|:-----------------|----:|----:|--------:|----------:|
| 1   | loc              |   3 |  12 |   5.281 | 0.0149108 |
| 5   | mowing           |   3 |  36 |   8.286 | 0.0002546 |
| 7   | weeds            |   1 |  48 |  59.057 | 0.0000000 |
| 2   | loc:mowing       |   9 |  36 |   1.666 | 0.1338704 |
| 4   | loc:weeds        |   3 |  48 |  11.418 | 0.0000090 |
| 6   | mowing:weeds     |   3 |  48 |   2.301 | 0.0890220 |
| 3   | loc:mowing:weeds |   9 |  48 |   2.047 | 0.0540013 |

<br>

### **Fisher’s method for comparing means**

``` r
#mowing
cld_mowing_fisher <-cld(emmeans(all_interwbm_fixed_2023, ~  mowing,type = "response"), Letters = letters, sort = TRUE, adjust="none", reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_mowing_fisher
```

    ##  mowing emmean   SE   df lower.CL upper.CL .group
    ##  NWC       621 63.6 39.8    492.1      749  a    
    ##  EWC       439 63.6 39.8    310.7      568   b   
    ##  AWC       203 63.6 39.8     73.9      331    c  
    ##  LWC       154 63.6 39.8     25.3      283    c  
    ## 
    ## Results are averaged over the levels of: loc, weeds 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
#weeds
cld_weeds_fisher <- cld(emmeans(all_interwbm_fixed_2023, ~  weeds, type = "response"), Letters = letters, sort = FALSE, adjust="none", reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_weeds_fisher
```

    ##  weeds emmean   SE df lower.CL upper.CL .group
    ##  M        147 50.5 23     42.5      252   b   
    ##  SW       561 50.5 23    456.7      666  a    
    ## 
    ## Results are averaged over the levels of: loc, mowing 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
#mowing|weeds
cld_mowing_weeds_fisher <-cld(emmeans(all_interwbm_fixed_2023, ~  mowing|weeds, 
                              type = "response"), Letters = letters, 
                              sort = TRUE, adjust="none", reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_mowing_weeds_fisher
```

    ## weeds = M:
    ##  mowing emmean   SE   df lower.CL upper.CL .group
    ##  EWC     227.6 83.9 82.9    60.69      395  a    
    ##  NWC     170.7 83.9 82.9     3.81      338  a    
    ##  AWC     114.6 83.9 82.9   -52.32      281  a    
    ##  LWC      75.3 83.9 82.9   -91.61      242  a    
    ## 
    ## weeds = SW:
    ##  mowing emmean   SE   df lower.CL upper.CL .group
    ##  NWC    1070.8 83.9 82.9   903.93     1238  a    
    ##  EWC     651.2 83.9 82.9   484.27      818   b   
    ##  AWC     290.5 83.9 82.9   123.63      457    c  
    ##  LWC     232.5 83.9 82.9    65.63      399    c  
    ## 
    ## Results are averaged over the levels of: loc 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
#mowing|weeds|loc
cld_mowing_weeds_loc_fisher <-cld(emmeans(all_interwbm_fixed_2023, ~  mowing|weeds|loc, type = "respons"), Letters = letters, sort = TRUE, adjust="none", reversed=TRUE)
cld_mowing_weeds_loc_fisher
```

    ## weeds = M, loc = CU:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  NWC     349.40 168 82.9     15.6      683  a    
    ##  AWC     300.50 168 82.9    -33.3      634  a    
    ##  EWC     291.30 168 82.9    -42.5      625  a    
    ##  LWC     173.35 168 82.9   -160.5      507  a    
    ## 
    ## weeds = SW, loc = CU:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  NWC     760.15 168 82.9    426.3     1094  a    
    ##  EWC     758.35 168 82.9    424.5     1092  a    
    ##  LWC     273.60 168 82.9    -60.2      607   b   
    ##  AWC      92.35 168 82.9   -241.5      426   b   
    ## 
    ## weeds = M, loc = FH:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  EWC     233.80 168 82.9   -100.0      568  a    
    ##  LWC      94.85 168 82.9   -239.0      429  a    
    ##  NWC      82.95 168 82.9   -250.9      417  a    
    ##  AWC       6.65 168 82.9   -327.2      340  a    
    ## 
    ## weeds = SW, loc = FH:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  EWC     635.55 168 82.9    301.7      969  a    
    ##  NWC     485.20 168 82.9    151.4      819  ab   
    ##  LWC     118.65 168 82.9   -215.2      452   b   
    ##  AWC      89.40 168 82.9   -244.4      423   b   
    ## 
    ## weeds = M, loc = VT:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  EWC     280.50 168 82.9    -53.3      614  a    
    ##  NWC     240.50 168 82.9    -93.3      574  a    
    ##  AWC     127.00 168 82.9   -206.8      461  a    
    ##  LWC      33.00 168 82.9   -300.8      367  a    
    ## 
    ## weeds = SW, loc = VT:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  NWC    1072.50 168 82.9    738.7     1406  a    
    ##  AWC     547.50 168 82.9    213.7      881   b   
    ##  EWC      88.50 168 82.9   -245.3      422    c  
    ##  LWC      77.00 168 82.9   -256.8      411    c  
    ## 
    ## weeds = M, loc = WI:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  EWC     104.80 168 82.9   -229.0      439  a    
    ##  AWC      24.20 168 82.9   -309.6      358  a    
    ##  NWC      10.00 168 82.9   -323.8      344  a    
    ##  LWC       0.00 168 82.9   -333.8      334  a    
    ## 
    ## weeds = SW, loc = WI:
    ##  mowing  emmean  SE   df lower.CL upper.CL .group
    ##  NWC    1965.50 168 82.9   1631.7     2299  a    
    ##  EWC    1122.30 168 82.9    788.5     1456   b   
    ##  LWC     460.90 168 82.9    127.1      795    c  
    ##  AWC     432.90 168 82.9     99.1      767    c  
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

# **FIGURES**

## **Mowing**

``` r
all_interwbm_clean_2023 |> 
  left_join(cld_mowing_fisher) |> 
  ggplot(aes(x = mowing, y = interwbm_kg_ha, fill = mowing)) +
  stat_summary(geom = "bar", fun = "mean", width = 0.7) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  stat_summary(geom="text", fun = "MeanPlusSe", aes(label= trimws(.group)),size=6.5,vjust=-0.5) +
  labs(
    x = "Timing of Mowing",
    y = expression("Interrow weed biomass" ~ (kg ~ ha^{-1})),
    title = str_c("The influence of interrow mowing on interrow weed biomass"),
    subtitle = expression(italic("P < 0.005"))) +
  
  scale_x_discrete(labels = c("As-needed\nmowing", "Early\nmowing", "Late\nmowing", "No\nmowing")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, end = 0.9, begin = 0.1) +
   theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("interwbm_plot_mowing.png", width = 8, height = 6, dpi = 300)
```

## **mowing\|weeds\|loc**

``` r
all_interwbm_clean_2023 |> 
  left_join(cld_mowing_weeds_loc_fisher) |> 
  ggplot(aes(x = mowing, y = interwbm_kg_ha, fill = mowing)) +
  facet_grid(weeds ~ loc, labeller = labeller(
    loc = c("CU" = "Aurora, NY", "FH" = "Hurley, NY", "ME" = "Orono, ME", "VT" = "Alburgh, VT", "WI" = "Arlington, WI"),
    weeds = c("M" = "Ambient Weeds", "SW" = "Ambient + Surrogate Weeds")
  )) +
  stat_summary(geom = "bar", fun = "mean", width = 0.7) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  stat_summary(geom="text", fun = "MeanPlusSe", aes(label= trimws(.group)),size=6.5,vjust=-0.5) +
  labs(
    x = "Timing of Mowing",
    y = expression("Interrow weed biomass" ~ (kg ~ ha^{-1})),
    title = str_c("The influence of interrow mowing on interrow weed biomass"),
    subtitle = expression(italic("P = 0.054"))) +
  
  scale_x_discrete(labels = c("As-needed\nmowing", "Early\nmowing", "Late\nmowing", "No\nmowing")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, end = 0.9, begin = 0.1) +
   theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )
```

![](interrow_weed_biomass_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("interwbm_plot_mowing_weeds_loc.png", width = 12, height = 6, dpi = 300)
```
