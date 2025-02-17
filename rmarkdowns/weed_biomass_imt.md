weed
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
  rename ('mowing'= treatment,'weeds'= microplot) |> 
  mutate(across(c(year, location, site_year, mowing, block, plot, weeds, ), as.factor)) 

#select and convert data for wbm analysis
weed_biomass_clean <-clean_combined |>             
  filter(weeds %in% c("SW", "M")) |>  # Keep rows where SW and M are microplots
  mutate(weed_biomass_grams_meter = (weed_biomass /0.5)) |> 
  mutate(weed_biomass_kg_ha = (weed_biomass_grams_meter *(10000))/(1000)) |>
  mutate(weed_biomass_lbs_ac = ((weed_biomass_grams_meter *(10000))/(1000))* 0.892179)
kable(head(weed_biomass_clean)) 
```

| id | year | location | site_year | mowing | block | plot | weeds | bean_emergence | bean_biomass | intrarow_weed_biomass | interrow_weed_biomass | weed_biomass | bean_population | bean_yield | weed_biomass_grams_meter | weed_biomass_kg_ha | weed_biomass_lbs_ac |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|:---|---:|:---|:---|---:|---:|---:|
| WI_B1_P101 | 2023 | WI | WI_2023 | NWC | 1 | 101 | M | 23 | 233.3 | 0 | 2 | 1.84 | 38 | 295.66000000000003 | 3.68 | 36.8 | 32.832187 |
| WI_B1_P101_SW | 2023 | WI | WI_2023 | NWC | 1 | 101 | SW | 24 | 61.7 | 31.66 | 155.9 | 187.56 | 28 | 88.81 | 375.12 | 3751.2 | 3346.741865 |
| WI_B1_P102 | 2023 | WI | WI_2023 | LWC | 1 | 102 | M | 14 | 248.4 | 0 | 0 | 0.00 | 27 | 321.16000000000003 | 0.00 | 0.0 | 0.000000 |
| WI_B1_P102_SW | 2023 | WI | WI_2023 | LWC | 1 | 102 | SW | 16 | 65.6 | 28.76 | 50 | 78.76 | 32 | 97.45 | 157.52 | 1575.2 | 1405.360361 |
| WI_B1_P103 | 2023 | WI | WI_2023 | AWC | 1 | 103 | M | 17 | 279.3 | 0 | 0.21 | 0.21 | 34 | 302.72000000000003 | 0.42 | 4.2 | 3.747152 |
| WI_B1_P103_SW | 2023 | WI | WI_2023 | AWC | 1 | 103 | SW | 15 | 80.1 | 27.56 | 22.26 | 49.82 | 32 | 118.17 | 99.64 | 996.4 | 888.967156 |

\#GLMM

``` r
model_tweedie_log <- glmmTMB(weed_biomass_lbs_ac ~ mowing*weeds + (1|site_year) + (1|site_year:block)+ (1|site_year:block:mowing), 
  data = weed_biomass_clean, 
  family = tweedie(link = "log")

)

###It assumes that mowing effects may vary by block, in addition to site-year and block-level variation.
#This would be useful if you suspect that mowing impacts weed biomass differently in different blocks.
#The model accounts for hierarchical structure down to the mowing level within blocks.

### Two checks specifically for a generalize linear approach
simulateResiduals(model_tweedie_log,plot = TRUE) # Residuals and normality look good
```

![](weed_biomass_imt_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.156 0.904 0.05048307 0.832 0.12 0.64 0.076 0.716 0.03671421 0.592 0.224 0.664 0.07983672 0.836 0.03441097 0.536 0.244 0.332 0.07823883 0.224 ...

``` r
check_model(model_tweedie_log) #Perfect, preditions match real data
```

    ## `check_outliers()` does not yet support models of class `glmmTMB`.

![](weed_biomass_imt_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
summary(model_tweedie_log )
```

    ##  Family: tweedie  ( log )
    ## Formula:          
    ## weed_biomass_lbs_ac ~ mowing * weeds + (1 | site_year) + (1 |  
    ##     site_year:block) + (1 | site_year:block:mowing)
    ## Data: weed_biomass_clean
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4518.5   4567.5  -2246.2   4492.5      307 
    ## 
    ## Random effects:
    ## 
    ## Conditional model:
    ##  Groups                 Name        Variance  Std.Dev. 
    ##  site_year              (Intercept) 2.621e-01 5.119e-01
    ##  site_year:block        (Intercept) 1.709e-02 1.307e-01
    ##  site_year:block:mowing (Intercept) 7.749e-09 8.803e-05
    ## Number of obs: 320, groups:  
    ## site_year, 10; site_year:block, 40; site_year:block:mowing, 160
    ## 
    ## Dispersion parameter for tweedie family (): 9.14 
    ## 
    ## Conditional model:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        5.31896    0.23147  22.979  < 2e-16 ***
    ## mowingEWC          0.47221    0.21934   2.153   0.0313 *  
    ## mowingLWC         -0.21338    0.23379  -0.913   0.3614    
    ## mowingNWC          0.41594    0.22166   1.876   0.0606 .  
    ## weedsSW            1.20082    0.20675   5.808 6.32e-09 ***
    ## mowingEWC:weedsSW  0.02726    0.27794   0.098   0.9219    
    ## mowingLWC:weedsSW  0.10251    0.29527   0.347   0.7285    
    ## mowingNWC:weedsSW  0.22576    0.27811   0.812   0.4169    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
VarCorr(model_tweedie_log )
```

    ## 
    ## Conditional model:
    ##  Groups                 Name        Std.Dev.  
    ##  site_year              (Intercept) 5.1192e-01
    ##  site_year:block        (Intercept) 1.3072e-01
    ##  site_year:block:mowing (Intercept) 8.8026e-05

#### Joint test (anova)

``` r
model_tweedie_log |> 
  joint_tests() |> 
  kable()  
```

|     | model term   | df1 | df2 | F.ratio |   Chisq |   p.value |
|:----|:-------------|----:|----:|--------:|--------:|----------:|
| 1   | mowing       |   3 | Inf |  11.983 |  35.949 | 0.0000001 |
| 3   | weeds        |   1 | Inf | 156.426 | 156.426 | 0.0000000 |
| 2   | mowing:weeds |   3 | Inf |   0.279 |   0.837 | 0.8404547 |

``` r
options(contrasts = c("contr.sum", "contr.poly"))
Anova(model_tweedie_log, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: weed_biomass_lbs_ac
    ##                 Chisq Df Pr(>Chisq)    
    ## (Intercept)  528.0379  1  < 2.2e-16 ***
    ## mowing        12.9105  3   0.004834 ** 
    ## weeds         33.7342  1  6.318e-09 ***
    ## mowing:weeds   0.8376  3   0.840455    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

<br>

## Fisher compact letter display

### Mowing (Significant)

``` r
cld_weed_control_fisher <-cld(emmeans(model_tweedie_log, ~  mowing, type = "response"), Letters = letters,adjust = "none", sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_weed_control_fisher
```

    ##  mowing response    SE  df asymp.LCL asymp.UCL .group
    ##  NWC         632 119.0 Inf       436       915  a    
    ##  EWC         605 114.0 Inf       418       875  a    
    ##  AWC         372  72.1 Inf       255       544   b   
    ##  LWC         316  62.0 Inf       216       465   b   
    ## 
    ## Results are averaged over the levels of: weeds 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the log scale 
    ## Tests are performed on the log scale 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

### Weeds (Significant)

``` r
cld_weed_control_fisher <-cld(emmeans(model_tweedie_log, ~  weeds, type = "response"), Letters = letters,adjust = "none", sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_weed_control_fisher
```

    ##  weeds response    SE  df asymp.LCL asymp.UCL .group
    ##  SW         878 153.0 Inf       624      1235  a    
    ##  M          242  44.4 Inf       169       346   b   
    ## 
    ## Results are averaged over the levels of: mowing 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the log scale 
    ## Tests are performed on the log scale 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

# Figures

## Weed-control (S)

``` r
weed_biomass_clean |> 
  left_join(cld_weed_control_fisher) |> 
  ggplot(aes(x = factor(mowing, levels = c("NWC", "EWC", "LWC", "AWC")), y = response, fill = mowing)) +
  #stat_summary(geom = "bar", fun = "mean", width = 0.7) +
  #stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  #stat_summary(geom="text", fun = "MeanPlusSe", aes(label= trimws(.group)),size=6.5,vjust=-0.5) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=.2,
                 position=position_dodge(.9))+
geom_text(aes(label = trimws(.group), y = response + (SE + 30)), size = 7) +
  labs(
    x = "",
     y = expression("Weed biomass" ~ (lbs * "/" * a)),
    #title = str_c("Influence of interrow weed control on weed biomass"),
    subtitle = expression(italic("P < 0.005"))) +
  
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

![](weed_biomass_imt_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("weed_biomass_mowing_lbsacre.png", width = 10, height = 8, dpi = 300)
```
