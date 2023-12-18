---
title: "EOO Edits to no_exit_iucn.Rmd" 
author: "Sophia V. Richter"
date: "13 December, 2023"
output:
  html_document:
    code_folding: show
    highlight: default
    keep_md: yes
    theme: journal
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: yes
      toc_depth: 3
editor_options: 
  chunk_output_type: console
---

```{=html}
<style>
pre code, pre, code {
  white-space: pre !important;
  overflow-x: scroll !important;
  word-break: keep-all !important;
  word-wrap: initial !important;
}
</style>
```



```r
# Data wrangling
library(dplyr)
library(tidyr)
library(readr)
# Figures
library(GGally)
library(ggplot2)
# Stats
library(lme4)
```

# Load data


```r
merge_dat <- readRDS("/workdir/sr2352/HMEI-internship/data/HMEI-GDrive/data/sorted/merge_dat.Rds")
realm_cats <- sort(unique(unlist(strsplit(unique(merge_dat$Realms), "[;]"))))
```


```r
# Modify data frame
eoo_all_df <- merge_dat %>% 
  # remove species with 0 or NA AOH
  filter(!(is.na(areaAOH)), areaAOH > 0) %>%
  mutate(
    # convert AOH to km^2
    AOHkm = areaAOH/1000000,
    # Identify NES
    EOO_NES = AOHkm < 20000,
    # add column for whether AOH falls below 100 km (more than 10 km away from threshold)
    one_hundred = AOHkm < 100, # changed from ten
    # add column for whether AOH falls below more than 1 km below 5000 km threshold
    five_thousand = AOHkm < 5000, # changed from five_hundred
    # add column for whether AOH falls below more than 1 km below 20000 km threshold
    twenty_thousand = AOHkm < 20000, # changed from two_thousand
    # column if one_hundred is lowest threshold
    first_lowest = one_hundred, # was ten_lowest
    # column if 5000 is lowest threshold 
    second_lowest = xor(one_hundred, five_thousand), #was five_lowest
    # column if 20000 is lowest threshold
    third_lowest = xor(five_thousand, twenty_thousand), # was two_lowest
    # record what category should be
    Category = ifelse(first_lowest == TRUE, "Critically Endangered", 
                      ifelse(second_lowest == TRUE, "Endangered", 
                             ifelse(third_lowest == TRUE, "Vulnerable", 
                                    "Near Threatened"))),
    # Calculate difference to next threshold
    Difference = ifelse(first_lowest == TRUE, 100 - AOHkm, 
                        ifelse(second_lowest == TRUE, 5000 - AOHkm, 
                               ifelse(third_lowest == TRUE, 20000 - AOHkm, 
                                      NA)))) %>% 
  # Calculate Difference between Category and redlistCategory
  mutate(num_Category = ifelse(Category == "Critically Endangered", 1, 
                               ifelse(Category == "Endangered", 2, 
                                      ifelse(Category == "Vulnerable", 3, 
                                             ifelse(Category == "Near Threatened", 4, 
                                                    NA)))),
         num_redlist = ifelse(redlistCategory == "Critically Endangered", 1, 
                              ifelse(redlistCategory == "Endangered", 2, 
                                     ifelse(redlistCategory == "Vulnerable", 3, 
                                            ifelse(redlistCategory == "Near Threatened", 4, 
                                                   NA)))),
         cat_red_change = num_Category - num_redlist,
         # Standardised difference
         difference_prop = ifelse(Category == "Critically Endangered", Difference / 100, 
                                  ifelse(Category == "Endangered", Difference / 4900, 
                                         ifelse(Category == "Vulnerable", Difference / 15000, 
                                                NA))),
         # Flip the scale so bigger numbers = closer to threshold for downlisting
         proximity = 1 - difference_prop)

# Calculate quantiles
quants <-
    eoo_all_df %>% 
    group_by(Category) %>%
    dplyr::summarise(diff25 = quantile(Difference, probs = 0.25, na.rm = TRUE), # lower
              diff50 = quantile(Difference, probs = 0.5, na.rm = TRUE), # median
              diff75 = quantile(Difference, probs = 0.75, na.rm = TRUE)) # upper

# Add to df
eoo_all_df <- 
    left_join(eoo_all_df, quants, by = "Category") %>% 
    mutate(
        # Identify species furthest from downlisting threshold
        diff_top25 = Difference >= diff75,
        # Identify species closest to downlisting threshold
        diff_bottom25 = Difference <= diff25
        )

eoo_nes_df <-
  eoo_all_df %>% 
  # remove Differences that are NA
    # (i.e. species that would not be classed as threatened according to AOH)
  filter(!(is.na(Difference)))

# Sanity check
unique(eoo_nes_df$EOO_NES)
```

```
## [1] TRUE
```



```r
# Long dataframe
realms_long_all <- 
 eoo_all_df %>%  
  dplyr::select(-Realms) %>% 
  pivot_longer(cols= all_of(realm_cats), 
               names_to='Realms', values_to='value') %>% 
  dplyr::filter(value == TRUE) %>% 
  dplyr::select(-value)

realms_long_nes <-
  realms_long_all %>% 
  dplyr::filter(EOO_NES)
```

There are 3536 species with a habitat that qualifies a species for a threatened category. 

Negative value means Habitat Category is uplisted compared to redlistCategory, 0 means they match, positive means Habitat Category is downlisted compared to redlistCategory.


```r
cat_change <- eoo_all_df %>% 
  group_by(cat_red_change) %>% 
  summarise(Sum = n())

cat_change
```

```
## # A tibble: 6 × 2
##   cat_red_change   Sum
##            <dbl> <int>
## 1             -2   224
## 2             -1  1290
## 3              0  1449
## 4              1   659
## 5              2   203
## 6              3    44
```

```r
nochange <- cat_change %>% filter(cat_red_change == 0) %>% pull(Sum)

decrease <- cat_change %>% filter(cat_red_change < 0) %>% summarize(sum = sum(Sum)) %>% pull(sum)

increase <- cat_change %>% filter(cat_red_change > 0) %>% summarize(sum = sum(Sum)) %>% pull(sum)

change_dat <- 
  eoo_all_df %>% 
  filter(cat_red_change == -2) %>% 
  select(species, className, redlistCategory)
```

1449 species (37.4515379 %) do not change category. 1514 species (39.1315585 %) have less AOH than their current red list category (- value). 906 species (23.4169036 %) species have more AOH than their red list category (+ value).


# No Exit Species by EOO (EOO_NES)

Classes of NES

```r
# Table
eoo_nes_df %>% 
  group_by(className) %>% 
  summarise(Count = n()) %>% 
  arrange(-Count)
```

```
## # A tibble: 5 × 2
##   className     Count
##   <chr>         <int>
## 1 amphibia       1602
## 2 aves            776
## 3 mammalia        752
## 4 magnoliopsida    67
## 5 reptilia         60
```

Category of NES

```r
#table
eoo_nes_df %>% 
  group_by(Category) %>% 
  summarize(Count = n()) %>% 
  arrange(-Count)
```

```
## # A tibble: 3 × 2
##   Category              Count
##   <chr>                 <int>
## 1 Endangered             1858
## 2 Critically Endangered  1016
## 3 Vulnerable              383
```

```r
# graphs
ggplot(eoo_nes_df, aes(x=className, fill=className)) + 
  geom_bar() + 
  facet_wrap(~Category) + 
  ggtitle("Class and Threshold of NES by EOO") + 
  theme(axis.text.x = element_blank()) + 
  labs(fill="Class") + xlab(" ") + ylab("Count")
```

<img src="EOO_no_exit_iucn_files/figure-html/nes-category-1.png" style="display: block; margin: auto;" />

Realm of Counts of EOO_NES

Realms of each Conservation Reliant Species

```r
eoo_nes_df %>% 
  group_by(Realms) %>% 
  summarise(Count = n()) %>% 
  arrange(-Count)
```

```
## # A tibble: 26 × 2
##    Realms                 Count
##    <chr>                  <int>
##  1 Neotropical              650
##  2 Oriental                 603
##  3 Panamanian               502
##  4 Afrotropical             397
##  5 Madagascan               293
##  6 Oceanina                 165
##  7 Neotropical;Panamanian   155
##  8 Nearctic                 149
##  9 Australian                82
## 10 Sino-Japanese             65
## # ℹ 16 more rows
```

Number of CRS in each realm

```r
realms_long_nes %>% 
  group_by(Realms) %>% 
  summarize(Count = n()) %>% 
  arrange(-Count)
```

```
## # A tibble: 13 × 2
##    Realms         Count
##    <chr>          <int>
##  1 Neotropical      805
##  2 Panamanian       679
##  3 Oriental         666
##  4 Afrotropical     398
##  5 Madagascan       293
##  6 Oceanina         180
##  7 Nearctic         171
##  8 Sino-Japanese    128
##  9 Australian        92
## 10 Palearctic        80
## 11 Saharo-Arabian    32
## 12 Neartic            6
## 13 Oceanian           6
```

```r
ggplot(realms_long_nes, aes(x=Realms)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Number of CRS in each Realm")
```

<img src="EOO_no_exit_iucn_files/figure-html/realm-nes-count-1.png" style="display: block; margin: auto;" />

```r
ggplot(realms_long_nes, aes(x=Realms, fill=Realms)) + 
  geom_bar() + 
  facet_wrap(~Category) + 
  theme(axis.text.x = element_blank()) + 
  ggtitle("Category and Realm of CRS")
```

<img src="EOO_no_exit_iucn_files/figure-html/realm-nes-count-2.png" style="display: block; margin: auto;" />

# Difference Distribution

Class

```r
ggplot(eoo_nes_df, aes(x=Difference, fill=className, alpha = 0.01)) + 
  geom_histogram() + 
  ggtitle("Distribution of Differences to Next Threshold Across Class")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-density-class-1.png" style="display: block; margin: auto;" />

```r
ggplot(eoo_nes_df, aes(x=Difference, fill=className, alpha=0.01)) + 
  geom_density() + 
  ggtitle("Density of Differences to Next Threshold Across Class")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-density-class-2.png" style="display: block; margin: auto;" />

```r
ggplot(eoo_nes_df, aes(x=Difference)) + 
  geom_histogram() + 
  facet_wrap(~className) + 
  ggtitle("Distribution of Differences to Next Threshold Across Class")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-density-class-3.png" style="display: block; margin: auto;" />

```r
ggplot(eoo_nes_df, aes(x=Difference)) + 
  geom_density() + 
  facet_wrap(~className) + 
  ggtitle("Density of Differences to Next Threshold Across Class")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-density-class-4.png" style="display: block; margin: auto;" />

Histogram of Differences by Class

```r
ggplot(eoo_nes_df, aes(x=Difference, fill=className)) + 
  geom_histogram(alpha=0.7) + 
  ggtitle("Distribution of Differences to Next Threshold \n Across Class and Category") +
  facet_wrap(~ Category, scales = "free", 
             labeller = label_wrap_gen(width = 2, multi_line = TRUE)) + 
  labs(fill="Class") + 
  ylab("Count") + 
  xlab("Difference (km^2)")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-hist-class-1.png" style="display: block; margin: auto;" />

Category

```r
ggplot(eoo_nes_df, aes(x=Difference, fill=Category, alpha=0.5)) + 
  geom_histogram() + 
  ggtitle("Distribution of Differences to Next Threshold Across Category")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-hist-category-1.png" style="display: block; margin: auto;" />

Realm

```r
ggplot(realms_long_nes, aes(x=Difference, fill=Realms)) + 
  geom_histogram() + 
  ggtitle("Distribution of Differences Across Realm")
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-hist-realm-1.png" style="display: block; margin: auto;" />

```r
ggplot(realms_long_nes, aes(x=Difference)) + 
  geom_histogram() + 
  facet_wrap(~Realms)
```

<img src="EOO_no_exit_iucn_files/figure-html/diff-hist-realm-2.png" style="display: block; margin: auto;" />


# Top 25% Differences



### Class

```r
diff_top25 %>% 
  group_by(className) %>% 
  summarise(Count = n()) %>% 
  arrange(-Count)
```

```
## # A tibble: 5 × 2
##   className     Count
##   <chr>         <int>
## 1 amphibia        451
## 2 aves            181
## 3 mammalia        150
## 4 reptilia         19
## 5 magnoliopsida    14
```

```r
ggplot(diff_top25, aes(x=className)) + 
  geom_bar() + 
  ggtitle("Distribution of Top 25% (of each category) AOH Differences - AKA Furthest from Downlisting")
```

<img src="EOO_no_exit_iucn_files/figure-html/top-diff-class-1.png" style="display: block; margin: auto;" />

```r
ggplot(diff_top25, aes(x=className, fill=className)) + 
  geom_bar() + 
  facet_wrap(~Category) + 
  ggtitle("Distribution of Top 25% AOH Differences (furthest from downlisting)") + 
  theme(axis.text.x = element_text(angle = 90))
```

<img src="EOO_no_exit_iucn_files/figure-html/top-diff-class-2.png" style="display: block; margin: auto;" />

### Category


### Realm


# GLM

= column for species name + class + whether NES (0 or 1)
glm(NES ~ class, family = "binomial")


Realm NES


```r
# I think we should focus on endemic species here, to avoid double-counting
glm_full_nes <- 
  glm(EOO_NES ~ className * Realms + className * Category + Realms * Category, 
      data = filter(realms_long_all, realm_endemic),
      family = "binomial", maxit=100)

# Drop & test each interaction in turn
glm_dropInter1_nes <- update(glm_full_nes, ~.- className:Realms)
anova(glm_dropInter1_nes, glm_full_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category + className:Category + 
##     Realms:Category
## Model 2: EOO_NES ~ className * Realms + className * Category + Realms * 
##     Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3373 1.5254e-12                     
## 2      3345 1.5254e-12 28        0        1
```

```r
glm_dropInter2_nes <- update(glm_full_nes, ~.- className:Category)
anova(glm_dropInter2_nes, glm_full_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category + className:Realms + 
##     Realms:Category
## Model 2: EOO_NES ~ className * Realms + className * Category + Realms * 
##     Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3356 1.5254e-12                     
## 2      3345 1.5254e-12 11        0        1
```

```r
glm_dropInter3_nes <- update(glm_full_nes, ~.- Realms:Category)
anova(glm_dropInter3_nes, glm_full_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category + className:Realms + 
##     className:Category
## Model 2: EOO_NES ~ className * Realms + className * Category + Realms * 
##     Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3376 1.5254e-12                     
## 2      3345 1.5254e-12 31        0        1
```

```r
#-------------------------------------------------------------------------------
# All had equally small effect (p-value of 1, small deviance). dropping realm:Category has highest DF
# so make this the new reference model and repeat with remaining interactions
# ------------------------------------------------------------------------------

glm_dropInter4_nes <- update(glm_dropInter3_nes, ~.- className:Realms)
anova(glm_dropInter4_nes, glm_dropInter3_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category + className:Category
## Model 2: EOO_NES ~ className + Realms + Category + className:Realms + 
##     className:Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3404 1.5254e-12                     
## 2      3376 1.5254e-12 28        0        1
```

```r
glm_dropInter5_nes <- update(glm_dropInter3_nes, ~.- className:Category)
anova(glm_dropInter5_nes, glm_dropInter3_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category + className:Realms
## Model 2: EOO_NES ~ className + Realms + Category + className:Realms + 
##     className:Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3387 1.5254e-12                     
## 2      3376 1.5254e-12 11        0        1
```

```r
#-------------------------------------------------------------------------------
# Dropping either interaction term had same deviance and p-value effect. Dropping className:Category had higher DF
# so make this the new reference model and repeat with remaining interaction
# ------------------------------------------------------------------------------

glm_dropInter6_nes <- update(glm_dropInter5_nes, ~.- className:Realms)
anova(glm_dropInter6_nes, glm_dropInter5_nes, test = "LRT") # this interaction is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms + Category
## Model 2: EOO_NES ~ className + Realms + Category + className:Realms
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3415 1.5254e-12                     
## 2      3387 1.5254e-12 28        0        1
```

```r
#-------------------------------------------------------------------------------
# Drop ALL interactions - HOORAY
# now test significance of individual variables
# ------------------------------------------------------------------------------

# Test significance of class
glm_dropClass_nes <- update(glm_dropInter6_nes, ~.- className)
anova(glm_dropClass_nes, glm_dropInter6_nes, test = "LRT") # class is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ Realms + Category
## Model 2: EOO_NES ~ className + Realms + Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3419 1.5254e-12                     
## 2      3415 1.5254e-12  4        0        1
```

```r
# Test significance of realm
glm_dropRealm_nes <- update(glm_dropInter6_nes, ~.- Realms)
anova(glm_dropRealm_nes, glm_dropInter6_nes, test = "LRT") # realm is not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Category
## Model 2: EOO_NES ~ className + Realms + Category
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      3427 1.5254e-12                     
## 2      3415 1.5254e-12 12        0        1
```

```r
# Test significance of category
glm_dropCat_nes <- update(glm_dropInter6_nes, ~.- Category)
anova(glm_dropCat_nes, glm_dropInter6_nes, test = "LRT") # category is significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EOO_NES ~ className + Realms
## Model 2: EOO_NES ~ className + Realms + Category
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1      3418       2039                          
## 2      3415          0  3     2039 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Compare AIC as well, for good measure
AIC(glm_dropInter6_nes,glm_dropClass_nes,glm_dropRealm_nes,glm_dropCat_nes)
```

```
##                    df      AIC
## glm_dropInter6_nes 20   40.000
## glm_dropClass_nes  16   32.000
## glm_dropRealm_nes   8   16.000
## glm_dropCat_nes    17 2072.973
```

```r
# all quite similar, except for dropping Category which produces a *much* worse model
```

--> I am not sure if we want to add the plot of model-predicted values? I would guess it is very similar (in that it is not helpful) to the last data set. 

### LM on Difference 
- try difference / max difference for these models 

```r
nes_difference <- 
  eoo_nes_df %>% 
  filter(
    # Drop NAs
    !(is.na(Difference)),
    # Restrict to species occurring in one Realm only
    realm_endemic
  )
# Make a standard lm
lm_prop <- lm(difference_prop ~ className, 
              data = eoo_nes_df)

# Model validation plots
par(mfrow = c(2,2))
plot(lm_prop)
```

<img src="EOO_no_exit_iucn_files/figure-html/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

```r
summary(lm_prop)
```

```
## 
## Call:
## lm(formula = difference_prop ~ className, data = eoo_nes_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.77235 -0.14856  0.09132  0.20315  0.30812 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.774184   0.006505 119.022  < 2e-16 ***
## classNameaves          -0.064079   0.011387  -5.628 1.98e-08 ***
## classNamemagnoliopsida -0.021699   0.032464  -0.668    0.504    
## classNamemammalia      -0.082304   0.011508  -7.152 1.05e-12 ***
## classNamereptilia      -0.010911   0.034234  -0.319    0.750    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2603 on 3252 degrees of freedom
## Multiple R-squared:  0.01934,	Adjusted R-squared:  0.01814 
## F-statistic: 16.04 on 4 and 3252 DF,  p-value: 5.211e-13
```

```r
# Make a standard lm
lm_prop_cat <- lm(difference_prop ~ Category, 
                  data = nes_difference)

# Model validation plots
par(mfrow = c(2,2))
plot(lm_prop_cat)
```

<img src="EOO_no_exit_iucn_files/figure-html/unnamed-chunk-1-2.png" style="display: block; margin: auto;" />

```r
summary(lm_prop_cat)
```

```
## 
## Call:
## lm(formula = difference_prop ~ Category, data = nes_difference)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.77348 -0.14685  0.09029  0.19586  0.34405 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         0.697809   0.008294  84.133  < 2e-16 ***
## CategoryEndangered  0.086926   0.010406   8.353  < 2e-16 ***
## CategoryVulnerable -0.042751   0.016400  -2.607  0.00919 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2582 on 2987 degrees of freedom
## Multiple R-squared:  0.03626,	Adjusted R-squared:  0.03562 
## F-statistic:  56.2 on 2 and 2987 DF,  p-value: < 2.2e-16
```

```r
# Make a standard lm
lm_prop_realm <- lm(difference_prop ~ Realms, 
                    data = eoo_nes_df)

# Model validation plots
par(mfrow = c(2,2))
plot(lm_prop_realm)
```

<img src="EOO_no_exit_iucn_files/figure-html/unnamed-chunk-1-3.png" style="display: block; margin: auto;" />

```r
summary(lm_prop_realm)
```

```
## 
## Call:
## lm(formula = difference_prop ~ Realms, data = eoo_nes_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7857 -0.1416  0.0963  0.2093  0.4352 
## 
## Coefficients:
##                                                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                             0.755231   0.013141  57.472  < 2e-16 ***
## RealmsAfrotropical;Saharo-Arabian                      -0.098574   0.262158  -0.376  0.70694    
## RealmsAustralian                                       -0.023322   0.031760  -0.734  0.46280    
## RealmsAustralian;Oceanina                              -0.074404   0.083834  -0.888  0.37487    
## RealmsMadagascan                                        0.001389   0.020166   0.069  0.94510    
## RealmsNearctic                                          0.001890   0.025155   0.075  0.94012    
## RealmsNearctic;Panamanian                              -0.094160   0.057348  -1.642  0.10071    
## RealmsNeartic                                           0.213257   0.107696   1.980  0.04777 *  
## RealmsNeotropical                                      -0.027913   0.016678  -1.674  0.09429 .  
## RealmsNeotropical;Panamanian                           -0.012598   0.024799  -0.508  0.61147    
## RealmsOceanian                                         -0.127009   0.107696  -1.179  0.23835    
## RealmsOceanina                                         -0.030310   0.024252  -1.250  0.21146    
## RealmsOceanina;Oriental                                -0.279474   0.117828  -2.372  0.01776 *  
## RealmsOriental                                         -0.036552   0.016922  -2.160  0.03085 *  
## RealmsOriental;Palearctic                              -0.078334   0.262158  -0.299  0.76511    
## RealmsOriental;Palearctic;Saharo-Arabian;Sino-Japanese -0.324422   0.185607  -1.748  0.08058 .  
## RealmsOriental;Palearctic;Sino-Japanese                -0.291716   0.185607  -1.572  0.11612    
## RealmsOriental;Saharo-Arabian                          -0.173375   0.151737  -1.143  0.25329    
## RealmsOriental;Saharo-Arabian;Sino-Japanese            -0.093248   0.107696  -0.866  0.38664    
## RealmsOriental;Sino-Japanese                           -0.023547   0.041602  -0.566  0.57143    
## RealmsPalearctic                                        0.015082   0.036532   0.413  0.67974    
## RealmsPalearctic;Saharo-Arabian                        -0.078158   0.099831  -0.783  0.43374    
## RealmsPalearctic;Sino-Japanese                         -0.284699   0.088260  -3.226  0.00127 ** 
## RealmsPanamanian                                        0.009542   0.017585   0.543  0.58742    
## RealmsSaharo-Arabian                                   -0.083616   0.073798  -1.133  0.25728    
## RealmsSino-Japanese                                     0.032322   0.035034   0.923  0.35629    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2618 on 3231 degrees of freedom
## Multiple R-squared:  0.01454,	Adjusted R-squared:  0.006914 
## F-statistic: 1.907 on 25 and 3231 DF,  p-value: 0.004292
```
- much smaller p-value for category (close to 0) than classname (0.06501)
- very high p-value for realm


```r
# Round negative values to zero (these are the ones that are < 1 km from the threshold)
nes_difference$proximity[nes_difference$proximity < 0] <- 0

# Plot the distribution of standardised differences
ggplot(nes_difference, 
       aes(x = proximity, fill = className)) +
  geom_histogram() +
  facet_wrap(~ Category) + 
    labs(title = "Proximity to Downlisting by Category and Class")
```

<img src="EOO_no_exit_iucn_files/figure-html/nes-proximity-1.png" style="display: block; margin: auto;" />

```r
# GLM with Poisson because relative diff is actually a proportion
glm_diff <- glm(proximity ~ className + Realms + AOHkm, 
                data = nes_difference,
                family = "binomial") # ignore the non-integer warnings

# Model inference
summary(glm_diff)
```

```
## 
## Call:
## glm(formula = proximity ~ className + Realms + AOHkm, family = "binomial", 
##     data = nes_difference)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9613  -0.5064  -0.2268   0.2556   1.9249  
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.433e+00  1.346e-01 -10.648   <2e-16 ***
## classNameaves           7.939e-02  1.152e-01   0.689    0.491    
## classNamemagnoliopsida  1.789e-01  3.341e-01   0.536    0.592    
## classNamemammalia       1.516e-01  1.166e-01   1.300    0.193    
## classNamereptilia       1.125e-01  3.323e-01   0.339    0.735    
## RealmsAustralian       -2.086e-01  2.924e-01  -0.714    0.475    
## RealmsMadagascan       -7.054e-02  1.864e-01  -0.378    0.705    
## RealmsNearctic          6.649e-02  2.292e-01   0.290    0.772    
## RealmsNeartic          -2.144e+00  2.338e+00  -0.917    0.359    
## RealmsNeotropical       1.272e-01  1.535e-01   0.828    0.408    
## RealmsOceanian          7.960e-01  8.553e-01   0.931    0.352    
## RealmsOceanina         -2.426e-02  2.191e-01  -0.111    0.912    
## RealmsOriental          7.642e-02  1.545e-01   0.495    0.621    
## RealmsPalearctic       -1.917e-01  3.445e-01  -0.556    0.578    
## RealmsPanamanian        8.175e-02  1.658e-01   0.493    0.622    
## RealmsSaharo-Arabian    1.487e-01  6.347e-01   0.234    0.815    
## RealmsSino-Japanese    -2.817e-01  3.393e-01  -0.830    0.406    
## AOHkm                   1.352e-04  1.242e-05  10.888   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1125.63  on 2989  degrees of freedom
## Residual deviance:  970.24  on 2972  degrees of freedom
## AIC: 2786.4
## 
## Number of Fisher Scoring iterations: 5
```

```r
anova(glm_diff, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: proximity
## 
## Terms added sequentially (first to last)
## 
## 
##           Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
## NULL                       2989    1125.63              
## className  4   20.095      2985    1105.53 0.0004783 ***
## Realms    12    8.218      2973    1097.32 0.7678357    
## AOHkm      1  127.074      2972     970.24 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Drop class
class_results <- anova(update(glm_diff, ~.- className), glm_diff, test = "LRT")
# Drop realm
realm_results <- anova(update(glm_diff, ~.- Realms), glm_diff, test = "LRT")
# Drop AOH
aoh_results <- anova(update(glm_diff, ~.- AOHkm), glm_diff, test = "LRT")

### Only absolute AOH is a strong predictor or proximity to threshold

#############################
# Plot model-predicted values
#############################

# Make up some new data
newdat <- expand.grid(
  # All levels of taxonomic class
  className = unique(glm_diff$data$className),
  # All levels of realm
  Realms = unique(glm_diff$data$Realms),
  # Sequence of 100 made up values for AOH, bounded by observed data
  AOHkm = seq(min(glm_diff$data$AOHkm),
              max(glm_diff$data$AOHkm),
              length.out = 100))

# Use model to predict distance values
predvals <- predict(glm_diff, newdata = newdat, type = "link", se.fit = TRUE)

# Function to calculate inverse logit
inv.logit <- function(x) exp(x)/(1 + exp(x))

# Function to calculate critical significance value
ci.critical <- 
  function(siglevel) qnorm((100 - siglevel) / 100 / 2, lower.tail = FALSE)
ci_const = ci.critical(95)

# Add vals to newdat dataframe
newdat <- 
  newdat %>% 
  mutate(proximity = predvals$fit,
         se = predvals$se.fit,
         CI_lo = proximity - (ci_const * se),
         CI_hi = proximity + (ci_const * se)) 
# Back transform
newdat[,c("proximity", "CI_lo", "CI_hi")] <- 
  inv.logit(newdat[,c("proximity", "CI_lo", "CI_hi")])

# Plot model-predicted values
ggplot(newdat, 
       aes(x = AOHkm, y = proximity)) +
  # # Add confidence intervals
  # geom_ribbon(aes(ymin = CI_lo,
  #                 ymax = CI_hi,
  #                 fill = realm),
  #             alpha = 0.3) +
  # Add fitted line
  geom_line(aes(colour = Realms)) +
  # Facet by taxonomic class
  facet_grid(~ className) +
  theme_classic()
```

<img src="EOO_no_exit_iucn_files/figure-html/nes-proximity-2.png" style="display: block; margin: auto;" />

```r
# Since class and realm aren't relevant, we can also just pick a reference level
# or average across everything, and plot one line only
newdat_avg <- 
  group_by(newdat, AOHkm) %>% 
  summarise(proximity = median(proximity),
            CI_lo = median(CI_lo),
            CI_hi = median(CI_hi))
ggplot(newdat_avg, 
       aes(x = AOHkm, y = proximity)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = CI_lo,
                  ymax = CI_hi),
              alpha = 0.1) +
  # Add fitted line
  geom_line() +
  theme_classic()
```

<img src="EOO_no_exit_iucn_files/figure-html/nes-proximity-3.png" style="display: block; margin: auto;" />

```r
# Proximity to threshold increases as absolute AOH increases
```

### Correlation between className and Category

```r
ggplot(eoo_nes_df, aes(className, Category)) + 
  geom_count()
```

<img src="EOO_no_exit_iucn_files/figure-html/class-category-corr-1.png" style="display: block; margin: auto;" />

```r
class_cat <- table(eoo_nes_df$className, eoo_nes_df$Category) 

chisq <- chisq.test(class_cat)
chisq
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  class_cat
## X-squared = 237.99, df = 8, p-value < 2.2e-16
```
p-value is very small (< 2.2e-16), therefore there is a strong correlation between Category and class


# Spatial Data
Save data frame for spatial analysis


```r
nes_top25 <- 
  eoo_all_df %>%
  rename(scientificName = species) %>%
  mutate(
    # Match species name to how it appears in occurrence dataframe
    scientificName = gsub(" ", "_", scientificName)) 
```


```r
# Summarize NES by realm
realm_nes <-
  nes_top25 %>%
  filter(!(is.na(Realms))) %>% 
  # Count species in each realm
  group_by(Realms, className) %>% 
  summarise(n = n(),
            n_NES = length(which(EOO_NES)),
            n_nonNES = length(which(!(EOO_NES))),
            prop_NES = n_NES / n)
```


```r
# Inspect the loaded objects - what might we need in the manuscript?
ls()
```

```
##  [1] "aoh_results"        "cat_change"         "change_dat"         "chisq"              "ci_const"           "ci.critical"        "class_cat"          "class_results"      "crit_realms"        "decrease"           "diff_bottom25"     
## [12] "diff_top25"         "endan_realms"       "eoo_all_df"         "eoo_nes_df"         "glm_diff"           "glm_dropCat_nes"    "glm_dropClass_nes"  "glm_dropInter1_nes" "glm_dropInter2_nes" "glm_dropInter3_nes" "glm_dropInter4_nes"
## [23] "glm_dropInter5_nes" "glm_dropInter6_nes" "glm_dropRealm_nes"  "glm_full_nes"       "increase"           "inv.logit"          "lm_prop"            "lm_prop_cat"        "lm_prop_realm"      "merge_dat"          "nes_difference"    
## [34] "nes_top25"          "newdat"             "newdat_avg"         "nochange"           "predvals"           "quants"             "realm_cats"         "realm_nes"          "realm_results"      "realms_long_all"    "realms_long_nes"   
## [45] "top_realm"          "vul_realms"
```

```r
save(eoo_all_df,eoo_nes_df,realms_long_all,realms_long_nes,
     glm_diff,class_results,realm_results,aoh_results,newdat_avg,
     file = "HMEI-GDrive/data/sorted/eoo_msdata.Rdata")
```

```
## Error in gzfile(file, "wb"): cannot open the connection
```



# EDGE species analysis
load files

```r
edge_amph <- read.csv('data/EDGE_data/EDGE_amphibia.csv')
edge_aves <- read.csv('data/EDGE_data/EDGE_aves.csv')
edge_mamm <- read.csv('data/EDGE_data/EDGE_mammalia.csv')
edge_rept <- read.csv('data/EDGE_data/EDGE_reptilia.csv')

# keep only top 100 in each
edge_amph <- edge_amph %>% filter(EDGE.Rank <= 100)
edge_aves <- edge_aves %>% filter(EDGE.Rank <= 100)
edge_mamm <- edge_mamm %>% filter(EDGE.Rank <= 100)
edge_rept <- edge_rept %>% filter(EDGE.Rank <= 100)
```

compare edge to nes

```r
amph <- inner_join(eoo_nes_df, edge_amph, by=c('species'='Species'))
aves <- inner_join(eoo_nes_df, edge_aves, by=c('species'='Species'))
mamm <- inner_join(eoo_nes_df, edge_mamm, by=c('species'='Species'))
rept <- inner_join(eoo_nes_df, edge_rept, by=c('species'='Species'))
```

There are 43 amphibians, 39 birds, 44 mammals, and 4 reptile NES by EOO that are also EDGE species. 
