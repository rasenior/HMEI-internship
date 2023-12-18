---
title: "Analysis of Data with EOO Thresholds" 
author: "Sophia Richter"
date: "18 December, 2023"
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
  markdown: 
    wrap: 72
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


# Load packages


```r
library(readr)
library(kableExtra)
library(ggplot2)
library(stringr)
library(ggtext)
library(png)
library(grid)
library(RColorBrewer)
library(Hmisc)
library(tidyverse)

# Define function to capitalise first letter
capitalise <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}
```


```r
# Load data
load("data/HMEI-GDrive/data/sorted/eoo_msdata.Rdata") # eoo objects
load("data/HMEI-GDrive/data/sorted/msdata.Rdata") # aoo objects
```


```r
# Function to make significance levels pretty
siglevel <- function(pval){
  ifelse(pval < 0.001, "P < 0.001",
         ifelse(pval < 0.05, 
                paste("P = ", signif(pval, 2), sep = ""),
                "P > 0.05 ")
         )
}
```

Analysis To-Do:

X   recreate eoo_nes_df with EOO thresholds (find document where i make
    that) from eoo_all_df
X   calculate distance from threshold
X   add column: those species whose existing AOH are furthest (top 25%)
    from the nearest downlisting threshold as eoo_nes_df\$diff_top25
X   calculate percent: signif(100\*(nrow(eoo_nes_df) /
    nrow(eoo_all_df)),2)
-   find country distribution of EOO thresholds
X   run glm to test whether relative difference from downlisting
    threshold varied by taxonomic class, zoogeographic realm and
    absolute AOH, limited to species that occur in a single realm
X   find and run class_results, realm_results, aoh_results (i think this
    is the glm)

Figures:

X   figure 1: eoo_nes_df

X   figure 2: eoo_nes_df, assess whether species are closer downlisting
    or uplisting

Mapping:

-   top countries with nes and extreme nes and by taxonomic class, top
    realms with nes and enes
-   " " contains the majority of No Exit plants, while No Exit reptiles
    included in our analyses are largely found in " "
-   create map_country_combi.png with EOO thresholds

| Col1 | Critically Endangered | Endangered | Vulnerable |
|------|-----------------------|------------|------------|
| AOO  | 10                    | 500        | 2,000      |
| EOO  | 100                   | 5,000      | 20,000     |

EDGE Analysis

X   " " amphibian, " " bird, " " mammal, and " " reptile No Exit Species
    included in the top 100 EDGE species for their taxa

# Running no_exit_iucn.Rmd

Making a copy of no_exit_iucn.Rmd but with EOO thresholds (titled
eoo_no_excit_iucn.Rmd). Should cover majority of analyses needed (except
perhaps mapping)

-   need to upload jung2015_RL to data
-   question: is iucn_dat.csv different from iucn_dat_corrected.csv ? I
    only have iucn_dat.csv so I used that
-   note: with higher thresholds, now 3257 species are NES and only 612
    are not!

# Difference between AOO and EOO Thresholds

-   Create a dataframe with both AOO_NES and EOO_NES and compare
    differences (e.g. which species are different)
    


```r
# combine nes_df and eoo_nes_df
aoo_eoo_full <- full_join(all_df, eoo_all_df, by = "species")

# remove species where Category and EOO_Category match by adding column to see if they are equal and then filtering by that column
aoo_eoo_diff <- aoo_eoo_full %>% 
    select(species, className = className.x, areaAOH = areaAOH.x, Realms = Realms.x, countries = countries.x, redlistCategory = redlistCategory.x, realm_endemic = realm_endemic.x, country_endemic = country_endemic.x, AOHkm = AOHkm.y, NES, AOO_Category = Category, aoo_difference = Difference.x, aoo_num_category = num_Category.x, aoo_num_redlist = num_redlist.x, aoo_cat_red_change = cat_red_change.x, aoo_diff_prop = difference_prop.x, aoo_prox = proximity.x, EOO_NES, EOO_Category, eoo_difference = Difference.y, eoo_numcategory = num_Category.y, eoo_catredchange = cat_red_change.y, eoo_diffprop = difference_prop.y, eoo_prox = proximity.y) %>%
    mutate(same_cat = ifelse(AOO_Category == EOO_Category, "Same", "Different")) %>% 
    mutate(aoo_eoo_cat_change = aoo_num_category - eoo_numcategory)  %>% # calc change in category
    mutate(aoo_match = ifelse(AOO_Category == redlistCategory, "AOO Match", "AOO Mismatch")) %>% 
    mutate(eoo_match = ifelse(EOO_Category == redlistCategory, "EOO Match", "EOO Mismatch"))  %>% # if threshold matches actual red list
    mutate(cat_diff = ifelse(AOO_Category == "Endangered" & EOO_Category == "Critically Endangered", "Critical to Endangered", ifelse(AOO_Category == "Vulnerable" & EOO_Category == "Critically Endangered", "Critical to Vulnerable", ifelse(AOO_Category == "Vulnerable" & EOO_Category == "Endangered", "Endangered to Vulnerable", ifelse(AOO_Category == "Near Threatened" & EOO_Category == "Vulnerable", "Vulnerable to Near Threatened", ifelse(AOO_Category == "Near Threatened" & EOO_Category == "Endangered", "Endangered to Near Threatened","No Change"))))))

# calculate change in category between AOO_Category and EOO_Category
# create summary table of category change
aoo_eoo_diff %>% 
    group_by(aoo_eoo_cat_change) %>% 
    summarize(sum = n())
```

```
## # A tibble: 3 × 2
##   aoo_eoo_cat_change   sum
##                <dbl> <int>
## 1                  0  1699
## 2                  1  1784
## 3                  2   386
```

```r
aoo_eoo_diff %>% 
    group_by(className, aoo_eoo_cat_change) %>% 
    summarize(sum = n())
```

```
## # A tibble: 15 × 3
## # Groups:   className [5]
##    className     aoo_eoo_cat_change   sum
##    <chr>                      <dbl> <int>
##  1 amphibia                       0   647
##  2 amphibia                       1   835
##  3 amphibia                       2   141
##  4 aves                           0   490
##  5 aves                           1   435
##  6 aves                           2   112
##  7 magnoliopsida                  0    20
##  8 magnoliopsida                  1    45
##  9 magnoliopsida                  2     3
## 10 mammalia                       0   517
## 11 mammalia                       1   435
## 12 mammalia                       2   127
## 13 reptilia                       0    25
## 14 reptilia                       1    34
## 15 reptilia                       2     3
```

```r
# change from EOO to AOO
aoo_eoo_diff %>% 
    group_by(cat_diff) %>% 
    summarize(sum = n())
```

```
## # A tibble: 5 × 2
##   cat_diff                        sum
##   <chr>                         <int>
## 1 Critical to Endangered          681
## 2 Endangered to Near Threatened   386
## 3 Endangered to Vulnerable        720
## 4 No Change                      1699
## 5 Vulnerable to Near Threatened   383
```

```r
aoo_eoo_diff %>% 
    group_by(className, cat_diff) %>% 
    summarize(sum = n())
```

```
## # A tibble: 25 × 3
## # Groups:   className [5]
##    className cat_diff                        sum
##    <chr>     <chr>                         <int>
##  1 amphibia  Critical to Endangered          420
##  2 amphibia  Endangered to Near Threatened   141
##  3 amphibia  Endangered to Vulnerable        343
##  4 amphibia  No Change                       647
##  5 amphibia  Vulnerable to Near Threatened    72
##  6 aves      Critical to Endangered          113
##  7 aves      Endangered to Near Threatened   112
##  8 aves      Endangered to Vulnerable        185
##  9 aves      No Change                       490
## 10 aves      Vulnerable to Near Threatened   137
## # ℹ 15 more rows
```

```r
# look at differences between categorization by class
ggplot(aoo_eoo_diff, aes(x = className, fill = as.character(aoo_eoo_cat_change))) +
    geom_bar()
```

<img src="EOO_Analysis_files/figure-html/diff-df-1.png" style="display: block; margin: auto;" />

```r
# by realm
ggplot(aoo_eoo_diff, aes(x = Realms, fill = as.character(aoo_eoo_cat_change))) +
    geom_bar() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

<img src="EOO_Analysis_files/figure-html/diff-df-2.png" style="display: block; margin: auto;" />

```r
# listings that match AOO but not EOO
ggplot(aoo_eoo_diff %>% filter(aoo_match == "AOO Match" & same_cat == "Different"), 
       aes(x = AOO_Category)) +
    geom_bar() 
```

<img src="EOO_Analysis_files/figure-html/diff-df-3.png" style="display: block; margin: auto;" />

```r
ggplot(aoo_eoo_diff %>% filter(eoo_match == "EOO Match" & same_cat == "Different"), 
       aes(x = EOO_Category)) +
    geom_bar()
```

<img src="EOO_Analysis_files/figure-html/diff-df-4.png" style="display: block; margin: auto;" />

2170 species are categorized differently between AOO and EOO thresholds. 1784 species are uplisted once by using EOO thresholds and 386 species are uplisted twice by using EOO thresholds as opposed to AOO thresholds. 

How many species red listings match AOO habitat and how many match EOO habitat? 
1313 species match red list category with AOO. 
1449 species match red list category with EOO. 

Of those that have different AOO/EOO categories? 
711 species match red list category by AOO of those that do not share the same threshold for EOO and AOO. 
847 species match red list category by EOO of those that do not share the same threshold for EOO and AOO. 

Of the species that have a mismatch between AOO and EOO thresholds, AOO Thresholds only accurately predict redlistings for Endangered and Vulnerable categories as opposed to EOO predicting critically endangered, endandered, and vulnerable. 

After No Change, the most common change between AOO and EOO is to move from Vulnerable to Endangered, then Endangered to Critically Endangered, then Near Threatened to Endangered, and lastly Near Threatened to Vulnerable. No species move from Vulnerable or Near Threatened to Critically Endangered by using EOO thresholds.


```r
aoo_eoo_diff %>% 
    group_by(cat_diff) %>% 
    summarize(sum = n())  %>% 
    arrange(-sum)
```

```
## # A tibble: 5 × 2
##   cat_diff                        sum
##   <chr>                         <int>
## 1 No Change                      1699
## 2 Endangered to Vulnerable        720
## 3 Critical to Endangered          681
## 4 Endangered to Near Threatened   386
## 5 Vulnerable to Near Threatened   383
```



Make map of species that have different AOO/EOO categories? 

I mapped the species that had a different classification between AOO and EOO. Here are the maps: 

```r
realm_map <- 
  readPNG("/workdir/sr2352/HMEI-internship/data/figs/diff/map_realm_combi.png") %>% 
  rasterGrob(interpolate = TRUE)
ggplot() +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_custom(realm_map,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
```

<img src="EOO_Analysis_files/figure-html/diff-maps-1.png" style="display: block; margin: auto;" />

```r
country_map <- 
  readPNG("/workdir/sr2352/HMEI-internship/data/figs/diff/map_country_combi.png") %>% 
  rasterGrob(interpolate = TRUE)
ggplot() +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_custom(country_map,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
```

<img src="EOO_Analysis_files/figure-html/diff-maps-2.png" style="display: block; margin: auto;" />

It looks like NES that are classified differently between AOO and EOO thresholsd are most commonly found in areas around the equator. Follows similar pattern to all NES. 

Top realms: Neotropical, Orential, Panamanian
Top countries: Colombia, Indonesia, Madagascar

# Manuscript Results Section Re-written:

Of the 3869 threatened species with AOH data,
3257
(84%) are identifiable
as No Exit Species: species that have a total AOH of \< 20000 km^2^
(Fig. \@ref(fig:fig-1)). The remaining
612 threatened species with AOH \>
20000 km^2^ are, therefore, not considered to be limited by available
habitat quantity, and instead they are likely to be listed as threatened
because of their small or declining population sizes.

Most No Exit Species are amphibians
(1602;
49%),
followed by birds (776;
24%),
mammals (752;
23%),
plants in assessed families of Magnoliopsida
(67;
2.1%),
and, finally, species in assessed families of reptiles
(60;
1.8%).
Based on the Red List thresholds for listing under criterion B2,
1016 species in our dataset have a
total AOH that falls below the 100 km^2^ threshold for Critically
Endangered, 1858 species have
more than 100 km^2^ of habitat but less than 5000 km^2^ (Endangered),
and 383 species have between
5000-20000 km^2^ of habitat (Vulnerable).

Extreme No Exit Species are those species whose existing AOH are
furthest (top 25%) from the nearest downlisting threshold. We identified
815 such species, comprising
451
amphibians, 181
birds, 150
mammals,
19
reptiles, and
14
plants.


```r
nes <- eoo_nes_df %>% 
  mutate(EOO_Category = str_replace(EOO_Category, "Critically Endangered", 
                                paste("(a) AOH < 100 km","\U00B2",sep= ""))) %>% 
  mutate(EOO_Category = str_replace(EOO_Category, "Endangered", 
                                paste("(b) 100 < AOH < 5000 km","\U00B2",sep= ""))) %>% 
  mutate(EOO_Category = str_replace(EOO_Category, "Vulnerable", 
                                paste("(c) 5000 < AOH < 20000 km","\U00B2",sep= "")))

# relevel Categories
nes$EOO_Category <- 
  factor(nes$EOO_Category, 
         levels = c(paste("(a) AOH < 100 km","\U00B2",sep= ""), 
                    paste("(b) 100 < AOH < 5000 km","\U00B2",sep= ""), 
                    paste("(c) 5000 < AOH < 20000 km","\U00B2",sep= "")))
nes$className <- 
  factor(nes$className, 
         levels = unique(nes$className),
         labels = capitalize(unique(nes$className)))

# Code to add pics to labels
classes <- levels(nes$className)
labels <- paste(
  "<img src=", 
  "'",
  file.path("data/HMEI-GDrive/resources/phylopic", paste(tolower(classes), 'png', sep = ".")),
  "' ",
  "width='10' />", 
  classes,
  sep = ""
)
names(labels) <- classes

max_val <- 834
ggplot(nes, aes(x=className, fill=className)) + 
    geom_bar(alpha = 0.7) + 
    facet_wrap(~EOO_Category, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
    theme_bw() + theme(panel.grid = element_blank()) +
    labs(fill="Class") +
    ylab("Count") + 
    scale_fill_brewer(palette="Set1") +
    scale_y_continuous(breaks = seq(0, max_val, by = 100), limits=c(0, max_val)) +
    theme(axis.text.x = element_markdown(color = "black",
                                         angle = 45, 
                                         hjust = 1, vjust = 1,
                                         margin = unit(c(-5, 0, 0, 0), "pt")),
          axis.title.x = element_blank(),
          legend.position = "none") +
    scale_x_discrete(labels = labels)
```

<div class="figure" style="text-align: center">
<img src="EOO_Analysis_files/figure-html/fig-1-1.png" alt="The total number of threatened species classified as 'No Exit Species' (NES) by EOO, within each taxonomic class and AOH range. AOH ranges correspond to the threshold values used by the Red List to classify species under, criterion B2, as Critically Endangered (a), Endangered (b), or Vulnerable (c)."  />
<p class="caption">The total number of threatened species classified as 'No Exit Species' (NES) by EOO, within each taxonomic class and AOH range. AOH ranges correspond to the threshold values used by the Red List to classify species under, criterion B2, as Critically Endangered (a), Endangered (b), or Vulnerable (c).</p>
</div>

Considering the difference between species' AOH and the nearest
downlisting threshold highlights those threatened species that are
particularly near or far from being able to be downlisted to the next
lowest threat category - in other words, the extent to which a species'
potential for future downlisting is contingent on habitat restoration or
creation. For No Exit Species, there are three possible ranges of AOH
values, bounded by the different Red List thresholds of criterion B2:
AOH \< 100 km^2^; 100 \< AOH ≤ 5000 km^2^; and 5000 \< AOH ≤ 20000
km^2^. In each range and across different taxonomic classes, the AOH of
most species falls closer to the threshold for uplisting (maximum
difference) than the threshold for downlisting (minimum difference, i.e.
0 km^2^). This can be seen in the left skewed distribution of
differences between AOH and the nearest downlisting threshold (Fig.
\@ref(fig:fig-2)).

We found no indication that species' difference from the nearest
downlisting threshold differs between taxonomic classes (Deviance =
1.89, DF = 4,
P > 0.05 ) or biogeographic realms (Deviance
= 7.02, DF = 12,
P > 0.05 ). Species' with greater absolute
AOH are relatively closer to the threshold for downlisting, and thus
require relatively less additional habitat area to permit future
downlisting (Deviance = 127, DF =
1, P < 0.001), although
this value may still be large in absolute terms.


```r
ggplot(eoo_nes_df, aes(x=Difference, fill=className)) +
  geom_density(alpha = 0.3, colour = "transparent") +
  facet_wrap(~ EOO_Category, scales = "free", 
             labeller = label_wrap_gen(width = 15, multi_line = TRUE)) + 
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(fill="Class") + 
  ylab("Density") +
  xlab(paste("Difference between EOO-AOH and downlisting threshold (km","\U00B2",")",sep= "")) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_brewer(palette="Set1") 
```

<div class="figure" style="text-align: center">
<img src="EOO_Analysis_files/figure-html/fig-2-1.png" alt="Distribution of the difference between species' absolute AOH and the nearest threshold for downlisting by EOO. Curves are shaded by taxonomic class and are derived from a Gaussian kernel density estimate. This alternative to a histogram is used to display the distribution of continuous data, based on the probability of observing data at each location along the x axis."  />
<p class="caption">Distribution of the difference between species' absolute AOH and the nearest threshold for downlisting by EOO. Curves are shaded by taxonomic class and are derived from a Gaussian kernel density estimate. This alternative to a histogram is used to display the distribution of continuous data, based on the probability of observing data at each location along the x axis.</p>
</div>



Pull this information from final mapping dataframes: country_total and realm_total (added section to EOO_crs_maps_v2.Rmd)

By mapping total No Exit Species by country (Fig. 3), we find that a few
countries harbor a disproportionate share of all species whose recovery
on the Red List is limited by habitat area. Across all taxa, the three
countries with the most No Exit Species are Colombia, Madagascar, and Indonesia. 
These countries also contain
the highest number of Extreme No Exit Species - the species with AOH
values furthest, in relative terms, from the nearest threshold for
downlisting. Comparing among zoogeographic realms, the three realms with
the most No Exit Species are Neotropical (506), Oriental (472), and Panamanian (375). 
These are also the three realms with the most Extreme No Exit
Species.

Breaking down our geographic results by taxonomic class, No Exit
amphibians are largely found in Colombia, Ecuador, Mexico, Madagascar, and China, 
while No Exit birds are in Colombia, Indonesia, Brazil, Peru, and Ecuador, 
and No Exit mammals are in Indonesia, Madagascar, and India. Not all threatened 
magnoliopsid and reptile groups were
able to be considered in these analyses, because they had not been
comprehensively assessed on the Red List at the time of downloading
these data (14^th^ July 2020). For assessed groups, China contains the
majority of No Exit plants, while No Exit reptiles included in our
analyses are largely found in Madagascar.

(ref:cap-3) The total number of threatened species within each country
classified as (a) 'No Exit Species' (NES) and (b) 'Extreme No Exit
Species' (ENES), which are those species in the upper quartile of
differences between AOH and the nearest threshold for downlisting.
Countries are shaded from low count (yellow) to high (red).


```r
map <- 
  readPNG("/workdir/sr2352/HMEI-internship/data/figs/map_country_combi.png") %>% 
  rasterGrob(interpolate = TRUE)
ggplot() +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_custom(map,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
```

<div class="figure" style="text-align: center">
<img src="EOO_Analysis_files/figure-html/fig-3-1.png" alt="(ref:cap-3)"  />
<p class="caption">(ref:cap-3)</p>
</div>
