---
title: "BMIN503/EPID600 Final Project"
author: "Killian Rohn"
output: 
  html_document:
    keep_md: true
    toc: false 
    depth: 3 
    theme: paper 
    highlight: tango
---

```
## [1] 56000
```
***
### Overview

I want to look at if/how marital status impacts survival across different genders and cancer  sites. I know in general epi literature, there is a tendency to see that marriage typically is a stronger predictor of survival in men than in women depending on the disease.I plan to further stratify by race.


### Introduction 

It has been found across social epidemiological studies that marital status can be a predictor of survival outcomes for various diseases. Sometimes it is found that there is a  stronger association between being married and longer survival for men than for women. I  intend to investigate whether or not this pattern holds true for those diagnosed with various forms of cancer in the US and Puerto Rico, utilizing the SEER dataset. I would also like to further explore the effects of race on these potential associations. Epidemiology itself is an interdisciplinary field, and this project will also utilize the fields of social epidemiology, oncology, and biostatistics.

### Methods

I am using the SEER database below (over 8 million observations so far, so I can't upload it):
Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) SEER*Stat Database: Incidence - SEER 18 Regs Research Data + Hurricane Katrina Impacted Louisiana Cases, Nov 2018 Sub (1975-2016 varying) - Linked To County Attributes - Total U.S., 1969-2017 Counties,National Cancer Institute, DCCPS, Surveillance Research Program, released April 2019, based on the November 2018 submission. 

```r
library(tidyverse)
#load raw dataset
seer <- read.csv("C:/Users/kmroh/Documents/BMI Cert/Fall 2020/BMIN503/dataset1.csv", header = T)
#view column names and rename variables of interest
colnames(seer)
```

```
##  [1] "Patient.ID"                               "Sex"                                      "Race.recode..White..Black..Other."        "Year.of.birth"                            "Year.of.diagnosis"                        "Marital.status.at.diagnosis"              "Survival.months"                          "COD.to.site.recode"                       "SEER.cause.specific.death.classification"
## [10] "Vital.status.recode..study.cutoff.used."  "Primary.Site...labeled"
```

```r
seer <- seer %>%
  rename(patid = Patient.ID, 
         sex = Sex, 
         year.dx = Year.of.diagnosis,
         mstatus = Marital.status.at.diagnosis,
         race = Race.recode..White..Black..Other.,
         surv.months = Survival.months,
         cod.site = COD.to.site.recode,
         cod = SEER.cause.specific.death.classification,
         vital.status = Vital.status.recode..study.cutoff.used.,
         yob = Year.of.birth,
         prim.site = Primary.Site...labeled) %>%
  select(-year.dx,-yob)
seer$surv.months <- as.integer(seer$surv.months)
#see possible marital statuses
table(seer$mstatus)
```

```
## 
##                       Divorced Married (including common law)                      Separated         Single (never married)                        Unknown  Unmarried or Domestic Partner                        Widowed 
##                         775276                        5171073                         105821                        1271327                         636193                           8124                        1449457
```

```r
#remove entries which are not married, partnered, or single at time of diagnosis
seer <- seer %>%
  filter(mstatus == "Married (including common law)" |
         mstatus == "Unmarried or Domestic Partner" |
         mstatus == "Single (never married)")
#recode to those who are partnered or not at the time of diagnosis
seer$part <- ifelse(seer$mstatus != "Single (never married)",
                    1,
                    0)
seer$part <- factor(seer$part, labels = c("Single","Partnered"))
#recode race variable
table(seer$race)
```

```
## 
##                                                     Black Other (American Indian/AK Native, Asian/Pacific Islander)                                                   Unknown                                                     White 
##                                                    601671                                                    449192                                                     30447                                                   5369214
```

```r
seer$race <- ifelse(seer$race == "Other (American Indian/AK Native, Asian/Pacific Islander)",
                    "Other", ifelse(seer$race == "White",
                                    "White", ifelse(seer$race == "Black",
                                                    "Black", ifelse(seer$race == "Unknown",
                                                                    "Unknown", 0))))
#remove those of unknown race 
seer <- seer %>%
  filter(race != "Unknown")
#remove those whose cause of death is unknown, or whose tumor is not the first
table(seer$cod)
```

```
## 
##          Alive or dead of other cause Dead (attributable to this cancer dx)            Dead (missing/unknown COD)                   N/A not first tumor 
##                               3471983                               1910445                                 42447                                995202
```

```r
seer <- seer %>%
  filter(cod == "Alive or dead of other cause Dead" |
         cod == "Dead (attributable to this cancer dx)")
#remove observations with unknown primary site and those that were alive at the study close
seer <- seer %>%
  filter(prim.site != "C80.9-Unknown primary site" |
           cod != "Alive")
#per classmate's feedback I'm setting a cap for survival months at 120 (10 years)
seer$surv.months <- ifelse(seer$surv.months >= 120, 120, seer$surv.months)
#create group average of survival month by part variable
seer <- seer %>%
  group_by(part) %>%
  mutate(avg.pstatus = mean(surv.months, na.rm = T))

#create group average of survival month by sex
seer <- seer %>%
  ungroup() %>%
  group_by(sex) %>%
  mutate(avg.sex = mean(surv.months, na.rm = T))

#create group average of survival month by mar and sex variables
seer <- seer %>%
  ungroup() %>%
  group_by(part,sex) %>%
  mutate(avg.part.sex = mean(surv.months, na.rm = T))

#recode primary site variable to body category
seer$c.code <- str_extract(seer$prim.site, "C[0-9][0-9]")
seer$c.code <- as.numeric(str_extract(seer$c.code, "[0-9][0-9]"))
seer$group <- ifelse(seer$c.code <= 14, "Lip, Oral Cavity and Pharynx",
 ifelse(seer$c.code <= 26,  "Digestive Organs",
 ifelse(seer$c.code <= 30,  NA,
 ifelse(seer$c.code <= 39,  "Respiratory and Intrathoracic Organs",
 ifelse(seer$c.code <= 41,  "Bone and Articular Cartilage",
 ifelse(seer$c.code == 42,  NA,
 ifelse(seer$c.code <= 44,  "Skin",
 ifelse(seer$c.code <= 49,  "Mesothelial and Soft Tissue",
 ifelse(seer$c.code == 50,  "Breast",
 ifelse(seer$c.code <= 58,  "Genital Organs",
 ifelse(seer$c.code == 59,  NA,
 ifelse(seer$c.code <= 63,  "Genital Organs",
 ifelse(seer$c.code <= 68,  "Urinary Tract",
 ifelse(seer$c.code <= 72,  "Eye, Brain or Central Nervous System",
 ifelse(seer$c.code <= 75,  "Thyroid & Endocrine Glands",
 ifelse(seer$c.code <= 80,  "Ill-Defined Sites",
 ifelse(seer$c.code <= 96,  "Lymphoid, Hematopoietic and Related Tissue", NA)))))))))))))))))
seer <- seer %>%
  ungroup() %>%
  group_by(part,sex,race) %>%
  mutate(avg.race = mean(surv.months, na.rm = T)) %>%
  ungroup() %>%
  group_by(part,sex,group) %>%
  mutate(avg.grp = mean(surv.months, na.rm = T)) %>%
  na.omit()

#create groups for pairwise ttests and anovas (there was probably a more elegant way to do this but this worked)
seer$psgroup[seer$sex == "Female" & seer$part == "Partnered"] <- "Part.Fem"
seer$psgroup[seer$sex == "Female" & seer$part == "Single"] <- "Sing.Fem"
seer$psgroup[seer$sex == "Male" & seer$part == "Partnered"] <- "Part.Male"
seer$psgroup[seer$sex == "Male" & seer$part == "Single"] <- "Sing.Male"
table(seer$psgroup)
```

```
## 
##  Part.Fem Part.Male  Sing.Fem Sing.Male 
##    529872    872233    160083    209728
```

```r
seer$psrgroup[seer$sex == "Female" & seer$part == "Single" & seer$race == "White"] <- "White.Sing.Fem"
seer$psrgroup[seer$sex == "Female" & seer$part == "Single" & seer$race == "Black"] <- "Black.Sing.Fem"
seer$psrgroup[seer$sex == "Female" & seer$part == "Single" & seer$race == "Other"] <- "Oth.Sing.Fem"
seer$psrgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$race == "White"] <- "White.Part.Fem"
seer$psrgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$race == "Black"] <- "Black.Part.Fem"
seer$psrgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$race == "Other"] <- "Oth.Part.Fem"
seer$psrgroup[seer$sex == "Male" & seer$part == "Single" & seer$race == "White"] <- "White.Sing.Male"
seer$psrgroup[seer$sex == "Male" & seer$part == "Single" & seer$race == "Black"] <- "Black.Sing.Male"
seer$psrgroup[seer$sex == "Male" & seer$part == "Single" & seer$race == "Other"] <- "Oth.Sing.Male"
seer$psrgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$race == "White"] <- "White.Part.Male"
seer$psrgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$race == "Black"] <- "White.Part.Male"
seer$psrgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$race == "Other"] <- "Oth.Part.Male"
table(seer$psrgroup)
```

```
## 
##  Black.Part.Fem  Black.Sing.Fem Black.Sing.Male    Oth.Part.Fem   Oth.Part.Male    Oth.Sing.Fem   Oth.Sing.Male  White.Part.Fem White.Part.Male  White.Sing.Fem White.Sing.Male 
##           38071           36727           44788           40081           66154            9637           11778          451720          806079          113719          153162
```

```r
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Bone and Articular Cartilage"] <- "Bone.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Breast"] <- "Breast.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Digestive Organs"] <- "Dig.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Eye, Brain or Central Nervous System"] <- "Eye.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Genital Organs"] <- "Gen.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Ill-Defined Sites"] <- "Ill.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Lip, Oral Cavity and Pharynx"] <- "Lip.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Mesothelial and Soft Tissue"] <- "Meso.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Respiratory and Intrathoracic Organs"] <- "Resp.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Skin"] <- "Skin.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Thyroid & Endocrine Glands"] <- "Thy.Sing.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Single" & seer$group == "Urinary Tract"] <- "Uri.Sing.Fem"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Bone and Articular Cartilage"] <- "Bone.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Breast"] <- "Breast.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Digestive Organs"] <- "Dig.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Eye, Brain or Central Nervous System"] <- "Eye.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Genital Organs"] <- "Gen.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Ill-Defined Sites"] <- "Ill.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Lip, Oral Cavity and Pharynx"] <- "Lip.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Mesothelial and Soft Tissue"] <- "Meso.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Respiratory and Intrathoracic Organs"] <- "Resp.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Skin"] <- "Skin.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Thyroid & Endocrine Glands"] <- "Thy.Sing.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Single" & seer$group == "Urinary Tract"] <- "Uri.Sing.Male"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Bone and Articular Cartilage"] <- "Bone.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Breast"] <- "Breast.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Digestive Organs"] <- "Dig.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Eye, Brain or Central Nervous System"] <- "Eye.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Genital Organs"] <- "Gen.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Ill-Defined Sites"] <- "Ill.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Lip, Oral Cavity and Pharynx"] <- "Lip.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Mesothelial and Soft Tissue"] <- "Meso.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Respiratory and Intrathoracic Organs"] <- "Resp.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Skin"] <- "Skin.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Thyroid & Endocrine Glands"] <- "Thy.Part.Fem"
seer$psbgroup[seer$sex == "Female" & seer$part == "Partnered" & seer$group == "Urinary Tract"] <- "Uri.Part.Fem"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Bone and Articular Cartilage"] <- "Bone.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Breast"] <- "Breast.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Digestive Organs"] <- "Dig.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Eye, Brain or Central Nervous System"] <- "Eye.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Genital Organs"] <- "Eye.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Ill-Defined Sites"] <- "Ill.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Lip, Oral Cavity and Pharynx"] <- "Lip.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Mesothelial and Soft Tissue"] <- "Meso.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Respiratory and Intrathoracic Organs"] <- "Resp.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Skin"] <- "Skin.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Thyroid & Endocrine Glands"] <- "Thy.Part.Male"
seer$psbgroup[seer$sex == "Male" & seer$part == "Partnered" & seer$group == "Urinary Tract"] <- "Uri.Part.Male"
```

### Results

Overall, both female sex and being partnered are associated with prolonged survival in cancer patients with statistically significant p-values of less than 0.05 in all ANOVAs and t-tests performed. The one caveat to the statistical significance of these results is that survival is generally only prolongued by a matter of a few months; while this may be emotionally meaningful for the loved ones of cancer patients, I also think that this could also mean prolongued suffering for terminally ill patients. Further, although the cleaned dataset has ~1.7 million observations, it is possible that that means the dataset is overpowered to detect significance. There are a few exceptions to the above stated pattern when subsetting the dataset by cancer sites. Specifically, bone and articular cartiledge patients survive longer when partnered, however this tendency is not statistically significant and there is virtually no difference between sexes; skin cancer patients are statistically differentiated between groups, except for single males and partnered females; mesothial single males are not statistically differentiated from females, whether single or partnered; breast patients are significantly differentiated by sex but not by partnership status; partnered genital patients survive longer than single patients, however in this group men survive longer than women and these findings are statistically significant -- this pattern is also true for urinary tract patients; single male and female thyroid patients and ill-defined sites patients are not statistically differentiated.

```r
library(rstatix)
library(ggpubr)
library(survival)
library(survminer)

plots <- function(xvar, xname, gtitle, gavg){
  
  p <- ggplot(seer, aes(x=as.factor(xvar), y=surv.months)) +
  geom_boxplot(aes(middle = mean(surv.months))) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = gtitle, 
       x = xname, 
       y = "Survival Months") +
  stat_summary(fun.y=mean, colour="yellow") +
  geom_text(aes(label = round(gavg,2), y = gavg))
  print(p)
  #t-test for significance
  t.test(data = seer, surv.months~xvar)
  
  #kaplan-meier
  km_fit <- surv_fit(Surv(surv.months) ~ xvar, data=seer)
  km <- ggsurvplot(km_fit, data = seer, pval = T, conf.int = T)
  print(km)
  #cox proportional hazards regression
  cox <- coxph(Surv(surv.months) ~ as.vector(unlist(xvar)), data = seer)
  summary(cox)
}

#plot survival months by recoded Partnership status
plots(seer$part, 
      "Relationship Status", 
      "Survival Months by Partnership Status at Diagnosis \nAll Sexes", 
      seer$avg.pstatus)
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ as.vector(unlist(xvar)), 
##     data = seer)
## 
##   n= 1771916, number of events= 1771916 
## 
##                                   coef exp(coef) se(coef)     z Pr(>|z|)    
## as.vector(unlist(xvar))Single 0.176786  1.193375 0.001852 95.46   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##                               exp(coef) exp(-coef) lower .95 upper .95
## as.vector(unlist(xvar))Single     1.193      0.838     1.189     1.198
## 
## Concordance= 0.516  (se = 0 )
## Likelihood ratio test= 8803  on 1 df,   p=<2e-16
## Wald test            = 9113  on 1 df,   p=<2e-16
## Score (logrank) test = 9137  on 1 df,   p=<2e-16
```

```r
#plot survival months by sex
plots(seer$sex, 
      "Sex", 
      "Survival Months by Sex", 
      seer$avg.sex)
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-3.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ as.vector(unlist(xvar)), 
##     data = seer)
## 
##   n= 1771916, number of events= 1771916 
## 
##                                 coef exp(coef) se(coef)   z Pr(>|z|)    
## as.vector(unlist(xvar))Male 0.157312  1.170361 0.001542 102   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##                             exp(coef) exp(-coef) lower .95 upper .95
## as.vector(unlist(xvar))Male      1.17     0.8544     1.167     1.174
## 
## Concordance= 0.526  (se = 0 )
## Likelihood ratio test= 10516  on 1 df,   p=<2e-16
## Wald test            = 10405  on 1 df,   p=<2e-16
## Score (logrank) test = 10427  on 1 df,   p=<2e-16
```

```r
#plot survival months by relationship status and sex
  ggplot(seer, aes(x=as.factor(part), y=surv.months)) +
  geom_boxplot(aes(color = sex, middle = mean(surv.months))) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(title = "Survival Months by Partnership Status at Diagnosis \nand Sex", 
       x = "Relationship status", 
       y = "Survival Months") +
  stat_summary(fun.y=mean, colour="yellow") +
  geom_text(aes(label = round(avg.part.sex,2), y = avg.part.sex)) +
  facet_wrap(~sex)
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-5.png)<!-- -->

```r
  #paired t-test for significance
  pairwise.t.test(seer$surv.months, seer$psgroup, p.adjust.method = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  seer$surv.months and seer$psgroup 
## 
##           Part.Fem Part.Male Sing.Fem
## Part.Male < 2e-16  -         -       
## Sing.Fem  < 2e-16  1.3e-09   -       
## Sing.Male < 2e-16  < 2e-16   < 2e-16 
## 
## P value adjustment method: bonferroni
```

```r
  #kaplan-meier
  km_fit <- surv_fit(Surv(surv.months) ~ part + sex, data=seer)
  gg <- ggsurvplot(km_fit, data = seer, pval = T, conf.int = T)
  gg$plot + facet_wrap(~sex)
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-6.png)<!-- -->

```r
  #cox proportional hazards regression
  cox <- coxph(Surv(surv.months) ~ part + sex, data = seer)
  summary(cox)
```

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer)
## 
##   n= 1771916, number of events= 1771916 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)    
## partPartnered -0.187262  0.829226  0.001854 -101.0   <2e-16 ***
## sexMale        0.165014  1.179410  0.001544  106.9   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.8292     1.2059    0.8262    0.8322
## sexMale          1.1794     0.8479    1.1758    1.1830
## 
## Concordance= 0.535  (se = 0 )
## Likelihood ratio test= 20348  on 2 df,   p=<2e-16
## Wald test            = 20544  on 2 df,   p=<2e-16
## Score (logrank) test = 20588  on 2 df,   p=<2e-16
```

```r
#plot survival months by part, sex, and race
ggplot(seer, aes(x=as.factor(part), y=surv.months)) +
  geom_boxplot(aes(color = sex, middle = mean(surv.months))) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(title = "Survival Months by Partnership Status at Diagnosis, \nSex, and Race", 
       x = "Relationship status", 
       y = "Survival Months") +
  stat_summary(fun.y=mean, colour="yellow") +
  facet_grid(~ race + sex) +
  geom_text(aes(label = round(avg.race,2), y = avg.race))
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-7.png)<!-- -->

```r
#pairwise t-test and anova based on partner status, sex and race
summary(aov(surv.months ~ psrgroup, data = seer))
```

```
##                  Df    Sum Sq Mean Sq F value Pr(>F)    
## psrgroup         10 2.252e+07 2251604    2079 <2e-16 ***
## Residuals   1771905 1.919e+09    1083                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
pairwise.t.test(seer$surv.months, seer$psrgroup, p.adjust.method = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  seer$surv.months and seer$psrgroup 
## 
##                 Black.Part.Fem Black.Sing.Fem Black.Sing.Male Oth.Part.Fem Oth.Part.Male Oth.Sing.Fem Oth.Sing.Male White.Part.Fem White.Part.Male White.Sing.Fem
## Black.Sing.Fem  < 2e-16        -              -               -            -             -            -             -              -               -             
## Black.Sing.Male < 2e-16        < 2e-16        -               -            -             -            -             -              -               -             
## Oth.Part.Fem    3.5e-06        < 2e-16        < 2e-16         -            -             -            -             -              -               -             
## Oth.Part.Male   < 2e-16        1.00000        < 2e-16         < 2e-16      -             -            -             -              -               -             
## Oth.Sing.Fem    0.00017        < 2e-16        < 2e-16         2.9e-14      < 2e-16       -            -             -              -               -             
## Oth.Sing.Male   < 2e-16        < 2e-16        0.14091         < 2e-16      < 2e-16       < 2e-16      -             -              -               -             
## White.Part.Fem  < 2e-16        < 2e-16        < 2e-16         < 2e-16      < 2e-16       < 2e-16      < 2e-16       -              -               -             
## White.Part.Male < 2e-16        < 2e-16        < 2e-16         < 2e-16      < 2e-16       1.00000      < 2e-16       < 2e-16        -               -             
## White.Sing.Fem  < 2e-16        < 2e-16        < 2e-16         < 2e-16      < 2e-16       0.28486      < 2e-16       < 2e-16        1.00000         -             
## White.Sing.Male < 2e-16        < 2e-16        < 2e-16         < 2e-16      < 2e-16       < 2e-16      0.00121       < 2e-16        < 2e-16         < 2e-16       
## 
## P value adjustment method: bonferroni
```

```r
#kaplan-meier
km_fit <- survfit(Surv(surv.months) ~ part + sex + race, data=seer)
gg <- ggsurvplot(km_fit, data = seer, pval = T, conf.int = T)
gg$plot + facet_wrap(~race + sex)
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-8.png)<!-- -->

```r
#cox proportional hazards regression
cox <- coxph(Surv(surv.months) ~ part + sex + race, data = seer)
summary(cox)
```

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex + race, data = seer)
## 
##   n= 1771916, number of events= 1771916 
## 
##                    coef exp(coef)  se(coef)       z Pr(>|z|)    
## partPartnered -0.180700  0.834686  0.001884 -95.911  < 2e-16 ***
## sexMale        0.165084  1.179492  0.001545 106.884  < 2e-16 ***
## raceOther      0.014265  1.014368  0.003646   3.913 9.12e-05 ***
## raceWhite     -0.056912  0.944677  0.002477 -22.975  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.8347     1.1981    0.8316    0.8378
## sexMale          1.1795     0.8478    1.1759    1.1831
## raceOther        1.0144     0.9858    1.0071    1.0216
## raceWhite        0.9447     1.0586    0.9401    0.9493
## 
## Concordance= 0.536  (se = 0 )
## Likelihood ratio test= 21358  on 4 df,   p=<2e-16
## Wald test            = 21570  on 4 df,   p=<2e-16
## Score (logrank) test = 21615  on 4 df,   p=<2e-16
```

```r
#plot survival months by part, sex and group

plots.group <- function(gname){
p <- ggplot(subset(seer, group %in% c(gname)), aes(x=as.factor(part), y=surv.months)) +
  geom_boxplot(aes(color = sex, middle = mean(surv.months))) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(title = paste("Survival Months by Partnership Status at Diagnosis \nand Sex \nGroup: ", gname), 
       x = "Relationship status", 
       y = "Survival Months")  +
  stat_summary(fun.y=mean, colour="yellow") +
  geom_text(aes(label = round(avg.grp,2), y = avg.grp)) +
  facet_wrap(~sex)
print(p)
#pairwise t-test based on partner status, sex and cancer site
seer1 <- subset(seer, group %in% c(gname))
pairwise.t.test(seer1$surv.months, seer1$psbgroup, p.adjust.method = "bonferroni")
#kaplan-meier
  km_fit <- surv_fit(Surv(surv.months) ~ part + sex, data=seer1)
  km <- ggsurvplot(km_fit, data = seer1, pval = T, conf.int = T)
  km <- km$plot + facet_wrap(~ sex)
  print(km)
  #cox proportional hazards regression
  cox <- coxph(Surv(surv.months) ~ part + sex, data = seer1)
  summary(cox)
}

plots.group("Lip, Oral Cavity and Pharynx")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-9.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-10.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 46635, number of events= 46635 
## 
##                   coef exp(coef) se(coef)       z Pr(>|z|)    
## partPartnered -0.29941   0.74126  0.01018 -29.399   <2e-16 ***
## sexMale        0.09875   1.10379  0.01103   8.954   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.7413      1.349    0.7266    0.7562
## sexMale          1.1038      0.906    1.0802    1.1279
## 
## Concordance= 0.544  (se = 0.001 )
## Likelihood ratio test= 925.7  on 2 df,   p=<2e-16
## Wald test            = 960.8  on 2 df,   p=<2e-16
## Score (logrank) test = 967.5  on 2 df,   p=<2e-16
```

```r
plots.group("Digestive Organs")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-11.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-12.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 503057, number of events= 503057 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)    
## partPartnered -0.172728  0.841367  0.003510 -49.20   <2e-16 ***
## sexMale        0.065414  1.067601  0.002983  21.93   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.8414     1.1885    0.8356    0.8472
## sexMale          1.0676     0.9367    1.0614    1.0739
## 
## Concordance= 0.524  (se = 0 )
## Likelihood ratio test= 2729  on 2 df,   p=<2e-16
## Wald test            = 2803  on 2 df,   p=<2e-16
## Score (logrank) test = 2809  on 2 df,   p=<2e-16
```

```r
plots.group("Respiratory and Intrathoracic Organs")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-13.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-14.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 499193, number of events= 499193 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)    
## partPartnered -0.156271  0.855328  0.003671 -42.57   <2e-16 ***
## sexMale        0.124250  1.132299  0.003049  40.75   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.8553     1.1691    0.8492    0.8615
## sexMale          1.1323     0.8832    1.1256    1.1391
## 
## Concordance= 0.527  (se = 0 )
## Likelihood ratio test= 3291  on 2 df,   p=<2e-16
## Wald test            = 3321  on 2 df,   p=<2e-16
## Score (logrank) test = 3326  on 2 df,   p=<2e-16
```

```r
plots.group("Bone and Articular Cartilage")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-15.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-16.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 6159, number of events= 6159 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)  
## partPartnered -0.059789  0.941964  0.025691 -2.327    0.020 *
## sexMale       -0.004884  0.995128  0.026707 -0.183    0.855  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.9420      1.062    0.8957    0.9906
## sexMale          0.9951      1.005    0.9444    1.0486
## 
## Concordance= 0.486  (se = 0.004 )
## Likelihood ratio test= 5.45  on 2 df,   p=0.07
## Wald test            = 5.46  on 2 df,   p=0.07
## Score (logrank) test = 5.46  on 2 df,   p=0.07
```

```r
plots.group("Skin")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-17.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-18.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 41155, number of events= 41155 
## 
##                   coef exp(coef) se(coef)      z Pr(>|z|)    
## partPartnered -0.41058   0.66327  0.01034 -39.70   <2e-16 ***
## sexMale        0.23331   1.26277  0.01196  19.51   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.6633     1.5077     0.650    0.6768
## sexMale          1.2628     0.7919     1.234    1.2927
## 
## Concordance= 0.572  (se = 0.002 )
## Likelihood ratio test= 2085  on 2 df,   p=<2e-16
## Wald test            = 2127  on 2 df,   p=<2e-16
## Score (logrank) test = 2158  on 2 df,   p=<2e-16
```

```r
plots.group("Mesothelial and Soft Tissue")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-19.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-20.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 21323, number of events= 21323 
## 
##                   coef exp(coef) se(coef)     z Pr(>|z|)    
## partPartnered -0.10426   0.90099  0.01496 -6.97 3.16e-12 ***
## sexMale        0.11504   1.12192  0.01375  8.37  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered     0.901     1.1099     0.875    0.9278
## sexMale           1.122     0.8913     1.092    1.1526
## 
## Concordance= 0.531  (se = 0.002 )
## Likelihood ratio test= 123.3  on 2 df,   p=<2e-16
## Wald test            = 124  on 2 df,   p=<2e-16
## Score (logrank) test = 124.1  on 2 df,   p=<2e-16
```

```r
plots.group("Breast")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-21.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-22.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 138415, number of events= 138415 
## 
##                    coef exp(coef)  se(coef)       z Pr(>|z|)    
## partPartnered -0.285960  0.751293  0.006517 -43.878   <2e-16 ***
## sexMale        0.047703  1.048859  0.027472   1.736   0.0825 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.7513     1.3310    0.7418     0.761
## sexMale          1.0489     0.9534    0.9939     1.107
## 
## Concordance= 0.533  (se = 0.001 )
## Likelihood ratio test= 1825  on 2 df,   p=<2e-16
## Wald test            = 1928  on 2 df,   p=<2e-16
## Score (logrank) test = 1941  on 2 df,   p=<2e-16
```

```r
plots.group("Genital Organs")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-23.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-24.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 210986, number of events= 210986 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)    
## partPartnered -0.291082  0.747454  0.005620  -51.8   <2e-16 ***
## sexMale       -0.684915  0.504133  0.004572 -149.8   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.7475      1.338    0.7393    0.7557
## sexMale          0.5041      1.984    0.4996    0.5087
## 
## Concordance= 0.619  (se = 0.001 )
## Likelihood ratio test= 26693  on 2 df,   p=<2e-16
## Wald test            = 28178  on 2 df,   p=<2e-16
## Score (logrank) test = 29198  on 2 df,   p=<2e-16
```

```r
plots.group("Urinary Tract")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-25.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-26.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 97142, number of events= 97142 
## 
##                    coef exp(coef)  se(coef)      z Pr(>|z|)    
## partPartnered -0.258327  0.772343  0.008615 -29.99   <2e-16 ***
## sexMale       -0.122031  0.885121  0.007598 -16.06   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.7723      1.295    0.7594    0.7855
## sexMale          0.8851      1.130    0.8720    0.8984
## 
## Concordance= 0.535  (se = 0.001 )
## Likelihood ratio test= 1184  on 2 df,   p=<2e-16
## Wald test            = 1249  on 2 df,   p=<2e-16
## Score (logrank) test = 1256  on 2 df,   p=<2e-16
```

```r
plots.group("Eye, Brain or Central Nervous System")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-27.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-28.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 66704, number of events= 66704 
## 
##                   coef exp(coef) se(coef)      z Pr(>|z|)    
## partPartnered 0.135084  1.144632 0.008934 15.120   <2e-16 ***
## sexMale       0.079780  1.083048 0.008023  9.944   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered     1.145     0.8736     1.125     1.165
## sexMale           1.083     0.9233     1.066     1.100
## 
## Concordance= 0.516  (se = 0.001 )
## Likelihood ratio test= 336.9  on 2 df,   p=<2e-16
## Wald test            = 331.1  on 2 df,   p=<2e-16
## Score (logrank) test = 331.5  on 2 df,   p=<2e-16
```

```r
plots.group("Thyroid & Endocrine Glands")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-29.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-30.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 8573, number of events= 8573 
## 
##                   coef exp(coef) se(coef)      z Pr(>|z|)    
## partPartnered -0.23259   0.79248  0.02424 -9.595  < 2e-16 ***
## sexMale        0.09139   1.09569  0.02165  4.221 2.43e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.7925     1.2619    0.7557     0.831
## sexMale          1.0957     0.9127    1.0502     1.143
## 
## Concordance= 0.522  (se = 0.003 )
## Likelihood ratio test= 106.1  on 2 df,   p=<2e-16
## Wald test            = 109.1  on 2 df,   p=<2e-16
## Score (logrank) test = 109.5  on 2 df,   p=<2e-16
```

```r
plots.group("Ill-Defined Sites")
```

![](final_project_ROHN_files/figure-html/unnamed-chunk-2-31.png)<!-- -->![](final_project_ROHN_files/figure-html/unnamed-chunk-2-32.png)<!-- -->

```
## Call:
## coxph(formula = Surv(surv.months) ~ part + sex, data = seer1)
## 
##   n= 132574, number of events= 132574 
## 
##                    coef exp(coef)  se(coef)       z Pr(>|z|)    
## partPartnered -0.151413  0.859492  0.006473 -23.393  < 2e-16 ***
## sexMale        0.024706  1.025014  0.005673   4.355 1.33e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##               exp(coef) exp(-coef) lower .95 upper .95
## partPartnered    0.8595     1.1635    0.8487    0.8705
## sexMale          1.0250     0.9756    1.0137    1.0365
## 
## Concordance= 0.521  (se = 0.001 )
## Likelihood ratio test= 551.5  on 2 df,   p=<2e-16
## Wald test            = 566  on 2 df,   p=<2e-16
## Score (logrank) test = 567.1  on 2 df,   p=<2e-16
```
