######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate models for post-referral decisions (Section V)
# 1. Load libraries and data
# 2. Set color palette 
# 3. Generate reporter type, tract bins, add'l vars
# 4. Models of screened in-out
# 5. Models of investigation-assessment track
# 6a. Models of disposition for investigation (substantiated finding) 
# 6b. Models of disposition for assessment (services needed)
# Authors: Sean, Shelby, Ryan, Connor, Brandon, Jessica, Hannah, MPC
# Updated: September 2020 
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)
library(scales) # check palette
library(car)
library(ggpubr)

source("report-code/gen_qoi.R")

# load data
# setwd("/Volumes/PIDL20/data2020") 
rsw <- readRDS("data2020/ref_supp_wide.RDS") # wide data/child as unit
# files created ADD ME


# ..........................................................................................
# 2. Set color palette ----
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")
# show_col(pidlpal)


# ..........................................................................................
# 3. Generate race3, reporter type, tract bins ----
# race3
rsw <- rsw %>% 
  mutate(race3 = case_when(race == "Black" ~ "Black",
                           race == "White" ~ "White",
                           race == "MultiRace" ~ "Multiracial",
                           TRUE ~ NA_character_)) %>% 
  mutate(race3 = factor(race3, levels = c("White", "Black", "Multiracial"))) %>% # relevel so white is reference category in model
  droplevels()

# sector, including nonprofessional/unknown 
hc <- c("Hospital/Clinic", "Medical Professional, Other", "Physician, Private", 
        "Public/Private Mental Health", "Emergency Medical Services Personnel")
edu <- c("School- Public", "School-Private")
legal <- c("Court/Probation", "Law Enforcement")
ss <- c("Family Services Specialist", "Social Services-Private", "Social Worker", 
        "Day Care Provider", "Eligibility Worker", "Substitute Care Provider")

rsw <- rsw %>% 
  mutate(sector = case_when(reporter1 %in% hc ~ "Healthcare",
                            reporter1 %in% edu ~ "Education",
                            reporter1 %in% ss ~ "Social Services",
                            reporter1 %in% legal ~ "Legal",
                            TRUE ~ "Non-Professional"))

# tract bins: missing/other tracts = Unknown
rsw <- rsw %>%
  mutate(perc_wt = case_when(
    tract_ref1 %in% c(900, 1000, 402, 700) ~ ">75",
    tract_ref1 %in% c(302, 502, 201, 202, 600, 800) ~ "50-75",
    tract_ref1 %in% c(401, 501) ~ "<50",
    TRUE ~ "Unknown"),
    perc_pov = case_when(
      tract_ref1 %in% c(202, 600, 401, 201, 501) ~ ">20",
      tract_ref1 %in% c(700, 1000, 800, 402) ~ "10-20",
      tract_ref1 %in% c(900, 302, 502) ~ "<10",
    TRUE ~ "Unknown"),
    perc_wt = factor(perc_wt, levels = c(">75", "50-75", "<50", "Unknown")),
    perc_pov = factor(perc_pov, levels = c("<10", "10-20", ">20", "Unknown")))
 
# remaining variables
# age, missing age
rsw <- rsw %>% 
  mutate(age2 = ifelse(is.na(age_ref1), 0, age_ref1),
         agemiss = if_else(is.na(age_ref1), 1, 0),
         numref3 = if_else(numref>2, 1, 0),
         gender2 = case_when(as.character(gender) == "Male" ~ "Male",
                             as.character(gender) == "Female" ~ "Female",
                             TRUE ~ NA_character_))

rsw <- rsw %>% 
  mutate(phys_neg = if_else(phys_neg1 == "Y" | phys_neg2 == "Y" | phys_neg3 == "Y" | phys_neg4 == "Y" | phys_neg5 == "Y",
                            1, 0, missing = 0),
         phys_ab = if_else(phys_ab1 == "Y" | phys_ab2 == "Y" | phys_ab3 == "Y" | phys_ab4 == "Y" | phys_ab5 == "Y",
                           1, 0, missing = 0),
         ment_ab = if_else(ment_ab1 == "Y" | ment_ab2 == "Y" | ment_ab3 == "Y" | ment_ab4 == "Y" | ment_ab5 == "Y",
                           1, 0, missing = 0),
         med_neg = if_else(med_neg1 == "Y" | med_neg2 == "Y" | med_neg3 == "Y" | med_neg4 == "Y" | med_neg5 == "Y",
                           1, 0, missing = 0),
         sex_ab = if_else(sex_ab1 == "Y" | sex_ab2 == "Y" | sex_ab3 == "Y" | sex_ab4 == "Y" | sex_ab5 == "Y",
                          1, 0, missing = 0),
         substance_ex = if_else(substance_ex1 == "Y" | substance_ex2 == "Y" | substance_ex3 == "Y" | substance_ex4 == "Y" | substance_ex5 == "Y",
                                1, 0, missing = 0))

# ever_screen, ever_inv, ever_find, ever_svcs
rsw <- rsw %>% 
  mutate(
    ever_screen = if_else(screenin1 == "Y" | screenin2 == "Y" | screenin3 == "Y" | screenin4 == "Y" | screenin5 == "Y",
                          "Yes","No", missing = "No"),
    ever_inv = if_else(ever_screen == "Yes" & 
                         (track1 == "Investigation" | track2 == "Investigation" | track3 == "Investigation" | track4 == "Investigation" | track5 == "Investigation"),
                       "Yes", "No", missing = "No"),
    ever_find = if_else(ever_inv == "Yes" & 
                          (str_detect(disp1, "Founded") | str_detect(disp2, "Founded") | str_detect(disp3, "Founded") | str_detect(disp4, "Founded") | str_detect(disp5, "Founded")),
                        "Yes", "No", missing = "No"),
    ever_asss = if_else(ever_screen == "Yes" &
                          (track1 == "Family Assessment" | track2 == "Family Assessment" | track3 == "Family Assessment" | track4 == "Family Assessment" | track5 == "Family Assessment"),
                        "Yes", "No", missing = "No"),
    ever_svcs = if_else(ever_asss == "Yes" & 
                          (disp1 == "Services Needed" | disp2 == "Services Needed" | disp3 == "Services Needed" | disp4 == "Services Needed" | disp5 == "Services Needed"),
                        "Yes", "No", missing = "No"))
  
rsw <- rsw %>% 
  mutate(ever_screen = factor(ever_screen, levels = c("No", "Yes")),
         ever_inv = factor(ever_inv, levels = c("No", "Yes")),
         ever_find = factor(ever_find, levels = c("No", "Yes")),
         ever_asss = factor(ever_asss, levels = c("No", "Yes")),
         ever_svcs = factor(ever_svcs, levels = c("No", "Yes")))

# ..........................................................................................
# 4. Models of screened in-out ----
# WIDE/child as unit
# demographics
screen1 <- glm(ever_screen ~ race3 + gender2 + agemiss + age2,
               data = rsw, family = binomial(link="logit"))
summary(screen1)

# demographics, alleged maltreatment, and number of referrals
screen2 <- glm(ever_screen ~ race3 + gender2 + agemiss + age2 + 
                 phys_ab + phys_neg + sex_ab +
                 numref3,
               data=rsw, family=binomial(link="logit"))
summary(screen2)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
screen3 <- glm(ever_screen ~ race3 + gender2 + age2 + agemiss + 
                 phys_ab + phys_neg + sex_ab +
                 numref3 + sector + perc_wt,
               data=rsw, family=binomial(link="logit"))
summary(screen3)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
screen4 <- glm(ever_screen ~ race3 + gender2 + age2 + agemiss + 
                 phys_ab + phys_neg + sex_ab +
                 numref3 + sector + perc_pov,
               data=rsw, family=binomial(link="logit"))
summary(screen4)


# ..........................................................................................
# 5. Models of investigation-assessment track ----
# WIDE/child as unit
rsw_screen <- rsw %>% 
  filter(ever_screen == "Yes")

# demographics
track1 <- glm(ever_inv ~ race3 + gender2 + agemiss + age2,
              data = rsw_screen, family = binomial(link="logit"))
summary(track1)

# demographics, alleged maltreatment, and number of referrals
track2 <- glm(ever_inv ~ race3 + gender2 + agemiss + age2 + 
               ment_ab + phys_ab + phys_neg + med_neg +
                numref3,
              data=rsw_screen, family=binomial(link="logit"))
summary(track2)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
track3 <- glm(ever_inv ~ race3 + gender2 + age2 + agemiss + 
                ment_ab + phys_ab + phys_neg + med_neg +
                numref3 + sector + perc_wt,
              data=rsw_screen, family=binomial(link="logit"))
summary(track3)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
track4 <- glm(ever_inv ~ race3 + gender2 + age2 + agemiss + 
                ment_ab + phys_ab + phys_neg + med_neg +
                numref3 + sector + perc_pov,
              data=rsw_screen, family=binomial(link="logit"))
summary(track4)


# ..........................................................................................
# 6a. Models of disposition for investigation (substantiated finding) ----
# WIDE/child as unit
rsw_invest <- rsw %>% 
  filter(ever_screen == "Yes", ever_inv == "Yes")

# demographics
find1 <- glm(ever_find ~ race3 + gender2 + age2 + agemiss,
             data = rsw_invest, family = binomial(link="logit"))
summary(find1)

# demographics, alleged maltreatment, and number of referrals
find2 <- glm(ever_find ~ race3 + gender2 + agemiss + age2 + 
               ment_ab + phys_ab + phys_neg + sex_ab + med_neg +
               numref3,
             data=rsw_invest, family=binomial(link="logit"))
summary(find2)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
find3 <- glm(ever_find ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg +
               numref3 + sector + perc_wt,
             data=rsw_invest, family=binomial(link="logit"))
summary(find3)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
find4 <- glm(ever_find ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg +
               numref3 + sector + perc_pov,
             data=rsw_invest, family=binomial(link="logit"))
summary(find4)


# ..........................................................................................
# 6b. Models of disposition for assessment (services needed) ----
# WIDE/child as unit
rsw_assess <- rsw %>% 
  filter(ever_screen == "Yes", ever_asss == "Yes")

# demographics
svcs1 <- glm(ever_svcs ~ race3 + gender2 + age2 + agemiss,
             data = rsw_assess, family = binomial(link="logit"))
summary(svcs1)

# demographics, alleged maltreatment, and number of referrals
svcs2 <- glm(ever_svcs ~ race3 + gender2 + agemiss + age2 + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               numref3,
             data = rsw_assess, family=binomial(link="logit"))
summary(svcs2)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
svcs3 <- glm(ever_svcs ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               numref3 + sector + perc_wt,
             data=rsw_assess, family=binomial(link="logit"))
summary(svcs3)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
svcs4 <- glm(ever_svcs ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               numref3 + sector + perc_pov,
             data=rsw_assess, family=binomial(link="logit"))
summary(svcs4)

