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
# 7. Model tables
# Authors: Sean, Shelby, Ryan, Connor, Brandon, Jessica, Hannah, MPC
# Updated: September 2020 
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)
library(scales) # check palette
library(ggpubr)
library(lmtest)
library(sandwich)
library(stargazer)

source("report-code/gen_qoi.R")

# load data
# setwd("/Volumes/PIDL20/data2020") 
rsl <- readRDS("data2020/ref_supp_long.RDS") # long data/referral as unit
# files created ADD ME


# ..........................................................................................
# 2. Set color palette ----
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")
# show_col(pidlpal)


# ..........................................................................................
# 3. Generate race3, reporter type, tract bins ----
# race3
rsl <- rsl %>% 
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

rsl <- rsl %>% 
  mutate(sector = case_when(reporter %in% hc ~ "Healthcare",
                            reporter %in% edu ~ "Education",
                            reporter %in% ss ~ "Social Services",
                            reporter %in% legal ~ "Legal",
                            TRUE ~ "Non-Professional"))

# tract bins: missing/other tracts = Unknown
rsl <- rsl %>%
  mutate(tract_name = ifelse(is.na(tract_name), "unknown", tract_name),
         perc_wt = fct_collapse(tract_name, 
                                ">75" = c("Census Tract 9", "Census Tract 10", "Census Tract 4.02", "Census Tract 7"),
                                "50-75" = c("Census Tract 3.02", "Census Tract 5.02", "Census Tract 2.01", 
                                            "Census Tract 2.02", "Census Tract 6", "Census Tract 8"),
                                "<50" = c("Census Tract 4.01", "Census Tract 5.01"),
                                other_level = "Unknown"),
         perc_pov = fct_collapse(tract_name,
                                 ">20" = c("Census Tract 2.02", "Census Tract 6", "Census Tract 4.01", 
                                           "Census Tract 2.01", "Census Tract 5.01"),
                                 "10-20" = c("Census Tract 7", "Census Tract 10", "Census Tract 8", 
                                             "Census Tract 4.02"),
                                 "<10" = c("Census Tract 9", "Census Tract 3.02", "Census Tract 5.02"),
                                 other_level = "Unknown"),
         perc_pov = fct_relevel(perc_pov, "<10")) 

# remaining variables
# age, missing age
rsl <- rsl %>% 
  mutate(age2 = ifelse(is.na(age), 0, age),
       agemiss = if_else(is.na(age), 1, 0),
       refnum3 = if_else(refnum>2, 1, 0),
       gender2 = case_when(as.character(gender) == "Male" ~ "Male",
                           as.character(gender) == "Female" ~ "Female",
                           TRUE ~ NA_character_))

# disposition
rsl <- rsl %>% 
  mutate(find = if_else(disp %in% c("FL1", "FL2", "FL3"), "Yes", "No"),
         find = factor(find, levels = c("No", "Yes")),
         svcs = if_else(disp == "SVC", "Yes", "No"),
         svcs = factor(svcs, levels = c("No", "Yes")))


# ..........................................................................................
# 4. Models of screened in-out ----
# LONG/referral as unit
# demographics
screen1 <- glm(screen_in ~ race3 + gender2 + agemiss + age2,
               data = rsl, family = binomial(link="logit"))
summary(screen1)
# vcovCL(screen1, cluster = ~ cid)
coeftest(screen1, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, and number of referrals
screen2 <- glm(screen_in ~ race3 + gender2 + agemiss + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex +
                 refnum3,
               data=rsl, family=binomial(link="logit"))
summary(screen2)
coeftest(screen2, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
screen3 <- glm(screen_in ~ race3 + gender2 + age2 + agemiss + 
                 phys_ab + phys_neg + sex_ab + med_neg +
                 refnum3 + sector +perc_wt,
               data=rsl, family=binomial(link="logit"))
summary(screen3)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
screen4 <- glm(screen_in ~ race3 + gender2 + age2 + agemiss + 
                 phys_ab + phys_neg + sex_ab + med_neg +
                 refnum3 + sector + perc_pov,
               data=rsl, family=binomial(link="logit"))
summary(screen4)
coeftest(screen4, vcov = vcovCL, cluster = ~ cid)

AIC(screen1, screen2, screen3, screen4)


# generate predicted probabilities from screen3: race3 (sector, perc_wt)
set.seed(1017)
pred_prob_screen <- gen_qoi(rsl, "race3", screen3)
pred_prob_screen # check the output
pred_prob_screen <- pred_prob_screen %>% 
  mutate(group = fct_relevel(group, "Black", "White", "Multiracial"))

# plot predicted probabilities
p1 <- ggplot(pred_prob_screen, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of being Screened In by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Screened In",
       caption = "Note: error bars are 90% credible intervals")
p1 

# generate predicted probabilities from screen3: sector (perc_wt, race3)
set.seed(1017)
pred_prob_screen <- gen_qoi(rsl, "sector", screen3)
pred_prob_screen # check the output
pred_prob_screen <- pred_prob_screen %>% 
  mutate(group = fct_relevel(group, "Non-Professional", "Education", "Legal", "Healthcare", "Social Services"))

# plot predicted probabilities
p2 <- ggplot(pred_prob_screen, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of being Screened In by Reporter Type",
       x = "", color = "Sector",
       y = "Predicted Probability of Screened In",
       caption = "Note: error bars are 90% credible intervals")
p2

# generate predicted probabilities from screen3: perc_wt (race3, sector)
set.seed(1017)
pred_prob_screen <- gen_qoi(rsl, "perc_wt", screen3)
pred_prob_screen # check the output
pred_prob_screen <- pred_prob_screen %>% 
  mutate(group = fct_relevel(group, ">75", "50-75", "<50", "Unknown"))

# plot predicted probabilities
p3 <- ggplot(pred_prob_screen, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of being Screened In by Tract Race",
       x = "", color = "Tract",
       y = "Predicted Probability of Screened In",
       caption = "Note: error bars are 90% credible intervals")
p3

# generate predicted probabilities from screen4: perc_pov
set.seed(1017)
pred_prob_screen <- gen_qoi(rsl, "perc_pov", screen4)
pred_prob_screen # check the output
pred_prob_screen <- pred_prob_screen %>% 
  mutate(group = fct_relevel(group, "<10", "10-20", ">20", "Unknown"))

# plot predicted probabilities
p4 <- ggplot(pred_prob_screen, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of being Screened In by Tract Poverty",
       x = "", color = "Tract",
       y = "Predicted Probability of Screened In",
       caption = "Note: error bars are 90% credible intervals")
p4 

# ggarrange(p1, p2, p3, p4)
# decide which, if any, to show -- maybe arrange in a grid with common titles/labels


# ..........................................................................................
# 5. Models of investigation-assessment track ----
# LONG/referral as unit
rsl_screen <- rsl %>% 
  filter(screen_in == "Yes")

# demographics
track1 <- glm(track ~ race3 + gender2 + agemiss + age2,
               data = rsl_screen, family = binomial(link="logit"))
summary(track1)
coeftest(track1, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, and number of referrals
track2 <- glm(track ~ race3 + gender2 + agemiss + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex +
                 refnum3,
               data=rsl_screen, family=binomial(link="logit"))
summary(track2)
coeftest(track2, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
track3 <- glm(track ~ race3 + gender2 + age2 + agemiss + 
                 ment_ab + phys_ab + phys_neg + med_neg +
                 refnum3 + sector + perc_wt,
               data=rsl_screen, family=binomial(link="logit"))
summary(track3)
coeftest(track3, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
track4 <- glm(track ~ race3 + gender2 + age2 + agemiss + 
                ment_ab + phys_ab + phys_neg + med_neg +
                refnum3 + sector + perc_pov,
              data=rsl_screen, family=binomial(link="logit"))
summary(track4)
coeftest(track4, vcov = vcovCL, cluster = ~ cid)

AIC(track1, track2, track3, track4)


# generate predicted probabilities from track3: race3 (sector, perc_wt)
set.seed(1017)
pred_prob_invest <- gen_qoi(rsl, "race3", track3)
pred_prob_invest # check the output
pred_prob_invest <- pred_prob_invest %>% 
  mutate(group = fct_relevel(group, "Black", "White", "Multiracial"))

# plot predicted probabilities
ggplot(pred_prob_invest, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Investigation by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Investigation",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from track3: sector (perc_wt, race3)
set.seed(1017)
pred_prob_invest <- gen_qoi(rsl, "sector", track3)
pred_prob_invest # check the output
pred_prob_invest <- pred_prob_invest %>% 
  mutate(group = fct_relevel(group, "Non-Professional", "Education", "Legal", "Healthcare", "Social Services"))

# plot predicted probabilities
ggplot(pred_prob_invest, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Investigation by Reporter Type",
       x = "", color = "Sector",
       y = "Predicted Probability of Investigation",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from track3: perc_wt (race3, sector)
set.seed(1017)
pred_prob_invest <- gen_qoi(rsl, "perc_wt", track3)
pred_prob_invest # check the output
pred_prob_invest <- pred_prob_invest %>% 
  mutate(group = fct_relevel(group, ">75", "50-75", "<50", "Unknown"))

# plot predicted probabilities
ggplot(pred_prob_invest, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Investigation by Tract Race",
       x = "", color = "Tract",
       y = "Predicted Probability of Investigation",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from track4: perc_pov
set.seed(1017)
pred_prob_invest <- gen_qoi(rsl, "perc_pov", track4)
pred_prob_invest # check the output
pred_prob_invest <- pred_prob_invest %>% 
  mutate(group = fct_relevel(group, "<10", "10-20", ">20", "Unknown"))

# plot predicted probabilities
ggplot(pred_prob_invest, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Investigation by Tract Poverty",
       x = "", color = "Tract",
       y = "Predicted Probability of Investigation",
       caption = "Note: error bars are 90% credible intervals")


# ..........................................................................................
# 6a. Models of disposition for investigation (substantiated finding) ----
# LONG/referral as unit
rsl_invest <- rsl %>% 
  filter(screen_in == "Yes", track == "invest")

# demographics
find1 <- glm(find ~ race3 + gender2 + age2 + agemiss,
              data = rsl_invest, family = binomial(link="logit"))
summary(find1)
coeftest(find1, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, and number of referrals
find2 <- glm(find ~ race3 + gender2 + agemiss + age2 + 
                ment_ab + phys_ab + phys_neg + sex_ab + med_neg +
                refnum3,
              data=rsl_invest, family=binomial(link="logit"))
summary(find2)
coeftest(find2, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
find3 <- glm(find ~ race3 + gender2 + age2 + agemiss + 
                ment_ab + phys_ab + phys_neg + med_neg +
                refnum3 + sector + perc_wt,
              data=rsl_invest, family=binomial(link="logit"))
summary(find3)
coeftest(find3, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
find4 <- glm(find ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg +
               refnum3 + sector + perc_pov,
             data=rsl_invest, family=binomial(link="logit"))
summary(find4)
coeftest(find4, vcov = vcovCL, cluster = ~ cid)

AIC(find1, find2, find3, find4)


# generate predicted probabilities from find3: race3 (sector, perc_wt)
set.seed(1017)
pred_prob_find <- gen_qoi(rsl, "race3", find3)
pred_prob_find # check the output
pred_prob_find <- pred_prob_find %>% 
  mutate(group = fct_relevel(group, "Black", "White", "Multiracial"))

# plot predicted probabilities
ggplot(pred_prob_find, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Substantiated Finding by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Substantiated Finding",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from find3: sector (perc_wt, race3)
set.seed(1017)
pred_prob_find <- gen_qoi(rsl, "sector", find3)
pred_prob_find # check the output
pred_prob_find <- pred_prob_find %>% 
  mutate(group = fct_relevel(group, "Non-Professional", "Education", "Legal", "Healthcare", "Social Services"))

# plot predicted probabilities
ggplot(pred_prob_find, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Substantiated Finding by Sector",
       x = "", color = "Sector",
       y = "Predicted Probability of Substantiated Finding",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from find3: perc_wt (race3, sector)
set.seed(1017)
pred_prob_find <- gen_qoi(rsl, "perc_wt", find3)
pred_prob_find # check the output
pred_prob_find <- pred_prob_find %>% 
  mutate(group = fct_relevel(group, ">75", "50-75", "<50", "Unknown"))

# plot predicted probabilities
ggplot(pred_prob_find, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Substantiated Finding by Tract Race",
       x = "", color = "Tract",
       y = "Predicted Probability of Substantiated Finding",
       caption = "Note: error bars are 90% credible intervals")

# generate predicted probabilities from find4: perc_pov
set.seed(1017)
pred_prob_find <- gen_qoi(rsl, "perc_pov", find4)
pred_prob_find # check the output
pred_prob_find <- pred_prob_find %>% 
  mutate(group = fct_relevel(group, "<10", "10-20", ">20", "Unknown"))

# plot predicted probabilities
ggplot(pred_prob_find, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Substantiated Finding by Tract Poverty",
       x = "", color = "Tract",
       y = "Predicted Probability of Substantiated Finding",
       caption = "Note: error bars are 90% credible intervals")


# ..........................................................................................
# 6b. Models of disposition for assessment (services needed) ----
# LONG/referral as unit
rsl_assess <- rsl %>% 
  filter(screen_in == "Yes", track == "assess")

# demographics
svcs1 <- glm(svcs ~ race3 + gender2 + age2 + agemiss,
             data = rsl_assess, family = binomial(link="logit"))
summary(svcs1)
coeftest(svcs1, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, and number of referrals
svcs2 <- glm(svcs ~ race3 + gender2 + agemiss + age2 + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               refnum3,
             data = rsl_assess, family=binomial(link="logit"))
summary(svcs2)
coeftest(svcs2, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (white)
svcs3 <- glm(svcs ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               refnum3 + sector + perc_wt,
             data=rsl_assess, family=binomial(link="logit"))
summary(svcs3)
coeftest(svcs3, vcov = vcovCL, cluster = ~ cid)

# demographics, alleged maltreatment, number of referrals, reporter, and tract (poverty)
svcs4 <- glm(svcs ~ race3 + gender2 + age2 + agemiss + 
               ment_ab + phys_ab + phys_neg + med_neg + substance_ex +
               refnum3 + sector + perc_pov,
             data=rsl_assess, family=binomial(link="logit"))
summary(svcs4)
coeftest(svcs4, vcov = vcovCL, cluster = ~ cid)

AIC(svcs1, svcs2, svcs3, svcs4)

# generate predicted probabilities from svcs3: race3 (sector, perc_wt)
set.seed(1017)
pred_prob_svcs <- gen_qoi(rsl, "race3", svcs3)
pred_prob_svcs # check the output
pred_prob_svcs <- pred_prob_svcs %>% 
  mutate(group = fct_relevel(group, "Black", "White", "Multiracial"))

# plot predicted probabilities
p1 <- ggplot(pred_prob_svcs, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Services by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Services",
       caption = "Note: error bars are 90% credible intervals")
p1

# generate predicted probabilities from find3: sector (perc_wt, race3)
set.seed(1017)
pred_prob_svcs <- gen_qoi(rsl, "sector", svcs3)
pred_prob_svcs # check the output
pred_prob_svcs <- pred_prob_svcs %>% 
  mutate(group = fct_relevel(group, "Non-Professional", "Education", "Legal", "Healthcare", "Social Services"))

# plot predicted probabilities
p2 <- ggplot(pred_prob_svcs, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Services by Sector",
       x = "", color = "Sector",
       y = "Predicted Probability of Services",
       caption = "Note: error bars are 90% credible intervals")
p2

# generate predicted probabilities from find3: perc_wt (race3, sector)
set.seed(1017)
pred_prob_svcs <- gen_qoi(rsl, "perc_wt", svcs3)
pred_prob_svcs # check the output
pred_prob_svcs <- pred_prob_svcs %>% 
  mutate(group = fct_relevel(group, ">75", "50-75", "<50", "Unknown"))

# plot predicted probabilities
p3 <- ggplot(pred_prob_svcs, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Services by Tract Race",
       x = "", color = "Tract",
       y = "Predicted Probability of Services",
       caption = "Note: error bars are 90% credible intervals")
p3

# generate predicted probabilities from find4: perc_pov
set.seed(1017)
pred_prob_svcs <- gen_qoi(rsl, "perc_pov", svcs4)
pred_prob_svcs # check the output
pred_prob_svcs <- pred_prob_find %>% 
  mutate(group = fct_relevel(group, "<10", "10-20", ">20", "Unknown"))

# plot predicted probabilities
p4 <- ggplot(pred_prob_svcs, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=pidlpal) +
  labs(title="Predicted Probability of Services by Tract Poverty",
       x = "", color = "Tract",
       y = "Predicted Probability of Services",
       caption = "Note: error bars are 90% credible intervals")
p4


# ..........................................................................................
# 7. Model tables ----

# controlling for tract race
stargazer(screen3, track3, find3, svcs3, 
          title = "Logit Models of Post-Referral Decisions, controlling for Tract Racial Composition",
          covariate.labels=c("Black", "Multiracial", "Male",
                             "Age", "Age Missing", 
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect",
                             "Alleged Sexual Abuse", "Allged Medical Neglect", "Alleged Substance-Exposed Infant",
                             "More than 3 Referrals", "Repoter: Healthcare", "Reporter: Legal", "Reporter: Non-Professional",
                             "Reporter: Social Services", "Tract: 50-75% White", "Tract: Less than 50% White", "Tract: Unknown"),
          dep.var.caption = "", omit.stat = "theta",
          column.labels = c("Screen In", "Investigate", "Finding", "Services"),
          type = "latex", star.cutoffs = c(0.1, 0.05, .01),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

# controlling for tract poverty
stargazer(screen4, track4, find4, svcs4, 
          title = "Logit Models of Post-Referral Decisions, controlling for Tract Poverty",
          covariate.labels=c("Black", "Multiracial", "Male",
                             "Age", "Age Missing",
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect",
                             "Alleged Sexual Abuse", "Alleged Medical Neglect", "Alleged Substance-Exposed Infant",
                             "More than 3 Referrals", "Reporter: Healthcare", "Reporter: Legal", "Reporter: Non-Professional",
                             "Reporter: Social Services", "Tract: 10-20% Poverty", "Tract: More than 20% Poverty", "Tract: Unknown"),
          dep.var.caption = "", omit.stat = "theta",
          column.labels = c("Screen In", "Investigate", "Finding", "Services"),
          type = "latex", star.cutoffs = c(0.1, 0.05, .01),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

