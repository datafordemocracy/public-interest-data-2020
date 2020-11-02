######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate models for re-referral (Section VI)
# 1. Load libraries and data
# 2. Set color palette 
# 3a. Generate tract bins, add'l vars
# 3b. Create reduced data set
# 4a. Descriptive analysis, rereferral rates
# 4b. Descriptive analysis, rereferral tables
# 5. Models
# 6. Model figures
# 7. Tables
# Authors: Carrie, Chase, Ethan, Hannah, MPC
# Updated: September 2020 
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)
library(scales) # check palette
library(ggpubr)
library(lubridate)
library(stargazer)

source("report-code/gen_qoi.R")

# load data
rsl <- readRDS("data2020/ref_supp_long.RDS") # long data/referral as unit
# rsw <- readRDS("data2020/ref_supp_wide.RDS")
# files created ADD ME


# ..........................................................................................
# 2. Set color palette ----
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")
# show_col(pidlpal)


# ..........................................................................................
# 3a. Generate tract bins, add'l vars ----
# race3
rsl <- rsl %>% 
  mutate(race3 = case_when(race == "Black" ~ "Black",
                           race == "White" ~ "White",
                           race == "MultiRace" ~ "Multiracial",
                           TRUE ~ NA_character_)) %>% 
  mutate(race3mod = factor(race3, levels = c("White", "Black", "Multiracial")), # relevel so white is reference category in model
         race3fig = factor(race3, levels = c("Black", "White", "Multiracial"))) %>% # relevel for consistent ordering in figure
  droplevels()

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
         svcs = factor(svcs, levels = c("No", "Yes")),
         disp3 = case_when(
           disp %in% c("FL1", "FL2", "FL3") ~ "Finding",
           disp == "SVC" ~ "Services",
           TRUE ~ "Neither"
         ),
         disp3 = factor(disp3, levels = c("Neither", "Services", "Finding")),
         track2 = fct_recode(track2, "Assess" = "assess", "Investigate" = "invest"))


# ..........................................................................................
# 3b. Create reduced data set ----
#  ids with at least one screened-in referral
screenin <- rsl %>% filter(screen_in == "Yes") %>% distinct(cid)
rsl_screen <- rsl %>% filter(cid %in% screenin$cid)

# by child, keep first screened in referral and all subsequent referrals
#  (e.g., drop initial referrals if screened out)
rsl_screen <- rsl_screen %>% arrange(cid, ref_dt) %>% 
  group_by(cid) %>%
  mutate(screenstart = cumsum(case_when(screen_in == "Yes" ~ 1,
                                  TRUE ~ 0))) %>% 
  ungroup() %>% 
  filter(screenstart > 0)

#  group by cid and generate lead (next) referral if available
rsl_screen1 <- rsl_screen %>% 
  arrange(cid, ref_dt) %>% 
  group_by(cid) %>% 
  mutate(lref_date = lead(ref_dt),
         lref_days = (as.duration(lref_date - ref_dt)/ddays(1))) %>% 
  filter(lref_days > 1 | is.na(lref_days)) %>%  # remove subsequent referral within 1 day
  mutate(lref_date = lead(ref_dt), 
         lref_days = (as.duration(lref_date - ref_dt)/ddays(1)),
         lref_60 = ifelse(lref_days > 60, 1, 0),
         lref_5 = ifelse(lref_days >= 5, 1, 0),
         lref_1 = ifelse(lref_days > 1, 1, 0),
         linvalid_an = lead(invalid_an),
         lmed_neg = lead(med_neg),
         lment_ab = lead(ment_ab),
         lphys_ab = lead(phys_ab),
         lphys_neg = lead(phys_neg),
         lsex_ab = lead(sex_ab),
         lsubstance_ex = lead(substance_ex),
         lscreen = lead(screen_in),
         ltrack = lead(track2),
         ldisp = lead(disp),
         lfind = lead(find),
         lsvcs = lead(svcs),
         lref = ifelse(is.na(lref_date), "No", "Yes"),
         lref = factor(lref, levels = c("No", "Yes")),
         lref_60f = if_else(lref_60 == 1, 1, 0, missing = 0)) %>% 
  ungroup()  

# do we want to keep all referrals from first screened in referral and beyond
#   or focus on just first screened in referral and whether there is a
#   a subsequent referral?

# keep only first referral initial screened in referral, available in lead (l) vars
#  which means keep only first screened in referral
rsl_screen2 <- rsl_screen1 %>% arrange(cid, ref_dt) %>% 
  group_by(cid) %>% slice(1) %>% ungroup()

# including only those 60 days or later
rsl_screen3 <- rsl_screen %>% 
  arrange(cid, ref_dt) %>% 
  group_by(cid) %>% 
  mutate(lref_date = lead(ref_dt),
         lref_days = (as.duration(lref_date - ref_dt)/ddays(1))) %>% 
  filter(lref_days > 59 | is.na(lref_days)) %>%  # remove subsequent referral within 1 day
  mutate(lref_date = lead(ref_dt), 
         lref_days = (as.duration(lref_date - ref_dt)/ddays(1)),
         lref_60 = ifelse(lref_days > 60, 1, 0),
         lref_5 = ifelse(lref_days >= 5, 1, 0),
         lref_1 = ifelse(lref_days > 1, 1, 0),
         linvalid_an = lead(invalid_an),
         lmed_neg = lead(med_neg),
         lment_ab = lead(ment_ab),
         lphys_ab = lead(phys_ab),
         lphys_neg = lead(phys_neg),
         lsex_ab = lead(sex_ab),
         lsubstance_ex = lead(substance_ex),
         lscreen = lead(screen_in),
         ltrack = lead(track2),
         ldisp = lead(disp),
         lfind = lead(find),
         lsvcs = lead(svcs),
         lref = ifelse(is.na(lref_date), "No", "Yes"),
         lref = factor(lref, levels = c("No", "Yes")),
         lref_60f = if_else(lref_60 == 1, 1, 0, missing = 0)) %>% 
  ungroup()  

rsl_screen4 <- rsl_screen3 %>% arrange(cid, ref_dt) %>% 
  group_by(cid) %>% slice(1) %>% ungroup()



# ..........................................................................................
# 4a. Descriptive analysis, rereferral rates ----
# number/rate of rereferral prior to disp vs after (e.g, 6 month from referral date)

# a. rate of rereferral
# i. subsequent to initial screened in referral
prop.table(table(rsl_screen2$lref)) # 50.2%
prop.table(table(rsl_screen4$lref)) # 42.0%

# rereferrals are before or after disposition (60 days, don't have actual disposition date)
ggplot(rsl_screen2, aes(x = lref_days)) + geom_histogram(bins = 50) +
  geom_vline(xintercept = 60) + 
  geom_vline(xintercept = 175)

prop.table(table(rsl_screen2$lref_60)) # 72.8% post disposition/60 days
prop.table(table(rsl_screen2$lref_5)) # 2.3% under 5 days

rsl_screen2 %>% 
  filter(!is.na(lref_days)) %>% 
  mutate(days_group = case_when(
    lref_days < 176 ~ "Within 6 Months", 
    lref_days >= 176 & lref_days < 352 ~ "6 to 12 Months",
    lref_days >= 352 ~ "More than 12 Months"
  ),
  days_group = factor(days_group, levels = c("Within 6 Months", "6 to 12 Months", "More than 12 Months"))) %>% 
  ggplot(aes(x = days_group)) + geom_bar()

rsl_screen2 %>% 
  filter(!is.na(lref_days)) %>% 
  mutate(days_group = case_when(
    lref_days < 176 ~ "Within 6 Months", 
    lref_days >= 176 & lref_days < 352 ~ "6 to 12 Months",
    lref_days >= 352 ~ "More than 12 Months"
  ),
  days_group = factor(days_group, levels = c("Within 6 Months", "6 to 12 Months", "More than 12 Months"))) %>% 
  count(days_group)

# are pre-disposition/post-disposition rereferrals screened in?
prop.table(table(rsl_screen2$lref_60, rsl_screen2$lscreen), 1)
# pre-disposition: 27.0% screened in
# post-disposition: 55.8% screened in

prop.table(table(rsl_screen2$lref_5, rsl_screen2$lscreen), 1)
# within 5 days: 16.7% screened in
# after 5 days: 48.7% screened in


# are the rapid rereferrals reporting something new? do new allegations increase the odds of the second rapid referral being screened in?
tmp <- rsl_screen2 %>% filter(lref_5 == 0) %>%  # lref_5 = 0 if under 5, 1 if 5 or more
  dplyr::select(screen_in, lscreen, track2, ltrack, invalid_an, linvalid_an, med_neg, lmed_neg, ment_ab, lment_ab, phys_ab, lphys_ab, phys_neg, lphys_neg, sex_ab, lsex_ab, substance_ex, lsubstance_ex) %>% 
  mutate(track_match = track2 == ltrack,
         medneg_add = ifelse(med_neg == "No" & lmed_neg == "Yes", 1, 0), # med_neg as new allegation
         mentab_add = ifelse(ment_ab == "No" & lment_ab == "Yes", 1, 0), # ment_ab as new allegation
         physab_add = ifelse(phys_ab == "No" & lphys_ab == "Yes", 1, 0), # phys_ab as new allegation
         physneg_add = ifelse(phys_neg == "No" & lphys_neg == "Yes", 1, 0), # phys_neg as new allegation
         sexab_add = ifelse(sex_ab == "No" & lsex_ab == "Yes", 1, 0), # sex_ab as new allegation
         substancex_add = ifelse(substance_ex == "No" & lsubstance_ex == "Yes", 1, 0), # substance_ex as new allegation
         sum_add = rowSums(across(medneg_add:substancex_add))) # sum of new allegations
table(tmp$sum_add)

# I think there might be something to this -- at a minimum, if reporting a fuller model, should consider adding this variable...


# ..........................................................................................
# 4b. Descriptive analysis, rereferral tables ----

# b. rate of rereferral by race, by place; by prior track, prior disposition
# i. subsequent to initial screened in referral
# by race
tab <- table(rsl_screen2$lref, rsl_screen2$race3)
prop.table(tab, 2)
chisq.test(tab)
# FIGURE

p1 <- rsl_screen2 %>% filter(!is.na(race3fig)) %>% 
  group_by(race3fig, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2 %>% 
              group_by(race3fig) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", value, tot)) %>% 
  ggplot(aes(x = race3fig, y = value, fill = lref)) + 
  geom_col(position = "stack") +
  scale_fill_manual(values = pidlpal) +
  labs(title="By Race of Child", 
       x="Race of Referred Child", 
       y="Number of Referred Children", 
       fill="Re-Referred") +
  geom_text(aes(x = race3fig, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -20)
p1

# by place
# exclude unknown tract
rsl_screen2_tract <- rsl_screen2 %>% filter(perc_pov != "Unknown") %>% 
  droplevels()
tab1 <- table(rsl_screen2_tract$lref, rsl_screen2_tract$perc_pov)
prop.table(tab1, 2)
chisq.test(tab1)
tab2 <- table(rsl_screen2_tract$lref, rsl_screen2_tract$perc_wt)
prop.table(tab2, 2)
chisq.test(tab2)

# FIGURE: Tract Pov
p2 <- rsl_screen2_tract %>% 
  group_by(perc_pov, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2_tract %>% 
              group_by(perc_pov) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", value, tot)) %>% 
  ggplot(aes(x = perc_pov, y = value, fill = lref)) + 
  geom_col(position = "stack") +
  scale_fill_manual(values = pidlpal) +
  labs(title="By Tract Poverty Level", 
       x="Tract Poverty Level", 
       y="Number of Referred Children", 
       fill="Re-Referred") +
  geom_text(aes(x = perc_pov, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -20)
p2

# FIGURE: Tract Race
p3 <- rsl_screen2_tract %>% 
  group_by(perc_wt, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2_tract %>% 
              group_by(perc_wt) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", value, tot)) %>% 
  ggplot(aes(x = perc_wt, y = value, fill = lref)) + 
  geom_col(position = "stack") +
  scale_fill_manual(values = pidlpal) +
  labs(title="By Tract Racial Composition", 
       x="Tract Racial Composition", 
       y="Number of Referred Children", 
       fill="Re-Referred") +
  geom_text(aes(x = perc_wt, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -20)
p3

reref <- ggarrange(p1, p2, p3, nrow = 1, common.legend = TRUE, legend = "bottom")

reref_comb <- annotate_figure(reref,
                top = text_grob("Re-Referral Rates",
                                face = "bold", size = 15))

ggsave("plot/reref1.pdf", plot = reref_comb, device = "pdf", width=8, height=4, units="in")


# by prior track
tab <- table(rsl_screen2$lref, rsl_screen2$track2)
prop.table(tab, 2)
chisq.test(tab)

tab3 <-table(rsl_screen2$lref, rsl_screen2$track2, rsl_screen2$race3)
prop.table(tab3, 2)

# FIGURE: re-referral by race, faceted by track
p1 <- rsl_screen2 %>% filter(!is.na(race3fig), track2 != "none") %>% 
  group_by(race3fig, track2, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2 %>% 
              group_by(race3fig, track2) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", ref_per/100, 1)) %>% 
  ggplot(aes(x = race3fig, y = value, fill = lref)) + 
  geom_col(position = "fill") +
  scale_fill_manual(values = pidlpal) +
  facet_wrap(~track2) +
  labs(title="By Respone Track: Assess or Investigate", 
       x="", 
       y="", 
       fill="Re-Referred") +
  geom_text(aes(x = race3fig, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -.1)
p1

# by prior disp: among prior track=investigation (find=yes, no)
rsl_screen2_in <- rsl_screen2 %>% filter(track2 == "Investigate")
tab <- table(rsl_screen2_in$lref, rsl_screen2_in$find)
prop.table(tab, 2)
chisq.test(tab)

# FIGURE: rereferral by race among investigations, faceted by disposition

p2 <- rsl_screen2 %>% filter(!is.na(race3fig), track2 == "Investigate") %>% 
  group_by(race3fig, find, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2 %>% filter(!is.na(race3fig), track2 == "Investigate") %>% 
              group_by(race3fig, find) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", ref_per/100, 1)) %>% 
  ggplot(aes(x = race3fig, y = value, fill = lref)) + 
  geom_col(position = "fill") +
  scale_fill_manual(values = pidlpal) +
  facet_wrap(~find) +
  labs(title="By Investigation Outcome: Substantiation", 
       x="", 
       y="Proportion of Referred Children", 
       fill="Re-Referred") +
  geom_text(aes(x = race3fig, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -.1)
p2

# by prior disp: among all (svcs = yes, no)
tab <- table(rsl_screen2$lref, rsl_screen2$svcs)
prop.table(tab, 2)
chisq.test(tab)

# by prior disp: among prior track=assessment
rsl_screen2_as <- rsl_screen2 %>% filter(track2 == "Assess")
tab <- table(rsl_screen2_as$lref, rsl_screen2_as$svcs)
prop.table(tab, 2)
chisq.test(tab)



# FIGURE: rereferral by race among assessed, faceted by disposition

p3 <- rsl_screen2 %>% filter(!is.na(race3fig), track2 == "Assess") %>% 
  group_by(race3fig, svcs, lref) %>% 
  summarize(value = n()) %>% 
  ungroup() %>%
  left_join(rsl_screen2 %>% filter(!is.na(race3fig), track2 == "Assess") %>% 
              group_by(race3fig, svcs) %>% 
              summarize(tot = n())) %>% 
  mutate(ref_per = round((value/tot)*100,1),
         ref_pos = ifelse(lref == "Yes", ref_per/100, 1)) %>% 
  ggplot(aes(x = race3fig, y = value, fill = lref)) + 
  geom_col(position = "fill") +
  scale_fill_manual(values = pidlpal) +
  facet_wrap(~svcs) +
  labs(title="By Assessment Outcome: Services", 
       x="Race of Referred Child", 
       y="", 
       fill="Re-Referred") +
  geom_text(aes(x = race3fig, y = ref_pos, label = paste0(ref_per, "%")), nudge_y = -.1)
p3

reref2 <- ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE, legend = "bottom")

reref2_comb <- annotate_figure(reref2,
                              top = text_grob("Re-Referral Rates by Race of Child and",
                                              face = "bold", size = 15))

ggsave("plot/reref2.pdf", plot = reref2_comb, device = "pdf", width=4, height=8, units="in")



# year -- cases that enter later in the study period have less opportunity for rereferral
tab <- table(rsl_screen2$lref, rsl_screen2$ref_yr)
prop.table(tab, 2)
chisq.test(tab)


# ..........................................................................................
# 5. Models ----
rsl_screen2 <- rsl_screen2 %>% filter(track2 != "none")

# probability of rereferral and visual
# i. subsequent to initial screened in referral
reref1 <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr),
               data = rsl_screen2, family = binomial(link="logit"))
summary(reref1)

# demographics, alleged maltreatment
reref2 <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr) +
                ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex,
              data = rsl_screen2, family = binomial(link="logit"))
summary(reref2)

# demographics, alleged maltreatment, post-referral outcomes
reref3 <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr) +
                ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex +
                factor(track2) + disp3,
              data = rsl_screen2, family = binomial(link="logit"))
summary(reref3)

# demographics, alleged maltreatment, post-referral outcomes
reref3b <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr) +
                factor(track2) + disp3,
              data = rsl_screen2, family = binomial(link="logit"))
summary(reref3b)

# demographics, alleged maltreatment, and tract (pov)
reref4 <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr) +
                ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex +
                factor(track2) + disp3 + perc_pov,
              data = rsl_screen2, family = binomial(link="logit"))
summary(reref4)

# demographics, alleged maltreatment, and tract (white)
reref5 <- glm(lref ~ race3mod + gender2 + age2 + factor(ref_yr) +
                ment_ab + phys_ab + phys_neg + sex_ab + med_neg + substance_ex +
                factor(track2) + disp3 + perc_wt,
              data = rsl_screen2, family = binomial(link="logit"))
summary(reref5)

AIC(reref1, reref2, reref3, reref3b, reref4,reref5)

# ..........................................................................................
# 6. Figures ----

# track (reref4)
set.seed(1017)
pred_prob_rereftrk <- gen_qoi(rsl_screen2, "track2", reref4)
pred_prob_rereftrk # check the output
pred_prob_rereftrk <- pred_prob_rereftrk %>% 
  mutate(group = fct_relevel(group, "assess", "invest"))

# plot predicted probabilities
p1 <- ggplot(pred_prob_rereftrk, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  ylim(0.3, 0.7) +
  coord_flip() +
  scale_color_manual(values=pidlpal, guide = FALSE) +
  scale_x_discrete(labels = c("Assessment" = "Assess", "Investigation" =  "Investigate")) +
  labs(title="By Previous Track",
       x = "",
       y = "Predicted Probability of Re-Referral")
p1


# disp (reref4)
set.seed(1017)
pred_prob_rerefdisp <- gen_qoi(rsl_screen2, "disp3", reref4)
pred_prob_rerefdisp # check the output
pred_prob_rerefdisp <- pred_prob_rerefdisp %>% 
  mutate(group = fct_relevel(group, "Services", "Finding", "No Finding/Services", ))

# plot predicted probabilities
p2 <- ggplot(pred_prob_rerefdisp, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  ylim(0.3, 0.7) +
  coord_flip() +
  scale_color_manual(values=pidlpal, guide = FALSE) +
  scale_x_discrete(labels = c("No Finding/Services"="Neither", "Finding"="Finding", "Services"="Services")) +
  labs(title="By Previous Disposition",
       x = "",
       y = "Predicted Probability of Re-Referral")
p2


# race (reref4)
set.seed(1017)
pred_prob_rerefrc <- gen_qoi(rsl_screen2, "race3mod", reref4)
pred_prob_rerefrc # check the output
pred_prob_rerefrc <- pred_prob_rerefrc %>% 
  mutate(group = fct_relevel(group, "Black", "White", "Multiracial"))

# plot predicted probabilities
p3 <- ggplot(pred_prob_rerefrc, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  ylim(0.3, 0.7) +
  coord_flip() +
  scale_color_manual(values=pidlpal, guide = FALSE) +
  labs(title="By Race of Child",
       x = "",
       y = "Predicted Probability of Re-Referral")
p3


# tract pov (reref4)
set.seed(1017)
pred_prob_rerefpov <- gen_qoi(rsl_screen2, "perc_pov", reref4)
pred_prob_rerefpov # check the output
pred_prob_rerefpov <- pred_prob_rerefpov %>% 
  mutate(group = fct_relevel(group, "<10", "10-20", ">20"))

p4 <- ggplot(pred_prob_rerefpov, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  ylim(0.3, 0.7) +
  coord_flip() +
  scale_color_manual(values=pidlpal, guide = FALSE) +
  labs(title="By Tract Poverty Rate",
       x = "% Poverty", 
       y = "Predicted Probability of Re-Referral")
p4


# tract wt (reref5)
set.seed(1017)
pred_prob_rerefwt <- gen_qoi(rsl_screen2, "perc_wt", reref5)
pred_prob_rerefwt # check the output
pred_prob_rerefwt <- pred_prob_rerefwt %>% 
  mutate(group = fct_relevel(group, "<50", "50-75", ">75"))

# plot predicted probabilities
p5 <- ggplot(pred_prob_rerefwt, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  ylim(0.3, 0.7) +
  coord_flip() +
  scale_color_manual(values=pidlpal, guide = FALSE) +
  labs(title="By Tract Racial Composition",
       x = "% White", color = "Tract Race: \n% White",
       y = "Predicted Probability of Re-Referral")
p5


reref_mod <- ggarrange(p3, p4, p5, p1, p2, nrow = 5)

reref_mod_comb <- annotate_figure(reref_mod,
                               top = text_grob("Predicted Probability of Re-Referral",
                                               face = "bold", size = 15),
                               bottom = text_grob("Note: error bars are 90% credible intervals",
                                                  hjust = 1.05, x = 1, face = "italic", size = 10))

ggsave("plot/reref3.pdf", plot = reref_mod_comb, device = "pdf", width=6, height=12, units="in")


# ..........................................................................................
# 7. Table ----

# controlling for tract poverty
stargazer(reref4, reref5, 
          title = "Logit Models of Re-Referral Probability, controlling for",
          covariate.labels=c("Black", "Multiracial", "Male",
                             "Age", "Year: 2016", "Year: 2017",
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect",
                             "Alleged Sexual Abuse", "Alleged Medical Neglect", "Alleged Substance-Exposed Infant",
                             "Track: Investigate", "Disposition: Services", "Disposition: Finding",
                             "Tract: 10-20% Poverty", "Tract: More than 20% Poverty", "Tract: Unknown",
                             "Tract: 50-75% White", "Tract: Less than 50% White", "Tract: Unknown"),
          dep.var.caption = "", omit.stat = "theta",
          column.labels = c("Tract Race", "Tract Poverty"),
          type = "latex", star.cutoffs = c(0.2, 0.1, .05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)


