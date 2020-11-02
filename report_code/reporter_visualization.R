######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate visuals on reporter type (Section IV)
# 1. Load libraries and data
# 2. Set color palette 
# 3. Generate reporter type
# 4. Create reporter figures
# 5. Create reporter disproprotionality
# 6. Create reporter by referral type
# Authors: Sean, Shelby, Ryan, Connor, Brandon, Jessica, MPC
# Updated: July 2020 
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)
library(scales) # check palette
library(ggpubr)

# load data
# setwd("/Volumes/PIDL20/data2020") 
rsl <- readRDS("data2020/ref_supp_long.RDS")
# files created ADD ME


# ..........................................................................................
# 2. Set color palette ----
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")
show_col(pidlpal)


# ..........................................................................................
# 3. Generate race3 and reporter type ----
rsl <- rsl %>% 
  mutate(race3 = case_when(race == "Black" ~ "Black",
                           race == "White" ~ "White",
                           race == "MultiRace" ~ "Multiracial",
                           TRUE ~ NA_character_)) %>% 
  mutate(race3 = factor(race3, levels = c("Black", "White", "Multiracial"))) %>% 
  droplevels()

# mandatory/not
# professional domain/non professional relation
# professional/not
# NOTE: 1,152 reports having missing reporter type (out of 3,442 reports)

# define mandatory reporters
mandatory <- c("Court/Probation", "Day Care Provider", "Family Services Specialist", 
               "Hospital/Clinic", "Law Enforcement", "Medical Professional, Other", 
               "Physician, Private", "Public/Private Mental Health", "School- Public", 
               "School-Private", "Social Services-Private", "Social Worker", 
               "Emergency Medical Services Personnel")
unk <- c("Unknown", NA)

rsl <- rsl %>% 
  mutate(mandatory_reporter = case_when(reporter %in% mandatory ~ "Yes", 
                                        reporter %in% unk ~ "Unknown", 
                                        TRUE ~ "No"))

# define professional domain 
hc <- c("Hospital/Clinic", "Medical Professional, Other", "Physician, Private", 
        "Public/Private Mental Health", "Emergency Medical Services Personnel")
edu <- c("School- Public", "School-Private")
legal <- c("Court/Probation", "Law Enforcement")
ss <- c("Family Services Specialist", "Social Services-Private", "Social Worker", 
        "Day Care Provider", "Eligibility Worker", "Substitute Care Provider")

# NOTES: included mental health in healthcare category(only 10 reports)
#   included day care provider in social services category (only 3 reports)
#   included Substitute Care Provider in social services 
#      (includes adoptive and foster parents, as foster parents receive some
#      support for children under their care and are licensed and mandatory reporters)

rsl <- rsl %>% 
  mutate(sector = case_when(reporter %in% hc ~ "Healthcare",
                            reporter %in% edu ~ "Education",
                            reporter %in% ss ~ "Social Services",
                            reporter %in% legal ~ "Legal",
                            TRUE ~ "Non-Professional"))

# NOTE: Non-Professional includes other/unknown values, from DSS: 
#   we can view the other and unknown as "non-professional" reporters

rsl <- rsl %>% 
  mutate(prof_reporter = ifelse(sector == "Non-Professional", "Non-Professional", "Professional"))


# ..........................................................................................
# 4. Create reporter figures ----

# by prof/non
p1 <- ggplot(rsl, aes(x = prof_reporter)) + 
  geom_bar(fill = pidlpal[1]) +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25, size=3.5, 
            position = position_dodge(0.9)) + 
  labs(title="Reports by Reporter Type",
       y="# of Reports",
       x="Sector") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
p1

# by sector
p2 <- ggplot(filter(rsl, prof_reporter == "Professional"), aes(x = fct_infreq(sector))) + 
  geom_bar(fill = pidlpal[2:5]) +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25, size=3.5, 
            position = position_dodge(0.9)) + 
  labs(title="Reports by Reporter Type",
       subtitle = "Professional Sector",
       y="# of Reports",
       x="Sector") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
p2

p1p2 <- ggarrange(p1, p2,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
p1p2
ggsave("../plot/report_sector.pdf", plot = p1p2, device="pdf", width=8, height=10)

# prof/non by race
# values for figure
totals <- data.frame(rbind(prop.table(table(rsl$prof_reporter, rsl$race3), 1)))
totals <- cbind(totals, rsl %>% drop_na(race3) %>% count(prof_reporter))
totals <- totals %>% 
  mutate(labelblack = paste0(round(Black*100, 1), "%"),
         labelwhite = paste0(round(White*100, 1), "%"), 
         labelmulti = paste0(round(Multiracial*100, 1), "%"))

# figure
p3 <- rsl %>% drop_na(race3) %>% 
  ggplot(aes(x = prof_reporter, fill = race3)) + 
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pidlpal[1:3]) + 
  labs(title="Reporter Type and Race",
       y="% of Reports",
       x="Reporter Type",
       fill="Race") +
  geom_text(aes(prof_reporter, 1, label = n, fill = NULL), nudge_y = .05, data = totals) +
  geom_text(aes(prof_reporter, Multiracial, label = labelmulti, fill = NULL, vjust = 1.5), size = 3, data = totals) +
  geom_text(aes(prof_reporter, Multiracial+White, label = labelwhite, fill = NULL, vjust = 1.5), size = 3, data = totals) +
  geom_text(aes(prof_reporter, Multiracial+White+Black, label = labelblack, fill = NULL, vjust = 1.5), size = 3, data = totals) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
p3

# values for figure
totals2 <- data.frame(rbind(prop.table(table(rsl$sector, rsl$race3), 1)))
totals2 <- cbind(totals2, rsl %>% drop_na(race3) %>% count(sector))
totals2 <- totals2 %>% 
  mutate(labelblack = paste0(round(Black*100, 1), "%"),
         labelwhite = paste0(round(White*100, 1), "%"), 
         labelmulti = paste0(round(Multiracial*100, 1), "%"))

# sector by race
p4 <- rsl %>% drop_na(race3) %>% 
  ggplot(aes(x = fct_infreq(sector), fill = race3)) + 
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pidlpal[1:3]) + 
  scale_x_discrete(labels=c("Non-Professional" = "Non-\n Professional", "Education" = "Education",
                            "Legal" = "Legal", "Healthcare" = "Healthcare", "Social Services" = "Social\n Services")) +
  labs(title="Reporter Sector and Race",
       y="% of Reports",
       x="Reporter Type",
       fill="Race") +
  geom_text(aes(sector, 1, label = n, fill = NULL), nudge_y = .05, data = totals2) +
  geom_text(aes(sector, Multiracial, label = labelmulti, fill = NULL, vjust = 1.5), size = 3, data = totals2) +
  geom_text(aes(sector, Multiracial+White, label = labelwhite, fill = NULL, vjust = 1.5), size = 3, data = totals2) +
  geom_text(aes(sector, Multiracial+White+Black, label = labelblack, fill = NULL, vjust = 1.5), size = 3, data = totals2) 
p4

p3p4 <- ggarrange(p3, p4,
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)
p3p4
ggsave("../plot/report_race_sector.pdf", plot = p3p4, device="pdf", width=8, height=10)

# chi-square: prof/non
tab <- table(rsl$race3, rsl$prof_reporter)
prop.table(tab, 2)
chisq.test(tab)

# chi-square: sector
rsl_tmp <- rsl %>% filter(sector != "Non-Professional")
tab <- table(rsl_tmp$race3, rsl_tmp$sector)
prop.table(tab, 2)
chisq.test(tab)


# ..........................................................................................
# 5. Create reporter disproportionality

# Charlottesville 2014-2018 ACS data
childpop <- read.csv("data2020/cville_childpop.csv")  
childpop <- childpop %>% 
  mutate(race = recode(race, "MultiRace" = "Multiracial"))

# create summary df
rdi_report <- totals2 %>% select(Black:n) %>% 
  pivot_longer(cols = Black:Multiracial, names_to = "race3", values_to = "refprop")
rdi_report <- rdi_report %>% 
  left_join(childpop, by = c("race3" = "race"))

# generate rdi
rdi_report <- rdi_report %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe),
         race3 = factor(race3, levels = c("Black", "White", "Multiracial")),
         sector = factor(sector, levels = c("Non-Professional", "Education", 
                                            "Legal", "Healthcare", "Social Services")))

p5 <- ggplot(rdi_report, aes(x = fct_rev(race3), y = rd_mi, fill = race3)) +
  geom_col() + 
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2, color = "black") +
  geom_hline(yintercept = 1, color = "black") +
  scale_fill_manual(values = pidlpal)+
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  #expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index by Reporter Type",
       y = "", x = "", fill = "Race") +
  coord_flip() +
  facet_wrap(~sector, ncol = 1)
p5
# TBA: add value on axis, like tract rdi figs
ggsave("plot/rdi_sector.pdf", plot = p5, device="pdf", width=8, height=6)


# ..........................................................................................
# 6. Create reporter by referral type

# recode allegation variables to 0,1
var <- c(names(rsl)[29:34]) # identify allegation variables
rsl_alleg <- rsl %>% 
  select(cid, med_neg:substance_ex, reporter, race3, sector) %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))

# reshape to long
rsl_alleg_long <- rsl_alleg %>% 
  pivot_longer(cols = med_neg:substance_ex, names_to = "allege", values_to = "count") %>% 
  group_by(sector, allege) %>% 
  summarize(tot = sum(count))

rsl_alleg_long <- rsl_alleg_long %>% 
  left_join(rsl %>% 
              count(sector)) %>% 
  mutate(prop = round((tot/n)*100,1),
         label = ifelse(prop < 10, NA, prop))

rsl_alleg_long <- rsl_alleg_long %>% 
  mutate(allege = factor(allege, levels = c("phys_neg", "phys_ab", "ment_ab",
                                             "sex_ab", "med_neg", "substance_ex"),
                         labels = c("Physical Neglect", "Physical Abuse", "Mental Abuse",
                                    "Sexual Abuse", "Medical Neglect", "Substance Exposed Infrant")),
         sector = factor(sector, levels = c("Non-Professional", "Education", 
                                            "Legal", "Healthcare", "Social Services")))


# # figure
# p6 <- ggplot(rsl_alleg_long, aes(x = fct_rev(sector), y = tot, fill = fct_rev(allege))) +
#   geom_col(position = "fill") +
#   labs(title="Allegations by Reporter Type", 
#        x="Reporter Type", 
#        y="Proportion of Reports", 
#        fill="Allegation") + 
#   scale_fill_manual(values = rev(pidlpal),
#                     labels = c("Substance Exposed", "Medical Neglect",  
#                                "Sexual Abuse", "Mental Abuse", 
#                                "Physical Abuse", "Physical Neglect")) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   coord_flip()
# p6
# ggsave("../plot/report_allege.pdf", plot = p6, device="pdf", width=8, height=6)

# prof/nonprof, proportion of allegations, facet by reporter
p6 <- ggplot(rsl_alleg_long, aes(x = fct_rev(sector), y = prop, fill = sector)) + 
  geom_col() +
  scale_fill_manual(values = pidlpal) +
  labs(title="Allegations by Reporter Type", 
       x="Reporter Type", 
       y="Percent of Reports", 
       fill="Reporter Type") + 
  facet_wrap(~allege, ncol = 1) + 
  coord_flip()
p6
ggsave("plot/report_allege.pdf", plot = p6, device="pdf", width=8, height=10)

# chi-square: all
tab <- table(rsl$sector, rsl$substance_ex)
prop.table(tab, 1)
test <- chisq.test(tab)
test
round(test$residuals, 3)
round(100*test$residuals^2/test$statistic, 3)

# phys_neg: .0001, phys_ab: .0006, ment_ab: .0001, sex_ab: .7343, med_neg: .0001, substance_ex: .0001

# chi-square: prof/non
tab <- table(rsl$prof_reporter, rsl$sex_ab)
prop.table(tab, 1)
chisq.test(tab)

# phys_neg: .0111, phys_ab: .0127, ment_ab: .8571, sex_ab: .999, med_neg: .0278, substance_ex: .0001

# chi-square: sector
rsl_tmp <- rsl %>% filter(sector != "Non-Professional")
tab <- table(rsl_tmp$sector, rsl_tmp$sex_ab)
prop.table(tab, 1)
chisq.test(tab)

# phys_neg: .0001, phys_ab: .0098, ment_ab: .0001, sex_ab: 5715, med_neg: .0020, substance_ex: .0001


