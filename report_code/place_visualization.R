######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate visuals on data grouped by census tract (Section III)
# 1. Load libraries and data
# 2. Set color palette 
# 3. Create Population vs Referral graphics 
# 4. Graph RDI for perc_pov and perc_wt
# 5. Create data frame of allegation type by perc_pov, perc_wt 
# 6. Create allegation figures 
# Updated: October 2020 (Hannah, mpc)
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----

# install.packages("tidycensus")
library(tidycensus)
library(scales) # check palette
library(tidyverse)

setwd("/Volumes/NO NAME")

# load data
acs <- load("data2020/cville_acs.Rdata")
# files generated in acstracts_prep.R


# ..........................................................................................
# 2. Set color palette ----
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")
show_col(pidlpal)


# ..........................................................................................
# 3. Create Population vs Referral graphics ----
# relevel race in acs
acs_ref <- acs_ref %>% 
  mutate(race = fct_collapse(race, "Other" = c("Other", "Hispanic", "Asian"))) %>%
  mutate(race = factor(race, levels = c("Black", "White", "MultiRace", "Other"))) %>% 
  filter(!(is.na(race)))

acs_ref <- acs_ref %>%
  group_by(source, race) %>%
  summarize(count = sum(number), moe = moe_sum(moe, number)) %>%
  ungroup() 

acs_ref <- acs_ref %>%
  group_by(source) %>%
  mutate(total = sum(count), total_moe = moe_sum(moe, count), 
         perc = percent(count/total), perc_moe = moe_ratio(count, total, moe, total_moe))

acs_ref <- acs_ref %>% 
  mutate(race = fct_recode(race, Remaining = "Other",
                           Multiracial = "MultiRace"))

# stacked barplot for comparison
g <- acs_ref %>%
  ggplot(aes(y = count, x = source)) +
  geom_bar(stat = "identity", aes(fill=race), position = "fill") +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2014-2018 American Community Survey",
       y = "", x = "", fill = "Race") + 
  scale_fill_manual(values=pidlpal) +
  scale_x_discrete(labels=c("acs17" = "2014-2018 Population", "ref" = "2016-2018 Referrals")) +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 1, y = c(.13, .2, .69, .96), 
           label = c("17.1 (± 3.7) ", "6.1 (± 2.5)", "50.5 (± 3.7)", "26.3 (± 3.7)"), color = "black") + 
  annotate("text", x = 2, y = c(.06, .17, .4, .96), 
           label = c("9.1", "10.9", "24.0", "56.0"), color = "black")
g

ggsave("plot/2018_cville_prop.pdf", width=9, height=6, units="in") 


# ..........................................................................................
# 4. Graph RDI by perc_pov and perc_wt ----
## create data table of child population by perc_wt categories
pop_by_wt <- tract_pop_all %>%
  filter(race != "Remaining") %>% 
  select(perc_wt, race, pop, pop_moe) %>%
  group_by(perc_wt, race) %>%
  summarize(count = sum(pop), count_moe = moe_sum(pop_moe, pop)) %>%
  group_by(perc_wt) %>%
  mutate(pop = sum(count), pop_moe = moe_sum(count_moe, count)) %>%
  mutate(prop = count/pop, pmoe = (moe_prop(count, pop, count_moe, pop_moe))) %>%
  select(-c(pop, pop_moe))

## create data table of child population by perc_pov categories
pop_by_pov <- tract_pop_all %>%
  filter(race != "Remaining") %>% 
  select(perc_pov, race, pop, pop_moe) %>%
  group_by(perc_pov, race) %>%
  summarize(count = sum(pop), count_moe = moe_sum(pop_moe, pop)) %>%
  group_by(perc_pov) %>%
  mutate(pop = sum(count), pop_moe = moe_sum(count_moe, count)) %>%
  mutate(prop = count/pop, pmoe = (moe_prop(count, pop, count_moe, pop_moe))) %>%
  select(-c(pop, pop_moe))

#   ratio of % of [race of] children referred to CPS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion

## create long data frame of proportions
cville_prop_by_wt <- left_join(pop_by_wt, by_wt)
cville_prop_by_pov <- left_join(pop_by_pov, by_pov)

# generate intervals
ref_acs_by_wt <- cville_prop_by_wt %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe))

ref_acs_by_pov <- cville_prop_by_pov %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe))

ref_acs_by_wt2 <- ref_acs_by_wt %>% 
  arrange(match(race, c("White", "Black", "Multiracial", "Other"))) %>% 
  mutate(race = factor(race, levels = c("Black", "White", "Multiracial"))) %>%
  ungroup() %>%
  mutate(perc_pov = factor(perc_wt, levels = c(">50", "50-75", "<75")),
         rd_mi = round(rd_mi, 2)) %>% 
  droplevels()

ref_acs_by_pov2 <- ref_acs_by_pov %>% 
  arrange(match(race, c("White", "Black", "Multiracial", "Other"))) %>% 
  mutate(race = factor(race, levels = c("Black", "White", "Multiracial"))) %>%
  ungroup() %>%
  mutate(perc_pov = factor(perc_pov, levels = c(">20", "10-20", "<10")),
         rd_mi = round(rd_mi, 2)) %>% 
  droplevels()

# visualize
g <- ggplot(ref_acs_by_pov2, aes(x=fct_rev(race), y = rd_mi, fill = race, color=race)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~perc_pov) +
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2, color = "black") +
  geom_hline(yintercept = 1, color = "black") +
  geom_text(aes(x = fct_rev(race), y= ifelse(race == "White", 1.4, ifelse(race == "Multiracial" & perc_pov == "<10", 4, 0.75)), label = rd_mi), color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.75, 1, 1.5, 2, 3, 5, 10), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.75", "1", "1.5", "2", "3", "5", "10")) +
  coord_flip() + 
  scale_color_manual(values = pidlpal, guide = FALSE) +
  scale_fill_manual(values = pidlpal) +
  expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Graphs are split by the percentage of residents living in poverty",
       y = "", x = "", fill = "Race") 
g
ggsave("plot/RDI_pov.pdf", width=10, height=4, units="in")

# rdi_tb_pov <- ref_acs_by_pov2 %>% 
#   arrange(perc_pov) %>%
#   select("Percent in Poverty" = perc_pov, "Race" = race, "Disproportionality Index" = rd_mi) %>%
#   kable() %>%
#   kable_styling() %>%
#   collapse_rows(columns = 1:2, valign = "top")
# rdi_tb_pov

g <- ggplot(ref_acs_by_wt2, aes(x=fct_rev(race), y = rd_mi, fill = race, color=race)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2, color = "black") +
  geom_hline(yintercept = 1, color = "black") +
  geom_text(aes(x = fct_rev(race), y= ifelse(race == "White", 1.3, 0.75), label = rd_mi), color = "black") + 
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.75, 1, 1.5, 2, 3, 5, 10), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.75", "1", "1.5", "2", "3", "5", "10")) +
  coord_flip() + 
  scale_color_manual(values = pidlpal, guide = FALSE) +
  scale_fill_manual(values = pidlpal) +
  expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Graphs are split by the percentage of non-hispanic white residents",
       y = "", x = "", fill = "Race") +
  facet_wrap(~perc_wt)
g
ggsave("plot/RDI_wt.pdf", width=10, height=4, units="in")


# ..........................................................................................
# 5. Create data frame of allegation type by perc_pov, perc_wt ----
# par down CPS data to fips_code data, limit observations to one per child
rsl2 <- distinct(ref, cid, .keep_all = TRUE)

rsl_tract <- rsl2 %>%
  select(cid, race2, fips_ref, tract_ref, geoid_ref, tract_name, refnum, "phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex") %>%
  filter(!is.na(fips_ref)) %>%
  filter(fips_ref == 540) %>%
  mutate(fips_ref = as.factor(fips_ref)) %>%
  mutate(
    phys_ab = ifelse(phys_ab=="Yes", 1, 0),
    phys_neg = ifelse(phys_neg=="Yes", 1, 0),
    med_neg = ifelse(med_neg=="Yes", 1, 0),
    ment_ab = ifelse(ment_ab=="Yes", 1, 0),
    sex_ab = ifelse(sex_ab=="Yes", 1, 0),
    substance_ex = ifelse(substance_ex=="Yes", 1, 0))

rsl_tract <- rsl_tract %>%
  mutate(perc_wt = fct_collapse(rsl_tract$tract_name, 
                                ">75" = c("Census Tract 9", "Census Tract 10", "Census Tract 4.02", "Census Tract 7"),
                                "50-75" = c("Census Tract 3.02", "Census Tract 5.02", "Census Tract 2.01", 
                                            "Census Tract 2.02", "Census Tract 6", "Census Tract 8"),
                                "<50" = c("Census Tract 4.01", "Census Tract 5.01")),
         perc_pov = fct_collapse(rsl_tract$tract_name,
                                 ">20" = c("Census Tract 2.02", "Census Tract 6", "Census Tract 4.01", 
                                           "Census Tract 2.01", "Census Tract 5.01"),
                                 "10-20" = c("Census Tract 7", "Census Tract 10", "Census Tract 8", 
                                             "Census Tract 4.02"),
                                 "<10" = c("Census Tract 9", "Census Tract 3.02", "Census Tract 5.02"))) 
#total
total_alleg <- rsl_tract %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="total", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

## perc_pov
g20_alleg <- rsl_tract %>%
  filter(perc_pov == ">20") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="g20", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

m1020_alleg <- rsl_tract %>%
  filter(perc_pov == "10-20") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="m1020", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

l10_alleg <- rsl_tract %>%
  filter(perc_pov == "<10") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="l10", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

## perc_wt
g75_alleg <- rsl_tract %>%
  filter(perc_wt == ">75") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="g75", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

m5075_alleg <- rsl_tract %>%
  filter(perc_wt == "50-75") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="m5075", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

l50_alleg <- rsl_tract %>%
  filter(perc_wt == "<50") %>%
  select(c("phys_ab", "phys_neg", "med_neg", "ment_ab", "sex_ab", "substance_ex" )) %>%
  summarise_all(sum) %>%gather(key="case_type", value="l50", phys_ab, phys_neg, med_neg, ment_ab, sex_ab, substance_ex)

alleg_cat_pov <- left_join(total_alleg, g20_alleg) %>%
  left_join(m1020_alleg) %>%
  left_join(l10_alleg)
names(alleg_cat_pov) <- c("Allegation Type", "Total", "Greater than 20%", "Between 10% and 20%", "Less than 10%")

chisq.test(alleg_cat_pov[,3:5])
round(chisq.test(alleg_cat_pov[,3:5])$residuals, 3)

alleg_cat_pov <- gather(alleg_cat_pov, 'Total', "Greater than 20%", "Between 10% and 20%", "Less than 10%", key = "category", value = "count") %>%
  mutate(category = as.factor(category)) %>%
  mutate(category = fct_relevel(category, "Total", "Greater than 20%", "Between 10% and 20%", "Less than 10%"))

alleg_cat_wt <- left_join(total_alleg, g75_alleg) %>%
  left_join(m5075_alleg) %>%
  left_join(l50_alleg)
names(alleg_cat_wt) <- c("Allegation Type", "Total", "Greater than 75%", "Between 50% and 75%", "Less than 50%")

chisq.test(alleg_cat_wt[,3:5])

alleg_cat_wt <- gather(alleg_cat_wt, 'Total', "Greater than 75%", "Between 50% and 75%", "Less than 50%", key = "category", value = "count") %>%
  mutate(category = as.factor(category)) %>%
  mutate(category = fct_relevel(category, "Total", "Less than 50%", "Between 50% and 75%", "Greater than 75%"))


# ..........................................................................................
# 6. Create Allegation Figures ----
# Allegation by Percent White
wt_alleg <- ggplot(data = alleg_cat_wt, aes(x = category, y = count, fill = `Allegation Type`)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values = pidlpal, 
                    breaks=c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", "substance_ex"),
                    labels=c("Medical Neglect", "Mental Abuse", "Physical Abuse", 
                             "Physical Neglect", "Sexual Abuse", "Substance Exposure")) +
  labs(title = "Allegation Type by Percent White Census Tract Groupings") +
  xlab("Percentage of Census Tract Residents that are \nnon-Hispanic White") +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
wt_alleg

# Allegation by Percent Poverty 
pov_alleg <- ggplot(data = alleg_cat_pov, aes(x = category, y = count, fill = `Allegation Type`)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values = pidlpal, 
                    breaks=c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", "substance_ex"),
                    labels=c("Medical Neglect", "Mental Abuse", "Physical Abuse", 
                             "Physical Neglect", "Sexual Abuse", "Substance Exposure")) +
  labs(title = "Allegation Type by Percent Poverty Census Tract Groupings") +
  xlab("Percentage of Census Tract Residents that \nlive at or below the Federal Poverty Line") +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pov_alleg




