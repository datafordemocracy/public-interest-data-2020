######################################################################################
# LP 5440: Public Interest Data Lab - Spring 2020
# Week 3 Examples: visualization with ggplot
######################################################################################

# ..........................................................................................

# 1. Load libraries and data ----

# load libraries
library(tidyverse) # ggplot is in the tidyverse
library(RColorBrewer) # for colors for visualization

# set working directory
setwd("/Volumes/PIDL20/data2020") 

# read in data
rsl <- readRDS("ref_supp_long.RDS")

# ..........................................................................................

# 2. Basic ggplot syntax  ----

# all ggplot graphs start with ggplot(). the data is called first, and aes() comes
# after a comma and specifies which variable(s) we are using from the data frame
# (aes stands for aesthetics). we then add to that structure by using a + sign 
# to add more to the plot 

# let's start with a basic bar plot of race
ggplot(rsl, aes(race)) # just sets a base
ggplot(rsl, aes(race)) + geom_bar() # adds bars with counts on top
# this graph now gives us counts of children of each race in foster care

# now let's make a histogram
ggplot(rsl, aes(numref)) + geom_histogram(binwidth = 1) 
# 'binwidth' is an argument to geom_histogram which specifies bin size
# each geom_ function takes different arguments

# to check out all the ggplot types, type geom_ and a list will pop up


# ..........................................................................................

# 3. Colors, fills, facets ----

# use 'color' if it's geom_line or geom_point
# use 'fill' if it's geom_bar, geom_histogram or any area plot

# let's add colors to our bar plot
ggplot(rsl, aes(race, fill=race)) + geom_bar()
# nothing changes about the structure bc race is already in the plot
# sometimes we do this to add color to basic plots

# but if we add gender, we get more information
ggplot(rsl, aes(race, fill=gender)) + geom_bar()

# the default is to stack the fill within the bar, that's hard to compare
ggplot(rsl, aes(race, fill=gender)) + geom_bar(position="fill")
# position='dodge' breaks out gender side by side

# if we add race to our histogram
ggplot(rsl, aes(numref, fill=race)) + geom_histogram(binwidth = 1)
# we can now see histograms of number of referrals by race

# in order to make this easier to compare, let's add more specifications
ggplot(rsl, aes(numref, fill=race)) + 
  geom_histogram(binwidth = 1, 
                 position = "identity", # put the bars over each other, not stacked
                 alpha=0.5) # make this 50% transparent
# move new additions to the next line so it's easy to see

# still kinda hard to read - let's give each histogram more space with facet_wrap()
ggplot(rsl, aes(numref, fill=race)) + 
  geom_histogram(binwidth = 1, 
                 position = "identity", # put the bars over each other, not stacked
                 alpha=0.5) +
  facet_wrap(~race, scales="free") # gives each racial categories its own plot

# note: data frame names and variable names are not in quotes, but argument responses are
ggplot(rsl, aes(race, fill="race")) + geom_bar() # variable names shouldn't be in quotes

# say we wanted to get an unduplicated distribution of number of referrals
# let's count how many children belong to each race-gender pair
arb <- rsl %>% group_by(race, gender) %>% count()
# and let's plot those results
ggplot(arb, aes(gender, n, fill=race)) + geom_bar() #oh no - error!
# Error: stat_count() must not be used with a y aesthetic.

# if we're graphing pre-determined quantities, we need to use stat_summary instead
ggplot(arb, aes(gender, n, fill=race)) + 
  stat_summary(geom="bar", fun.y="sum", position = "dodge")

arb <- rsl %>% group_by(race) %>% count()

ggplot(arb, aes(reorder(race, -n), n, fill=race)) + 
  stat_summary(geom="bar", fun.y="sum", position = "dodge")


# ..........................................................................................

# 4. Labels, titles, etc. ----

# we add titles, axis labels, and legend titles with labs()
ggplot(rsl, aes(race, fill=gender)) + 
  geom_bar(position="dodge", color="black") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services")

# other useful functions:
# coord_flip() flips the axes
ggplot(rsl, aes(race, fill=gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services") + coord_flip()

# scale_fill_manual() allows us to pick colors manually
ggplot(rsl, aes(race, fill=gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services") + 
  scale_fill_manual(values=brewer.pal(9,"Blues")[c(3,5,7)])
# we're loading the "blues" color pallette with 9 levels, and using the 3rd, 5th and 7th

# ..........................................................................................

# 5. Saving Plots ----

# we can use pdf() before the plot code and dev.off() after the plot code to save plots
pdf("race-gender-counts.pdf", width=8, height=5)
ggplot(rsl, aes(race, fill=gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services") + 
  scale_fill_manual(values=brewer.pal(9,"Blues")[c(3,5,7)])
dev.off() # I use this one because it can be extended to do other useful things like font embeds

# we can also use ggsave() which does very similar things with only one function at the end
ggplot(rsl, aes(race, fill=gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services") + 
  scale_fill_manual(values=brewer.pal(9,"Blues")[c(3,5,7)])
ggsave("race-gender-counts.pdf", device="pdf", width=8, height=5)

# we can also save plots manually with the Export tab in the R Studio plot viewer
# for this project, we will save all plots as PDFs because they are the most compatible 
# and scalable with LaTeX and Overleaf - which will we use later for the report. 

# ..........................................................................................

# go forth and visualize!
