######################################################################################
# LP 5440: Public Interest Data Lab - Spring 2020
# Week 3 Examples: visualization with ggplot
######################################################################################

# ..........................................................................................

# 1. Load libraries and data ----

# load libraries
library(tidyverse) # ggplot is in the tidyverse

# set working directory
setwd("/Volumes/PIDL19") 

# load data
load("cps_clean.RData") 
foster <- readRDS("foster_clean.rds")
# files created in cps_clean.R & fc_clean.R

# ..........................................................................................

# 2. Basic ggplot syntax  ----

# all ggplot graphs start with ggplot(). the data is called first, and aes() comes
# after a comma and specifies which variable(s) we are using from the data frame
# (aes stands for aesthetics). we then add to that structure by using a + sign 
# to add more to the plot 

# let's start with a basic bar plot of race
ggplot(fc, aes(race)) # just sets a base
ggplot(fc, aes(race)) + geom_bar() # adds bars with counts on top
# this graph now gives us counts of children of each race in foster care

# now let's make a histogram
ggplot(dss, aes(numref)) + geom_histogram(binwidth = 1)
# 'binwidth' is an argument to geom_histogram which specifies bin size
# each geom_ function takes different arguments

# to check out all the ggplot types, type geom_ and a list will pop up

# ..........................................................................................

# 3. Colors, fills, facets ----

# use 'color' if it's geom_line or geom_point
# use 'fill' if it's geom_bar, geom_histogram or any area plot

# let's add colors to our bar plot
ggplot(fc, aes(race, fill=race)) + geom_bar()
# nothing changes about the structure bc race is already in the plot
# sometimes we do this to add color to basic plots

# but if we add gender, we get more information
ggplot(fc, aes(race, fill=gender)) + geom_bar()

# the default is to stack the fill within the bar, that's hard to compare
ggplot(fc, aes(race, fill=gender)) + geom_bar(position="dodge")
# position='dodge' breaks out gender side by side

# if we add race to our histogram
ggplot(dss, aes(numref, fill=race)) + geom_histogram(binwidth = 1)
# we can now see histograms of number of referrals by race

# in order to make this easier to compare, let's add more specifications
ggplot(dss, aes(numref, fill=race)) + 
  geom_histogram(binwidth = 1, 
                 position = "identity", # put the bars over each other, not stacked
                 alpha=0.5) # make this 50% transparent
# move new additions to the next line so it's easy to see

# still kinda hard to read - let's give each histogram more space with facet_wrap()
ggplot(dss, aes(numref, fill=race)) + 
  geom_histogram(binwidth = 1, 
                 position = "identity", # put the bars over each other, not stacked
                 alpha=0.5) +
  facet_wrap(~race, scales="free") # gives each racial categories its own plot

# note: data frame names and variable names are not in quotes, but argument responses are
ggplot(fc, aes(race, fill="race")) + geom_bar() # variable names shouldn't be in quotes

# ..........................................................................................

# 4. Labels, titles, etc. 

# we add titles, axis labels, and legend titles with labs()
ggplot(fc, aes(race, fill=gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Children in Foster Care by Race & Gender", 
       subtitle="More black boys than black girls in foster care", 
       x="Race", 
       y="Number of Children in Foster Care", 
       fill="Gender", 
       caption="Data from the Charlottesville Department of Social Services")

# other useful functions:
# coord_flip() flips the axes
# scale_fill_manual() allows us to pick colors manually


# ..........................................................................................

# go forth and visualize!

