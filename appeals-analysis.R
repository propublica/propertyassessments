# Appeals Analysis.R
# By: Sandhya Kambhampati & Jason Grotto 
# ProPublica Illinois
# October 16, 2017
--
#####################################################################################################################################
# Description: This program analyzes property tax appeals data from the 
# Cook County Assessor's Office (CCAO) to see which firms filed the most appeals 
# for commercial and industrial properties in Cook County from 2011 to 2016. 
#
# The 3.8 million records from appeals0316.csv contained many spelling and typographical errors, 
# was standardized using regular expressions and data cleaning tools in R, followed by extensive fact-checking and hand checks.
# 
# Below are the queries used for the analysis in the story. 
#######################################################################################################################################

#Set working directory 
setwd("~/Documents/projects/property-tax/appeals")

# load libraries
library(stringr)
library(reshape)
library(tidyverse)
library(dplyr)

# read in appeals data for just Berrios years 
berrios <- read.csv(file="berrios.csv", header = TRUE,
                    colClasses = c("taxyear"="character","pin"="character","class"="character","docket"="character","name"="character",
                                   "attny_code"="character","prior_av"="numeric", "prop_av"="numeric","final_av"="numeric",
                                   "total_av"="numeric","house_no"="character","dir"= "character", "str_name"="character",
                                   "str_suffix"="character","city"="character","zipcode"="character", "majclass_descr"= "character",
                                   "ass_win"= "numeric", "bor_win"= "numeric","win"= "numeric", "ass_reduction"= "numeric", 
                                   "bor_reduction"="numeric", "win_reduction"="numeric","nameclean"="character", "firm_name"= "character")#251797

# Analysis for story                                  

firms <- berrios %>%
  group_by(firm_name) %>%
  summarize( prop_av = sum(prop_av),reduction = sum(ass_reduction),count = n()) # this is the data for the table in the story  

# Other example queries 

# By attorneys
individuals <- berrios %>%
  group_by(nameclean) %>%
  summarize(count = n(), sum_total_av = sum(total_av),reduction = sum(ass_reduction), prop_av = sum(prop_av))
                    
# By asssor wins 
wins_ass_berrios <- berrios  %>%
    group_by(taxyear,nameclean) %>%
    summarise (count = n(), reduction = sum(ass_reduction),win = sum(ass_win)) %>%
    arrange(desc(win))   

# By bor wins
wins_bor_berrios <- berrios  %>%
  group_by(taxyear,nameclean) %>%
  summarise (count = n(), reduction = sum(bor_reduction),win = sum(bor_win)) %>%
  arrange(desc(win))