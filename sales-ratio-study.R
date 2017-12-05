# Sales Ratio Study.R
# By: Sandhya Kambhampati & Jason Grotto 
# ProPublica Illinois
# September 22, 2017
# PTAX - 2011 through 2015

###############################################################################
###############################################################################
# DESCRIPTION:
# This program is a sales ratio study on the property tax transfer records 
# from the Illinois Department of Revenue (IDoR). The data, known as 
# PTAX or green sheets, provides information on sales of property throughout 
# the state, including the sales price, the classification of the property and 
# details about whether the sale is and arm's length transation or some type of 
# compulsory sale. The data is self-reported.
#
# We've created a list of arm's-length property sales in Cook County between
# 2011 and 2015. We'll be using the Property Identification Numbers (PIN) 
# from this data to match against the CCAO and BOR detail files in order 
# to conduct sales ratio studies.
###############################################################################
###############################################################################

# set directory

setwd("~/Documents/projects/property-tax/commercial-analysis")

# load libraries
library(ggplot2)
library(stringr)
library(reshape)
library(tidyverse)
library(dplyr)
library(scales)


#####################################################################################################################
# I. Load data from CI Segment Aggregation.R as well as the hand-checked data for the buyer and seller relationship. 
# This data is now the final, cleaned data for both BOR and Pass 1. The code below is the analysis we did for the 
#sales ratio study presented in the story.
#####################################################################################################################
  
# Load these csvs with the flagged data in 

####################
# BOR             
#################### 
ptax_bor_sums_flags_cut <- read.csv(file="ptaxborflagsremoved.csv", header = TRUE, colClasses=
                                      c("ID"="character","NetConsideration"="numeric","DeedYear"="character","PropertyClass"="character","MinorPropertyClass"="character",
                                        "StreetAddress"="character","CityorVillage"="character","Township"="character", "ZipCode"="character","ParcelCount"="character",
                                        "County"="character","SellerName"="character","BuyerName"="character","assvalfinalsums"="numeric","MarketValuesums"="numeric","basefinalsums"="numeric","pincount"="character",
                                        "bor_valratio"="numeric", "bor_assratio"="numeric","RealPropertyClass"="character")) # all data that are going to be kept in the sales ratio study, 3373

# Divide the data into commercial vs industrial and rerun the stats
values_commercial <- c("00","35", "01","16","17","22", "23", "26", "27", "28", "29", "30", "31", "32", "33", "35","90","91", "92", "97", "99")
values_industrial <- c("50","80","81","83","87","89","93")
ptax_bor_sums_flags_cut$RealPropertyClass <-ifelse(ptax_bor_sums_flags_cut$MinorPropertyClass %in% values_commercial, "commercial","industrial")

# Run summary stats for the cut by variable
## By deed year 
results_borsumscut_deeds_flags<- arrange(ddply(ptax_bor_sums_flags_cut, "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(DeedYear))

## By PropertyClass 
results_borsumscut_property_flags<- arrange(ddply(ptax_bor_sums_flags_cut, "PropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(PropertyClass))

# By MinorPropertyClass
results_borsumscut_minorproperty_flags<- arrange(ddply(ptax_bor_sums_flags_cut, "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(MinorPropertyClass))

## By commercial 
results_borsumscut_commercial_flags<- arrange(ddply(ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "commercial"),], "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(MinorPropertyClass))

## Grab the raw data for this subset of just commercial 
ptax_bor_sums_cutcommercial_flags <- ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "commercial"),]
# That gives us 2,428 rows 

## By commercial by year 
results_borsumscut_commercialyears_flags<- arrange(ddply(ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "commercial"),], "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(DeedYear))

## graph by prd & cod  
quickplot(DeedYear,prd, data=results_borsumscut_commercialyears_flags)
quickplot(DeedYear,cod, data=results_borsumscut_commercialyears_flags)

## By industrial 
results_borsumscut_industrial_flags<- arrange(ddply(ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "industrial"),], "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(MinorPropertyClass))

## Grab the raw data for this subset of just industrial 
ptax_bor_sums_cutindustrial_flags <- ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "industrial"),]
# That gives us 945 rows 

## By industrial by year 
results_borsumscut_industrialyears_flags<- arrange(ddply(ptax_bor_sums_flags_cut[which(ptax_bor_sums_flags_cut$RealPropertyClass== "industrial"),], "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$bor_valratio), 3),
    median_ass = round(median(y$bor_assratio), 3),
    mad = round(mad(y$bor_valratio), 2),
    mean = round(mean(y$bor_valratio), 2),
    weighted = round(weighted.mean(y$bor_valratio, y$NetConsideration), 2),
    prd = round(mean(y$bor_valratio/weighted.mean(y$bor_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$bor_valratio) - y$bor_valratio)))/nrow(y))/median(y$bor_valratio)) * 100)
  )), desc(DeedYear))
---
####################
# First-pass            
#################### 

ptax_pass1_sums_flags_cut <- read.csv(file="ptaxpass1flagsremoved.csv", header = TRUE, colClasses=
                                        c("ID"="character","NetConsideration"="numeric","DeedYear"="character","PropertyClass"="character","MinorPropertyClass"="character",
                                          "assvalfinalsums"="numeric","MarketValuesums"="numeric","basefinalsums"="numeric","pincount"="character", "pass1_valratio"="numeric",
                                          "pass1_assratio"="numeric","RealPropertyClass"="character")) # all data that are going to be kept in the sales ratio study, 3366

# Divide the data into commercial vs industrial and rerun the stats
values_commercial <- c("00","35", "01","16","17","22", "23", "26", "27", "28", "29", "30", "31", "32", "33", "35","90","91", "92", "97", "99")
values_industrial <- c("50","80","81","83","87","89","93")
ptax_pass1_sums_flags_cut$RealPropertyClass <-ifelse(ptax_pass1_sums_flags_cut$MinorPropertyClass %in% values_commercial, "commercial","industrial")

# Rerun summary stats for the cut by variable
## By deed year 
results_pass1flagged_deed<- arrange(ddply(ptax_pass1_sums_flags_cut, "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(DeedYear))

## By PropertyClass 
results_pass1flagged_property<- arrange(ddply(ptax_pass1_sums_flags_cut, "PropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(PropertyClass))

## By MinorPropertyClass
results_pass1flagged_minorproperty<- arrange(ddply(ptax_pass1_sums_flags_cut, "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(MinorPropertyClass))

## By commercial 
results_pass1flagged_commercial<- arrange(ddply(ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "commercial"),], "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(MinorPropertyClass))

## Grab the raw data for this subset of just commercial 
ptax_pass1flagged_commercial <- ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "commercial"),]
# That gives us 2425 rows 

## By commercial by year 
results_pass1flagged_commercialyears<- arrange(ddply(ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "commercial"),], "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(DeedYear))

## graph by prd & cod  
quickplot(DeedYear,prd, data=results_pass1flagged_commercialyears)
quickplot(DeedYear,cod, data=results_pass1flagged_commercialyears)

## By industrial 
results_pass1flagged_industrial<- arrange(ddply(ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "industrial"),], "MinorPropertyClass", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(MinorPropertyClass))

## Grab the raw data for this subset of just industrial 
ptax_pass1flagged_industrial <- ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "industrial"),]
# That gives us 941 rows 

## By industrial by year 
results_pass1flagged_industrialyears<- arrange(ddply(ptax_pass1_sums_flags_cut[which(ptax_pass1_sums_flags_cut$RealPropertyClass== "industrial"),], "DeedYear", function(y) 
  c(N =  nrow(y),
    mv_sum = sum(y$basefinalsums),
    netcon = sum(y$NetConsideration),
    median_sales = round(median(y$pass1_valratio), 3),
    median_ass = round(median(y$pass1_assratio), 3),
    mad = round(mad(y$pass1_valratio), 2),
    mean = round(mean(y$pass1_valratio), 2),
    weighted = round(weighted.mean(y$pass1_valratio, y$NetConsideration), 2),
    prd = round(mean(y$pass1_valratio/weighted.mean(y$pass1_valratio, y$NetConsideration)), 2),
    cod = round(((sum(abs((median(y$pass1_valratio) - y$pass1_valratio)))/nrow(y))/median(y$pass1_valratio)) * 100)
  )), desc(DeedYear))