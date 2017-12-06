# First-pass analysis.R
# By: Jason Grotto & Sandhya Kambhampati 
# ProPublica Illinois
# Aug. 29, 2017

###############################################################################
# Description: This program analyzes property tax asssessment data from the 
# Cook County Assessor's Office (CCAO) to determine if there are parcels (PINs)
# that  have identical assessed values over multiple reassessment periods 
# between 2008 and 2015. The same scripts were also used to look at data under 
# the prior assessor Houlihan.
#
# The analysis focuses on PINs that have not changed during the study period.
# Because Cook County does not keep track of PIN changes, it's nearly impossible 
# to analyze PINs that have changed over time. 
#
# There are three assessment "passes" for each reassessment. The Pass 1 is the 
# CCAO's initial valuation. Property owners can appeal to the assessor's office
# to lower that initial valuation. Results from the appeal are called Pass 2. 
# If property owners are still not satisfied, they can appeal to the Board of
# review. Results from that appeal are final. 
#
# This analysis focuses on Pass 1 values, since that is the assessor's initial
# valuation. It is also the value that the assessor's office uses as a 
# comparison when property owners appeal to the assessor (Pass 2). Therefore, 
# this program examines the Pass 1 values in order to see if the assessor is 
# actually valuing commercial and industrial properties on Pass 1. s
###############################################################################

# set directory
setwd("~/Documents/projects/property-tax/commercial-analysis")

# load libraries 
library(scales)
library(dplyr)
library(sqldf)
library (tidyverse)

###############################################################################
# I. Load data from CI Segment Aggregation.R
###############################################################################
# read-in data
bor_ci_final <- read.csv(file="bor_ci_final.csv", header = TRUE, colClasses=
                           c("pin_taxyear"="character","pin"="character",
                             "TaxYear"="character","repro"="numeric",
                             "totalval"="numeric","assval"="numeric",
                             "market"="numeric","repro_val"="numeric",
                             "market_val"="numeric","ftfar_match"="numeric",
                             "land_repro"="numeric","land_totalval"="character",
                             "land_assval"="numeric","land_market"="numeric",
                             "land_market_val"="numeric","land_repro_val"="numeric",
                             "land_total"="numeric","assval_final"="numeric",
                             "base_final"="numeric","market_final"="numeric",
                             "keypin2"="character","market_base_diff"="numeric",
                             "assessed_diff"="numeric","market_base_pct"="numeric"))


pass1_ci_final <- read.csv(file="pass1_ci_final.csv", header = TRUE, colClasses=
                             c("pin_taxyear"="character","pin"="character",
                               "TaxYear"="character","repro"="numeric",
                               "totalval"="numeric","assval"="numeric",
                               "market"="numeric","repro_val"="numeric",
                               "market_val"="numeric","ftfar_match"="numeric",
                               "land_repro"="numeric","land_totalval"="character",
                               "land_assval"="numeric","land_market"="numeric",
                               "land_market_val"="numeric","land_repro_val"="numeric",
                               "land_total"="numeric","assval_final"="numeric",
                               "base_final"="numeric","market_final"="numeric",
                               "keypin2"="character","market_base_diff"="numeric",
                               "assessed_diff"="numeric"))

###############################################################################
# I. Top-level summary statistics
###############################################################################

# examine changes in value by year - this comes from bor_ci_final because it 
#   has the detailed reduction amounts
reductions <- ddply(bor_ci_final, "TaxYear", function(x) 
  c(count=format(nrow(x), big.mark = ','),
    wins = format(nrow(x[which(x$market_base_pct > 0),]), big.mark = ','),
    win_pct = round((nrow(x[which(x$market_base_pct > 0),])/nrow(x)) * 100,2),
    reduction_pct = round(mean(x[which(x$market_base_pct > 0),]$market_base_pct),2),
    total_reduct_market = dollar_format()(sum(x[which(x$market_base_pct > 0),]$market_base_diff)),
    total_reduct_assessed = dollar_format()(round(sum(x[which(x$market_base_pct > 0),]$assessed_diff
    )))))

# calculate number of PINs with same assessed values and percentages over two 
#   and three reassessment years
sum_not_assessing <- ddply(not_assessing_final_geo_class, "TRI", function(x) 
  c(count = nrow(x), 
    hit2 = sum(x$hit2),
    pct_hit2 = round((sum(x$hit2)/nrow(x))*100,2), 
    hit3 = sum(x$hit3),
    pct_hit3 = round((sum(x$hit3)/nrow(x))*100,2)))

#//////////////////////////////////////////////////////////////////////////////
# NOTE: This is the key finding from the first cut of the data. The CCAO 
# reassesses properties every three years. The office does this by dividing the 
# county up into three triennial areas and valuing one each year. 
#
# The results here show that for triennial area 1 (City of Chicago) 27,161 PINs
# had the same reassessed values over two reassessment periods. That's 67 
# percent of our universe. More than 9,000 PINs had the same values over three
# triennial periods, which are the base assessement values over nine years. 
# That's 23 percent of our universe.
#//////////////////////////////////////////////////////////////////////////////

# rank non-reassessments by class descriptions
rank_class <- arrange(ddply(not_assessing_final_geo_class, "majclass", function(x) 
  c(count = format(nrow(x), big.mark = ','), 
    hit2 = format(sum(x$hit2), big.mark = ','),
    pct_hit2 = round((sum(x$hit2)/nrow(x))*100,2), 
    hit3 = format(sum(x$hit3), big.mark = ','),
    pct_hit3 = round((sum(x$hit3)/nrow(x))*100,2))), desc(pct_hit2))

#//////////////////////////////////////////////////////////////////////////////
# NOTE: This shows most of the not-assessed PINs are commercial properties.
#/////////////////////////////////////////////////////////////////////////////
