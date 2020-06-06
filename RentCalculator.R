##################################################################################################
# PROGRAMMING PROJECT, PROGRAMMING IN FINANCE II, SUMMERSEMESTER 2020, 
# WITH PROF. PETER H. GRUBER AT UNIVERSITA DELLA SVIZZERA ITALIANA
# authors: Ruth Peterhans ruth.maria.peterhans@usi.ch
#          Giuliano Giovanardi, giuliano.giovanardi@usi.ch
##################################################################################################

##################################################################################################
# CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND)
# The package RentCalculatoR calculates the change in rent of residential real estate according to Swiss law. 
# Changes in value-adding investments, depreciation, interest and reference interest rates can be 
# taken into account.
##################################################################################################

##################################################################################################
# REQUIRED INFORMATION
##################################################################################################
# For the calculation the function needs following information (variables):

# 1) The following information (variables) are taken from the system or from scraping:
#    They are coded in PART A.
#    - current month and year for the calculation on inflation and general cost increases
#    - current mortgage rent (mortgage, in %); from https://www.bwo.admin.ch/bwo/de/home/
#      mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html
#    - information about the inflation from xlsx-file on https://www.bfs.admin.ch/bfsstatic/
#     dam/assets/12827290/master/

# 2) For changings due to investments:
#    They are coded in PART B.
#    - actual net rent (act_rent, integer)
#    - total investment (investment, integer)
#    - value-increasing share (increasing_share, in %)
#    - lifespan (lifespan, in years)
#    - maintenance allowance (maintenance, 10 % by default due to a high court judgment)

# 3) For changing due to change in the market circumstances: 
#    They are coded in PART C.
#    - actual net rent (act_rent, integer)
#    - month and year of the last rent adjustment on reference rate (last_ref_rate_adj, date-format)
#    - month and year of the next termination date of the contract (termination, date-format)
#    - month and year of the last inflation adjustment (last_inflation_adj, date-format)
#    - flat rate for general cost increase (flat_rate_cost, from 0 to 1 %) 
#    - month and year of th last cost increase adjustment (last_cost_adj, date-format)


##################################################################################################
# - I would split of the tasks to 3 different parts (functions)
#   1) function to get the data from internet and the system (PART A)
#   2) function to calculate the change in rent with investion (PART B)
#   3) function to calculate the change in rent with change in circumstances (PART C)
#


# THIS ARE THE PACKAGES THAT ARE NEED: They will be  installed in case you dont have them

if ("xml2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xml2")
}
library(xml2)

############# repeat this for all the following packages maybe with a vector?

# install.packages(xml2, rvest, magrittr, tydyr)
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages(„readxl“)
# install.packages("xlsx", dependencies = FALSE)

library (readxl) # to read xls
library(rvest)
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(parse)
library(xlsx)

##################################################################################################
#
#  PART A: RentInformations(act_mortgage, act_date, infl_rate)
#
##################################################################################################

RentInformations <- function(){

  #############################
  # MORTGAGE
  #############################

  # get the mortgage rates from the website of the statistics of the government 
  mortgage_url<- "https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html"
  mortgage_urlHtml <- mortgage_url %>% read_html()
  mortgage_data <- mortgage_urlHtml %>% html_table(header=TRUE) %>% as.data.frame
  
  # clean up the data: set column names, define data types
  mortgage_data_clean <- data.frame(mortgage_data[,(1:2)])
  colnames(mortgage_data_clean) <- c("mortgage_rate", "valuable_from")
  mortgage_data_clean$mortgage_rate <- parse_number(mortgage_data_clean$mortgage_rate, locale = locale(decimal_mark=","))
  mortgage_data_clean$valuable_from <- strptime(mortgage_data_clean$valuable_from, format="%d.%m.%Y")
  mortgage_data_clean$valuable_from <- as.Date(mortgage_data_clean$valuable_from)
  
  # clean the line with additional Information, because the different way in measuring is not relevant to the calculations
  mortgage_data_clean <- mortgage_data_clean[complete.cases(mortgage_data_clean),]
  act_mortgage <- mortgage_data_clean$mortgage_rate[1]
  
  #############################
  # ACTUAL DATE
  #############################
  act_date <- as.character(Sys.Date())

  #############################
  # INFLATION RATE
  #############################
  # Inflation index from as xlsx download of the department for statistics
  # https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master
  
  ########################  ########################  ########################  ########################
  # notes:
  # @ Giuliano: Here I'm still strugeling because the xls-file has different register-tabs. 
  ########################  ########################  ########################  ########################
  
  #####################
  # Temporary Solution - reading the file from my workstation
  library(readxl)
  cc_d_05_02_08_1_ <- read_excel("Documents/UniLu/USI/USI-Master/USI-MasterSP20/Programming II/Programming_II-Projects/Rent-changing-calculator/cc-d-05.02.08 (1).xlsx", 
                                 sheet = "2015")
  inflation_base2015 <- cc_d_05_02_08_1_
  # Finish temporary Solution
  #####################

  
  #####################  
  # clean the spreadsheet of 2015 indices
  #####################
  inflation_clean <- data.frame(inflation_base2015[,(1:13)])
  colnames(inflation_clean) <- c("year", "-01-01", "-02-01", "-03-01", "-04-01", "-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01", "-12-01")
  inflation_clean$year <- as.Date.character(inflation_clean$year, format = "%Y")
  inflation_clean$year <- as.numeric(format(inflation_clean$year, "%Y"))

  # take only cases where year has a value and sort from actual to past years
  years_only <- complete.cases(inflation_clean$year)
  inflation_clean <- cbind(years_only, inflation_clean)
  inflation_clean <- subset(inflation_clean, years_only == TRUE)
  inflation_clean <- inflation_clean [,(2:14)]
  inflation_clean <- inflation_clean[order(inflation_clean$year, decreasing = TRUE),] 
  
  # actual inflation rate
  act_inflation_rate <- inflation_clean[1,]
  act_inflation_rate <- gather(act_inflation_rate)
  act_inflation_rate <- subset(act_inflation_rate, !is.na(act_inflation_rate$value))
  act_inflation_rate <- as.character(act_inflation_rate$value)
  act_inflation_rate <- as.numeric(dplyr::last(act_inflation_rate))
  
    
  #############################
  # PRINT
  #############################
  # overview of the results and report
  actual_data <- c(act_mortgage, act_date, act_inflation_rate)
  actual_answers <- c("The actual mortgage rate in % is:", "The actual date is:", 
                    "And the actual inflation rate is (points on 2015 basis)")
  result <- as.table(cbind(actual_answers, actual_data))
  print(result)

  }

 

  ##################################################################################################
  #
  #  PART B: RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
  #
  ##################################################################################################
  
  ########################
  # This I need only for testing the calculations. After they work we can delete it
  #################
  #act_rent_CHF <- 1000
  #investment_CHF <- 100000
  #increasing_share <- 50
  #lifespan <- 12
  #maintenance_rate <- 10
  #RentInvestCalculatoR(1000,100000,50,15)
  ########################
  
  RentInvestCalculatoR <- function(act_rent_CHF, investment_CHF, increasing_share, 
                                   lifespan, maintenance_rate=10){
  
  value_incr_share_CHF <- investment_CHF/100*increasing_share
  depreciation_CHF <- value_incr_share_CHF/lifespan 
  
  # the allowed interest by law is 0.5 % above the actual mortgage rate (divided by 2 parties)
  act_mortgage <- mortgage_data_clean$mortgage_rate[1]
  interest <- (act_mortgage+0.5)/2
  interest_CHF <- interest/100*value_incr_share_CHF
  incr_rent_CHF <- depreciation_CHF + interest_CHF
  incr_rent_monthly_CHF <- incr_rent_CHF/12
  
  # the allowed maintenance increase is 10 % of the increasing rent 
  maintenance_CHF <- incr_rent_CHF*maintenance_rate/100
  
  # total rent results and report
  total_add_rent_monthly_CHF <- (act_rent_CHF + incr_rent_CHF + maintenance_CHF)/12
  rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:", 
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, act_rent_CHF+total_add_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # rount to 5 cts.
  result <- as.table(cbind(rent_answers, rent_summary))
  print(result)
  }


##################################################################################################
#
#  PART C: RentCalculatorNewCircum(act_rent_CHF, reference_last_date, next_termination_date,
#                                inflation_rate_last_date, cost_incr_last_date, flat_rate = 0.01)
#
##################################################################################################

  ########################
  # This I need only for testing the calculations. After they work we can delete it
  ########################
  act_rent_CHF <- 1000
  reference_last_date <- "2015-03-31"
  next_termination_date <-"2020-09-30"
  inflation_rate_last_date <- "2017-12-31"
  cost_incr_last_date <- "2015-03-31"
  # flate_rate = 0.005
  #RentCalculatorNew(1000,"2015-03-31","2020-09-30","2017-12-31","2015-03-31")
  ########################
  
  
  #############################
  # CHANGE IN MORTGAGE REFERENCE RATE
  #############################
  
  # Change in mortgage reference rate
  mortgage_relevant_dates <- c(reference_last_date, act_date, next_termination_date)
  reference_last_rate <- subset(mortgage_data_clean, valuable_from < reference_last_date)
  reference_last_rate <- reference_last_rate[1,1]
  mortgage_relevant_rates <- c(reference_last_rate, act_mortgage) 
  
  # The change per 1/4 %  differs on the level of the mortgage rate (art. 13 VMWG Tenancy Law Regulation)
  if (act_mortgage < 5) {
    mortgage_factor <- 3
  } else if (act_mortgage > 6) {
    mortgage_factor <- 2
  } else {
  mortgage_factor <- 2.5
  }
  
  # Difference in mortgage
  mortgage_change_factor <- (act_mortgage - reference_last_rate)/.25
  mortgage_change <- mortgage_factor * mortgage_change_factor
  

  #############################
  # CHANGE IN INFLATION RATE
  #############################
  
  inflation_relevant_dates <- c(inflation_rate_last_date, act_date)
  inflation_rate_last <- subset(inflation_clean, inflation_clean$year < inflation_relevant_dates$inflation_rate_last_date)
  
  inflation_difference <- inflation_relevan 
  
  
  
  #############################
  # CHANGE IN COST INCREASE
  #############################
  
  cost_incr_last_date <- strptime(cost_incr_last_date, format ="%Y%m%d")
  
  cost_relevant_dates <- c(cost_incr_last_date, act_date)
  cost_change <- as.numeric(cost_incr_last_date) - as.numeric(act_date)
diff.Date(as.numeric(cost_incr_last_date), as.numeric(act_date))

as.Date.POSIXlt(act_date)
class(act_date)
new <- strptime(act_date, format="%F-%Z")

class(new)
?strptime 


##################################################################################################
#
#  PART D: RentCalculatorCombi(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate,
#                              reference_last_date, next_termination_date,
#                              inflation_rate_last_date, flat_rate = 0.01, cost_incr_last_date)
#
##################################################################################################
#the last function combines the other 3 (PART A-C)









##################################################################################################


######################################################################
# BIBLIOGRAPHY 
######################################################################
# - 

######################################################################
# AUTHOR'S DECLARATION 
######################################################################

# We hereby certify that:
# - We have written the program ourselves except for clearly marked pieces of code
# - We have tested the program and it ran without crashing 
# Giuliano Giovanardi and Ruth Peterhans   

