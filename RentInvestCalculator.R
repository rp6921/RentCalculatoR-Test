##########################################################################################################################
# PROGRAMMING PROJECT, PROGRAMMING IN FINANCE II, SUMMERSEMESTER 2020,
# WITH PROF. PETER H. GRUBER AT UNIVERSITA DELLA SVIZZERA ITALIANA
# authors: Ruth Peterhans ruth.maria.peterhans@usi.ch
#          Giuliano Giovanardi, giuliano.giovanardi@usi.ch
##########################################################################################################################

##########################################################################################################################
# CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND)
# With the package RentCalculatoR you can calculate the change in rent of residential real estate according to Swiss law.
# Changes in value-adding investments, depreciation, interests and reference interest rates can be taken into account.
##########################################################################################################################

##########################################################################################################################
# CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND) - FUNCTION RentInvestCalculatorR
##########################################################################################################################

# To calculate the changings due to investments we need from the user (PART B of the package):
#    - actual net rent (act_rent_CHF, integer)
#    - total investment (investment_CHF, integer)
#    - value-increasing share (increasing_share, in %)
#    - lifespan (lifespan, in years)
#    - maintenance allowance (maintenance_rate, 10 % by default due to a high court judgment)

# And we use the acutal mortgage (act_mortgage) from the RentInformations function (see in PART A)

##########################################################################################################################

###########################################################################################################################
#
#  PART B: RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
#
##########################################################################################################################

RentInvestCalculatoR <- function(act_rent_CHF, investment_CHF, increasing_share,
                                 lifespan, maintenance_rate=10){

  # calling the results of the RentInformations function (PART A) and take the actual mortgage
  act_data <- as.data.frame(RentInformations( ))
  act_mortgage <- as.numeric(act_data$act_mortgage)

  # the increasing share and the depreciation of the investment in CHF
  value_incr_share_CHF <- investment_CHF/100*increasing_share
  depreciation_CHF <- value_incr_share_CHF/lifespan

  # the allowed interest by law is 0.5 % above the actual mortgage rate (dived to 2 paries)
  allowed_interest <- (act_mortgage+.5)/2
  # this leads to a change due to interests
  interest_CHF <- value_incr_share_CHF*allowed_interest/100

  # the intermediate result for the change is the sum of the depreciation and the interests
  int_result <- depreciation_CHF+interest_CHF

  # the allowed maintenance increase is 10 % of the intermediate result
  maintenance_CHF <- int_result*maintenance_rate/100

  #############################
  # PRINT RESULTS
  #############################

  # total rent results and report
  total_add_rent_monthly_CHF <- (act_rent_CHF + int_result + maintenance_CHF)/12
  rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:",
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, act_rent_CHF+total_add_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
  rent_result <- as.table(cbind(rent_answers, rent_summary))
  print("*******************************************************************************")

  # return the results in a dataframe
  return_invest <- as.table(c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF, total_add_rent_monthly_CHF))
  return_invest <- round(return_invest/5, digits = 2)*5   # round to 5 cts.
  return_invest <- as.data.frame(return_invest)
  colnames (return_invest) <- c("name", "value")
  row3 <- as.vector(return_invest$name)
  row4 <- as.vector(return_invest$value)
  return_invest <- rbind(row4)
  colnames(return_invest) <- c("value_incr_share_CHF", "depreciation_CHF","interest_CHF", "maintenance_CHF", "total_add_rent_monthly_CHF")
  list(return_invest)
  }




# EXAMPLE for standard use
# ----------------------------------------------------------------------------------------------------------
# You put in the needed data of your actual rent and of the investment into the calculator.
# If you don't give a value for the maintenance rate, the calculator uses 10 % by default

# To test it for a actual rent of 1000 CHF, an investment of 100000 CHF,
# a value increasing share of 50 % and a livespan of 12 years:
 RentInvestCalculatoR(1000,100000,50,12)


# EXAMPLE with a quasi zero value for actual_rent (e.g. for calculating initial rents of new buildings)
# ----------------------------------------------------------------------------------------------------------
# The starting values of the actual rent basically can't be zero. If you would like to calculate the initial
# rent for a new building, you have to set the actual rent to a very small value. Of course you can also use
# it, if you buy an appartment and you want to know how much rent you should get out of it. The calculator
# gives you here an approximation, because you have to set an average lifespan over all components of the
# building (see also EXAMPLE with different components for more precise calculations).

# To test it for a new building let's say we would like to calculate the rent for a unit (appartment).
# The unit is valid an investment of 500000 CHF,  the value increasing share is 100 % because there was no
# value before, the average lifespan will be 50 years and the maintanence is set to 8 % because in the
# in the first years you still have guarantee and would not spend as much on maintenance.
# RentInvestCalculatoR(0.000001,500000,100,50,8)


# EXAMPLE with different components
# ----------------------------------------------------------------------------------------------------------
# If you would like to do the calculation for different components, you can use the caluluator of course
# different times and add the values up. You can look up different lifespans of components on
# https://www.mietrecht.ch/index.php?id=32 (unfortunately only available in german language).

# @ Giuliano: Could you describe this example with a table
# It will work like this "investment_name <- RentInvestCalculatoR(...,...,..., values of the table)
# investment_name  |   act_rent_CHF | investment_CHF  | increasing_share | lifespan    |  maintenance_rate
#  structure of building      0.001        250000          100 %             100 years      10 % by default
#  kitchen                    0.001         30000           100 %             15 years      8 %
#  bathroom                   0.001         50000           100 %             30 years      8 %
#  floors                     0.001         25000           100 %             12 years      8 %
#  windows                    0.001         40000           100 %             20 years      8 %
#   Total                     0.001       sum(????)        sum(????)         average(????)  average (????)

