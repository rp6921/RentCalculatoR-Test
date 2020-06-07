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
# CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND) - FUNCTION RentInformations
##########################################################################################################################

# The RentInformations function gets the relevant information from the system or from scraping of the
# webpage of the Department of Statistics. They are coded in PART A.
#    - current relevant and official mortgage rent (mortgage, in %); from https://www.bwo.admin.ch/bwo/de/home/
#      mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html
#    - current date for the calculation on inflation and on general cost increases
#    - official information about the inflation rates from xlsx-file on https://www.bfs.admin.ch/bfsstatic/
#     dam/assets/12827290/master/

##########################################################################################################################

##########################################################################################################################
#
#  PART A: RentInformations(), returns (act_mortgage, act_date, act_infl_rate)
#
##########################################################################################################################

RentInformations <- function(){


  # We use the following packages. They will be  installed in case you don't have them

  if ("xml2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("xml2")
  }

  if ("rvest" %in% rownames(installed.packages()) == FALSE) {
    install.packages("rvest")
  }

  if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tidyverse")
  }

  if ("readxl" %in% rownames(installed.packages()) == FALSE) {
    install.packages("readxl")
  }

  library(xml2)
  library(rvest)
  library(tidyverse)
  library(readxl)


  #############################
  # MORTGAGE - getting the actual mortgage (act_mortgage) from the Federal Office for Housing
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
  # ACTUAL DATE - getting the actual date (act_date) out of the system
  #############################
  act_date <- Sys.Date()

  #############################
  # INFLATION RATE - getting the inflation rate (act_infl_rate) from the Department of Statistics
  #############################
  # Inflation index from as xlsx download of the department for statistics
  # https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master

  URL_inflation <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/13047088/master"
  download.file(URL_inflation, destfile = "inflation_rates.xlsx")
  inflation_base2015 <- read_excel("inflation_rates.xlsx", sheet = "2015",
                                   col_names =  c("year", (1:17)))

  # clean the spreadsheet of 2015 indices
  inflation_clean <- data.frame(inflation_base2015[,(1:13)])
  inflation_clean$year <- as.Date.character(inflation_clean$year, format = "%Y")
  inflation_clean$year <- as.numeric(format(inflation_clean$year, "%Y"))

  # take only cases where year has a value and sort from actual to past years
  years_only <- complete.cases(inflation_clean$year)
  inflation_clean <- cbind(years_only, inflation_clean)
  inflation_clean <- subset(inflation_clean, years_only == TRUE)
  inflation_clean <- inflation_clean [,(2:14)]
  inflation_clean <- inflation_clean[order(inflation_clean$year, decreasing = TRUE),]

  # filter out the actual inflation rate
  act_inflation_rate <- inflation_clean[1,]
  act_inflation_rate <- gather(act_inflation_rate)
  act_inflation_rate <- subset(act_inflation_rate, !is.na(act_inflation_rate$value))
  act_inflation_rate <- as.character(act_inflation_rate$value)
  act_inflation_rate <- as.numeric(dplyr::last(act_inflation_rate))


  #############################
  # PRINT RESULTS
  #############################
  # overview of the results and report
  actual_data <- c(act_mortgage, as.character(act_date), act_inflation_rate)
  actual_answers <- c("The actual mortgage rate in % is:", "The actual date is:",
                      "And the actual inflation rate is (points on 2015 basis)")
  actual_print_result <- as.table(cbind(actual_answers, actual_data))
  print(actual_print_result)

  # return the actual data in a dataframe
  act_data <- data.frame(actual_data)
  act_data <- cbind(c("act_mortgage", "act_date", "act_inflation_rate"), act_data)
  colnames(act_data) <- c("name", "value")
  row1 <- as.vector(act_data$name)
  row2 <- as.vector(act_data$value)
  act_data <- rbind(row2)
  colnames(act_data) <- row1
  return(act_data)
}


# Example
# -------------------
# If you would like to know what the acutual relevant data for rents is.
# This output will be used from the other functions in Part B and C as well.
# act_results <- RentInformations()
