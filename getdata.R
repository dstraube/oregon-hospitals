setwd("~/R/OurOregon")
rm(list = ls())

suppressMessages(library(dplyr))

source("./utils.R", echo = FALSE)

# Read audited annual hospital reports from: https://www.oregon.gov/oha/OHPR/RSCH/pages/hospital_reporting.aspx.
# Files are provided in .xls form and do not have identical format from year to year. Here we read .csv files 
# which were generated in OpenOffice by:
#
#     1 - manually renaming columns for consistency
#     2 - deleting unnecessary columns and rows
#     3 - merging multiple worksheets if necessary
#     4 - saving as .csv with "Save cell content as shown" disabled - mostly unadorns numerals

# Hospitals change name/ownership or are mispelled in different year reports. Names were correlated by hand and saved
# to a .csv file. Read file and define a function to map name in a specific year to 2014 version of hospital name.

hospital.names <- read.csv("./data/hospitals.csv",
                           strip.white = TRUE,
                           stringsAsFactors = FALSE)

hospital.name.2014 <- function(current.name, year) {
    uid <- hospital.names[hospital.names$hospital == current.name & hospital.names$year == year, "uid"]
    hospital.names[hospital.names$year == 2014 & hospital.names$uid == uid, "hospital"]
}

first.year <- 2006
last.year <- 2014
hosp.data <- NULL
for ( year in seq(first.year, last.year) ) {
    tmp <- read.csv(paste0("./data/", year, ".csv"),
                    strip.white = TRUE,
                    na.strings = c("NA", "N/A"),
                    stringsAsFactors = FALSE)
    # Map hospital names to their 2014 equivalent.
    tmp$hospital <- sapply(tmp$hospital, hospital.name.2014, year = year)
    # Some years are missing type - grab from prior year.
    if ( sum(is.na(tmp$type)) > 0 ) {
        tmp$type <- NULL
        tmp <- left_join(tmp, hosp.data[hosp.data$year == (year - 1), c("hospital", "type")], by = "hospital")
    }
    # Convert percentages to decimal.
    tmp$op.margin <- as.numeric(gsub('%', '', tmp$op.margin)) / 100
    tmp$tot.margin <- as.numeric(gsub('%', '', tmp$tot.margin)) / 100
    # Some years didn't include Other Operating Revenue - convert via rule 1 (see below).
    tmp$other.op.rev <- ifelse(is.na(tmp$other.op.rev), tmp$tot.op.rev - tmp$npr, tmp$other.op.rev)
    # Reorder columns.
    tmp <- tmp[,c("hospital",              # hospital/facility name
                  "type",                  # A | B | DRG
                  "gpr",                   # gross patient revenue
                  "npr",                   # net patient revenue
                  "charity.care",          # charity care
                  "bad.debt",              # bad debt
                  "tot.uncomp.care",       # total uncompensated care
                  "tot.op.rev",            # total operating revenue
                  "tot.op.exp",            # total operating expense
                  "other.op.rev",          # other operating revenue
                  "net.non.op.rev",        # total non-operating revenue
                  "net.income",            # net income
                  "op.income",             # operating income
                  "op.margin",             # operating margin
                  "tot.margin")]           # total margin
    tmp$year <- year
    hosp.data <- rbind(hosp.data, tmp)
}
hosp.data$type <- factor(hosp.data$type)
hosp.data <- hosp.data[order(hosp.data$year, hosp.data$hospital, decreasing = FALSE),]

# Most of the data in the Oregon Health Authority spreadsheets is entered as numbers, i.e. not calculated,
# despite the fact that some fields are clearly derived from others and most likely were calculated by
# the hospitals originally. So that leaves room for both calculation and transciption error which can
# be sanity checked.
#
# Total Margin is sometimes a calculated value and sometimes a hardcoded value - and two different formulas
# are used! They are:
#
#     TM1 - net.income / (tot.op.rev + net.non.op.rev)
#     TM2 - net.income / tot.op.rev
# 
# and appear as follows:
#
#     2006 - TM1 as formula
#     2007 - TM1 as formula
#     2008 - TM2 hardcoded
#     2009 - TM2 as formula
#     2010 - TM2 hardcoded
#     2011 - TM2 hardcoded
#     2012 - TM1 hardcoded
#     2013 - TM1 hardcoded
#     2014 - TM1 hardcoded
#
# Given this discrepancy, we opt for formula TM1 and force all entries to comply.

hosp.data$tot.margin <- hosp.data$net.income / (hosp.data$tot.op.rev + hosp.data$net.non.op.rev)

# We now define some rules for sanity checking the data.
#
#     1   - tot.op.rev = npr + other.op.rev
#     2   - op.income = tot.op.rev - tot.op.exp
#     3   - op.margin = op.income / tot.op.rev
#     4   - net.income = op.income + net.non.op.rev
#     5   - tot.uncomp.care = charity.care + bad.debt
#
# Rules are allowed some slack for sigificant digit truncation.

close.enough <- function(expected, offered, permissible.pct.error) {
    lopct <- (100 - permissible.pct.error) / 100
    hipct <- (100 + permissible.pct.error) / 100
    if ( sign(expected) == sign(offered) ) {
        expected <- abs(expected)
        offered <- abs(offered)
        if ( (offered >= (lopct * expected)) && (offered <= (hipct * expected)) ) {
            return(TRUE)
        }
    }
    FALSE
}

# In this next section we define a function to test each rule, run it against the data, and print out failures.

pass.rule.1 <- function(row, permissible.pct.error) {
    # Kaiser Sunnyside has no npr or gpr data for 2012, 2013, 2014.
    if ( row$hospital == "Kaiser Sunnyside" && row$year %in% c(2012, 2013, 2014) ) {
        return(TRUE)
    }
    # Kaiser Westside Medical Center has no npr or gpr data for 2014.
    if ( row$hospital == "Kaiser Westside Medical Center" && row$year == 2014 ) {
        return(TRUE)
    }
    retval <- TRUE
    with(row, if ( !close.enough(npr + other.op.rev, tot.op.rev, permissible.pct.error) ) {
        cat(paste0('\n ', hospital, ' - ', year, '\n'))
        cat("\tRule 1: tot.op.rev = npr + other.op.rev\n")
        cat(paste0("\tnpr(", money(npr, 0), ") + other.op.rev(", money(other.op.rev, 0), ") ==> ",
                   money(tot.op.rev, 0), " given, ", money(npr + other.op.rev, 0), " expected\n"))
        retval <<- FALSE
    })
    retval
}

errs1 <- 0
permissible.pct.error <- 1
for ( row in seq(dim(hosp.data)[1]) ) {
    if ( !pass.rule.1(hosp.data[row,], permissible.pct.error) ) { errs1 <- errs1 + 1 }
}

pass.rule.2 <- function(row, permissible.pct.error) {
    retval <- TRUE
    with(row, if ( !close.enough(tot.op.rev - tot.op.exp, op.income, permissible.pct.error) ) {
        cat(paste0('\n ', hospital, ' - ', year, '\n'))
        cat("\tRule 2: op.income = tot.op.rev - tot.op.exp\n")
        cat(paste0("\ttot.op.rev(", money(tot.op.rev, 0), ") - tot.op.exp(", money(tot.op.exp, 0), ") ==> ",
                   money(op.income,0), " given, ", money(tot.op.rev - tot.op.exp, 0), " expected\n"))
        retval <<- FALSE
    })
    retval
}

errs2 <- 0
permissible.pct.error <- 1
for ( row in seq(dim(hosp.data)[1]) ) {
    if ( !pass.rule.2(hosp.data[row,], permissible.pct.error) ) { errs2 <- errs2 + 1 }
}

pass.rule.3 <- function(row, permissible.pct.error) {
    retval <- TRUE
    with(row, if ( !close.enough(op.income / tot.op.rev, op.margin, permissible.pct.error) ) {
        cat(paste0('\n ', hospital, ' - ', year, '\n'))
        cat("\tRule 3: op.margin = op.income / tot.op.rev\n")
        cat(paste0("\top.income(", money(op.income, 0), ") / tot.op.rev(", money(tot.op.rev, 0), ") ==> ",
                   pp(op.margin, 8), " given, ", pp(op.income / tot.op.rev, 8), " expected\n"))
        retval <<- FALSE
    })
    retval
}

errs3 <- 0
permissible.pct.error <- 1
for ( row in seq(dim(hosp.data)[1]) ) {
    if ( !pass.rule.3(hosp.data[row,], permissible.pct.error) ) { errs3 <- errs3 + 1 }
}

pass.rule.4 <- function(row, permissible.pct.error) {
    retval <- TRUE
    with(row, if ( !close.enough(op.income + net.non.op.rev, net.income, permissible.pct.error) ) {
        cat(paste0('\n ', hospital, ' - ', year, '\n'))
        cat("\tRule 4: net.income = op.income + net.non.op.rev\n")
        cat(paste0("\top.income(", money(op.income,0), ") + net.non.op.rev(", money(net.non.op.rev, 0), ") ==> ",
                   money(net.income, 0), " given, ", money(op.income + net.non.op.rev, 0), " expected\n"))
        retval <<- FALSE
    })
    retval
}

errs4 <- 0
permissible.pct.error <- 1
for ( row in seq(dim(hosp.data)[1]) ) {
    if ( !pass.rule.4(hosp.data[row,], permissible.pct.error) ) { errs4 <- errs4 + 1 }
}

pass.rule.5 <- function(row, permissible.pct.error) {
    retval <- TRUE
    with(row, if ( !close.enough(charity.care + bad.debt, tot.uncomp.care, permissible.pct.error) ) {
        cat(paste0('\n', hospital, ' - ', year, '\n'))
        cat("\tRule 5: tot.uncomp.care = charity.care + bad.debt\n")
        cat(paste0("\tcharity.care(", money(charity.care, 0), ") + bad.debt(", money(bad.debt, 0), ") ==> ",
                   money(tot.uncomp.care, 0), " given, ", money(charity.care + bad.debt, 0), " expected\n"))
        retval <<- FALSE
    })
    retval
}

errs5 <- 0
permissible.pct.error <- 1
for ( row in seq(dim(hosp.data)[1]) ) {
    if ( !pass.rule.5(hosp.data[row,], permissible.pct.error) ) { errs5 <- errs5 + 1 }
}

# No rule 5 errors!
         
cat(paste0('\n', dim(hosp.data)[1], " hospital-years tested\n",
           '\t', errs1, " rule 1 errors\n",
           '\t', errs2, " rule 2 errors\n",
           '\t', errs3, " rule 3 errors\n",
           '\t', errs4, " rule 4 errors\n",
           '\t', errs5, " rule 6 errors\n"))

# Final sanity check.
if ( sum(complete.cases(hosp.data)) > 0 ) {
    message("***** Warning - some incomplete cases exist! *****")
    print(hosp.data[!complete.cases(hosp.data),])
}

save(hosp.data, file = "./hospitals.RData")





