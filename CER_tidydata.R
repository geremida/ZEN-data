# Tidy Data for CER STC postcode data
# Geoff Acton
# 15 Sep 2021
# ============= 
# Getting CER csv data into Tidy Data format
# Postcode
# YearMonth
# PV_qty
# PV_kW
# SWH_qty - to be tidied up and included in System Test
# Data will be monthly figures as per CER data - qty & kW
# System Test for SGU data completed.  Grand totals & Noosa LGA - done
# =======================================================
# Next steps
# clone this process to get Heat Pump SHW & SWH files - done
# read data from CER_folder - done
# write temp rds files to tmp_folder - done
# NEXT -
# change joins to use dplyr instead of plyr - done (except for sys test)
# - output folder - RDS - done
# - maybe sys test & log files to test_folder
# do sys test for SWH & SWHASHP
# read csv postcode data from APVI exports for range of PV sizes
# join with Noosa data

# Plan for where data should be stored, eg Google Shared Drive
library(lubridate)    # needed for things like line 107 
library(tidyverse)
# ================================== F U N C T I O N =================
process_CER_raw_data <- function(CER_type, CER_value) {
  column_pattern_mask = "No column pattern"
  write_filename_mask = "No write filename"
if (CER_type =="SGU_Solar") {
  file_pattern = "SGU-Solar.csv"
  if (CER_value == "PV_qty") {
    column_pattern_mask = "...Installations.Quantity"
    write_filename_mask = "CER_PVqty"
  }
  if (CER_value == "PV_kW") {
    column_pattern_mask = "...SGU.Rated.Output.In.kW"
    write_filename_mask = "CER_PVkW"
  }
}
if (CER_type =="SWH_Solar") {
  file_pattern = "SWH-Solar.csv"
  column_pattern_mask = "...Installations.Quantity"
  write_filename_mask = "CER_SWHqty"
}
# EXCEPT - CER has changed the file mask for ASHP- current csv doesn't use some dashes
# so create a special filelist for ASHP files
  if (CER_type =="SWH_ASHP") {
    file_pattern = "SWH-Air-source-heat-pump.csv"
    column_pattern_mask = "...Installations.Quantity"
    write_filename_mask = "CER_SWHASHPqty" 
    file_list <- append(
      as.list(list.files(path=CER_data_folder,pattern = "SWH-Air source heat pump.csv")),
      as.list(list.files(path=CER_data_folder,pattern = file_pattern))
    )
  }
  else {
    # create list of files matching the pattern
    file_list <- as.list(list.files(path=CER_data_folder,pattern = file_pattern))  
  }
# This will get patterns such as "SGU-Solar.csv"
#  print(file_list)
  for (filename in file_list) {
    print(sprintf("Processing %s", filename))
    #  check if filename has "yyyy" in it - no year = current
    filename_year <- str_match(filename,"[0-9]+")
    if(!is.na(filename_year))  {
#     makes write_filename like "CER_PVqty_",filename_year,".rds"
      write_filename = paste(write_filename_mask,"_",filename_year,".rds",sep = "")
    } else {
      write_filename = paste(write_filename_mask,".rds",sep = "")
    }
    data <- read.csv(paste(CER_data_folder,filename,sep = ""))
    print(sprintf("Number of rows = %f", nrow(data)))
    # now make all columns numeric, except first one (postcode)
    all_columns <- colnames(data)
    all_columns_except_first <- all_columns[-1]
    # ========================
    # Removing commas from selected columns
    # Prior to setting to numeric, otherwise will get NA when casting
    # https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
    data[ , all_columns_except_first] <- 
      lapply(data[ , all_columns_except_first],
      function(x){ as.numeric(as.character(gsub(",", "", x))) })
    # now cast to numeric
    data[all_columns_except_first] <- sapply(data[all_columns_except_first],as.numeric)
# rename columns now, so can more easily remove duplicate rows (min value)
    data <- rename(data, Postcode = 1,
                 Previous = 2)      
    
    
    # check if any duplicate postcodes
    my_duplicated <- duplicated(data[,1])
    sum_my_duplicates = sum(my_duplicated)
    print(sprintf("Duplicate Postcode         = %f", sum_my_duplicates))
        # make column one (postcode) to character
    # ================ try as integer instead ===================
#    data[1] <- sapply(data[1],as.character)
    # do this later just before saving......
    if(sum_my_duplicates > 0) {
        my_duplicated_table <- data[my_duplicated,]
        # https://www.biostars.org/p/304213/
        # remove duplicates - keep highest value of a column
        data_before_remove <- data
        data <- data[order(data$Postcode, -abs(data$Previous) ), ] ### sort first
        data <- data[ !duplicated(data$Postcode), ]  ### Keep highest
        browser(text="Found duplicates")
    }
    
    if(is.na(filename_year)) {
      # current data doesn't have yyyy in filename
      # column_pattern will be like  "[a-zA-Z]+.[0-9]+...Installations.Quantity"
      column_pattern = paste("[a-zA-Z]+.[0-9]+",column_pattern_mask, sep = "")
    }
    else {
      # just interested in getting column data for the year of the filename
      # as historical files have data for 2 year spread
      column_pattern <- paste("[a-zA-Z]+.",filename_year,column_pattern_mask, sep = "")
    }
    # get a vector just containing column names we are interested in
    column_list_vector <- as.vector(
      na.omit(
        str_match(colnames(data),column_pattern)))
    # for now, values_to generic "CER_value_col"
    # and then rename the column just before saving
    # Note that pivot_longer is preferred over gather - https://tidyr.tidyverse.org/reference/pivot_longer.html
    data <- pivot_longer(data,
                         cols = all_of(column_list_vector),
                         names_to = "year_month_string",
                         values_to = "CER_value_col")
    # now add "dd=01" to the year_month_string so we can force it to as.Date
    # then add 1 month
    # and subtract a day so we get end-of-month date
    # as that is how the CER data is reported
    # see https://statisticsglobe.com/add-subtract-months-years-from-date-in-r
    data <- mutate(data,
                   year_month =
                     (as.Date(
                       paste0(sub("^([^.]*.[^.]*).*", "\\1",
                                  year_month_string),".01"),
                       format = "%B.%Y.%d" )
                      %m+% months(1))
                   %m-% days(1)
    )
    # rename the postcode column
    # already done above
#    data <- rename(data, Postcode = Small.Unit.Installation.Postcode)
    # and make Postcode column integer
    data <- data %>% mutate(Postcode = as.integer((Postcode)))  

    # swhashp_totals <- data.frame(year = numeric(),          
    #                              year_total = numeric(),
    #                              total_qty = numeric() )       
    if(CER_value == "SWHASHP_qty") {
      # before we remove columns, get the sum of last column & save it
      browser(text="about to SWHASHP adding rows for years")
      swhashp_totals <- swhashp_totals %>% add_row(year=as.integer(filename_year),
              year_total = sum(data$CER_value_col),
              total_qty = sum(data$Installations.Quantity.Total))
      browser(text="SWHASHP adding rows for years")
    }
    # now just retain the columns we want
    data <- data[,names(data) %in% c("Postcode",
                                     "year_month","CER_value_col")]
    # rename the CER_value_col
    if(CER_value == "PV_qty") {
      # rename the CER_value_col column
      data <- rename(data, PV_qty = CER_value_col)
      # get the columns in the right order
      data <- relocate(data, PV_qty, .after = year_month)
    }
    if(CER_value == "PV_kW") {
      # rename the CER_value_col column
      data <- rename(data, PV_kW = CER_value_col)
      # get the columns in the right order
      data <- relocate(data, PV_kW, .after = year_month)
    }
    if(CER_value == "SWH_qty") {
      # rename the CER_value_col column
      data <- rename(data, SWH_qty = CER_value_col)
      # get the columns in the right order
      data <- relocate(data, SWH_qty, .after = year_month)
    }
    if(CER_value == "SWHASHP_qty") {
      # rename the CER_value_col column
      data <- rename(data, SWHASHP_qty = CER_value_col)
      # get the columns in the right order
      data <- relocate(data, SWHASHP_qty, .after = year_month)
    }
# Sort by postcode, then year_month
    data <- data[
      with(data, order(Postcode, year_month)),
    ]
#    View(data)
    # save the tidy data format as .rds
    print(sprintf("Saving %s",write_filename)) 
    saveRDS(data, paste(tmp_folder,write_filename,sep = ""))
 }
# E N D   O F   L O O P  - should now have rds files for each year + current
# Now create a consolidated file of all years data for eg PVqty, PVkW, etc
# Read all the CER_PVqty/kW, etc rds files into a dataframe
# should give us a list matching patterns like "^CER_PVqty.*\\.rds$"
  setwd(tmp_folder)
  tidy_file_list <- as.list(list.files(pattern= paste("^",write_filename_mask,".*\\.rds$", sep = "")))
  # bind all the matching files into a data frame
  my_tidy_data <- do.call(rbind, lapply(tidy_file_list, readRDS))
  # now set working directory back to base
  setwd(base_folder)
  # Sort by postcode, then year_month
  my_tidy_data <- my_tidy_data[
    with(my_tidy_data, order(Postcode, year_month)),
  ]
  # save this to a file like CER_PVqty_all_years.rds in tmp folder
  print(sprintf("Writing %s", paste(write_filename_mask,"_all_years.rds", sep = "")))
  saveRDS(my_tidy_data, file = paste(tmp_folder,write_filename_mask,"_all_years.rds", sep = ""))
return(paste("====> exiting function: ",write_filename_mask))
}


# ================================== F U N C T I O N =================
test_CER_PVkW <- function() {

# Now do a test  
# Try doing test for CER_PVkW_all_years.rds
# and check totals against csv file in CER_data
# Then repeat for other data - PVqty, SWH, SWHASHP
print("========== S Y S T E M    T E S T - PVkW =========")
print("readRDS CER_PVkW_all_years.rds")
read_data <- readRDS(file=paste(tmp_folder,"CER_PVkW_all_years.rds",sep = ""))
print("Calculate totals")
calc_PV_kW_total <- sum(read_data$PV_kW,na.rm=TRUE)
print("read current - Postcode data for small-scale installations - SGU-Solar.csv")
print("and extract totals")
data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SGU-Solar.csv",sep = ""))
# Removing commas from selected columns
# Prior to setting to numeric
# https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                function(x){ as.numeric(as.character(gsub(",", "", x))) })
# set the last 2 columns as numeric
data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))

raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
print("===== Totals =====")
print(sprintf("Calc PV kW         = %f", calc_PV_kW_total))
print(sprintf("Raw PV kW          = %f", raw_PV_kW_total))
print(sprintf("Difference         = %f", calc_PV_kW_total - raw_PV_kW_total))
print(sprintf("Difference percent = %f", 100*(calc_PV_kW_total-raw_PV_kW_total)/raw_PV_kW_total))
}

# ================================== F U N C T I O N =================
test_CER_PVqty <- function() {
  
  # Now do a test  
  # Try doing test for CER_PVqty_all_years.rds
  # and check totals against csv file in CER_data
  # Then repeat for other data - PVqty, SWH, SWHASHP
  print("========== S Y S T E M    T E S T - PVqty =========")
  print("readRDS CER_PVqty_all_years.rds")
  read_data <- readRDS(file=paste(tmp_folder,"CER_PVqty_all_years.rds",sep = ""))
  print("Calculate totals")
  calc_PV_qty_total <- sum(read_data$PV_qty,na.rm=TRUE)
  
  print("read current - Postcode data for small-scale installations - SGU-Solar.csv")
  print("and extract totals")
  data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SGU-Solar.csv",sep = ""))
  # Removing commas from selected columns
  # Prior to setting to numeric
  # https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
  col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
  data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                  function(x){ as.numeric(as.character(gsub(",", "", x))) })
  # set the last 2 columns as numeric
  data_SGU$Installations.Quantity.Total <- as.numeric(as.character(data_SGU$Installations.Quantity.Total))
#  data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))
  
  raw_PV_qty_total <- sum(data_SGU$Installations.Quantity.Total,na.rm=TRUE)
#  raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
  print("===== Totals =====")
  print(sprintf("Calc PV total qty = %f", calc_PV_qty_total))
  print(sprintf("Raw PV total qty  = %f", raw_PV_qty_total))
#  print(sprintf("Calc PV kW        = %f", calc_PV_kW_total))
#  print(sprintf("Raw PV kW         = %f", raw_PV_kW_total))
  
  print(sprintf("Difference         = %f", calc_PV_qty_total - raw_PV_qty_total))
  print(sprintf("Difference percent = %f", 100*(calc_PV_qty_total-raw_PV_qty_total)/raw_PV_qty_total))
}

# ================================== F U N C T I O N =================
test_CER_SWH <- function() {
  
  # Now do a test  
  # Try doing test for CER_SWH_all_years.rds
  # and check totals against csv file in CER_data
  # Then repeat for other data - PVqty, SWH, SWHASHP
  print("========== S Y S T E M    T E S T - SWH =========")
  print("readRDS CER_SWHqty_all_years.rds")
  read_data <- readRDS(file=paste(tmp_folder,"CER_SWHqty_all_years.rds",sep = ""))
  print("Calculate totals")
  calc_SWH_qty_total <- sum(read_data$SWH_qty,na.rm=TRUE)
  
  print("read current - Postcode data for small-scale installations - SWH-Solar.csv")
  print("and extract totals")
  data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SWH-Solar.csv",sep = ""))
  # Removing commas from selected columns
  # Prior to setting to numeric
  # https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
  col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
  data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                  function(x){ as.numeric(as.character(gsub(",", "", x))) })
  # set the last column as numeric
  data_SGU$Installations.Quantity.Total <- as.numeric(as.character(data_SGU$Installations.Quantity.Total))
  #  data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))
  
  raw_SWH_qty_total <- sum(data_SGU$Installations.Quantity.Total,na.rm=TRUE)
  #  raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
  print("===== Totals =====")
  print(sprintf("Calc SWH total qty = %f", calc_SWH_qty_total))
  print(sprintf("Raw SWH total qty  = %f", raw_SWH_qty_total))

  print(sprintf("Difference         = %f", calc_SWH_qty_total - raw_SWH_qty_total))
  print(sprintf("Difference percent = %f", 100*(calc_SWH_qty_total-raw_SWH_qty_total)/raw_SWH_qty_total))
}

# ================================== F U N C T I O N =================
test_CER_SWHASHP <- function() {
  
  # Now do a test  
  # Try doing test for CER_SWH_all_years.rds
  # and check totals against csv file in CER_data
  # Then repeat for other data - PVqty, SWH, SWHASHP
  print("========== S Y S T E M    T E S T - SWHASHP =========")
  print("readRDS CER_SWHASHPqty_all_years.rds")
  read_data <- readRDS(file=paste(tmp_folder,"CER_SWHASHPqty_all_years.rds",sep = ""))
  print("Calculate totals")
  calc_SWHASHP_qty_total <- sum(read_data$SWHASHP_qty,na.rm=TRUE)
  
  print("read current - Postcode data for small-scale installations - SWH-Air source heat pump.csv")
  print("and extract totals")
  data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SWH-Air source heat pump.csv",sep = ""))
  # Removing commas from selected columns
  # Prior to setting to numeric
  # https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
  col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
  data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                  function(x){ as.numeric(as.character(gsub(",", "", x))) })
  # set the last column as numeric
  data_SGU$Installations.Quantity.Total <- as.numeric(as.character(data_SGU$Installations.Quantity.Total))
  #  data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))
  
  raw_SWHASHP_qty_total <- sum(data_SGU$Installations.Quantity.Total,na.rm=TRUE)
  #  raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
  print("===== Totals =====")
  print(sprintf("Calc SWHASHP total qty = %f", calc_SWHASHP_qty_total))
  print(sprintf("Raw SWHASHP total qty  = %f", raw_SWHASHP_qty_total))
  
  print(sprintf("Difference         = %f", calc_SWHASHP_qty_total - raw_SWHASHP_qty_total))
  print(sprintf("Difference percent = %f", 100*(calc_SWHASHP_qty_total-raw_SWHASHP_qty_total)/raw_SWHASHP_qty_total))

  browser(text="End of test_CER_SWHASHP function")  
  
  }



# ================ T H I S    I S   T H E    R E A L   P R O C E S S =============
# define some global variables for folders
base_folder <- getwd()
CER_data_folder <- paste(base_folder,"/CER_data/",sep = "")
tmp_folder <- paste(base_folder,"/tmp/",sep = "")
output_folder <- paste(base_folder,"/Output/",sep = "")
# test_folder <- paste(getwd(),"/test/",sep = "")
run_time <- sprintf("%s",Sys.time())
# Specify empty vectors in data.frame
swhashp_totals <- data.frame(year = numeric(),          
                     year_total = numeric(),
                     total_qty = numeric() )



sink(sprintf("CER tidydata:%s.log",run_time))
print("CER tidydata Log")
print("================")
print(sprintf("Run on %s",run_time))

print("Processing SGU_Solar - PV_kW.......")
process_CER_raw_data("SGU_Solar", "PV_kW")
test_CER_PVkW()

print("Processing SGU_Solar - PV_qty.......")
process_CER_raw_data("SGU_Solar", "PV_qty")
test_CER_PVqty()

print("Processing SWH_Solar - SWHqty.......")
process_CER_raw_data("SWH_Solar", "SWH_qty")
test_CER_SWH()

print("Processing SWH_ASHP - SWHASHPqty.......")
process_CER_raw_data("SWH_ASHP", "SWHASHP_qty")
test_CER_SWHASHP()



# now consolidate.......
# read in all_years for PVqty, PVkW, SWHqty & SWHASHPqty
# join & save as .rds
print("read in all_years for PVqty, PVkW, SWHqty & SWHASHPqty, join & save as .rds")
# https://www.statology.org/join-multiple-data-frames-dplyr/
setwd(tmp_folder)
data <- readRDS(file="CER_PVqty_all_years.rds") %>%
    full_join(readRDS(file="CER_PVkW_all_years.rds"),by = c("Postcode","year_month")) %>%
    full_join(readRDS(file="CER_SWHqty_all_years.rds"), by = c("Postcode","year_month")) %>%
    full_join(readRDS(file="CER_SWHASHPqty_all_years.rds"), by = c("Postcode","year_month"))
setwd(base_folder)  
# The joins may result in some numeric columns having NA values
# So convert all NA values in numeric columns to zero
# https://www.delftstack.com/howto/r/replace-na-with-0-in-r/
data <- mutate_if(data, is.numeric, ~replace(., is.na(.), 0))
# Sort by postcode, then year_month - just in case
data <- data[
  with(data, order(Postcode, year_month)),
]
# save joined data to a file - CER_PVqty_kW_all_years.rds
print("save joined data to a file - CER_PVqty_kW_SWH_SWHASHP_all_years.rds")
# ============ >>>>> save in safe, accessible place.....
saveRDS(data,file = paste(output_folder,"CER_PVqty_kW_SWH_SWHASHP_all_years.rds",sep = ""))
# and save a copy with a date/time in filename
saveRDS(data,file = paste(output_folder,"CER_PVqty_kW_SWH_SWHASHP_all_years - ",run_time ,".rds",sep = ""))

# ====================== S Y S T E M    T E S T =========  
# Test that it works ! 
# *************** Need to update for SWH & SWHASHP data ****************
print("====================== S Y S T E M    T E S T =========")
print("readRDS CER_PVqty_kW_SWH_SWHASHP_all_years.rds")
read_data <- readRDS(file=paste(output_folder,"CER_PVqty_kW_SWH_SWHASHP_all_years.rds",sep = ""))
print("Calculate totals")
calc_PV_qty_total <- sum(read_data$PV_qty,na.rm=TRUE)
calc_PV_kW_total <- sum(read_data$PV_kW,na.rm=TRUE)
calc_SWH_qty_total <- sum(read_data$SWH_qty,na.rm=TRUE)
calc_SWHASHP_qty_total <- sum(read_data$SWHASHP_qty,na.rm=TRUE)

how_many_duplicates <- sum(duplicated(read_data[,1:2]))


print("read current - Postcode data for small-scale installations - SGU-Solar.csv")
print("and extract totals")
data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SGU-Solar.csv",sep = ""))
# Removing commas from selected columns
# Prior to setting to numeric
# https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                function(x){ as.numeric(as.character(gsub(",", "", x))) })
# set the last 2 columns as numeric
data_SGU$Installations.Quantity.Total <- as.numeric(as.character(data_SGU$Installations.Quantity.Total))
data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))

raw_PV_qty_total <- sum(data_SGU$Installations.Quantity.Total,na.rm=TRUE)
raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
print("===== Totals =====")
print(sprintf("Calc PV total qty = %f", calc_PV_qty_total))
print(sprintf("Raw PV total qty  = %f", raw_PV_qty_total))
print(sprintf("Calc PV kW        = %f", calc_PV_kW_total))
print(sprintf("Raw PV kW         = %f", raw_PV_kW_total))
# Coolio ! - there's a very small discrepancy....





# aggregate the sum of qty & kW for each postcode in the consolidated .rds
print("aggregate the sum of qty & kW for each postcode in the consolidated .rds")
calc_SGU_PV_qty <- setNames(
          aggregate(x = read_data$PV_qty,                # Specify data column
          by = list(read_data$Postcode),      # Specify group indicator
          FUN = sum,                          # Specify function (i.e. sum)
          na.rm=TRUE),
          c("Postcode","Calc_PV_qty")
)
calc_SGU_PV_kW <- setNames(
  aggregate(x = read_data$PV_kW,                # Specify data column
            by = list(read_data$Postcode),      # Specify group indicator
            FUN = sum,                          # Specify function (i.e. sum)
            na.rm=TRUE),
  c("Postcode","Calc_PV_kW")
)
# and join them into a single table
calc_SGU_PV_qty_kW <- full_join(calc_SGU_PV_qty, calc_SGU_PV_kW, by = "Postcode")

# now get the current CER SGU csv
# rename Postcode column for joining & just keep totals columns
data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SGU-Solar.csv",sep = ""))
# rename the Postcode column
# better to use rename rather than relocate
data_SGU <- rename(data_SGU, Postcode = Small.Unit.Installation.Postcode)
# now just retain the columns we want
data_SGU <- data_SGU[,names(data_SGU) %in% c("Postcode",
                                 "Installations.Quantity.Total",
                                 "SGU.Rated.Output.In.kW.Total")]
# make the Postcode column - character
# ====== make as integer instead ================= ?????
data_SGU$Postcode <- as.integer(data_SGU$Postcode)

# mtcars %<>% mutate(qsec = as.integer(qsec))


# data_SGU$Postcode <- as.character(data_SGU$Postcode)
# make the columns numeric
# but make sure we strip out commas first !
data_SGU$Installations.Quantity.Total <- as.numeric(gsub(",","",
                data_SGU$Installations.Quantity.Total))
data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(gsub(",","",
                data_SGU$SGU.Rated.Output.In.kW.Total))
# now join all
compare_SGU_data <- full_join(calc_SGU_PV_qty_kW, data_SGU, by = "Postcode")
# may have to clear out N/A ????????
# save the test results
print("save test results")
saveRDS(compare_SGU_data,file = paste("Test : ",Sys.time(),".rds",sep = ""))
# and look for differences
compare_SGU_data$qty_check <- compare_SGU_data$Calc_PV_qty -
                              compare_SGU_data$Installations.Quantity.Total
compare_SGU_data$kW_check <- compare_SGU_data$Calc_PV_kW -
  compare_SGU_data$SGU.Rated.Output.In.kW.Total
# can also check by viewing the dataframe & sorting compare columns
print("====== Results of checks at postcode level =====")
print(sprintf("Sum of SGU qty checks : %f",sum(compare_SGU_data$qty_check,na.rm=TRUE)))
print(sprintf("Sum of SGU kW checks  : %f",sum(compare_SGU_data$kW_check,na.rm=TRUE)))
sink()
# ============== E N D   O F  S Y S T E M   T E S T ============
mypostcode <- read_data[read_data$Postcode=="2486",]

# for all months from 2020, get 4 entries per month, so 4 times PVqty & PVkW
# so maybe try with postcode as integer ????



setwd(tmp_folder)
test_data <- readRDS(file="CER_PVkW.rds")
# Sort by year_month then postcode - to check for duplicates
sorted_data <- test_data[
  with(test_data, order(year_month, Postcode)),
]
setwd(base_folder) 
converted_data <- test_data %>% mutate(Postcode = as.integer((Postcode)))

# Try doing test for CER_PVkW_all_years.rds
# and check totals against csv file in CER_data
# Then repeat for other data - PVqty, SWH, SWHASHP
print("========== S Y S T E M    T E S T - PVkW =========")
print("readRDS CER_PVkW_all_years.rds")
read_data <- readRDS(file=paste(tmp_folder,"CER_PVkW_all_years.rds",sep = ""))
print("Calculate totals")
calc_PV_kW_total <- sum(read_data$PV_kW,na.rm=TRUE)

print("read current - Postcode data for small-scale installations - SGU-Solar.csv")
print("and extract totals")
data_SGU = read.csv(paste(CER_data_folder,"Postcode data for small-scale installations - SGU-Solar.csv",sep = ""))
# Removing commas from selected columns
# Prior to setting to numeric
# https://statisticsglobe.com/modify-numbers-with-comma-as-thousand-separator-in-r
col_conv <- setdiff(names(data_SGU),"Small.Unit.Installation.Postcode")
data_SGU[ , col_conv] <- lapply(data_SGU[ , col_conv],  # Convert data
                                function(x){ as.numeric(as.character(gsub(",", "", x))) })
# set the last 2 columns as numeric
data_SGU$SGU.Rated.Output.In.kW.Total <- as.numeric(as.character(data_SGU$SGU.Rated.Output.In.kW.Total))

raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
print("===== Totals =====")
print(sprintf("Calc PV kW        = %f", calc_PV_kW_total))
print(sprintf("Raw PV kW         = %f", raw_PV_kW_total))




