# undo function so can trace - just for PVkW for now

library(lubridate)    # needed for things like line 107 
library(tidyverse)

# ================ T H I S    I S   T H E    R E A L   P R O C E S S =============
# define some global variables for folders
base_folder <- getwd()
CER_data_folder <- paste(base_folder,"/CER_data/",sep = "")
tmp_folder <- paste(base_folder,"/tmp/",sep = "")
output_folder <- paste(base_folder,"/Output/",sep = "")
# test_folder <- paste(getwd(),"/test/",sep = "")

#process_CER_raw_data("SGU_Solar", "PV_kW")

CER_type ="SGU_Solar"
CER_value = "PV_kW"

# Specify empty vectors in data.frame for unit testing
CER_totals <- data.frame(type = character(),
                             year = numeric(),          
                             year_total = numeric(),
                             total_qty = numeric() )


# ================================== F U N C T I O N =================
#process_CER_raw_data <- function(CER_type, CER_value) {
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
  # and for some strange reason 2005 has different format
  # but as data in SWHASHP 2005 appears incorrect, will get from 2006 file instead
  if (CER_type =="SWH_ASHP") {
    file_pattern = "SWH-Air-source-heat-pump.csv"
    column_pattern_mask = "...Installations.Quantity"
    write_filename_mask = "CER_SWHASHPqty" 
    file_list <- append(
      as.list(list.files(path=CER_data_folder,pattern = "SWH-Air source heat pump.csv")),
      as.list(list.files(path=CER_data_folder,pattern = file_pattern))
    )
    # # forget this one for now
    # file_list <- append(file_list,
    #   as.list(list.files(path=CER_data_folder,pattern = "Solar-Air-source-heat-pump.csv"))
    # )
  }else {
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
    
    # now make all columns numeric after stripping out commas
    all_columns <- colnames(data)
    data[ , all_columns] <- 
      lapply(data[ , all_columns],
             function(x){ as.numeric(as.character(gsub(",", "", x))) })
    # now cast to numeric
    data[all_columns] <- sapply(data[all_columns],as.numeric)
    # http://www.r-tutor.com/r-introduction/data-frame/data-frame-column-vector
# check if any duplicate postcodes
    my_duplicated <- duplicated(data[,1])
    if(sum(my_duplicated)>0) {
      print(sprintf("Duplicate Postcode         = %f", sum(my_duplicated)))
    # https://www.biostars.org/p/304213/
    # remove duplicates - keep highest value of a column
    # try it with [[1]] & [[2]]
    data <- data[order(data[[1]], -abs(data[[2]]) ), ] ### sort first
    data <- data[ !duplicated(data[[1]]), ]  ### Keep highest
    }
    if(is.na(filename_year)) {
      # current data doesn't have yyyy in filename
      # column_pattern will be like  "[a-zA-Z]+.[0-9]+...Installations.Quantity"
      column_pattern = paste("[a-zA-Z]+.[0-9]+",column_pattern_mask, sep = "")
    }
    else {
      # if special case for 2015 SWHASHP
      if (filename_year=="2016"  && CER_type =="SWH_ASHP") {
        print("special case 2016 SWH_ASHP")
        column_pattern = paste("[a-zA-Z]+.[0-9]+",column_pattern_mask, sep = "")
      } else
      # just interested in getting column data for the year of the filename
      # as historical files have data for 2 year spread
        column_pattern <- paste("[a-zA-Z]+.",filename_year,column_pattern_mask, sep = "")
    }
    # get a vector just containing column names we are interested in
    column_list_vector <- as.vector(
      na.omit(
        str_match(colnames(data),column_pattern)))
# grab the total installations quantity OR kW before doing pivot
# ignoring any NAs
    if(write_filename_mask == "CER_PVkW") {
      CER_total_qty = sum(data$SGU.Rated.Output.In.kW.Total,na.rm = TRUE) 
    } else {
      CER_total_qty = sum(data$Installations.Quantity.Total,na.rm = TRUE) 
    }
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
    data <- rename(data, Postcode = 1)
    # and make Postcode column integer
    data <- data %>% mutate(Postcode = as.integer((Postcode))) 
      # before we remove columns, get the sum of last column & save it
      CER_totals <- CER_totals %>% add_row(type = write_filename_mask,
                                                  year=as.integer(filename_year),
                                                  year_total = sum(data$CER_value_col,na.rm = TRUE),
                                                  total_qty = CER_total_qty)
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
#  return(paste("====> exiting function: ",write_filename_mask))
#}
  
  
# Now do a test  
  # Try doing test for CER_PVkW_all_years.rds
  # and check totals against csv file in CER_data
  # Then repeat for other data - PVqty, SWH, SWHASHP
  print("========== S Y S T E M    T E S T - SWHASHP =========")
  print("readRDS CER_SWHASHP_all_years.rds")
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
  # set the last 2 columns as numeric
  raw_SWHASHP_qty_total <- sum(data_SGU$Installations.Quantity.Total,na.rm=TRUE)
  #  raw_PV_kW_total <- sum(data_SGU$SGU.Rated.Output.In.kW.Total,na.rm=TRUE)
  print("===== Totals =====")
  print(sprintf("Calc SWHASHP total qty = %f", calc_SWHASHP_qty_total))
  print(sprintf("Raw SWHASHP total qty  = %f", raw_SWHASHP_qty_total))
  
  print(sprintf("Difference         = %f", calc_SWHASHP_qty_total - raw_SWHASHP_qty_total))
  print(sprintf("Difference percent = %f", 100*(calc_SWHASHP_qty_total-raw_SWHASHP_qty_total)/raw_SWHASHP_qty_total))
  
 
