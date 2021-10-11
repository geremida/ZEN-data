# Visualise Noosa data
# Geoff Acton
# 4 October 2021
# ============= 
# could try refactoring out most of the intermediate dataframes
# think about whether could include estimated kWh production per month
# then be able to estimate cumulative CO2-e tonnes of carbon for PV
# and maybe for SWH & SWHASHP
# output all the graphs somewhere
# then annotate them appropriately
# could they be embedded in Squarespace, say via AMCharts HTML Widget

library(tidyverse)
# =========== Now go for just Noosa ===============
read_data <- readRDS(file="CER_PVqty_kW_SWH_SWHASHP_all_years.rds")
# Don't include 4573 'cos we need just 20.7% of it for Noosa LGA
Noosa_rows <- c("4563", "4565", "4566", "4567", "4568", "4569", "4571")

Noosa_SGU_no_4573 <- filter(read_data, Postcode %in% Noosa_rows)  # equivalently, dat %>% filter(name %in% target)
Postcode_4573 <- read_data[read_data$Postcode=="4573",]
# multiply all numerics by factor .207   20.7% being the factor derived from Enegerx solar data 
# https://stackoverflow.com/questions/46912044/mutliply-several-columns-of-a-dataframe-by-a-factor-scalar
Postcode_4573[] = lapply(Postcode_4573, FUN = function(x) if (is.numeric(x)) return(x * 0.207) else return(x))
# this seems to work
# Combine datasets with the same columns
Noosa_SGU <- rbind(Noosa_SGU_no_4573,Postcode_4573)
# cumsum by group
# https://www.tutorialspoint.com/how-to-create-a-column-in-an-r-data-frame-with-cumulative-sum
# df%>%group_by(grp)%>%mutate(cumusum=cumsum(x1))
# https://stackoverflow.com/questions/16850207/calculate-cumulative-sum-cumsum-by-group
# explicitly use dplyr version of mutate - from comment in stackoverflow link above
# Just an update, you might have a package that has loaded plyr. 
# Explicitly referencing dplyr will fix it also: 
# ``` df %>% group_by(id) %>% dplyr::mutate(csum = cumsum(value)) ``` 
Noosa_SGU_tot_qty <- Noosa_SGU %>%
  group_by(Postcode)%>%
  dplyr::mutate(Total_PV_qty=cumsum(PV_qty))
# something wrong with the cumsum for 4573......
# but fixed if explicitly use dplyr::mutate
#
Noosa_SGU_tot_qty_kW <- Noosa_SGU_tot_qty %>%
  group_by(Postcode)%>%
  dplyr::mutate(Total_PV_kW=cumsum(PV_kW)) 
#
Noosa_PVqty_PVkW_SWHqty <- Noosa_SGU_tot_qty_kW %>%
  group_by(Postcode)%>%
  dplyr::mutate(Total_SWH_qty=cumsum(SWH_qty))  
#
Noosa_PVqty_PVkW_SWHqty_SWHASHPqty <- Noosa_PVqty_PVkW_SWHqty %>%
  group_by(Postcode)%>%
  dplyr::mutate(Total_SWHASHP_qty=cumsum(SWHASHP_qty))  

# yippee - this seems to work - but not if plyr loaded....
# perhaps retrofit it back into the combined rds
# but the data structure looks a bit ugly.....
# Get a set of totals for Noosa_LGA
# sum grouped by year_month will give Noosa LGA totals
# see https://www.statology.org/r-aggregate-multiple-columns/
# aggregate(sum_var ~ group_var, data = df, FUN = sum)
LGA_data <- aggregate(cbind(PV_qty,PV_kW,SWH_qty,SWHASHP_qty,Total_PV_qty,Total_PV_kW,Total_SWH_qty,Total_SWHASHP_qty)
                      ~ year_month, data = Noosa_PVqty_PVkW_SWHqty_SWHASHPqty, FUN = sum)

ggplot(LGA_data, 
       aes(x=year_month,y=Total_PV_kW)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=Total_PV_qty)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=Total_SWH_qty)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=Total_SWHASHP_qty)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=PV_qty)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=PV_kW)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=SWH_qty)) +
  geom_point()
ggplot(LGA_data, 
       aes(x=year_month,y=SWHASHP_qty)) +
  geom_point()


# Here's a quick visualisation for a single postcode
mypostcode <- Noosa_SGU_tot_qty_kW[Noosa_SGU_tot_qty_kW$Postcode=="4563",]
ggplot(mypostcode, 
       aes(x=year_month,y=Total_PV_qty)) +
  geom_point()

ggplot(mypostcode, 
       aes(x=year_month,y=Total_PV_kW)) +
  geom_point()



