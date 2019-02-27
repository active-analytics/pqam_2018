# clearing shit out
rm(list=ls())
cat("\014")

# loading libraries
library(lubridate)
library(tidyverse)
library(bizdays)

# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")

# sourcing functions
source("function/chain_weekly_old.R")
source("function/chain_weekly.R")
source("function/is_third_friday.R")
source("function/monthly_expiration.R")
source("function/monthly_last_td.R")
source("function/third_friday.R")

df_chain_old <- chain_weekly_old("SPY")
df_chain_new <- chain_weekly()


df_chain_old %>% 
    filter((expiration >= "2016-12-23") & (expiration <= "2018-07-20")) %>% 
    .$expiration ==
df_chain_new %>% 
    filter((expiration >= "2016-12-23") & (expiration <= "2018-07-20")) %>% 
    .$expiration
