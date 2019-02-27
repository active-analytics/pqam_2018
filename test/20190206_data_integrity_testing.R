rm(list = ls())
cat("\014")

library(tidyverse)


df_old <- read_csv("data_output/20181229_spy_weekly_opt_hist_5yr.csv")


df_new <- read_csv("data_output/spy_weekly_opt_hist_5yr.csv")


nrow(df_old)
nrow(df_new)

df_old %>% 
    group_by(data_date, expiration) %>% 
    summarize(num_row = n())

df_new %>% 
    group_by(data_date, expiration) %>% 
    summarize(num_row = n())


# df_old %>% 
#     filter(data_date == "2013-12-27") %>% View()
