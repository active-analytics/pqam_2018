# loading packages
library(tidyverse)
library(backtestr)

df_opt_hist <- 
    read_csv("data_output/spy_weekly_opt_hist_5yr.csv")

df_expiring_options <-
    df_opt_hist %>% 
        filter(expiration == monthly_expiration(2018, 11)) %>% 
        filter(data_date == expiration) %>% 
        filter(strike >= 270) %>% 
        filter(strike <= 280) %>% 
        select(underlying = underlying_symbol, 
               upx = underlying_price, type, expiration
               , data_date, strike)


write_csv(df_expiring_options, "spy_expiring_options.csv")
