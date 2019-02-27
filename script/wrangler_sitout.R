rm(list = ls())
cat("\014")


######################
## loading packages ##
######################
library(tidyverse)
library(lubridate)



#####################
## reading in data ##
#####################
df_managed_pnl <- # change this file for different risk manangement 
    read_csv("data_output/spy_weekly_2014_2018_managed_pnl_100_200.csv")
df_scaling <- 
    read_csv("data_output/spy_weekly_2014_2018_position_scaling.csv")
df_chain_hist <- 
    read_csv("data_output/spy_weekly_2014_2018_chain_hist.csv")
df_chain_desc <-
    read_csv("data_output/spy_weekly_2014_2018_chain_desc.csv")


#########################
## calculating sitouts ##
#########################
# grabbing the PNL sold
df_premium <- 
    df_scaling %>%
    filter(variation == 0.1) %>% 
    select(expiration, premium = strangle_prem_sold)


# here is the data that is used for the sit-out strategy
df_sit_out <- 
    df_managed_pnl %>% 
    filter(strategy == "strangle") %>% 
    filter(variation == 0.1) %>% 
    group_by(expiration) %>% 
    summarize(
        exp_pnl = sum(scaled_managed_pnl)
    ) %>%
    left_join(
        df_premium
        , by = "expiration"
    ) %>% 
    select(expiration, premium, exp_pnl) %>% 
    mutate(pnl_ratio = exp_pnl / premium)


# calculating sit-outs
df_sit_out$sitout <- NA
df_sit_out$sitout[1] <- FALSE
for (ix in 2:nrow(df_sit_out)){
    dbl_prev_ratio <- df_sit_out$pnl_ratio[ix - 1]
    if (dbl_prev_ratio < -2){
        df_sit_out$sitout[ix] <- TRUE
    } else {
        df_sit_out$sitout[ix] <- FALSE
    }
}


#########################
## writing data to CSV ##
#########################
#write_csv(df_sit_out, "spy_weekly_chain_sit_out_100_200.csv")


###################
## ANALYSIS CODE ##
###################
# # isolating strangle pnl only
# df_strangle_pnl <-
#     df_managed_pnl %>%
#     filter(strategy == "strangle") %>% 
#     filter(variation == 0.1)
# 
# # joining on the sitout mult and calculatint the situout pnl
# df_strangle_pnl <-
#     df_strangle_pnl %>%
#     left_join(
#         df_sit_out %>% select(expiration, sitout)
#         , by = "expiration"
#     ) %>%
#     mutate(
#         sitout_mult = as.integer(!sitout)
#     ) %>%
#     mutate(
#         sitout_pnl = scaled_managed_pnl * sitout_mult
#     )
# 
# 
# # calculating the daily pnl and the ttd pnl of the full strategy
# df_sitout_daily <-
#     df_strangle_pnl %>%
#     group_by(data_date) %>%
#     summarize(
#         dly_pnl = sum(scaled_managed_pnl)
#         , dly_pnl_sitout = sum(sitout_pnl)
#     ) %>%
#     mutate(
#         ttd_pnl = cumsum(dly_pnl)
#         , ttd_pnl_sitout = cumsum(dly_pnl_sitout)
#     )
# 
# 
# # gathering the columns so that we can utilize group_bys for comparison
# df_sitout_comparison <-
#     df_sitout_daily %>%
#     select(data_date, daily = dly_pnl, ttd = ttd_pnl) %>%
#     mutate(signal = "base") %>%
#     bind_rows(
#         df_sitout_daily %>%
#             select(data_date, daily = dly_pnl_sitout, ttd = ttd_pnl_sitout) %>%
#             mutate(signal = "sitout")
#     )
# 
# 
# # overall perforamance
# df_sitout_comparison %>%
#     mutate(year = year(data_date)) %>%
#     group_by(signal) %>%
#     summarize(
#         pnl = sum(daily)
#         , sharpe = (mean(daily) / sd(daily)) * sqrt(252)
#     )
# 
# 
# # calculating performance by year
# df_sitout_by_year <-
#     df_sitout_comparison %>%
#     mutate(year = year(data_date)) %>%
#     group_by(signal, year) %>%
#     summarize(
#         pnl = sum(daily)
#         , sharpe = (mean(daily) / sd(daily)) * sqrt(252)
#     )
