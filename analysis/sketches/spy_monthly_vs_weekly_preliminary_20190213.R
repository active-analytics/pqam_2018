# loading packages 
library(tidyverse)




#####################
## reading in data ##
#####################
df_pnl_weekly <- 
    read_csv("data_output/spy_weekly_2014_2018_pnl_scaled.csv")

df_pnl_monthly <- 
    read_csv("data_output/spy_monthly_2014_2018_pnl_scaled.csv")


###############################################################
## WEEKLY: calculating total pnl and sharpe for all variants ##
###############################################################
df_naked_daily <- 
    df_pnl_weekly %>% 
        group_by(strategy, variation, data_date) %>% 
        summarize(
            dly_pnl = sum(scaled_dly_opt_pnl)
        ) %>% 
        mutate(
            hedge = "naked"
        )

df_dh_daily <- 
    df_pnl_weekly %>% 
    group_by(strategy, variation, data_date) %>% 
    summarize(
        dly_pnl = sum(scaled_dly_tot_pnl)
    ) %>% 
    mutate(
        hedge = "delta-hedge"
    )

df_daily <- bind_rows(df_naked_daily, df_dh_daily)

df_daily %>% 
    group_by(strategy, variation, hedge) %>% 
    summarize(
        ann_avg_pnl = mean(dly_pnl) * 252
        , sharpe = (mean(dly_pnl) / sd(dly_pnl)) * sqrt(252)
    ) %>% 
    arrange(
        strategy, hedge, variation
    )




################################################################
## MONTHLY: calculating total pnl and sharpe for all variants ##
################################################################
df_naked_daily <- 
    df_pnl_monthly %>% 
    group_by(strategy, variation, data_date) %>% 
    summarize(
        dly_pnl = sum(scaled_dly_opt_pnl)
    ) %>% 
    mutate(
        hedge = "naked"
    )

df_dh_daily <- 
    df_pnl_monthly %>% 
    group_by(strategy, variation, data_date) %>% 
    summarize(
        dly_pnl = sum(scaled_dly_tot_pnl)
    ) %>% 
    mutate(
        hedge = "delta-hedge"
    )

df_daily <- bind_rows(df_naked_daily, df_dh_daily)

df_daily %>% 
    group_by(strategy, variation, hedge) %>% 
    summarize(
        total_pnl = mean(dly_pnl) * 252
        , sharpe = (mean(dly_pnl) / sd(dly_pnl)) * sqrt(252)
    ) %>% 
    arrange(
        strategy, hedge, variation
    )

################################################
## separating out calls and puts in strangles ##
################################################
df_pnl_monthly %>% 
    filter(strategy == "strangle") %>% 
    mutate(year = lubridate::year(expiration)) %>% 
    group_by(type, variation, year) %>% 
    summarize(
        pnl = sum(scaled_dly_opt_pnl)
    ) 



####################
## cumulative pnl ##
####################



########################################################
## looking into monthly naked 10-delta calls and puts ##
########################################################
# 10-delta calls are massive losers, while 10-delta puts are massive
# winners.  I wonder if this comes down to a single time period, say 
# early 2018, or if this is through-out.  This is what I want to look
# into right now.


# pnl by month
df_pnl_monthly %>% 
    filter(variation == 0.1) %>% 
    filter(strategy == "strangle") %>%
    mutate(
        year = lubridate::year(expiration)
        , month = lubridate::month(expiration, label = TRUE)
    ) %>%
    filter(year > 2013) %>% 
    group_by(year, month, type = factor(type)) %>% 
    summarize(
        pnl = sum(scaled_dly_opt_pnl)
    ) %>% 
    ggplot(aes(month, pnl, fill = type)) +
        geom_bar(stat = "identity", position="dodge") +
        facet_wrap(~year, nrow = 5)
    
    

df_pnl_monthly %>% 
    filter(expiration == backtestr::monthly_expiration(2014, 11)) %>% 
    filter(strategy == "strangle") %>% View()





## this is some bar graph code that I am going to recycle
# df_pnl_gathered %>%
#     filter(data_date > "2014-01-01") %>% 
#     filter(strategy == "delta_hedge") %>% 
#     mutate(
#         year = factor(lubridate::year(data_date))
#         , month = lubridate::month(data_date, label = TRUE)
#     ) %>% 
#     group_by(year, month) %>%
#     summarize(
#         pnl = sum(daily_pnl)
#     ) %>% 
#     #ungroup() %>%
#     ggplot(aes(month, pnl, fill = pnl)) +
#     geom_bar(stat = "identity") +
#     facet_wrap(~year, nrow = 5) +
#     labs(
#         title = "PNL by Month"
#         , subtitle = "delta-hedged strategy"
#         , x = "trade date"
#         , y = "annual pnl"
#         #, caption =  "option prices from Delta-Neutral"
#     )











