rm(list = ls())
cat("\014")

# loading packages
library(tidyverse)
library(lubridate)



#########################
## reading in the data ##
#########################
df_managed_pnl <- 
    read_csv("data_output/spy_weekly_managed_pnl_10_300_5yr.csv")






#######################################
## calculating daily-pnl and ttd pnl ##
#######################################
df_daily_pnl <-
    df_managed_pnl %>% 
        filter(strategy %in% c("put", "strangle")) %>% 
        group_by(strategy, data_date) %>% 
        summarize(
            dly_pnl = sum(scaled_managed_pnl)
        ) %>% 
        mutate(
            ttd_pnl = cumsum(dly_pnl)
        )
 

   
###################
## ttd-PNL graph ##
###################
df_daily_pnl %>% 
    ggplot(aes(x = data_date, y = ttd_pnl, color = strategy)) +
    geom_line()




###############################
## basic performance metrics ##
###############################
# strangle premium sold
df_scaling %>% 
    filter(variation == 0.1) %>% 
    filter(expiration < as.Date("2018-01-01")) %>% 
    .$strangle_prem_sold %>% 
    sum()

# put premium sold
df_scaling %>% 
    filter(variation == 0.1) %>% 
    filter(expiration < as.Date("2018-01-01")) %>%  
    .$put_prem_sold %>% 
    sum()

# total pnl by strategy
df_daily_pnl %>% 
    filter(data_date < as.Date("2018-01-01")) %>%
    group_by(strategy) %>% 
    summarize(
        total_pnl = sum(dly_pnl)
    )

# sharpe-ratio by strategy
df_daily_pnl %>% 
    group_by(strategy) %>% 
    summarize(
        sharpe = (mean(dly_pnl) / sd(dly_pnl)) * sqrt(252)
    )



##############################
## sitting out after losses ##
##############################




# checking the pnl of the sit-outs
#df_sit_out %>% filter(sitout) %>%  .$exp_pnl %>% sum()


# testing
# checking out a particular expiraitons; this expiration took a shit on the final day
# df_managed_pnl %>% 
#     filter(strategy == "strangle") %>% 
#     filter(expiration == "2015-08-21") %>% View()
# 
# 




# graph of the pnls
# df_sitout_daily %>% 
#     ggplot() +
#     geom_line(aes(x = data_date, y = ttd_pnl), color = "blue") +
#     geom_line(aes(x = data_date, y = ttd_pnl_sitout), color = "grey")



# using only days you have on trades to calculate Sharpe-Ratio
df_trade_days_only <- 
    df_strangle_pnl %>% 
    filter(!sitout) %>% 
    group_by(data_date) %>% 
    summarize(
        dly_pnl_sitout = sum(sitout_pnl)
    ) %>% 
    mutate(
        ttd_pnl_situout = cumsum(dly_pnl_sitout)
    )


# calculating sharpe ratio just to check 
(mean(df_sitout_daily$dly_pnl_sitout) / sd(df_sitout_daily$dly_pnl_sitout)) * sqrt(252)
(mean(df_trade_days_only$dly_pnl_sitout) / sd(df_trade_days_only$dly_pnl_sitout)) * sqrt(252)



    
# pnl by year
df_sitout_by_year %>% 
    filter(year > 2013) %>% 
    ggplot(aes(factor(year), pnl, fill = strategy)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
            title = "PNL by Year"
            , subtitle = "sitting out -vs- not sitting out"
            , x = "trade date"
            , y = "annual pnl"
        )

# sharpe by year
df_sitout_by_year %>% 
    filter(year > 2013) %>% 
    ggplot(aes(factor(year), sharpe, fill = strategy)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Sharpe by Year"
        , subtitle = "sitting out -vs- not sitting out"
        , x = "trade date"
        , y = "sharpe-ratio"
    )


##################################
## GARCH(1,1) buy/sell strategy ##
##################################



 




# calculating sharpe ratio
(mean(df_garch_daily$dly_pnl_garch) / sd(df_garch_daily$dly_pnl_garch)) * sqrt(252)






# graphing pnl by year
df_garch_by_year %>% 
    filter(year > 2013) %>% 
    ggplot(aes(factor(year), pnl, fill = strategy)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "PNL by Year"
        , subtitle = "garch -vs- no-garch"
        , x = "trade date"
        , y = "annual pnl"
    )


# sharpe by year
df_garch_by_year %>% 
    filter(year > 2013) %>% 
    ggplot(aes(factor(year), sharpe, fill = strategy)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Sharpe by Year"
        , subtitle = "garch -vs- no-garch"
        , x = "trade date"
        , y = "sharpe-ratio"
    )





####################
## things to add  ##
####################
# 1) First section will have puts and strangle, and will give high level 
#    performance characteristics for both.  I'll then argue that they are very 
#    similar strategies, with strangles having a higher Sharpe, and therefore
#    I'll just focus on strangles for the remainder of the analysis.


# 2) sitting out 

# 2) An analysis of worst outcomes for the strategy. (If time.)

# 3) comparison with Pritam's actual performance  (If time.)















