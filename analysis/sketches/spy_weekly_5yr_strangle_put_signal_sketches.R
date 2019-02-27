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
df_scaling <- 
    read_csv("data_output/spy_weekly_position_scaling_5yr.csv")
df_chain_hist <- 
    read_csv("data_output/spy_weekly_chain_hist_garch_5yr.csv")
df_chain_desc <-
    read_csv("data_output/spy_weekly_chain_desc_5yr.csv")





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
# determining the premium by expiration
df_premium <- 
    df_scaling %>%
        filter(variation == 0.1) %>% 
        select(expiration, premium = strangle_prem_sold)


# here is the data that is used for the sit-out strategy
df_sit_out <- 
    df_managed_pnl %>% 
        filter(strategy == "strangle") %>% 
        group_by(expiration) %>% 
        summarize(
            exp_pnl = sum(scaled_managed_pnl)
            , unwind = sum(breach)
        ) %>%
        left_join(
            df_premium
            , by = "expiration"
        ) %>% 
        select(expiration, premium, exp_pnl, unwind) %>% 
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

# checking the pnl of the sit-outs
#df_sit_out %>% filter(sitout) %>%  .$exp_pnl %>% sum()


# testing
# checking out a particular expiraitons; this expiration took a shit on the final day
# df_managed_pnl %>% 
#     filter(strategy == "strangle") %>% 
#     filter(expiration == "2015-08-21") %>% View()
# 
# 

# isolationg strangles only
df_strangle_pnl <-
    df_managed_pnl %>% 
        filter(strategy == "strangle")

# joining on the sitout mult and calculatint the situout pnl
df_strangle_pnl <- 
    df_strangle_pnl %>% 
        left_join(
            df_sit_out %>% select(expiration, sitout)
            , by = "expiration"
        ) %>% 
        mutate(
            sitout_mult = as.integer(!sitout)
        ) %>% 
        mutate(
            sitout_pnl = scaled_managed_pnl * sitout_mult
        )


# calculating the daily pnl and the ttd pnl of the full strategy
df_sitout_daily <- 
    df_strangle_pnl %>% 
        group_by(data_date) %>% 
        summarize(
            dly_pnl = sum(scaled_managed_pnl)
            , dly_pnl_sitout = sum(sitout_pnl)
        ) %>% 
        mutate(
            ttd_pnl = cumsum(dly_pnl)
            , ttd_pnl_sitout = cumsum(dly_pnl_sitout)
        )


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


# gathering the columns so that we can utilize group_bys for comparison
df_sitout_comparison <- 
    df_sitout_daily %>% 
        select(data_date, daily = dly_pnl, ttd = ttd_pnl) %>% 
        mutate(strategy = "no_sitout") %>% 
        bind_rows(
            df_sitout_daily %>% 
            select(data_date, daily = dly_pnl_sitout, ttd = ttd_pnl_sitout) %>% 
            mutate(strategy = "sitout")
        )


# calculating performance by year
df_sitout_by_year <- 
    df_sitout_comparison %>% 
        mutate(year = year(data_date)) %>% 
        group_by(strategy, year) %>% 
        summarize(
            pnl = sum(daily)
            , sharpe = (mean(daily) / sd(daily)) * sqrt(252)
        )
    
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
# calculating the volatility premium
df_vol_premium <- 
    df_chain_desc %>% 
        select(expiration, execution) %>% 
        left_join(
            df_chain_hist
            , by = c("expiration", "execution"="trade_date")
        ) %>% 
        select(expiration, execution, bid_swap_rate, garch_forecast) %>% 
        mutate(
            vol_prem = bid_swap_rate - garch_forecast
        )


# in-sample first year mean
dbl_first_year_avg <- 
    df_vol_premium %>%  
        filter(expiration < "2015-01-01") %>% 
        .$vol_prem %>% 
        mean()

# in-sample first year standard deviation
dbl_first_year_sd <- 
    df_vol_premium %>%  
    filter(expiration < "2015-01-01") %>% 
    .$vol_prem %>% 
    sd()

# calculating z-score of volatility premium forecast
df_vol_premium$z_score <- NA_real_
df_vol_premium$garch_sell <- NA_real_
for (ix_exp in 1:nrow(df_vol_premium)){

    dt_expiration <- df_vol_premium$expiration[ix_exp]
    dbl_vol_prem <-  df_vol_premium$vol_prem[ix_exp]
        
    if (dt_expiration < "2015-01-01"){
        dbl_vol_prem_mean <- dbl_first_year_avg
        dbl_vol_prem_sd <- dbl_first_year_sd
    } else {
        dbl_vol_prem_mean <- df_vol_premium[(ix_exp - 51):ix_exp, ]$vol_prem %>% mean()
        dbl_vol_prem_sd <- df_vol_premium[(ix_exp - 51):ix_exp, ]$vol_prem %>% sd()
    }
    
    dbl_z_score = (dbl_vol_prem - dbl_vol_prem_mean) / dbl_vol_prem_sd
    df_vol_premium$z_score[ix_exp] <- dbl_z_score
    
    bln_sell <- TRUE
    if (dbl_z_score < - 1.5) {
        bln_sell <- FALSE
    }
    df_vol_premium$garch_sell[ix_exp] <- bln_sell
    
}


# isolating pnl for strangle onsle onley
df_strangle_pnl <-
    df_managed_pnl %>% 
    filter(strategy == "strangle")


# joining in the volatility premium forcasting
df_strangle_pnl <- 
    df_strangle_pnl %>% 
    left_join(
        df_vol_premium %>% select(expiration, garch_sell)
        , by = "expiration"
    ) 


# calculating the pnl accoutng for the garch_based sell signals
df_strangle_pnl <- 
    df_strangle_pnl %>% 
    mutate(
        garch_pnl = scaled_managed_pnl * garch_sell
    )


# calculating the daily pnls 
df_garch_daily <- 
    df_strangle_pnl %>% 
    group_by(data_date) %>% 
    summarize(
        dly_pnl = sum(scaled_managed_pnl)
        , dly_pnl_garch = sum(garch_pnl)
    ) %>% 
    mutate(
        ttd_pnl = cumsum(dly_pnl)
        , ttd_pnl_garch = cumsum(dly_pnl_garch)
    )


# calculating sharpe ratio
(mean(df_garch_daily$dly_pnl_garch) / sd(df_garch_daily$dly_pnl_garch)) * sqrt(252)



# gathering a dataframe for easy comparison using group_bys
df_garch_comparison <- 
    df_garch_daily %>% 
    select(data_date, daily = dly_pnl, ttd = ttd_pnl) %>% 
    mutate(strategy = "no_garch") %>% 
    bind_rows(
        df_garch_daily %>% 
            select(data_date, daily = dly_pnl_garch, ttd = ttd_pnl_garch) %>% 
            mutate(strategy = "garch")
    )


# calculating
df_garch_by_year <- 
    df_garch_comparison %>% 
    mutate(year = year(data_date)) %>% 
    group_by(strategy, year) %>% 
    summarize(
        pnl = sum(daily)
        , sharpe = (mean(daily) / sd(daily)) * sqrt(252)
    )


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















