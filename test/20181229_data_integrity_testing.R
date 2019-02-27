# clearing shit out
rm(list=ls())
cat("\014")

# loading packages
library(tidyverse)
library(lubridate)
library(backtestr)
library(tidyquant)


#######################
## chain description ##
#######################
df_chain_desc <- 
    read_csv("data_output/spy_weekly_chain_desc_5yr.csv")

# reasonableness of numerical values
df_chain_desc$d2x %>% summary()
df_chain_desc$num_opts %>% summary()
df_chain_desc$exec_day_volume %>% summary()

# missing data
df_chain_desc[rowSums(is.na(df_chain_desc)) > 0, ]


###################
## chain history ##
###################
df_chain_hist <-
    read_csv("data_output/spy_weekly_chain_hist_5yr.csv")


# reasonableness of numerical values
df_chain_hist %>% 
#filter(trade_date != last_trade_date) %>% 
    .$implied_forward %>% 
    summary()

df_chain_hist %>% 
    filter(trade_date != last_trade_date) %>% 
    .$bid_swap_rate %>% 
    summary()

df_chain_hist %>% 
    filter(trade_date != last_trade_date) %>% 
    .$ask_swap_rate %>% 
    summary()

df_chain_hist %>% 
    filter(trade_date != last_trade_date) %>% 
    .$mid_swap_rate %>% 
    summary()

# comparing implied forward to spot prices
# this was a good check because it found some issues going on in 
# september of 2014
df_yahoo <-   
    tq_get(
        "SPY"
        , get = "stock.prices"
        , from = "2013-12-20"
        , to = "2018-11-30"
    )
df_yahoo

df_price_comparison <- 
    df_yahoo %>% 
        select(date, close) %>% 
        left_join(
            df_chain_hist %>% select(trade_date, implied_forward)
            , by = c("date" = "trade_date")
        )


ggplot(data = df_price_comparison) +
    geom_line(mapping = aes(x = date, y = close), color = "blue") +
    geom_line(mapping = aes(x = date, y = implied_forward), color = "red")
                  
df_price_comparison %>% 
    mutate(
        diff = abs(close - implied_forward)
        , pct_diff = abs(close - implied_forward) / close
    ) %>% 
    arrange(desc(diff)) %>% 
    View()


# lots of implied forwards greater than close prices
df_price_comparison %>% filter(implied_forward > close) %>% View()


# missing data
df_chain_hist[rowSums(is.na(df_chain_hist)) > 0, ]



####################
## option history ##
####################

df_opt_hist <- read_csv("data_output/spy_weekly_opt_hist_5yr.csv")


# checks how many execution date options there are, lowest is 14
df_chain_desc %>% 
    left_join(
        df_opt_hist
        , by = c("expiration", "execution" = "data_date")
    ) %>% 
    group_by(
        expiration, execution
    ) %>% 
    summarize(
        num_opts = sum(!is.na(strike))
    ) %>% 
    arrange(num_opts)


# missing data, there are about 25, from 12/16/2016, 12/23/2016, 12/30/2016
df_opt_hist[rowSums(is.na(df_opt_hist)) > 0, ] %>% View()


option_chain(
    trade_date = mdy("12/30/2016")
    , underlying = "SPY"
    , expiration = mdy("12/30/2016")
)



# bad option prices on expiration date - look at volatility skews
df_exec_day_options <- 
    df_opt_hist %>% 
        left_join(
            df_chain_desc %>% select(expiration, execution, last_trade_date)
            , by = "expiration"
        ) %>% 
        filter(data_date == execution)


# randomly sampling an expiraton and then plotting its execution
# day option prices (I notices a few with some gaps but generally
# speaking it looked fine)
dt_random_exp <- 
    sample_n(df_chain_desc, 1) %>% .$expiration %>% `[`(1)
df_exec_day_options %>% 
    filter(expiration == dt_random_exp) %>% 
    ggplot() +
        geom_point(aes(x = strike, y = mid))
dt_random_exp # printing to the screen



## check that you agree with option payoffs ##

# grabs all options on last trade date
df_exp_opt <- 
    df_opt_hist %>% 
    left_join(
        df_chain_desc %>% select(expiration, execution, last_trade_date)
        , by = "expiration"
    ) %>% 
    filter(data_date == last_trade_date)

# one off payoff function
payoff <- function(type, upx, strike){
    if (type == "call"){
        p <- max(upx - strike, 0)
    } else {
        p <- max(strike - upx, 0)
    }
    
    p
}


df_exp_opt %>% 
    mutate(
        payoff = 
            pmap_dbl(
                df_exp_opt %>% select(type, upx = underlying_price, strike)
                , payoff
            )
    ) %>% 
    filter(mid == payoff)





















