# the purpose of this script is to create the datasets for exercise #4
# for fm5990 spring 2019


######################
## loading packages ##
######################
library(tidyverse)
library(lubridate)
library(tidyquant)


#####################
## reading-in data ##
#####################
df_pnl <- read_csv("data_output/spy_monthly_2014_2018_pnl_scaled.csv")

df_chain_desc <- read_csv("data_output/spy_monthly_2014_2018_chain_desc.csv")

df_scaling <- read_csv("data_output/spy_monthly_2014_2018_position_scaling.csv")


###############################
## creating trades dataframe ##
###############################
df_trades <- 
    df_scaling %>% 
    filter(year(expiration) == 2018) %>% 
    filter(variation == 0.3) %>% 
    mutate(
        direction = "sell"
        , quantity = 1L
        , type = "call"
    ) %>%
    select(
       execution_date = execution, direction, quantity, underlying, type
       , expiration, d2x, trade_price = bid.call
    )



############################
## creating pnl dataframe ##
############################
df_call_pnl <-
    df_pnl %>%
        filter(variation == 0.3) %>%
        filter(strategy == "call") %>%
        filter(year(expiration) == 2018) %>% 
        select(
            underlying = underlying_symbol, upx = underlying_price, type, expiration
            , data_date, strike, bid, ask, implied_vol, delta, dly_opt_pnl, dly_dh_pnl 
        )



################################
## adding strike to df_trades ##
################################
df_trades <- 
    df_trades %>% 
        left_join(
            df_call_pnl %>% select(underlying, expiration, data_date, strike)
            , by = c("underlying", "expiration", "execution_date"="data_date")
        ) %>% 
        select(
            execution_date:type, strike, expiration:trade_price
        )



##################
## write to CSV ##
##################
write_csv(df_trades, "spy_2018_call_trade.csv")
write_csv(df_call_pnl, "spy_2018_call_pnl.csv")


chr_symbol <- c("IWM", "TNA")

################################
## IWM vs TNA assignment data ##
################################
df_IWM_TNA_px <- 
    tq_get(chr_symbol, get="stock.prices", from="2014-01-01", to="2019-01-01")


df_IWM_TNA_ret <- 
    df_IWM_TNA_px %>% 
        group_by(symbol) %>% 
        tq_transmute(
            select = adjusted
            , mutate_fun = periodReturn
            , period = "daily"
            , col_rename = "return"
        )


df_IWM_TNA <- 
    df_IWM_TNA_px %>% 
        left_join(
            df_IWM_TNA_ret
            , by = c("symbol", "date")
        )


#write_csv(df_IWM_TNA, "IWM_TNA_2014_2018.csv")


############################
## simple call assignment ##
############################
df_call_pnl %>% 
    filter(expiration == data_date) %>% 
    filter(upx < strike)



df_simple_call <- 
    df_call_pnl %>% 
        filter(
            expiration %in% c(as.Date("2018-09-21"))
        )

df_simple_call <- 
    df_simple_call %>% 
        mutate(
            mid = (bid + ask) / 2
        )


df_simple_call <- 
    df_simple_call %>% 
        select(
            underlying, upx, type, expiration, data_date, strike
            , bid, ask, mid, delta)


write_csv(df_simple_call, "spy_call_pnl.csv")

















