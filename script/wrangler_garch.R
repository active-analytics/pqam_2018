# clearing shit out
rm(list=ls())
cat("\014")

# load packages
library(tidyverse)
library(backtestr)
library(rugarch)
library(bizdays)
library(tidyquant)


# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


# reading in chain history
df_chain_hist <- 
    read_csv("data_output/spy_weekly_chain_hist_5yr.csv")

df_chain_hist <- 
    df_chain_hist %>% 
        mutate(
            d2x = bizdays(trade_date, expiration)
        )




df_chain_hist$garch_forecast <- NA_real_
spec <- ugarchspec()
for (ix_hist in 1:nrow(df_chain_hist)){
    
    
    chr_underlying <- "SPY"
    dt_trade <- df_chain_hist$trade_date[ix_hist]
    int_d2x <- df_chain_hist$d2x[ix_hist]
    dt_garch_start <- add.bizdays(dt_trade, -2520)
    
    
    # using tidyquant to get the prices from yahoo
    if(int_d2x != 0){
        df_px <- 
            tryCatch(
                tq_get(
                    chr_underlying
                    , get = "stock.prices"
                    , from = dt_garch_start
                    , to = dt_trade
                ) 
                , warning = function(cond) return(tibble(adjusted = NA_real_))
                , error = function(cond) return(tibble(adjusted = NA_real_))
            )
        
        # if there are no missing prices then fitting the data
        if(sum(is.na(df_px$adjusted))==0){
            df_px <- df_px %>% mutate(ret = adjusted/lag(adjusted) - 1)
            
            # fitting the model
            fit <- ugarchfit(spec, df_px$ret[2:nrow(df_px)], solver = 'hybrid')
            
            # forcasting until expiration using the fit
            ugfore <- ugarchforecast(fit, n.ahead = int_d2x)
            
            # calculated volatility until maturity
            dbl_vol_forecast <- ugfore@forecast$sigmaFor %>% mean() * sqrt(252)
            
            df_chain_hist$garch_forecast[ix_hist] <- dbl_vol_forecast    
        }    
    }


    # printing progress to screen
    print(
        paste0(chr_underlying, ": ", ix_hist, " of ", nrow(df_chain_hist))
    )
}


#write_csv(df_chain_hist, "spy_weekly_chain_hist_garch_5yr.csv")
# df_chain_hist %>% 
#     filter(int_d2x != 0) %>% 
#     filter(is.na(garch_forecast))
