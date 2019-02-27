library(tidyverse)



df_chain_pnl <- 
    read_csv("data_output/spy_monthly_2014_2018_chain_pnl.csv")



df_trade_pnl <- 
    read_csv("data_output/spy_monthly_2014_2018_pnl_scaled.csv")


df_strangle_pnl <- 
    df_chain_pnl %>% 
        filter(strategy == "strangle") %>% 
        filter(variation == 0.1)

df_strangle_pnl <- 
    df_strangle_pnl %>% 
        mutate(
            scaled_dly = scale_mult * dly_tot_pnl
        )


df_strangle_pnl$scaled_dly %>% sum()


df_trade_pnl %>% 
    filter(strategy == "strangle") %>% 
    filter(variation == 0.1) %>% 
    .$scaled_dly_tot_pnl %>% sum()



df_chain_hist_monthly <-
    read_csv("data_output/spy_monthly_2014_2018_chain_hist.csv")


df_chain_hist_weekly <-
    read_csv("data_output/spy_weekly_2014_2018_chain_hist.csv")
