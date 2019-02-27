#####################################
## td - 4/2/2015; expir - 4/2/2018 ##
#####################################

df_all_option <- option_all(ymd(20150402))

df_all_spy <- option_underlying(ymd(20150402), "SPY")
df_all_spy

df_exp_chain <- option_chain(ymd(20150402), "SPY", ymd(20150402))
df_exp_chain


option_chain(ymd(20150327), "SPY", ymd(20150402))
option_chain(ymd(20150402), "SPY", ymd(20150402))

df_all_spy <- option_underlying(ymd(20150327), "SPY")
df_all_spy <- option_underlying(ymd(20150402), "SPY")

df_all_spy %>% distinct(expiration) %>% arrange(expiration)

#######################################
## td - 12/14/2015; exp - 12/18/2015 ## 
#######################################

#df_exp_chain <- 
    option_chain(
        trade_date = ymd(20151211)
        , underlying = "SPY"
        , expiration = ymd(20151218)
    )

df_all_spy <- 
    option_underlying(
        trade_date = ymd(20150102)
        , underlying = "SPY"
    )
df_all_spy %>% distinct(expiration) %>% arrange(expiration) %>% View()










