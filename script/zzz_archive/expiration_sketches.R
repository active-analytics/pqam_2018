library(backtestr)
library(lubridate)
library(tidyvers)



# 2014
df_spy_20140102 <- 
    option_underlying(
        trade_date = ymd(20140102)
        , underlying = "SPY"
    )

df_spy_20140102 %>% 
    distinct(expiration) %>%
    arrange(expiration)

# 201501
df_spy_20150102 <-
    option_underlying(
        trade_date = ymd(20150102)
        , underlying = "SPY"
    )

df_spy_20150102 %>% 
    distinct(expiration) %>% 
    arrange(expiration)


# 201512
df_spy_20151201 <-
    option_underlying(
        trade_date = ymd(20151201)
        , underlying = "SPY"
    )

df_spy_20151201 %>% 
    distinct(expiration) %>% 
    arrange(expiration)



# 2016
df_spy_20160104 <-
    option_underlying(
        trade_date = ymd(20160104)
        , underlying = "SPY"
    )

df_spy_20160104 %>% 
    distinct(expiration) %>% 
    arrange(expiration)






# testing



