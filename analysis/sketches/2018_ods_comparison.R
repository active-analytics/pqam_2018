dbl_dly_ret_opt  <- 
(df_strategy_comp %>% 
    filter(strategy == "option_selling") %>% 
    filter(trade_date >= "2018-03-16") %>% 
    filter(trade_date <= "2018-12-21") %>% 
    .$dly_ret)


dbl_dly_ret_spy  <- 
    (df_strategy_comp %>% 
         filter(strategy == "spy") %>% 
         filter(trade_date >= "2018-03-16") %>% 
         filter(trade_date <= "2018-12-21") %>% 
         .$dly_ret)


(prod(dbl_dly_ret_opt + 1))^(252/195) - 1
(prod(dbl_dly_ret_spy + 1))^(252/195) - 1


(mean(dbl_dly_ret_opt) / sd(dbl_dly_ret_opt)) * sqrt(252)
(mean(dbl_dly_ret_spy) / sd(dbl_dly_ret_spy)) * sqrt(252)