df_chain_old <- read_csv("spy_weekly_chain_old.csv") 
df_chain_new <- read_csv("spy_weekly_chain_new.csv")
df_chain_hist_old <- read_csv("spy_weekly_chain_hist_old.csv")
df_chain_hist_new <- read_csv("spy_weekly_chain_hist_new.csv")
df_opt_hist_old <- read_csv("spy_weekly_opt_hist_old.csv")
df_opt_hist_new <- read_csv("spy_weekly_opt_hist_new.csv")


# chain
df_chain_old
df_chain_new

df_chain_old$d2x %>% summary() ==
    df_chain_new$d2x %>% summary()

df_chain_old$num_opts %>% summary() ==
    df_chain_new$num_opts %>% summary()

df_chain_old$exec_day_volume %>% summary() ==
    df_chain_new$exec_day_volume %>% summary()


# chain_hist
df_chain_hist_old
df_chain_hist_new

df_chain_hist_old$implied_forward %>% summary() ==
    df_chain_hist_new$implied_forward %>% summary()

df_chain_hist_old$bid_swap_rate %>% summary() ==
    df_chain_hist_new$bid_swap_rate %>% summary()

df_chain_hist_old$ask_swap_rate %>% summary() ==
    df_chain_hist_new$ask_swap_rate %>% summary()


df_chain_hist_old$mid_swap_rate %>% summary() ==
    df_chain_hist_new$mid_swap_rate %>% summary()

# opt_hist
df_opt_hist_old
df_opt_hist_new

df_opt_hist_old$my_implied_vol %>% summary() ==
    df_opt_hist_new$implied_vol %>% summary()

df_opt_hist_old$my_implied_vol %>% summary() ==
    df_opt_hist_new$implied_vol %>% summary()

## this one fails because for some reason in my old code there were
## a few NAs in the my_delta
df_opt_hist_old$my_delta %>% summary() ==
    df_opt_hist_new$delta %>% summary()

df_opt_hist_old$my_vega %>% summary() ==
    df_opt_hist_new$vega %>% summary()

df_opt_hist_old$my_theta %>% summary() ==
    df_opt_hist_new$theta %>% summary()

df_opt_hist_old %>% filter(is.na(my_delta)) %>% View()
df_opt_hist_new %>% filter(is.na(volume)) %>% View()


