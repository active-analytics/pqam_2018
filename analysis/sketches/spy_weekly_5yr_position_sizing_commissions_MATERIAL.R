#############
## testing ##
#############
# df_scaling %>%
#     select(expiration, scaling_aum_pnl = aum_pnl) %>%
#     left_join(
#         df_strangle_pnl %>%
#             group_by(expiration) %>%
#             summarize(
#                 strangle_aum_pnl = sum(sitout_pnl * aum_position_mult)
#             )
#         , by = "expiration"
#     ) %>%
#     filter(round(scaling_aum_pnl, 2) != round(strangle_aum_pnl, 2))
# 2,566,874
# df_scaling$aum[nrow(df_scaling)] + df_scaling$aum_pnl[nrow(df_scaling)]
# df_aum_daily$aum_ttd_pnl[nrow(df_aum_daily)]




###########################################################
## OLD methodology for calculating position size and PNL ##
###########################################################
# dbl_starting_aum = 1000000 # starting with a million
# df_scaling$aum <- NA_real_
# df_scaling$aum_position_mult <- NA_real_
# df_scaling$aum_pnl <- NA_real_
# df_scaling$aum[1] <- dbl_starting_aum
# df_scaling$aum_position_mult[1] <- dbl_starting_aum / df_scaling$implied_aum[1]
# df_scaling$aum_pnl[1] <-
#     df_scaling$exp_sitout_pnl[1] * df_scaling$aum_position_mult[1]
# # loop through and calculate pnl and new aum per expiration
# for(ix in 2:nrow(df_scaling)){
# 
#     dbl_prev_aum <- df_scaling$aum[ix - 1]
#     dbl_prev_aum_pnl <- df_scaling$aum_pnl[ix - 1]
#     dbl_curr_aum <- dbl_prev_aum + dbl_prev_aum_pnl
# 
#     df_scaling$aum[ix] <- dbl_curr_aum
#     df_scaling$aum_position_mult[ix] <-
#         dbl_curr_aum / df_scaling$implied_aum[ix]
#     df_scaling$aum_pnl[ix] <-
#         df_scaling$exp_sitout_pnl[ix] * df_scaling$aum_position_mult[ix]
# }
# #write_csv(df_scaling, "old_pnl_method.csv")


# testing
## df_old <- read_csv("old_pnl_method.csv")
## df_new <- read_csv("new_pnl_method.csv")


# calculating premium sold per expiration, number of options sold
# per exipiration, as well as notional value of underlying,
# df_scaling <- 
#     df_scaling %>% 
#         mutate(
#             aum_prem_sold = aum_position_mult * strangle_prem_sold
#             , aum_prem_percent = aum_prem_sold / aum
#             , options_sold = 2 *
#                 ((aum_prem_sold / (bid_put + bid_call)) / 100)
#         ) %>% 
#         left_join(
#             df_chain_desc %>% select(expiration, exec_day_volume)
#             , by = "expiration"
#         )

