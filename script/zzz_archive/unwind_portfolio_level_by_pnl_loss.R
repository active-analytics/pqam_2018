# clearing shit out
rm(list=ls())
cat("\014")

# load packages
library(tidyverse)
library(tictoc)


###################################
## loading parallax 2018 dataset ##
###################################
chr_path_root <- "/Users/Pritam/files/ods/plx_2018/data_output/"

# chain description
df_chain_desc <- 
    read_csv(paste0(chr_path_root, "spy_weekly_chain_desc_5yr.csv"))

# chain history
df_chain_hist <- 
    read_csv(paste0(chr_path_root, "spy_weekly_chain_hist_5yr.csv"))


# option history
df_opt_hist <- 
    read_csv(paste0(chr_path_root, "spy_weekly_opt_hist_5yr.csv"))


# position scaling
df_position_scaling <- 
    read_csv(paste0(chr_path_root, "spy_weekly_position_scaling_5yr.csv"))

# scaled put pnl
df_pnl_put <- 
    read_csv(paste0(chr_path_root, "spy_weekly_pnl_put_5yr.csv"))


# scaled strangle pnl
df_strangle_pnl_all <- 
    read_csv(paste0(chr_path_root, "spy_weekly_pnl_strangle_5yr.csv"))


# scaled call pnl
df_call_pnl <- 
    read_csv(paste0(chr_path_root, "spy_weekly_pnl_call_5yr.csv"))


## isolating only the 10-delta
df_strangle_pnl <-
    df_strangle_pnl_all %>% filter(variation == 0.1)



## adding signed-delta
df_strangle_pnl$signed_delta <- NA_real_
for (ix in 1:nrow(df_strangle_pnl)){
    chr_type <- df_strangle_pnl$type[ix]
    dbl_delta <- df_strangle_pnl$delta[ix]
    if (chr_type == "put"){
        df_strangle_pnl$signed_delta[ix] <- -dbl_delta
    } else {
        df_strangle_pnl$signed_delta[ix] <- dbl_delta
    }
}

# df_strangle_pnl %>% 
#     group_by(data_date) %>% 
#     summarize(
#         dly_opt_pnl = sum(dly_opt_pnl)
#         , delta = -sum(signed_delta)
#     )


dbl_thresh <- c(seq(0.1, 2.5, 0.1), seq(2.75, 10, 0.25))
lst_portfolio <- list()
for(ix_thresh in 1:length(dbl_thresh)){
    tic()
    # setting threshhold
    dbl_threshold <- dbl_thresh[ix_thresh]
    
    # loop through all the chains and perform the calculations
    for (ix_chn in 1:nrow(df_chain_desc)){
        #ix_chn <- 15
         
        # grabbing the expiration date for the current chain
        dt_expiration <- df_chain_desc$expiration[ix_chn]
        
        # grabbing all the trades for the current chain
        df_curr_chn_trades <- 
            df_strangle_pnl %>% 
            filter(expiration == dt_expiration) %>% 
            arrange(data_date)
        
        # grouping together the data needed for determing management action
        # basically this just combines the PNLs and greeks of the put/calls
        df_chain_pnl <- 
            df_curr_chn_trades %>% 
            group_by(data_date) %>% 
            summarize(
                strangle_mult = mean(strangle_mult)
                , upx = mean(underlying_price)
                , bid = sum(bid)
                , ask = sum(ask)
                , dly_opt_pnl = sum(dly_opt_pnl)
                , dly_tot_pnl = sum(dly_tot_pnl)
                , net_delta = -sum(signed_delta)
            ) %>% 
            mutate(
                ttd_pnl_naked = cumsum(dly_opt_pnl)
                , ttd_pnl_dh = cumsum(dly_tot_pnl)
            )
        #df_chain_pnl
        
        df_chain_pnl$breach_naked <- NA
        df_chain_pnl$manage_mult_naked <- NA_real_
        bln_breach_naked <- FALSE
        
        df_chain_pnl$breach_dh <- NA
        df_chain_pnl$manage_mult_dh <- NA_real_
        bln_breach_dh <- FALSE
        
        df_chain_pnl$breach_naked[1] <- FALSE # breach no allowed on execution date
        df_chain_pnl$manage_mult_naked[1] <- 1 # breach no allowed on execution date
        df_chain_pnl$breach_dh[1] <- FALSE # breach no allowed on execution date
        df_chain_pnl$manage_mult_dh[1] <- 1 # breach no allowed on execution date
        dbl_premium <- df_chain_pnl$bid[1] # premium is the max profit
        # looping through all the days of the chain and determining
        # when to unwind the positions
        for (ix_dt in 2:nrow(df_chain_pnl)){
            
            # grabbing the current ttd_pnl and previou ttd pnls
            dbl_ttd_naked <- df_chain_pnl$ttd_pnl_naked[ix_dt]
            dbl_prev_ttd_naked <- df_chain_pnl$ttd_pnl_naked[ix_dt - 1]
            dbl_ttd_dh <- df_chain_pnl$ttd_pnl_dh[ix_dt]
            dbl_prev_ttd_dh <- df_chain_pnl$ttd_pnl_dh[ix_dt - 1]
          
            if(ix_dt != nrow(df_chain_pnl)){
                # if there hasn't been a breach yet, check for a breach
                # if there has already been a breach, then stays a breach
                # naked
                if ((!bln_breach_naked) & 
                    (dbl_ttd_naked <  -(dbl_threshold * dbl_premium))) {
                    bln_breach_naked <- TRUE 
                }
                # delta-hedged
                if ((!bln_breach_dh) & 
                    (dbl_ttd_dh < -(dbl_threshold * dbl_premium))) {
                    bln_breach_dh <- TRUE   
                }
            } 
            
            
            # updating the breach column
            df_chain_pnl$breach_naked[ix_dt] <- bln_breach_naked
            df_chain_pnl$breach_dh[ix_dt] <- bln_breach_dh
            
            # this is the logic for how it's going to affect subsequent PNLs
            # the position is unwound the day of the breach, so all subsequent
            # pnls after the breach are zeroed out
            df_chain_pnl$manage_mult_naked[ix_dt] <-
                as.integer(!df_chain_pnl$breach_naked[ix_dt - 1])
            df_chain_pnl$manage_mult_dh[ix_dt] <-
                as.integer(!df_chain_pnl$breach_dh[ix_dt - 1])
            
        }
        
        
        ## adding expiration column and moving to the beginning of the dataframe
        df_chain_pnl <-
            df_chain_pnl %>% 
            mutate(
                expiration = dt_expiration
            ) 
        df_chain_pnl <-
            df_chain_pnl %>% 
            select(expiration) %>% 
            bind_cols(df_chain_pnl %>% select(-expiration))
        
        ## adding threshhold number
        df_chain_pnl <-
            df_chain_pnl %>% 
            mutate(
                threshold = dbl_threshold
            ) 
        df_chain_pnl <-
            df_chain_pnl %>% 
            select(threshold) %>% 
            bind_cols(df_chain_pnl %>% select(-threshold))
        
        
        
        lst_portfolio[[length(lst_portfolio) + 1]] <- df_chain_pnl  
    }
    toc() 
    
    print(paste0(dbl_threshold, ": complete."))
}



df_portfolio <- bind_rows(lst_portfolio)
#df_portfolio


print("DONE!")


####################
## writing to CSV ##
####################
write_csv(df_portfolio, "df_portfolio_unwind_by_pnl_loss_portfolio.csv")




# ##################
# ## ANALSYS CODE ##
# ##################
# # calculating the scaled, managed PNLs
# df_portfolio <-
#     df_portfolio %>%
#         mutate(
#             managed_naked = strangle_mult * dly_opt_pnl * manage_mult_naked
#             , managed_dh = strangle_mult * dly_tot_pnl * manage_mult_dh
#         )
# 
# # aggregating pnls by day so we can calculate Sharpe-Ratio
# df_daily_pnl <-
#     df_portfolio %>%
#         group_by(threshold, data_date) %>%
#         summarize(
#             dly_managed_naked = sum(managed_naked)
#             , dly_managed_dh = sum(managed_dh)
#         )
# 
# 
# 
# #########################
# ## calculating metrics ##
# #########################
# ## 1) annualized mean of daily pnl
# ## 2) annualized standard-deviation of daily pnl
# ## 3) annualized sharpe-ratio
# 
# # naked options
# df_metrics_naked <-
#     df_daily_pnl %>%
#         group_by(threshold) %>%
#         summarize(
#             avg_ann = (mean(dly_managed_naked) * 252) %>% round(2)
#             , std_ann = (sd(dly_managed_naked) * sqrt(252)) %>% round(2)
#             , sharpe =
#                 ((mean(dly_managed_naked) / sd(dly_managed_naked)) * sqrt(252)) %>% round(2)
# 
#         )
# 
# 
# # delta-hedged options
# df_metrics_dh <-
#     df_daily_pnl %>%
#     group_by(threshold) %>%
#     summarize(
#         avg_ann = (mean(dly_managed_dh) * 252) %>% round(2)
#         , std_ann = (sd(dly_managed_dh) * sqrt(252)) %>% round(2)
#         , sharpe =
#             ((mean(dly_managed_dh) / sd(dly_managed_dh)) * sqrt(252)) %>% round(2)
# 
#     )
# 
# 
# 
# ##############
# ## plotting ## (get these into a single plot with gridExtra)
# ##############
# 
# # naked
# df_metrics_naked %>%
#     ggplot(aes(x=threshold, y=sharpe)) +
#     geom_point() +
#     geom_smooth(se = FALSE)
# 
# 
# df_metrics_naked %>%
#     ggplot(aes(x=threshold, y=avg_ann)) +
#     geom_point() +
#     geom_smooth(se = FALSE)
# 
# 
# df_metrics_naked %>%
#     ggplot(aes(x=threshold, y=std_ann)) +
#     geom_point() +
#     geom_smooth(se = FALSE)
# 
# 
# 
# # naked
# df_metrics_dh %>%
#     ggplot(aes(x=threshold, y=sharpe)) +
#     geom_point() +
#     geom_smooth(se = FALSE)
# 
# 
# df_metrics_dh %>%
#     ggplot(aes(x=threshold, y=avg_ann)) +
#     geom_point() +
#     geom_smooth(se = FALSE)
# 
# 
# df_metrics_dh %>%
#     ggplot(aes(x=threshold, y=std_ann)) +
#     geom_point() +
#     geom_smooth(se = FALSE)





