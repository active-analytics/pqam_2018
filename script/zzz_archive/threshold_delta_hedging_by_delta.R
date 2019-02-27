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




#dbl_thresh <- c(0, 1)
dbl_thresh <- seq(0, 1, 0.05)

lst_portfolio <- list()
# looping through all the threshholds I want to test
for(ix_thresh in 1:length(dbl_thresh)){
    tic()
    # setting threshhold
    dbl_threshold <- dbl_thresh[ix_thresh]
    
    # loop through all the chains and perform the calculations
    for (ix_chn in 1:nrow(df_chain_desc)){
        
        dt_expiration <- df_chain_desc$expiration[ix_chn]
        
        # grabbing all the trades for the current chaing
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
            )
        
        
        # delta to hedge is slightly different than net delta
        # the only difference is that on expiration, you don't hedge 
        # the delta; it's either going to be zero or one
        # I just force it to zero for everything
        df_chain_pnl <-     
            df_chain_pnl %>% 
            mutate(
                delta_to_hedge = net_delta
            )
        df_chain_pnl$delta_to_hedge[nrow(df_chain_pnl)] <- 0
        
        
        #initializing columns
        df_chain_pnl$hedge <- NA_real_  # the delta-hedge that is on EOD
        df_chain_pnl$dh_pnl <- NA_real_ # pnl from the delta-hedge
        df_chain_pnl$tot_pnl <- NA_real_ # opt_pnl + delta_hedge PNL
        dbl_hedge <- 0
        for (ix_dt in 1:nrow(df_chain_pnl)){
            
            ## calculating delta-hedge
            dbl_delta_to_hedge <- df_chain_pnl$delta_to_hedge[ix_dt]
            if(ix_dt > 1){
                # previous day delta
                dbl_hedge <- df_chain_pnl$hedge[ix_dt - 1] 
            }
            # check if current delta after hedge is past threshold
            if (abs(dbl_delta_to_hedge + dbl_hedge) > dbl_threshold){
                # if so, then rebalance deltas
                dbl_hedge <- -dbl_delta_to_hedge
            }
            df_chain_pnl$hedge[ix_dt] <- dbl_hedge 
            
            ## calculating pnl from delta-hedge
            if(ix_dt == 1){
                df_chain_pnl$dh_pnl[ix_dt] <- 0
            } else {
                dbl_prev_upx <- df_chain_pnl$upx[ix_dt - 1]
                dbl_curr_upx <- df_chain_pnl$upx[ix_dt]
                # use previous day delta to calculate pnl from delta
                df_chain_pnl$dh_pnl[ix_dt] <-
                    df_chain_pnl$hedge[ix_dt - 1] * 
                    (dbl_curr_upx - dbl_prev_upx)
            }
            
            # total pnl: option + delta-hedge
            df_chain_pnl$tot_pnl[ix_dt] <-
                df_chain_pnl$dly_opt_pnl[ix_dt] + df_chain_pnl$dh_pnl[ix_dt]
        }
        #df_chain_pnl %>% View()
        
        
        # adding expiration column and moving to the beginning of the dataframe
        df_chain_pnl <-
            df_chain_pnl %>% 
            mutate(
                expiration = dt_expiration
            ) 
        df_chain_pnl <-
            df_chain_pnl %>% 
            select(expiration) %>% 
            bind_cols(df_chain_pnl %>% select(-expiration))
        
        # adding threshhold number
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

print("DONE!")


write_csv(df_portfolio, "df_portfolio_threshold_delta_hedging_by_delta.csv")


##############
## ANALYSIS ##
##############
# adding scaled PNL column
df_portfolio <-
    df_portfolio %>%
        mutate(
            scaled_tot_pnl = strangle_mult * tot_pnl
        )

# calculating daily pnls for each threshold
df_daily_pnl <-
    df_portfolio %>%
        group_by(threshold, data_date) %>%
        summarize(
            dly_tot_pnl = sum(scaled_tot_pnl)
        )

# calculating metrics
df_metrics <-
    df_daily_pnl %>%
        group_by(threshold) %>%
        summarize(
            avg_ann = (mean(dly_tot_pnl) * 252) %>% round(2)
            , std_ann = (sd(dly_tot_pnl) * sqrt(252)) %>% round(2)
            , sharpe =
                ((mean(dly_tot_pnl) / sd(dly_tot_pnl)) * sqrt(252)) %>% round(2)
            , dvar = quantile(dly_tot_pnl, 0.05)
        )
df_metrics    

##############
## PLOTTING ##
##############
# get these into a single plot with gridExtra    
df_metrics %>%
    ggplot(aes(x=threshold, y=sharpe)) +
    geom_point()


df_metrics %>%
    ggplot(aes(x=threshold, y=avg_ann)) +
    geom_point()


df_metrics %>%
    ggplot(aes(x=threshold, y=std_ann)) +
    geom_point()



df_metrics %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(sharpe))) +
    geom_point() +
    geom_text(check_overlap = TRUE)

df_metrics %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(threshold))) +
    geom_point() +
    geom_text(check_overlap = TRUE, size=3, nudge_y = 0.05)

df_metrics %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(sharpe))) +
    geom_point() +
    geom_text(check_overlap = TRUE, size=3, nudge_y = 0.05)



    
    
    
    
    







