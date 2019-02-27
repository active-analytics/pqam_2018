# clearing shit out
rm(list=ls())
cat("\014")

# load packages
library(tidyverse)


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


df_results <-
    tibble(
        # strategy = NA_character_
        # , threshold = NA_real_
        # , avg_daily = NA_real_
        # , std_daily = NA_real_
        # , sharpe = NA_real_
    )


# all the threshholds I want to test:
#dbl_thresh <- seq(0.25, 1.5, 0.05)

dbl_thresh <- c(0.5)
for(ix_thresh in 1:length(dbl_thresh)){
    dbl_threshhold = dbl_thresh[ix_thresh]
    ###########################################################
    ## prepping the dataframe that is going to hold the pnls ##
    ###########################################################
    # separating out the 0.10 variation
    df_strangle_pnl <- 
        df_strangle_pnl_all %>% 
        filter(variation == 0.1) %>% 
        arrange(expiration, type, data_date)
    
    # adding in the execution date, as this will be utilized in the
    # logic for determining a breach
    df_strangle_pnl <- 
        df_strangle_pnl %>% 
        left_join(
            df_chain_desc %>% select(expiration, execution)
            , by = "expiration"
        )
    
    
    ## calculate ttd_pnl over groups
    df_strangle_pnl <- 
        df_strangle_pnl %>% 
            group_by(expiration, type) %>% 
            arrange(expiration, type, data_date) %>% 
            mutate(
                ttd_pnl_naked = cumsum(dly_opt_pnl)
                , ttd_pnl_dh = cumsum(dly_tot_pnl)
            ) 
        
    
    # initilizing a couple of columns
    df_strangle_pnl$breach_naked <- FALSE
    df_strangle_pnl$breach_dh <- FALSE
    df_strangle_pnl$manage_mult_naked <- NA_real_
    df_strangle_pnl$manage_mult_dh <- NA_real_
    df_strangle_pnl$manage_mult_naked[1] <- 1 # the loop below starts at 2, 
    df_strangle_pnl$manage_mult_dh[1] <- 1 # so need to fill this in
    
    ##############################################################################
    ## looping through df_strangle_pnl and determining when breach has occurred ##
    ##############################################################################
    bln_breach_naked <- FALSE
    bln_breach_dh <- FALSE
    dbl_premium <- df_strangle_pnl$bid[1]
    for(ix_pnl in 2:nrow(df_strangle_pnl)){
        dt_trade <- df_strangle_pnl$data_date[ix_pnl]
        dt_exec <- df_strangle_pnl$execution[ix_pnl]
        dt_expiration <- df_strangle_pnl$expiration[ix_pnl]
        dbl_prev_ttd_naked <- df_strangle_pnl$ttd_pnl_naked[ix_pnl-1]
        dbl_ttd_naked <- df_strangle_pnl$ttd_pnl_naked[ix_pnl]
        dbl_prev_ttd_dh <- df_strangle_pnl$ttd_pnl_dh[ix_pnl-1]
        dbl_ttd_dh <- df_strangle_pnl$ttd_pnl_dh[ix_pnl]
        
        
        
        if(dt_trade == dt_exec){
            # there is never a breach on execution date
            bln_breach_naked <- FALSE
            bln_breach_dh <- FALSE
            dbl_premium <- df_strangle_pnl$bid[ix_pnl]
        } else {
            if(dt_trade != dt_expiration){
                ## if there hasn't been a breach yet, check for a breach
                ## if there has already been a breach, then stays a breach
                # naked
                if (
                    (!bln_breach_naked) & 
                    (dbl_ttd_naked > (dbl_threshhold * dbl_premium))
                    ) {
                    bln_breach_naked <- TRUE   
                }
                
                # dh
                if (
                    (!bln_breach_dh) & 
                    (dbl_ttd_dh > (dbl_threshhold * dbl_premium))
                ) {
                    bln_breach_dh <- TRUE   
                }
                
            } else {
                ## I don't allow for a breach on expiration date
                # naked
                if(
                    (!bln_breach_naked) & 
                    (dbl_prev_ttd_naked < (dbl_threshhold * dbl_premium))
                   ){
                    bln_breach_naked <- FALSE # going passed the threshold on 
                                              # expiration is not a  breach
                }
                
                # dh
                if(
                    (!bln_breach_dh) & 
                    (dbl_prev_ttd_dh < (dbl_threshhold * dbl_premium))
                ){
                    bln_breach_dh <- FALSE # going passed the threshold on 
                    # expiration is not a  breach
                }
            }
            
            
        }
        
        # update strangle PNL dataframe
        df_strangle_pnl$breach_naked[ix_pnl] <- bln_breach_naked
        df_strangle_pnl$breach_dh[ix_pnl] <- bln_breach_dh
        
        # this is the logic for how it's going to affect subsequent PNLs
        if(dt_exec == dt_trade){
            # you never breach on the first date
            df_strangle_pnl$manage_mult_naked[ix_pnl] <- 1
            df_strangle_pnl$manage_mult_dh[ix_pnl] <- 1
        } else {
            # based off of a breach from previous period
            df_strangle_pnl$manage_mult_naked[ix_pnl] <-
                as.integer(!df_strangle_pnl$breach_naked[ix_pnl - 1])
            df_strangle_pnl$manage_mult_dh[ix_pnl] <-
                as.integer(!df_strangle_pnl$breach_dh[ix_pnl - 1])
        }
    }
    
    
    
    ######################################
    ## calculating PNLs and comparisons ##
    ######################################
    # adding in the managed pnls
    df_strangle_pnl <- 
        df_strangle_pnl %>% 
        mutate(
            managed_naked_pnl = (manage_mult_naked) * dly_opt_pnl
            , managed_dh_pnl = (manage_mult_dh) * dly_tot_pnl 
        ) 
    
    # calculating the comparison PNLs
    df_comparision <- 
        df_strangle_pnl %>% 
        mutate(
            htm_naked = dly_opt_pnl * strangle_mult
            , htm_dh = dly_tot_pnl * strangle_mult
            , managed_naked = managed_naked_pnl * strangle_mult
            , managed_dh = managed_dh_pnl * strangle_mult
        ) %>% 
        group_by(data_date) %>% 
        summarize(
            htm_naked = sum(htm_naked)
            , htm_dh = sum(htm_dh)
            , managed_naked = sum(managed_naked)
            , managed_dh = sum(managed_dh)
        )
    
    
    ## testing
    # df_strangle_pnl %>% 
    #     select(expiration, type, data_date, bid, ask
    #            , dly_opt_pnl, ttd_pnl_naked, breach_naked
    #            , manage_mult_naked, managed_naked_pnl) %>% View()
    # 
    df_strangle_pnl %>%
        select(expiration, type, data_date, bid, ask
               , underlying_price, delta, implied_vol
               , dly_opt_pnl, dly_dh_pnl, dly_tot_pnl, ttd_pnl_dh, breach_dh
               , manage_mult_dh, managed_dh_pnl) %>% View()
        print(n = 50)
    
    
    
    # managed naked
    dbl_avg <- (df_comparision$managed_naked %>% mean() * 252) %>% round(2)
    dbl_std <- (df_comparision$managed_naked %>% sd() * sqrt(252)) %>% round(2)
    dbl_sharpe <- (dbl_avg / dbl_std)  %>% round(2)
    df_results <- 
        df_results %>% 
            bind_rows(
                tibble(
                    strategy = "naked"
                    , threshold = dbl_threshhold
                    , avg_annual = dbl_avg
                    , std_annual = dbl_std
                    , sharpe = dbl_sharpe
                )
                
            )
        
    
    # managed dh
    dbl_avg <- (df_comparision$managed_dh %>% mean() * 252)  %>% round(2)
    dbl_std <- (df_comparision$managed_dh %>% sd() * sqrt(252))  %>% round(2)
    dbl_sharpe <- (dbl_avg / dbl_std)  %>% round(2)
    df_results <- 
        df_results %>% 
            bind_rows(
                tibble(
                    strategy = "delta-hedge"
                    , threshold = dbl_threshhold
                    , avg_annual = dbl_avg
                    , std_annual = dbl_std
                    , sharpe = dbl_sharpe    
                )
                
            )
}

##############
## graphing ##
##############
## sharpe
# df_results %>%
#     ggplot(aes(x = threshold, y = sharpe)) +
#     geom_point() +
#     geom_smooth(method = "loess", se=FALSE) +
#     facet_wrap(~strategy, nrow=2)
# 
# 
# # pnl
# df_results %>%
#     ggplot(aes(x = threshold, y = avg_annual)) +
#     geom_point() +
#     geom_smooth(method = "loess", se=FALSE) +
#     facet_wrap(~strategy, nrow=2)




