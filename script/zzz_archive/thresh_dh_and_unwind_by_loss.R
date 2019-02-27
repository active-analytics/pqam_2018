# clearing things outs
rm(list = ls())
cat("\014")


# loading libraries
library(tidyverse)
library(tictoc)



# reading in data
df_thresh_dh <- 
    read_csv("data_output/df_portfolio_threshold_delta_hedging_by_delta.csv")

# renaming a columne
df_thresh_dh <- 
    df_thresh_dh %>% 
        rename(dh_threshold = threshold)

# seperating out all the unique threshold, expiration combinations
df_exp <- 
    df_thresh_dh %>% 
    distinct(expiration, dh_threshold)


dbl_loss_trigger <- seq(0.1, 3, 0.1)

#dbl_loss_trigger <- c(0.5, 1.5)
lst_portfolio <- list()
for(ix_trig in 1:length(dbl_loss_trigger)){
    
    # grabbing the current loss trigger
    dbl_loss_trig <- dbl_loss_trigger[ix_trig]
    
    tic()
    for (ix_exp in 1:nrow(df_exp)){
        dbl_dh_thresh <- df_exp$dh_threshold[ix_exp]
        dt_expiration <- df_exp$expiration[ix_exp]
        
        df_chain <- 
            df_thresh_dh %>% 
            filter(dh_threshold == dbl_dh_thresh) %>% 
            filter(expiration == dt_expiration)
        
        df_chain <- 
            df_chain %>% 
            mutate(
                ttd_pnl = cumsum(tot_pnl)
            )
        
        df_chain$breach <- NA
        df_chain$manage_mult <- NA
        
        df_chain$breach[1] <- FALSE
        df_chain$manage_mult[1] <- 1
        dbl_premium <- df_chain$bid[1]
        bln_breach <- FALSE
        for (ix_dt in 2:nrow(df_chain)){
            dbl_ttd <- df_chain$ttd_pnl[ix_dt]
            dbl_prev_ttd <- df_chain$ttd_pnl[ix_dt - 1]
            
            if((ix_dt != nrow(df_chain)) & (!bln_breach)){
                # if there hasn't been a breach yet, check for a breach
                if ((dbl_ttd <  -(dbl_loss_trig * dbl_premium))) {
                    bln_breach <- TRUE 
                }
            }
            
            # updating the breach column
            df_chain$breach[ix_dt] <- bln_breach
            
            # this is the logic for how it's going to affect subsequent PNLs
            # the position is unwound the day of the breach, so all subsequent
            # pnls after the breach are zeroed out
            df_chain$manage_mult[ix_dt] <-
                as.integer(!df_chain$breach[ix_dt - 1])
            
        }
        
        
        ## adding expiration column and moving to the beginning of the dataframe
        df_chain <-
            df_chain %>% 
            mutate(
                loss_trigger = dbl_loss_trig
            ) 
        df_chain <-
            df_chain %>% 
            select(loss_trigger) %>% 
            bind_cols(df_chain %>% select(-loss_trigger))
        
        lst_portfolio[[length(lst_portfolio) + 1]] <- df_chain
    }
    
    print(paste0(dbl_loss_trig, ": complete."))
    toc()
}



df_portfolio <- bind_rows(lst_portfolio)


#write_csv(df_portfolio, "df_portfolio_thresh_dh_and_unwind_by_loss.csv")
