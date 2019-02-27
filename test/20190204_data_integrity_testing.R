rm(list=ls())
cat("\014")

# the purpose of this script is to look into an anomaly that I noticed
# when looking at the portfolio level unwind by PNL loss

# load packages
library(tidyverse)
library(tictoc)


# read the data
df_portfolio <-
    read_csv("data_output/df_portfolio_unwind_by_pnl_loss_portfolio.csv")


#df_portfolio


##################
## ANALSYS CODE ##
##################
# calculating the scaled, managed PNLs
df_portfolio <-
    df_portfolio %>%
    mutate(
        managed_naked = strangle_mult * dly_opt_pnl * manage_mult_naked
        , managed_dh = strangle_mult * dly_tot_pnl * manage_mult_dh
    )

# aggregating pnls by day so we can calculate Sharpe-Ratio
df_daily_pnl <-
    df_portfolio %>%
    group_by(threshold, data_date) %>%
    summarize(
        dly_managed_naked = sum(managed_naked)
        , dly_managed_dh = sum(managed_dh)
    )



#########################
## calculating metrics ##
#########################
## 1) annualized mean of daily pnl
## 2) annualized standard-deviation of daily pnl
## 3) annualized sharpe-ratio

# naked options
df_metrics_naked <-
    df_daily_pnl %>%
    group_by(threshold) %>%
    summarize(
        avg_ann = (mean(dly_managed_naked) * 252) %>% round(2)
        , std_ann = (sd(dly_managed_naked) * sqrt(252)) %>% round(2)
        , sharpe =
            ((mean(dly_managed_naked) / sd(dly_managed_naked)) * sqrt(252)) %>% round(2)
        
    )


# delta-hedged options
df_metrics_dh <-
    df_daily_pnl %>%
    group_by(threshold) %>%
    summarize(
        avg_ann = (mean(dly_managed_dh) * 252) %>% round(2)
        , std_ann = (sd(dly_managed_dh) * sqrt(252)) %>% round(2)
        , sharpe =
            ((mean(dly_managed_dh) / sd(dly_managed_dh)) * sqrt(252)) %>% round(2)
        
    )



##############
## plotting ## (get these into a single plot with gridExtra)
##############

# naked
df_metrics_naked %>%
    ggplot(aes(x=threshold, y=sharpe)) +
    geom_point() +
    geom_smooth(se = FALSE)


df_metrics_naked %>%
    ggplot(aes(x=threshold, y=avg_ann)) +
    geom_point() +
    geom_smooth(se = FALSE)


df_metrics_naked %>%
    ggplot(aes(x=threshold, y=std_ann)) +
    geom_point() +
    geom_smooth(se = FALSE)



# delta-hedged
df_metrics_dh %>%
    ggplot(aes(x=threshold, y=sharpe)) +
    geom_point() +
    geom_smooth(se = FALSE)


df_metrics_dh %>%
    ggplot(aes(x=threshold, y=avg_ann)) +
    geom_point() +
    geom_smooth(se = FALSE)


df_metrics_dh %>%
    ggplot(aes(x=threshold, y=std_ann)) +
    geom_point() +
    geom_smooth(se = FALSE)




df_six <- 
    df_portfolio %>%
    filter(threshold == 9.6) #%>%
    #arrange(expiration, data_date)


df_eight <- 
    df_portfolio %>% 
    filter(threshold == 9.8) %>% 
    arrange(expiration, data_date)


df_six %>% select(expiration, data_date, breach_dh) %>% 
    left_join(
        df_eight %>% select(expiration, data_date, breach_dh)
        , by = c("expiration", "data_date")
    ) %>% 
    filter(breach_dh.x != breach_dh.y)



df_portfolio %>% 
    filter(threshold == 9.6) %>% 
    filter(expiration == "2018-02-09") %>% View()


df_portfolio %>% 
    filter(threshold == 10.0) %>% 
    filter(breach_naked == TRUE)




# dual scatter plots naked options
plt_thresh <-
    df_metrics_naked %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(threshold))) +
    geom_text(check_overlap = TRUE, size=3) +
    ggtitle("Threshold")

plt_sharpe <-
    df_metrics_naked %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(sharpe))) +
    geom_text(check_overlap = TRUE, size=3) +
    ggtitle("Sharpe-Ratio")

gridExtra::grid.arrange(plt_thresh, plt_sharpe, nrow=1)



# dual scatter plots delta-hedge options
plt_thresh <-
    df_metrics_dh %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(threshold))) +
    geom_text(check_overlap = TRUE, size=3) +
    ggtitle("Threshold")

plt_sharpe <-
    df_metrics_dh %>%
    ggplot(aes(x=avg_ann, y=std_ann, label = factor(sharpe))) +
    geom_text(check_overlap = TRUE, size=3) +
    ggtitle("Sharpe-Ratio")

gridExtra::grid.arrange(plt_thresh, plt_sharpe, nrow=1)

