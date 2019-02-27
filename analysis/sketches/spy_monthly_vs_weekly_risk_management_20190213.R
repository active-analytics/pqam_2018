# loading packages
library(tidyverse)

# reading in data
df_portfolio_monthly <-
    read_csv("../../data/plx_2018/spy_monthly_2018_2014_managed_pnl.csv")


df_portfolio_weekly <-
    read_csv("../../data/plx_2018/spy_weekly_2018_2014_managed_pnl.csv")


##################
## ANALSYS CODE ##
##################
# calculating the scaled, managed PNLs
# df_portfolio <-
#     df_portfolio %>%
#         mutate(
#             dly_managed = strangle_mult * tot_pnl * manage_mult
#         )

# aggregating pnls by day so we can calculate Sharpe-Ratio
df_daily_pnl <-
    df_portfolio_monthly %>%
        filter(strategy == "strangle") %>% 
        filter(variation == 0.1) %>% 
        group_by(loss_trigger, dh_threshold, data_date) %>%
        summarize(
            dly_managed= sum(scaled_managed_pnl)
        )


df_daily_pnl <-
    df_portfolio_weekly %>%
    filter(strategy == "strangle") %>% 
    filter(variation == 0.1) %>% 
    group_by(loss_trigger, dh_threshold, data_date) %>%
    summarize(
        dly_managed= sum(scaled_managed_pnl)
    )



#########################
## calculating metrics ##
#########################
## 1) annualized mean of daily pnl
## 2) annualized standard-deviation of daily pnl
## 3) annualized sharpe-ratio

# naked options
df_metrics <-
    df_daily_pnl %>%
        group_by(loss_trigger, dh_threshold) %>%
        summarize(
            avg_ann = (mean(dly_managed) * 252) %>% round(2)
            , std_ann = (sd(dly_managed) * sqrt(252)) %>% round(2)
            , sharpe =
                ((mean(dly_managed) / sd(dly_managed)) * sqrt(252)) %>% round(2)

        )


#write_csv(df_metrics, "combined_risk_management_metric.csv")

df_metrics %>% 
    filter(loss_trigger <= 2) %>% 
    ggplot(aes(dh_threshold, loss_trigger, label = avg_ann)) +
    geom_raster(aes(fill = avg_ann), interpolate = TRUE)  +
    geom_text(check_overlap = FALSE, size=2) + 
    ggtitle("Annualized Avg Daily PNL")


df_metrics %>% 
    filter(loss_trigger <= 2) %>% 
    ggplot(aes(dh_threshold, loss_trigger, label = sharpe)) +
    geom_raster(aes(fill = sharpe), interpolate = TRUE)  +
    geom_text(check_overlap = FALSE, size=2) + 
    ggtitle("Sharpe-Ratio")

    
