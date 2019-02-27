monthly_expiration <- function(year, month){
    
    # find third friday of month
    dt_third_friday <- third_friday(year, month)
    
    # prior to 2/20/2015, expirations were listed as a Saturday
    if(dt_third_friday < lubridate::ymd(20150220)) {
        dt_monthly_exp <- dt_third_friday + 1
    } else {
        dt_monthly_exp <- dt_third_friday
        # if third friday of month falls on a holiday,
        # then the expiration is the previous business day
        if (!bizdays::is.bizday(dt_third_friday)){
            dt_monthly_exp <- 
                bizdays::add.bizdays(dt_third_friday, -1)
        }
    }
   dt_monthly_exp
}