monthly_last_td <- function(year, month){
    
    dt_monthly_exp <- monthly_expiration(year, month)
    
    dt_last_td <- dt_monthly_exp
    # prior to 2/20/2015, expiration date was a Saturday
    if(dt_monthly_exp < lubridate::ymd(20150220)){
        dt_last_td <- bizdays::add.bizdays(dt_monthly_exp, -1)
    }
    
    dt_last_td
}