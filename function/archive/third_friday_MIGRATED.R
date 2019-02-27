third_friday <- function(year, month){
    
    ## exception handling
    # check if it's a valid year
    # check if it's a valid month
    
    
    chr_first_of_month <-
        paste0(
            as.character(year), "-"
            , as.character(month), "-"
            , "1"
        ) 
    
    dt_first_of_month <- lubridate::ymd(chr_first_of_month)
    
    chr_first_day <- 
        as.character(lubridate::wday(dt_first_of_month, label = TRUE))
    
    if (chr_first_day == "Sun") {
        int_first_friday <- 6
    } else if (chr_first_day == "Mon"){
        int_first_friday <- 5
    } else if (chr_first_day == "Tue"){
        int_first_friday <- 4
    } else if (chr_first_day == "Wed"){
        int_first_friday <- 3
    } else if (chr_first_day == "Thu"){
        int_first_friday <- 2
    } else if (chr_first_day == "Fri"){
        int_first_friday <- 1
    } else if (chr_first_day == "Sat"){
        int_first_friday <- 7
    }
    
    int_third_friday <- int_first_friday + 14 
    
    dt_third_friday <- 
        lubridate::ymd(
            paste0(
                as.character(year), "-"
                , as.character(month), "-"
                , as.character(int_third_friday)
            )
        )
    
    dt_third_friday
}