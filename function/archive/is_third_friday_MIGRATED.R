is_third_friday <- function(dt_friday){
    
    # exception-handling to add
    # check if it's a valid date
    # check if it's a friday
    
    #dt_friday <- ymd(20130920)
    dt_first_of_month <- lubridate::floor_date(dt_friday, "month")
    
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
    
    bln_third_friday <- FALSE
    if (int_third_friday == lubridate::day(dt_friday)){
        bln_third_friday <- TRUE
    }
    
    bln_third_friday
}