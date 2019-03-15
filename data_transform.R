# Log Transformation Script
## Script to transform the data stored in the corpus of logs into a more machine informative format.


dayname_to_diff <- function(dayname){
    require(lubridate)
    require(dplyr)
    out <- case_when(dayname == "--Monday--" ~ days(0),
                     dayname == "--Tuesday--" ~ days(1),
                     dayname == "--Wednesday--" ~ days(2),
                     dayname == "--Thursday--" ~ days(3),
                     dayname == "--Friday--" ~ days(4),
                     dayname == "--Saturday--" ~ days(5),
                     dayname == "--Sunday--" ~ days(6))
    return(out)
}

reassign_date <- function(BadDateTS, NewDate){
    date(BadDateTS) <- NewDate
    return(BadDateTS)
}

log_extractor <- function(lines, basedate_file){
    require(dplyr)
    require(stringr)
    require(lubridate)
    require(zoo)

linedf <- lines %>% 
    as.data.frame() 
    
linedf2 <-
    rename(linedf, rawlog = .)

# Set of regexes
daymatch <- "--(\\S*)--"
timematch <- "(\\d{2}:\\d{2})"
flagmatch <- "\\d{2}:\\d{2} (FLAG):"
jobnomatch <- "\\(\\S*\\)$"
logmatch <- "\\d{2} (\\S*) \\("
                
linedf3 <- linedf2 %>%
    mutate(basedate = str_match(basedate_file, "\\d{2}-\\d{2}-\\d{2}"),
           dayval = ifelse(grepl(daymatch,rawlog), str_match(rawlog, daymatch), NA),
           timeval = ifelse(grepl(timematch,rawlog), str_match(rawlog, timematch), NA),
           jobval = ifelse(grepl(jobnomatch,rawlog), str_match(rawlog, jobnomatch), NA),
           flagval = grepl(flagmatch, rawlog),
           log = ifelse(grepl(logmatch,rawlog), str_match(rawlog, logmatch)[,2], NA)) %>%
    filter(!is.na(timeval)| !is.na(dayval))


linedf4 <- linedf3 %>%
    mutate(carried_dayval = na.locf(dayval),
           datestamp = as.Date(basedate, format = "%d-%m-%y") + dayname_to_diff(carried_dayval),
           time_as_if_today = as.POSIXct(strptime(timeval, format = "%H:%M")),
           timestamp = reassign_date(time_as_if_today, datestamp))

linedf5 <- linedf4 %>%
    filter(!is.na(timeval)) %>%
    select(timestamp, log, flagval, jobval)

return(linedf5)
}
