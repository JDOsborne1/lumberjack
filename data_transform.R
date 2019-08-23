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
logmatch <- "\\d{2} ([\\s*\\S*]*) \\("
                
linedf3 <- linedf2 %>%
    mutate(basedate = str_match(basedate_file, "\\d{2}-\\d{2}-\\d{2}"),
           dayval = ifelse(grepl(daymatch,rawlog), str_match(rawlog, daymatch), NA),
           timeval = ifelse(grepl(timematch,rawlog), str_match(rawlog, timematch), NA),
           jobval = ifelse(grepl(jobnomatch,rawlog), str_match(rawlog, jobnomatch), NA),
           flagval = grepl(flagmatch, rawlog),
           log = str_match(rawlog, logmatch)[,2]) %>%
    filter(!is.na(timeval)| !is.na(dayval))


linedf4 <- linedf3 %>%
    mutate(carried_dayval = na.locf(dayval),
           datestamp = as.Date(basedate, format = "%d-%m-%y") + dayname_to_diff(carried_dayval),
           time_as_if_today = as.POSIXct(strptime(timeval, format = "%H:%M")),
           timestamp = reassign_date(time_as_if_today, datestamp))

linedf5 <- linedf4 %>%
    mutate(timelead = lead(timestamp, default = linedf4[nrow(linedf4),"timestamp"])) %>%
    mutate(timediff = (timelead - timestamp))

linedf6 <- linedf5 %>%
    filter(!is.na(timeval)) %>%
    select(timestamp, log, flagval, jobval, timediff)


return(linedf6)
}


aggregateJobs <- function(df){
  df %>% 
    mutate(JobNumber = case_when(
      JobNumber == "(AzzuriSegmentationInternal)" ~ "(INTERNAL)" 
      , JobNumber == "(INTERNALAttribution)" ~ "(INTERNAL)" 
      , JobNumber == "(OpenData)" ~ "(INTERNAL)" 
      , JobNumber == "(INFOSEC)" ~ "(INTERNAL)" 
      , JobNumber == "(JN00000)" ~ "(ADMIN)" 
      , JobNumber == "(TIMESHEETS)" ~ "(ADMIN)" 
      , TRUE ~ JobNumber 
    )) %>% 
    return()
}
