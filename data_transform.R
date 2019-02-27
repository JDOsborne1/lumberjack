# Log Transformation Script
## Script to transform the data stored in the corpus of logs into a more machine informative format.

### Will Require the following
#* Regex Interpretation - Stringi/Stringr
#* Datetime Interpretation - (lubridate)
#* Data Framework - (Tidy)

library(tidyverse)
library(readr)
library(stringi)
library(stringr)

files <- list.files(path = getwd(), pattern = ".txt")

# Set of regexes
daymatch <- "--(\\S*)--"
timematch <- "(\\d{2}:\\d{2})"
flagmatch <- "\\d{2}:\\d{2} (FLAG):"
jobnomatch <- "\\(\\S*\\)$"
logmatch <- "\\d{2} (\\S*) \\("



lines <- as.vector(read_lines(files[[1]]))

linedf <- lines %>% 
    as.data.frame() 
    
linedf2 <-
    rename(linedf, rawlog = .)
                
linedf3 <- linedf2 %>%
    mutate(basedate = str_match(files[[1]], "\\d{2}-\\d{2}-\\d{2}"),
           dayval = ifelse(grepl(daymatch,rawlog), str_match(rawlog, daymatch), NA),
           timeval = ifelse(grepl(timematch,rawlog), str_match(rawlog, timematch), NA),
           jobval = ifelse(grepl(jobnomatch,rawlog), str_match(rawlog, jobnomatch), NA),
           flagval = grepl(flagmatch, rawlog),
           log = ifelse(grepl(logmatch,rawlog), str_match(rawlog, logmatch)[,2], NA))

# ~~Use Zoo to propagate the day observations forward~~
# Remove the dayflags and whitespace by removing the is.na timevals 
# Generate the full datetime timestamp from the constituent parts
## Make a custom function to transform the days of the week into lubridate timediffs
# fix the log extract so the actual message can be removed from the raw log
# remove all the extra elements so that the dataframe is made up of:
# the log message, a full timestamp and a flag for logs of note

library(lubridate)

dayname_to_diff <- function(dayname){
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

linedf4 <- linedf3 %>%
    mutate(carried_dayval = na.locf(dayval),
           datestamp = as.Date(basedate, format = "%d-%m-%y") + dayname_to_diff(carried_dayval),
           time_as_if_today = as.POSIXct(strptime(timeval, format = "%H:%M")),
           timestamp = reassign_date(time_as_if_today, datestamp))

linedf5 <- linedf4 %>%
    filter(!is.na(timeval)) %>%
    select(rawlog, flagval, log, timestamp)


as.interval("16:23")
as.duration("16:23")
as.period("16:23", format = "%h:%m")
test <- as.Date("10-12-01")
hour(test) <- 16
minute(test) <- 23
hours(12)+minutes(34)
x<- strptime("16:23", format = "%H:%M")