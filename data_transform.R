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
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(zoo)

linedf2 <- lines %>% 
    enframe(name = NULL, value = "rawlog")


# Set of regexes
daymatch <- "--(\\S*)--"
timematch <- "(\\d{2}:\\d{2})"
flagmatch <- "\\d{2}:\\d{2} (TODO):"
jobnomatch <- "\\(\\S*\\)$"
logmatch <- "\\d{2} ([\\s*\\S*]*) \\("

ref_date <- str_match(basedate_file, "\\d{2}-\\d{2}-\\d{2}")[[1]]                

linedf3 <- linedf2 %>%
    mutate(
       basedate = ref_date
      ,
      dayval = ifelse(grepl(daymatch,rawlog), str_match(rawlog, daymatch), NA),
      timeval = ifelse(grepl(timematch,rawlog), str_match(rawlog, timematch), NA),
      jobval = ifelse(grepl(jobnomatch,rawlog), str_match(rawlog, jobnomatch), NA),
      flagval = grepl(flagmatch, rawlog),
      log = str_match(rawlog, logmatch)[,2]
      ) %>%
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
      , JobNumber == "(HelpDesk)" ~ "(INTERNAL)" 
      , JobNumber == "(JN00000)" ~ "(ADMIN)" 
      , JobNumber == "(Jn00000)" ~ "(ADMIN)" 
      , JobNumber == "(TIMESHEETS)" ~ "(ForceAdmin)" 
      , JobNumber == "(DefenderKMI)" ~ "(DefenderKMIs)"
      , JobNumber == "(TechcorneR)" ~ "(INTERNAL)"
      , JobNumber == "(FridBrek)" ~ "(INTERNAL)"
      , JobNumber == "(EvoqueButton)" ~ "(DefenderButton)"
      , JobNumber == "(DefenderButtonBrief)" ~ "(DefenderButton)"
      , JobNumber == "(CustomerJourney)" ~ "(INTERNAL)"
      , JobNumber == "(CustomerJourneys)" ~ "(INTERNAL)"
      , JobNumber == "(Internal)" ~ "(INTERNAL)"
      , JobNumber == "(TeamConn)" ~ "(INTERNAL)"
      , JobNumber == "(Handover)" ~ "(INTERNAL)"
      , JobNumber == "(HANDOVER)" ~ "(INTERNAL)"
      , JobNumber == "(GDPRErasure)" ~ "(INTERNAL)"
      , JobNumber == "(Filesystems)" ~ "(INTERNAL)"
      , JobNumber == "(StreetVista)" ~ "(INTERNAL)"
      , JobNumber == "(LRE_Regular_Ext)" ~ "(PR-001571 - LRE Retention Ongoing)"
      , JobNumber == "(LRERegular)" ~ "(PR-001571 - LRE Retention Ongoing)"
      , JobNumber == "(LRERetirement)" ~ "(PR-001571 - LRE Retention Ongoing)"
      , JobNumber == "(StratStand)" ~ "(PR-000458 Strategic Stand Ups)"
      , JobNumber == "(PR-001704)" ~ "(PR-001780 Jan Newsletter PCA)"
      , JobNumber == "(JLAIntro)" ~ "(JLADSReview)"
      , JobNumber == "(LeadValue)" ~ "(PR-000467)"
      , TRUE ~ JobNumber 
    )) %>% 
    # mutate_at(vars(JobNumber), toupper) %>% 
    return()
}
