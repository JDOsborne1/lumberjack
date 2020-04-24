
library(tidyverse)
library(readr)
library(here)
library(openssl)

source(here::here("data_transform.R"))

logpath <- "C:/Users/joseph.osborne/Logging/"

files <- list.files(path = logpath, pattern = ".txt")

logfile <- read_csv(paste0(logpath,"logfile.csv")) %>% as.data.frame()
logfile <- head(logfile, 0)

if (nrow(logfile) == 0){
    maxdate <- as.Date("2000-01-01")
} else {
    maxdate <- max(logfile$timestamp, na.rm = T)
}

newlogs <- as.Date(str_match(files, "\\d{2}-\\d{2}-\\d{2}"), format = "%d-%m-%y") > maxdate
files <- files[newlogs]

for (i in files){
lines <- as.vector(read_lines(paste0(logpath, i)))
x <- log_extractor(lines, i)
logfile <- rbind(logfile, x)
}
logfile <- logfile %>%
    filter(!is.na(log))

write_csv(logfile, paste0(logpath,"logfile.csv"))

config::get("MissingJobnos")

Log_Summary <- logfile %>%
  rename(Timestamp = timestamp, Log = log, JobNumber = jobval, Flag = flagval, Timediff = timediff)  %>%
  filter(
    Timestamp > as.Date("2020-04-20")
         &
           Timestamp < as.Date("2020-04-26")
    ) %>%
  mutate(
    Day = lubridate::wday(Timestamp, label = T) 
    , Pretty.Date = format(as.Date(Timestamp), "%d-%b")
  ) %>%
  aggregateJobs() %>% 
  # filter(JobNumber %in% config::get("MissingJobnos")) %>%
  group_by(Pretty.Date, JobNumber) %>%
  summarise(
    Timeseg = as.integer(sum(Timediff))
    ,Seconds.Spent = lubridate::as.duration(Timeseg)
    , Day = max(Day)
    , All.Logs = glue::glue_collapse(Log, sep = "\n")
    ) %>%
  filter(!JobNumber %in% c("(-)")) %>% 
  select(Pretty.Date, Day, JobNumber, Seconds.Spent, All.Logs) %>% 
  mutate(Hours.Spent = Seconds.Spent/dhours(1))
View(Log_Summary)

Log_Summary %>% 
  group_by(Day) %>% 
  # filter(JobNumber != "(LeadValue)") %>% 
  summarise(Day.Worked = sum(Hours.Spent, na.rm = T)) %>% pull(Day.Worked) %>% sum(na.rm = T)
Log_Summary %>% 
  group_by(JobNumber) %>% 
  summarise(Total.Time = sum(Hours.Spent))


Tasks <- logfile %>% 
  filter(flagval) %>% 
  mutate(sha.log = str_trunc(sha1(log), 10, ellipsis = "") ) %>% 
  select(
    sha.log
    , log
    , timestamp
    ) 


completed_tasks <- 
  tribble(
    ~sha.log, ~completion.date
    ,  "e1a183cb4d", as.Date("2020-02-27") 
    )