
library(tidyverse)
library(readr)

#source("data_transform.R")

logpath <- "C:/Users/j_osborne/Documents/JDO-Logs/"

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


Log_Summary <- logfile %>%
  rename(Timestamp = timestamp, Log = log, JobNumber = jobval, Flag = flagval, Timediff = timediff)  %>%
  filter(Timestamp > as.Date("2019-08-04")) %>% 
  mutate(
    Day = lubridate::wday(Timestamp, label = T) 
    , Pretty.Date = format(as.Date(Timestamp), "%d-%b")
  ) %>%
  aggregateJobs() %>% 
  group_by(Pretty.Date, JobNumber) %>%
  summarise(Timeseg = as.integer(sum(Timediff)),Seconds.Spent = lubridate::as.duration(Timeseg), Day = max(Day)) %>%
  filter(JobNumber != "(-)") %>% 
  select(Pretty.Date, Day, JobNumber, Seconds.Spent) %>% 
  mutate(Hours.Spent = Seconds.Spent/dhours(1))
View(Log_Summary)


