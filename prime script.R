
library(tidyverse)
library(readr)

source("data_transform.R")

logpath <- "C:/Users/j_osborne/Documents/JDO-Logs/"

files <- list.files(path = logpath, pattern = ".txt")

logfile <- read_csv(paste0(logpath,"logfile.csv")) %>% as.data.frame()
maxdate <- if_else(nrow(logfile) == 0, as.POSIXct("2000-01-01"),  max(logfile$timestamp, na.rm = T))
newlogs <- as.Date(str_match(files, "\\d{2}-\\d{2}-\\d{2}"), format = "%d-%m-%y") > maxdate
files <- files[newlogs]

for (i in files){
lines <- as.vector(read_lines(paste0(logpath, i)))
x <- log_extractor(lines, i)
logfile <- rbind(logfile, x)
}

write_csv(logfile, paste0(logpath,"logfile.csv"))
