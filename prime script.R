
library(tidyverse)
library(readr)

files <- list.files(path = getwd(), pattern = ".txt")
lines <- as.vector(read_lines(files[[1]]))

x <- log_extractor(lines)

write_csv(x,"logfile.csv")

test <- read_csv("logfile.csv")