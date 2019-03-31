library(tidyverse)
library(lubridate)
library(viridis)


Logfile <- logfile %>%
  filter(!is.na(log)) %>% 
  rename(Timestamp = timestamp, Log = log, JobNumber = jobval, Flag = flagval) %>% 
  filter(Timestamp > (Sys.Date() - days(6)))

Log_Summary <- Logfile %>%
  mutate(Timelead = lead(Timestamp, default = Logfile[nrow(Logfile["Log"]),"Timestamp"])) %>%
  mutate(Timediff = (Timelead - Timestamp)) %>%
  group_by(Day = lubridate::wday(Timestamp, label = T), JobNumber) %>%
  summarise(Timeseg = as.integer(sum(Timediff)),Seconds_spent = lubridate::as.duration(Timeseg)) %>%
  filter(JobNumber != "(-)")

Beginning <- as.Date(min(Logfile$Timestamp))

Plot_Dat <- Log_Summary %>%
  rename(xBin = Day, yRate1 = Timeseg, yBin = JobNumber) %>%
  mutate(yRatemin = yRate1/3600)
PlotTit <- paste0("Jobnumber composition of work time for ",Beginning)
PlotSubTit <- "Breakdown of hours spent in work by the Jobnumber of the billed time"
xlab <-  "Day of the Week"
ylab <- "Hours in the Day"
Palette <- c(magma(5, begin = 0.5, end = 1), cividis(5, begin = 0.5, end = 1), inferno(5, begin = 0.5, end = 1))





Plot <- Plot_Dat %>%
  ggplot(aes(x = xBin)) +
  geom_bar(aes(fill=yBin, y =yRatemin), position=position_stack(reverse=TRUE), stat="identity") +
  geom_text(aes(label= round(yRatemin,3), y =yRatemin),
            position=position_stack(vjust=0.5),size=3) +
  scale_fill_manual(values = Palette) + 
  labs(title = PlotTit, subtitle = PlotSubTit, x = xlab, y = ylab, fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(), panel.border=element_blank(), legend.position="bottom") +
  scale_y_continuous()
print(Plot)
