library(tidyverse)
library(lubridate)
library(viridis)



Log_Summary <- logfile %>%
  rename(Timestamp = timestamp, Log = log, JobNumber = jobval, Flag = flagval, Timediff = timediff)  %>%
  filter(Timestamp > as.Date("2019-07-22")) %>% 
  mutate(
         Day = lubridate::wday(Timestamp, label = T) 
         , Pretty.Date = format(as.Date(Timestamp), "%d-%b")
         ) %>%
  group_by(Pretty.Date, JobNumber) %>%
  summarise(Timeseg = as.integer(sum(Timediff)),Seconds.Spent = lubridate::as.duration(Timeseg), Day = max(Day)) %>%
  filter(JobNumber != "(-)") %>% 
  select(Pretty.Date, Day, JobNumber, Seconds.Spent)



Plot_Dat <- Log_Summary %>%
  rename(xBin = Pretty.Date, yRate1 = Seconds.Spent, yBin = JobNumber) %>%
  mutate(yRatemin = yRate1/3600)
PlotTit <- paste0("Jobnumber composition of work time for ",unique(Log_Summary$Pretty.Date)[1])
PlotSubTit <- "Breakdown of hours spent in work by the Jobnumber of the billed time"
xlab <-  "Day of the Week"
ylab <- "Hours in the Day"
Palette <- c(magma(15, begin = 0.5, end = 1), cividis(15, begin = 0.5, end = 1), inferno(15, begin = 0.5, end = 1), viridis(15, begin = 0.5, end = 1) )





Plot <- Plot_Dat %>%
  ggplot(aes(x = xBin)) +
  geom_bar(aes(fill=yBin, y =yRatemin), position=position_stack(reverse=TRUE), stat="identity") +
  geom_text(aes(label= round(yRatemin,3), y =yRatemin),
            position=position_stack(vjust=0.5),size=3) +
  scale_fill_manual(values = Palette) + 
  labs(title = PlotTit, subtitle = PlotSubTit, x = xlab, y = ylab, fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 90), panel.border=element_blank(), legend.position="bottom") +
  scale_y_continuous()
print(Plot)
