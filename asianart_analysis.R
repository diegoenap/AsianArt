library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
source("asianart_func.R")

# Read Visitors data ====

asianart_visitors <- readVisitorData()
asianart_visitors <- asianart_visitors[Date >= "2016-01-01"]
head(asianart_visitors)
summary(asianart_visitors)
str(asianart_visitors)

# Read GA data ====

ga_allws_nochn_all <- readQueryExplorerData("data/ga/asian_allws_nochn_all.tsv", c("Date"))
ga_allws_nochn_os <- readQueryExplorerData("data/ga/asian_allws_nochn_os.tsv", c("Date"))
ga_allws_nochn_us <- readQueryExplorerData("data/ga/asian_allws_nochn_us.tsv", c("Date"))
ga_allws_nochn_do <- readQueryExplorerData("data/ga/asian_allws_nochn_do.tsv", c("Date"))
ga_allws_nochn_lo <- readQueryExplorerData("data/ga/asian_allws_nochn_lo.tsv", c("Date"))

ga_allws_chn_all <- readQueryExplorerData("data/ga/asian_allws_chn_all.tsv", c("Date", "Channel"))
ga_allws_chn_os <- readQueryExplorerData("data/ga/asian_allws_chn_os.tsv", c("Date", "Channel"))
ga_allws_chn_us <- readQueryExplorerData("data/ga/asian_allws_chn_us.tsv", c("Date", "Channel"))
ga_allws_chn_do <- readQueryExplorerData("data/ga/asian_allws_chn_do.tsv", c("Date", "Channel"))
ga_allws_chn_lo <- readQueryExplorerData("data/ga/asian_allws_chn_lo.tsv", c("Date", "Channel"))

ga_plan_nochn_all <- readQueryExplorerData("data/ga/asian_plan_nochn_all.tsv", c("Date"))
ga_plan_nochn_os <- readQueryExplorerData("data/ga/asian_plan_nochn_os.tsv", c("Date"))
ga_plan_nochn_us <- readQueryExplorerData("data/ga/asian_plan_nochn_us.tsv", c("Date"))
ga_plan_nochn_do <- readQueryExplorerData("data/ga/asian_plan_nochn_do.tsv", c("Date"))
ga_plan_nochn_lo <- readQueryExplorerData("data/ga/asian_plan_nochn_lo.tsv", c("Date"))

ga_plan_chn_all <- readQueryExplorerData("data/ga/asian_plan_chn_all.tsv", c("Date", "Channel"))
ga_plan_chn_os <- readQueryExplorerData("data/ga/asian_plan_chn_os.tsv", c("Date", "Channel"))
ga_plan_chn_us <- readQueryExplorerData("data/ga/asian_plan_chn_us.tsv", c("Date", "Channel"))
ga_plan_chn_do <- readQueryExplorerData("data/ga/asian_plan_chn_do.tsv", c("Date", "Channel"))
ga_plan_chn_lo <- readQueryExplorerData("data/ga/asian_plan_chn_lo.tsv", c("Date", "Channel"))

ga_tickets_nochn_all <- readQueryExplorerData("data/ga/asian_tickets_nochn_all.tsv", c("Date"))
ga_tickets_nochn_os <- readQueryExplorerData("data/ga/asian_tickets_nochn_os.tsv", c("Date"))
ga_tickets_nochn_us <- readQueryExplorerData("data/ga/asian_tickets_nochn_us.tsv", c("Date"))
ga_tickets_nochn_do <- readQueryExplorerData("data/ga/asian_tickets_nochn_do.tsv", c("Date"))
ga_tickets_nochn_lo <- readQueryExplorerData("data/ga/asian_tickets_nochn_lo.tsv", c("Date"))

ga_tickets_chn_all <- readQueryExplorerData("data/ga/asian_tickets_chn_all.tsv", c("Date", "Channel"))
ga_tickets_chn_os <- readQueryExplorerData("data/ga/asian_tickets_chn_os.tsv", c("Date", "Channel"))
ga_tickets_chn_us <- readQueryExplorerData("data/ga/asian_tickets_chn_us.tsv", c("Date", "Channel"))
ga_tickets_chn_do <- readQueryExplorerData("data/ga/asian_tickets_chn_do.tsv", c("Date", "Channel"))
ga_tickets_chn_lo <- readQueryExplorerData("data/ga/asian_tickets_chn_lo.tsv", c("Date", "Channel"))

ga_visit_nochn_all <- readQueryExplorerData("data/ga/asian_visit_nochn_all.tsv", c("Date"))
ga_visit_nochn_os <- readQueryExplorerData("data/ga/asian_visit_nochn_os.tsv", c("Date"))
ga_visit_nochn_us <- readQueryExplorerData("data/ga/asian_visit_nochn_us.tsv", c("Date"))
ga_visit_nochn_do <- readQueryExplorerData("data/ga/asian_visit_nochn_do.tsv", c("Date"))
ga_visit_nochn_lo <- readQueryExplorerData("data/ga/asian_visit_nochn_lo.tsv", c("Date"))

ga_visit_chn_all <- readQueryExplorerData("data/ga/asian_visit_chn_all.tsv", c("Date", "Channel"))
ga_visit_chn_os <- readQueryExplorerData("data/ga/asian_visit_chn_os.tsv", c("Date", "Channel"))
ga_visit_chn_us <- readQueryExplorerData("data/ga/asian_visit_chn_us.tsv", c("Date", "Channel"))
ga_visit_chn_do <- readQueryExplorerData("data/ga/asian_visit_chn_do.tsv", c("Date", "Channel"))
ga_visit_chn_lo <- readQueryExplorerData("data/ga/asian_visit_chn_lo.tsv", c("Date", "Channel"))


# Create weekend dataframe ====

daysinfo <- data.table(Date = seq(as.Date("2016-01-01"), as.Date("2018-06-30"), by = "day"))
daysinfo$weekNum <- wday(daysinfo$Date, week_start = 1)
daysinfo$isFirstSunday <- wday(daysinfo$Date, week_start = 1) == 7 & day(daysinfo$Date) <= 7


# Exploring Visitors ====

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(stat = "identity", size = 0.25) +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  theme_bw()

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(stat = "identity", size = 0.25) +
  theme_bw() +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-01-01", "2017-12-31")), ylim = c(0, 5000))




ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Weekends") +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-06-01", "2017-07-31")), ylim = c(0, 5000))

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_point(size = 0.75, col = "grey15") +
  geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Weekends and Thursdays") +
  geom_vline(xintercept = weekends[isThursday == TRUE, Date], col = "indianred2", lty = 2) +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-09-01", "2017-10-31")), ylim = c(0, 5000))

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_point(size = 0.5, col = "grey15") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_line(data = trendValues(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-01-01", "2017-12-31")), ylim = c(0, 5000))


