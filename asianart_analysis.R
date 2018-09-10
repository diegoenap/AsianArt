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


ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey40") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_vline(xintercept = daysinfo[weekNum == 7, Date], col = "lightblue", lty = 2) +
  geom_vline(xintercept = daysinfo[isFirstSunday == TRUE, Date], col = "red", lty = 2) +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-06-15", "2017-12-31")))

ggplot(ga_allws_chn_us[Channel == "Organic Search"], aes(Date, UniquePageviews)) +
  geom_line(size = 0.5, col = "grey40") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_vline(xintercept = daysinfo[weekNum == 7, Date], col = "lightblue", lty = 2) +
  geom_vline(xintercept = daysinfo[isFirstSunday == TRUE, Date], col = "red", lty = 2) +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-06-15", "2017-12-31")))



# Correlation tests ====

teststart <- "2016-07-06"  # Values for Full Data
testend <- "2018-05-30"  # 31 days smaller than limit 2018-06-30
maxlag <- 31
visitordata <- asianart_visitors[Date >= teststart & Date <= as.Date(testend) + maxlag]

# --- All Web Site
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_allwebsite.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_allwebsite.csv")

# --- Plan your Visit
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_plan_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "PLAN-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_plan.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_plan.csv")


# --- Tickets
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_tickets_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "TICKETS-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_tickets.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_tickets.csv")


# --- Visit
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_visit_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "VISIT-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_visit.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_visit.csv")


plotLagComparisonv2(movingAverage(ga_allws_chn_us[Channel == "Organic Search"]),
                    movingAverage(visitordata),
                    plotstart = "2017-06-15",
                    plotend = "2017-12-31",
                    metric = "UniquePageviews",
                    dlag = 1)

plotLagComparisonv2(ga_allws_chn_us[Channel == "Organic Search"],
                    visitordata,
                    plotstart = "2017-06-15",
                    plotend = "2017-12-31",
                    metric = "UniquePageviews",
                    dlag = 1)
