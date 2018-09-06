library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
source("asianart_func.R")

# Read Visitors data ====

asianart_visitors <- readVisitorData()
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
