# Asian Art Museum functions

# Read dataset functions ====

readVisitorData <- function(addDateCols = FALSE, beonicfile = NULL) {
  # Asian Art Museum visitors
  data <- fread("data/visitors/AsianArt_visitation.csv", header = TRUE)
  names(data) <- c("Date", "visitors")
  data$Date <- dmy(data$Date)
  # Complete missing days for whole year
  daysToComplete <- data.frame(Date = seq(min(data$Date), max(data$Date), by = "day"))
  data <- as.data.table(merge(daysToComplete, data, by = "Date", all.x = TRUE))
  return(data)
}


readQueryExplorerData <- function(filename, colNames, addDateCols = FALSE) {
  # AsianArt GA (from Google Query Explorer)
  # Join all the files in one dataset
  data <- read.table(filename, header = TRUE, fileEncoding = "UTF-16LE", sep = "\t", quote = "\"", stringsAsFactors = FALSE)
  data <- as.data.table(data)
  # Modify columns
  names(data) <- c(colNames, "Users", "UsersNew", "PercNewSessions", "Sessions", "BounceRate", "AvgSessionDuration", "Pageviews", "PageviewsSession", "UniquePageviews", "AvgTimeOnPage")
  data$Date <- ymd(data$Date)
  data$PercNewSessions <- data$PercNewSessions / 100
  data$BounceRate <- data$BounceRate / 100
  data$UsersOld <- data$Users - data$UsersNew
  data$UsersNew <- NULL
  data$SessionsNew <- data$Sessions * data$PercNewSessions
  data$SessionsOld <- data$Sessions * (1 - data$PercNewSessions)
  data$SessionsBounce <- data$Sessions * data$BounceRate
  data$SessionsNoBounce <- data$Sessions * (1 - data$BounceRate)
  # Add date parts
  if (addDateCols) {
    # data$Year <- factor(year(data$Date))
    # data$Week <- week(data$Date)
    # data$DayOfYear <- yday(data$Date)
    data$IsoWeek <- factor(paste(isoyear(data$Date), sprintf("%02i", isoweek(data$Date)), sep = ""))
  }
  data <- data[order(Date)]
  return(data)
}


completeGADates <- function(data, limitDates = NULL, replaceNA = 0) {
  # Complete the dataset with the missing date so it contains all the dates within the range
  if (is.null(limitDates))
    daysToComplete <- data.frame(Date = seq(min(data$Date), max(data$Date), by = "day"))
  else
    daysToComplete <- data.frame(Date = seq(min(as.Date(limitDates)), max(as.Date(limitDates)), by = "day"))
  data <- as.data.table(merge(daysToComplete, data, by = "Date", all.x = TRUE))
  # Replace NA values
  data[is.na(Users)]$Users <- replaceNA
  data[is.na(UsersOld)]$UsersOld <- replaceNA
  data[is.na(Sessions)]$Sessions <- replaceNA
  data[is.na(AvgSessionDuration)]$AvgSessionDuration <- replaceNA
  data[is.na(Pageviews)]$Pageviews <- replaceNA
  data[is.na(PageviewsSession)]$PageviewsSession <- replaceNA
  data[is.na(UniquePageviews)]$UniquePageviews <- replaceNA
  data[is.na(AvgTimeOnPage)]$AvgTimeOnPage <- replaceNA
  data[is.na(SessionsNew)]$SessionsNew <- replaceNA
  data[is.na(SessionsOld)]$SessionsOld <- replaceNA
  data[is.na(SessionsBounce)]$SessionsBounce <- replaceNA
  data[is.na(SessionsNoBounce)]$SessionsNoBounce <- replaceNA
  return(data)
}


movingAverage <- function(data, w = 7) {
  # Calculates the moving average of the variables in the dataset, given the window (w).
  # Returns a new data.table with the dates and moving averages
  # The returning dataset will have w-1 rows
  if (nrow(data) >= w) {
    data <- data[order(Date)]
    n <- nrow(data)
    # Get the structure of the dataset
    if ("visitors" %in% colnames(data))
      tmpdf <- data[1, .(Date, visitors)]
    else
      tmpdf <- data[1, .(Date, Users, UsersOld, Sessions, SessionsNew, SessionsOld, SessionsBounce, SessionsNoBounce, AvgSessionDuration, Pageviews, PageviewsSession, UniquePageviews, AvgTimeOnPage)]
    # Compute moving average
    for (i in 1:(n - w + 1)) {
      if ("visitors" %in% colnames(data))  # Select columns depending on dataset
        mv_values <- apply(data[i:(i + w - 1), .(visitors)], 2, mean)
      else
        mv_values <- apply(data[i:(i + w - 1), .(Users, UsersOld, Sessions, SessionsNew, SessionsOld, SessionsBounce, SessionsNoBounce, AvgSessionDuration, Pageviews, PageviewsSession, UniquePageviews, AvgTimeOnPage)], 2, mean)
      tmprow <- data.frame(Date = data[i + w - 1, Date], t(mv_values))
      tmpdf <- rbind(tmpdf, tmprow)
    }
    return(tmpdf[-1])
  } else {
    stop("Moving average window should be greater or equal to the number of rows")
  }
}


trendValues <- function(data) {
  # Replace the metric's values with the trend
  data <- data[order(Date)]
  if ("visitors" %in% colnames(data)) {
    data$visitors = as.numeric(stl(ts(data$visitors, frequency = 7), s.window = "periodic")$time.series[,2])
  } else {
    data$Sessions = as.numeric(stl(ts(data$Sessions, frequency = 7), s.window = "periodic")$time.series[,2])
    data$SessionsNew = as.numeric(stl(ts(data$SessionsNew, frequency = 7), s.window = "periodic")$time.series[,2])
    data$SessionsOld = as.numeric(stl(ts(data$SessionsOld, frequency = 7), s.window = "periodic")$time.series[,2])
    data$SessionsBounce = as.numeric(stl(ts(data$SessionsBounce, frequency = 7), s.window = "periodic")$time.series[,2])
    data$SessionsNoBounce = as.numeric(stl(ts(data$SessionsNoBounce, frequency = 7), s.window = "periodic")$time.series[,2])
    data$AvgSessionDuration = as.numeric(stl(ts(data$AvgSessionDuration, frequency = 7), s.window = "periodic")$time.series[,2])
    data$Users = as.numeric(stl(ts(data$Users, frequency = 7), s.window = "periodic")$time.series[,2])
    data$UsersOld = as.numeric(stl(ts(data$UsersOld, frequency = 7), s.window = "periodic")$time.series[,2])
    data$Pageviews = as.numeric(stl(ts(data$Pageviews, frequency = 7), s.window = "periodic")$time.series[,2])
    data$PageviewsSession = as.numeric(stl(ts(data$PageviewsSession, frequency = 7), s.window = "periodic")$time.series[,2])
    data$UniquePageviews = as.numeric(stl(ts(data$UniquePageviews, frequency = 7), s.window = "periodic")$time.series[,2])
    data$AvgTimeOnPage = as.numeric(stl(ts(data$AvgTimeOnPage, frequency = 7), s.window = "periodic")$time.series[,2])
  }
  return(data)
}


# Plot functions ====

getPlotGA <- function(gadata, gametric, holidays = NULL, showTrend = TRUE, showSH = FALSE, showWE = FALSE, showPH = FALSE, showCS = FALSE, showE = FALSE, ylimits = NULL, logData = FALSE, impDates = TRUE) {
  #xlablimits <- as.Date(c("2015-07-01", "2018-05-31"))
  xlablimits <- as.Date(c("2016-07-06", "2018-06-30"))
  ylabel <- ""
  if (logData) {
    gadata <- logValues(gadata, impDates = impDates)
    ylabel <- "(log)"
  } 
  p <- ggplot(gadata, aes_string("Date", gametric)) +
    scale_x_date(limits = xlablimits) +
    theme_bw()
  if (!is.null(ylimits)) p <- p + scale_y_continuous(limits = ylimits)
  if (showSH) p <- p + geom_vline(xintercept = holidays[SchoolHoliday == 1, Date], col = "grey90")
  if (showWE) p <- p + geom_vline(xintercept = holidays[IsWeekend == 1, Date], col = "lightgrey", lty = 3)
  if (showPH) p <- p + geom_vline(xintercept = holidays[PublicHoliday == 1, Date], col = "lightgrey")
  if (showCS) p <- p + geom_vline(xintercept = holidays[CruiseShip == 1, Date], col = "lightgrey")
  if (showE) p <- p + geom_vline(xintercept = holidays[Event == 1, Date], col = "lightgrey")
  p <- p + geom_line(col = "grey90")
  if (showTrend) p <- p + geom_line(data = movingAverage(gadata), aes_string("Date", gametric), col = "blue")
  p <- p + labs(x = "", y = paste(gametric, ylabel))
  return(p)
}

getPlotGAList <- function(data, holidays = NULL, showTrend = TRUE, logData = FALSE, impDates = TRUE) {
  p <- vector("list", 12)
  p[[1]] <- getPlotGA(data, "Sessions", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[2]] <- getPlotGA(data, "SessionsNew", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[3]] <- getPlotGA(data, "SessionsOld", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[4]] <- getPlotGA(data, "SessionsBounce", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[5]] <- getPlotGA(data, "SessionsNoBounce", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[6]] <- getPlotGA(data, "AvgSessionDuration", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[7]] <- getPlotGA(data, "Users", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[8]] <- getPlotGA(data, "UsersOld", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[9]] <- getPlotGA(data, "Pageviews", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[10]] <- getPlotGA(data, "PageviewsSession", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[11]] <- getPlotGA(data, "UniquePageviews", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  p[[12]] <- getPlotGA(data, "AvgTimeOnPage", holidays, showTrend = showTrend, showSH = FALSE, logData = logData, impDates = impDates)
  return(p)
}