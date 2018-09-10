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


# Correlation test functions ====

metricsList <- c("Users", "UsersOld", "Sessions", "SessionsNew", "SessionsOld", "SessionsBounce", "SessionsNoBounce", "AvgSessionDuration", "Pageviews", "PageviewsSession", "UniquePageviews", "AvgTimeOnPage")

logValues <- function(data) {
  # Replace the metric's values with the log value
  data <- data[order(Date)]
  if (canApplyLog(data))
    if ("visitors" %in% colnames(data)) {
      data$visitors = log(imputeDates(data)$visitors)
    } else {
      data$Sessions = log(data$Sessions)
      data$SessionsNew = log(data$SessionsNew)
      data$SessionsOld = log(data$SessionsOld)
      data$SessionsBounce = log(data$SessionsBounce)
      data$SessionsNoBounce = log(data$SessionsNoBounce)
      data$AvgSessionDuration = log(data$AvgSessionDuration)
      data$Users = log(data$Users)
      data$UsersOld = log(data$UsersOld)
      data$Pageviews = log(data$Pageviews)
      data$PageviewsSession = log(data$PageviewsSession)
      data$UniquePageviews = log(data$UniquePageviews)
      data$AvgTimeOnPage = log(data$AvgTimeOnPage)
    }
  else
    stop("Cannot apply Log, dataset contains zeros")
  return(data)
}

canApplyLog <- function(data) {
  # Checks if the dataset have 0 and returns TRUE if it don't
  if ("visitors" %in% colnames(data))
    ans <- nrow(data[visitors == 0]) > 0
  else
    ans <- nrow(data[Users == 0]) > 0 | nrow(data[UsersOld == 0]) > 0 | nrow(data[Sessions == 0]) > 0 | nrow(data[SessionsNew == 0]) > 0 | nrow(data[SessionsOld == 0]) > 0 | nrow(data[SessionsBounce == 0]) > 0 | nrow(data[SessionsNoBounce == 0]) > 0 | nrow(data[AvgSessionDuration == 0]) > 0 | nrow(data[Pageviews == 0]) > 0 | nrow(data[PageviewsSession == 0]) > 0 | nrow(data[UniquePageviews == 0]) > 0 | nrow(data[AvgTimeOnPage == 0]) > 0
  return(!ans)
}

getCorrelationMatrix <- function(gametrics, visitors, maxlag = 31) {
  # Returns a matrix where row are the GA metrics and the column are the correlation value at each lag point (from 0 to maxlag)
  # It keeps the GA metrics fixed and try different lags going forwards with the Visitors, that is why Visitors dataset should be larger (+ maxlag).
  #
  # Validations
  if (gametrics[1, Date] != visitors[1, Date]) stop("Datasets must start with the same date")
  if (nrow(gametrics) + maxlag > nrow(visitors)) stop("Visitor dataset must be larger, considering the extra lag dates at the end (at least 'maxlag' larger)")
  #
  # Initialize dataframe to store results
  corrdf <- data.frame(Metrics = metricsList)
  # Vector that will contain columns names for the result dataframe. Each loop adds a column name (Lag_#)
  corrcolnames <- "Metrics"
  # Get date range, based on the GA Metrics
  dmin <- min(gametrics[, Date])
  dmax <- max(gametrics[, Date])
  # Get correlations
  for (i in 0:maxlag) {  # For each lag
    corrvals <- numeric(12)
    for (j in 1:12) {  # For each metric
      # Get correlation
      c <- round(cor(gametrics[, get(as.character(corrdf[j, 1]))], visitors[Date >= dmin + i & Date <= dmax + i, visitors]), 3)
      corrvals[j] <- ifelse(is.na(c), 0, c)
    }
    corrcolnames <- cbind(corrcolnames, paste("Lag", i, sep = "_"))  # Add column name with lag number
    corrdf <- cbind(corrdf, corrvals)                                # Add results to dataframe
    names(corrdf) <- corrcolnames                                    # Update column names
  }
  return(corrdf)
}

getMaxCorrelations <- function(corrmatrix) {
  # Pick a correlation matrix and returns a data.table with the maximum correlation value for each GA metric and their respective lag
  data.table(Metrics = metricsList,
             MaxCorr = apply(corrmatrix[, -1], 1, max),
             Lag = apply(corrmatrix[, -1], 1, which.max) - 1)
}

getMaxCorrelationsTable <- function(gametrics, visitors, tptype, maxlag = 31, tofile = NULL) {
  # Generates multiple correlation matrices and acumulates the MaxCorrelations for different test:
  # Normal values, impute, trends, logs, moving average, etc.
  # Each test is associated with a number, i.e. MaxCorr1 and Lag1 correspond to "Normal values, no impute"
  # Returns a data.table with all the tests
  #
  # Initialize table
  ans <- data.table(Type = tptype,
                    Metrics = metricsList,
                    stringsAsFactors = FALSE)
  #
  # 1) GA Unchanged - Visitors Unchanged
  co <- getCorrelationMatrix(gametrics, visitors, maxlag = maxlag)
  comax <- getMaxCorrelations(co)
  ans <- cbind(ans, comax[, 2:3])
  #
  # 2) GA Log - Visitors Unchanged
  if (canApplyLog(gametrics)) {
    co <- getCorrelationMatrix(logValues(gametrics), visitors, maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # 3) GA Log - Visitors Log
  if (canApplyLog(gametrics) & canApplyLog(visitors)) {
    co <- getCorrelationMatrix(logValues(gametrics), logValues(visitors), maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # 4) GA Moving average - Visitors Impute
  co <- getCorrelationMatrix(movingAverage(gametrics), visitors[-(1:6)], maxlag = maxlag)
  comax <- getMaxCorrelations(co)
  ans <- cbind(ans, comax[, 2:3])
  #
  # 5) GA Moving average - Visitors Moving average
  co <- getCorrelationMatrix(movingAverage(gametrics), movingAverage(visitors), maxlag = maxlag)
  comax <- getMaxCorrelations(co)
  ans <- cbind(ans, comax[, 2:3])
  #
  # 6) GA Moving average Log - Visitors
  if (canApplyLog(gametrics)) {
    co <- getCorrelationMatrix(movingAverage(logValues(gametrics)), visitors[-(1:6)], maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # 7) GA Moving average Log - Visitors Moving average
  if (canApplyLog(gametrics)) {
    co <- getCorrelationMatrix(movingAverage(logValues(gametrics)), movingAverage(visitors), maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # 8) GA Moving average Log - Visitors Log
  if (canApplyLog(gametrics) & canApplyLog(visitors)) {
    co <- getCorrelationMatrix(movingAverage(logValues(gametrics)), logValues(visitors[-(1:6)]), maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # 9) GA Moving average Log - Visitors Moving average Log
  if (canApplyLog(gametrics) & canApplyLog(visitors)) {
    co <- getCorrelationMatrix(movingAverage(logValues(gametrics)), movingAverage(logValues(visitors)), maxlag = maxlag)
    comax <- getMaxCorrelations(co)
    ans <- cbind(ans, comax[, 2:3])
  } else
    ans <- cbind(ans, 0, 0)
  #
  # Change names
  names(ans) <- c("Type", "Metrics", paste(c("MaxCorr", "Lag"), rep(1:9, each = 2), sep = ""))
  # Max values
  # ans <- rbind(ans,
  #              c(c(Type = tptype, Metrics ="MaxIndex"), apply(ans[, -(1:2)], 2, which.max)),
  #              c(c(Type = tptype, Metrics ="MaxValue"), apply(ans[, -(1:2)], 2, max)))
  # ans[13:14, seq(4, 22, by = 2)] <- 0
  # Return
  return(ans)
}

getBestCorrelations <- function(mct) {
  # Given a MaxCorrelationTable (mct) it creates a data.table with correlation and lag for a test, for each type of dataset
  # Example, for a Full dataset of Wellington, UniquePageviews, maxcorr, at lag, test position
  types <- unique(mct$Type)
  ans <- data.table(Type = "", Metrics = "", Corr = 0, Lag = 0, Pos = 0)
  for (l in types) {
    tmp <- mct[Type == l]
    #i <- which.max(c(as.matrix(tmp[, c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21), with = FALSE])))
    i <- which.max(c(as.matrix(tmp[, seq(3, ncol(mct), by = 2), with = FALSE])))
    r <- (i-1) %% 12 + 1  # Row
    c <- ((i-1) %/% 12 + 1) * 2 + 1  # Col
    tmp <- tmp[r, c(1, 2, c, c + 1), with = FALSE]
    tmp <- cbind(tmp, (i-1) %/% 12 + 1)  # Add the position (Indicates the type of dataset, normal, rtend, log, impute, etc.)
    names(tmp) <- c("Type", "Metrics", "Corr", "Lag", "Pos")
    ans <- rbind(ans, tmp)
  }
  return(ans[-1])
}

