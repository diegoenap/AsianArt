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