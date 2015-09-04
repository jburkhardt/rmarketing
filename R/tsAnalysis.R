#' Basic Time Series Analysis
#' 
#' @description Function to perform basic time series operations on unvariate time series.
#' tsAnalysis() collects a set of functions which are applied on the univariate time series.
#' @examples
#' data <- genData(n=36) #Generate sample data for time series of length 36
#' tsa <- tsAnalysis(x=data$data, start='2012-01-01', description='Test Time Series', period='month')
#' tsa$series #time series
#' tsa$chart #time series plot
#' tsa$chartYoY #time series plot year over year
#' tsa$chartDecomp #plot of time series decomposition
#' tsa$Data #return data frame 
#' @param x Data in a univariate fashion as a vector.
#' @param start Starting point of time in format 'yyyy-mm-dd'. Example: start='2001-01-01'
#' @param description Short description of time series.
#' @param period Element of: \{'day', 'week', 'month', 'year'\}
#' @return Object with $series, $chart, $chartYoY, $chartDecomp, $Data
#' @export


tsAnalysis <- function(x=data, start='2001-01-01', description='my new xts time series', period='month'){
  
#'  Create date vector.
  date <- seq(as.Date(paste(start)), by=period, along.with=x)
#'  Create univariate time series.
  series <- xts::xts(x, date, descr=paste(description))
#'  Create basic time series char with ggplot2.     
  chart <- ggplot2::qplot(x=date, y=as.numeric(series)) + geom_line() + labs(x='', y='', title=description)   
#'  Extract year, month, week from date
  year <- format(date,'%Y')
  month <- format(date,'%m')
  week <- sub(pattern='....-W', replacement='', x=ISOweek(date))
  day <- format(date,'%d')

#'  Generate YoY Plots.
#'  Create an dataframe for plotting with ggplot.
    tempData <- data.frame(as.numeric(day), as.numeric(series), year, month, week, date)
    colnames(tempData) <- c("day", "value", "year", "month", "week", "date")
    # create YoY charts depending on time period. 
    if(period == 'month'){
      chartYoY <- ggplot2::qplot(x=month, y=value, data=tempData, group=year, colour=year) + geom_line() + labs(x='', y='', title=description)
    }
    else if(period == 'week'){
      chartYoY <- ggplot2::qplot(x=week, y=value, data=tempData, group=year, colour=year) + geom_line() + labs(x='', y='', title=description)
    }
    else if(period == 'day'){
      chartYoY <- ggplot2::qplot(x=day, y=value, data=tempData, colour=year, group=year, geom="line") + facet_grid(month ~ .) + labs(x='', y='', title=description)
    }
    else{
      print("There are no YoY charts for period == 'year'.")
    }

#' Time Series Decomposition
#' Determine frequency depending on the time period.
#' Note: use switch()
    if(period == 'month'){
      freq <- 12
    }
    else if(period == 'week'){
      freq <- 52
    }
    else if(period == 'day'){
      freq <- 365
    }
    else{
      freq <- 1
    }
    
    if(length(x)/freq <= 2){
    chartDecomp <- warning('Time series decomposition only possible when time series has more than two periods.')
    }
    else{
#'  note: stl doesn´t work with xts.
#'  Create additional ts object.
    seriesTS <- stats::ts(data=x,start=as.Date(paste(start)),frequency=freq)
#'  Time series decomposition with stl.
    decomp <- stats::stl(seriesTS, s.window= 'periodic')
#'  Extend tempData dateframe with decomposition data.
    tempData$seasonal <- decomp$time.series[,1]
    tempData$trend <- decomp$time.series[,2]
    tempData$remainder <- decomp$time.series[,3]
#'  Transform dataframe for decomposition plot.
    tempDataDecomp <- reshape2::melt(tempData,id=c("date"), c("value", "seasonal", "trend", "remainder"))
#'  Create decomposition chart with ggplot.
    chartDecomp <-  ggplot2::qplot(x=date, y=value, data=tempDataDecomp) + geom_line() + facet_grid(variable ~ .) + labs(x='', y='', title=description)
    }
    
#'  Customize Data Frame
  if(period == 'month' && length(x)/freq <= 2){
      Data <- subset(tempData, select= c(date, year, month, value))
    }
  else if(period == 'month' && length(x)/freq > 2){
      Data <- subset(tempData, select= c(date, year, month, value, seasonal, trend, remainder))
    }
  else if(period == 'week' && length(x)/freq <= 2){
      Data <- subset(tempData, select= c(date, year, month, week, value))
    }
  else if(period == 'week' && length(x)/freq > 2){
      Data <- subset(tempData, select= c(date, year, month, week, value, seasonal, trend, remainder))
    }
  else if(period == 'day' && length(x)/freq <= 2){
      Data <- subset(tempData, select= c(date, year, month, week, day, value))
    }
  else if(period == 'day' && length(x)/freq > 2){
      Data <- subset(tempData, select= c(date, year, month, week, day, value, seasonal, trend, remainder))
    }
  else{
    Data <- subset(tempData, select= c(date, year, value, seasonal, trend, remainder))
  }
  

#' Return the functions 
    return = list(chart = chart,
                  series = series,
                  chartYoY = chartYoY,
                  Data = Data,
                  chartDecomp = chartDecomp)
}