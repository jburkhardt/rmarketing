#' Sample Data Frame
#' 
#' @description Create sample data.
#' @param n Lenght of sample time series.
#' @param start Set start date.
#' @return Dataframe containing date and data vector.
#' 
#' @export
genData <- function(n=750, start="2013-01-01"){
        date <- seq(from=as.Date(start),to = as.Date(start)+n-1, by="day")
        data <- 1:n + n/6 * (sin(1:n) + rnorm(n))
        df <- data.frame(date,data)
        df
      }
