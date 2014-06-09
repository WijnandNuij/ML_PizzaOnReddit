
freqPlot <- function(pizzaData, var="requester_account_age_in_days_at_request")
{
        require(ggplot2)
        print(summary(pizza[pizza$requester_received_pizza==TRUE][[var]]))
        print(summary(pizza[pizza$requester_received_pizza==FALSE][[var]]))
        qplot(pizzaData[[var]], data = pizza, geom = "freqpoly", colour = requester_received_pizza)
}

normalize <- function(pizzaData)
{
        normalized <- apply(as.data.frame(pizzaData$requester_account_age_in_days_at_request), 2, function(x) (x-min(x)) /(max(x)-min(x)))
        pizzaData <- cbind(pizzaData, normalized)
        pizzaData <- setnames(pizzaData, "pizzaData$requester_account_age_in_days_at_request", "normalizedAccountAge")
}

addPostHour <- function(pizzaData)
{
        times <- as.POSIXct(pizzaData$unix_timestamp_of_request_utc, origin="1970-01-01", tz="GMT")
        hours <- as.numeric(format(times, "%H"))
        pizzaData <- cbind(pizzaData, hours)
}

addPostDayOfWeek <- function(pizzaData)
{
        times <- as.POSIXct(pizzaData$unix_timestamp_of_request_utc, origin="1970-01-01", tz="GMT")
        weekday <- as.numeric(format(times, "%u"))
        pizzaData <- cbind(pizzaData, weekday)
}

addPostDayOfMonth <- function(pizzaData)
{
        times <- as.POSIXct(pizzaData$unix_timestamp_of_request_utc, origin="1970-01-01", tz="GMT")
        monthday <- as.numeric(format(times, "%e"))
        pizzaData <- cbind(pizzaData, monthday)
}


crosstables <- function(data, var="requester_received_pizza")
{
        require(gmodels)
        CrossTable(data[["requester_received_pizza"]])
        CrossTable(data[[var]], data[["requester_received_pizza"]], prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, digits=2)
}