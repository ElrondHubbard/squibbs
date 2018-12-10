library(dplyr)
library(ggplot2)
eg_data <- data.frame(
# ID
id=c(as.integer(1:1000)) ,
# Random short trip YN 85/15 split
short_trip_yn = c(sample( c(0,1), 1000, replace=TRUE, prob=c(0.85, 0.15))),
# Random period var, 45/55 split
period = c(sample( c(1,2), 1000, replace=TRUE, prob=c(0.45, 0.55))))
# Trip = 1
eg_data$trip <- 1
# Create dates, rbind them by month to one column, cbind column to dataframe
eg_dates <- data.frame(dates = c(gsub("-", "", sample(seq(as.Date("2018-03-01"), as.Date("2018-03-31"), by = "day"), 200, replace=TRUE))))
eg_dates_apr <- data.frame(dates = c(gsub("-", "", sample(seq(as.Date("2018-04-01"), as.Date("2018-04-30"), by = "day"), 350, replace=TRUE))))
eg_dates_may <- data.frame(dates = c(gsub("-", "", sample(seq(as.Date("2018-05-01"), as.Date("2018-05-31"), by = "day"), 300, replace=TRUE))))
eg_dates_jun <- data.frame(dates = c(gsub("-", "", sample(seq(as.Date("2018-06-01"), as.Date("2018-06-30"), by = "day"), 150, replace=TRUE))))
eg_dates <- rbind(eg_dates, eg_dates_apr, eg_dates_may, eg_dates_jun)
rm(eg_dates_apr, eg_dates_may, eg_dates_jun)
eg_data <- cbind(eg_data, eg_dates)
# Shift ID
eg_data$id_shift <- paste(eg_data$id, eg_data$dates, sep="")

stack_bar_data <- subset(eg_data, select=c(1,6,4,2,5,3)) # ID, shiftid, trip, short_trip, period, start_date  
stack_bar_data$trip_type <- ifelse(stack_bar_data$short_trip_yn==1,"short","long") 
stack_bar_data$period <- as.factor(stack_bar_data$period)
stack_bar_data$trip_type <- as.factor(stack_bar_data$trip_type)

# Stacked bar
stack_bar_trips_type <- stack_bar_data
stack_bar_trips_type <- stack_bar_trips_type %>% group_by(trip_type, period) %>% mutate(sum_type_period = sum(trip)) 
stack_bar_trips_type$dupes <- duplicated(stack_bar_trips_type[,6:7])

# By trip type and period
stack_bar_trips_type <- subset(stack_bar_trips_type, stack_bar_trips_type$dupes==FALSE, select=c(6:8))
stack_bar_trips_type <- ungroup(stack_bar_trips_type)
stack_bar_trips_type <- rbind(stack_bar_trips_type, (data.frame(period="1 + 2", trip_type="long", sum_type_period=sum(stack_bar_trips_type[which(stack_bar_trips_type[,2]=="long"),3]))))
stack_bar_trips_type <- rbind(stack_bar_trips_type, (data.frame(period="1 + 2", trip_type="short", sum_type_period=sum(stack_bar_trips_type[which(stack_bar_trips_type[,2]=="short"),3]))))
stack_bar_trips_type$period <- factor(stack_bar_trips_type$period, levels = c("1 + 2", "1", "2"))
stack_bar_trips_type_period <- (
    ggplot(data = stack_bar_trips_type, aes(x=period, y=sum_type_period, fill=trip_type)) +
    geom_bar(stat="identity", color = "black", position='stack') +
    scale_fill_manual(values=c("red", "green")) +
    geom_text(aes(label=sum_type_period), vjust=1.6, size=3.5)    
    )
stack_bar_trips_type_period

stack_bar_trips_type_period4 <- (
    ggplot(data = stack_bar_trips_type, aes(x=period, y=sum_type_period, fill=trip_type)) +
        geom_bar(stat="identity", color = "black", position='stack') +
        scale_fill_manual(values=c("red", "green")) +
        geom_text(aes(label=sum_type_period), vjust=1.25,size=3.5)    
)
stack_bar_trips_type_period4



