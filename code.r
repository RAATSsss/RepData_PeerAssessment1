require(ggplot2)
#load data
df<-read.csv('~/RepData_PeerAssessment1/activity.csv')
head(df)

# aggregate data by day
agg<-data.frame(tapply(df$steps, df$date, sum))
colnames(agg) <- 'numofSteps'
hist(agg$numofSteps)#hist
agg.mean<-mean(agg$numofSteps, na.rm = TRUE)#mean
agg.median<-median(agg$numofSteps, na.rm = TRUE)#median

# plot average steps per time interval
ave<-data.frame(tapply(df$steps, df$interval, FUN=function(x) mean(x,na.rm = TRUE)))#calculate average steps
colnames(ave) <- 'aveStep' # change col name
ave$rownumber <- rownames(ave)
plot(x = ave$rownumber,y = ave$aveStep,type = 'l')#line plot

# missing value
naRows.steps <- nrow(df[df$steps == 'NA',])#number of rows with NA in column 'steps'
naRows.date <- nrow(df[df$date == 'NA',])#number of rows with NA in column 'date'
naRows.interval <- nrow(df[df$interval == 'NA',])#number of rows with NA in column 'interval'

# fill missing steps using average steps by time intervals
df.null <- subset(df, is.na(df$steps))
df.fillnull <- merge(df.null, ave, by.x = 'interval', by.y = 'rownumber')
df.fillnull$steps <- df.fillnull$aveStep
df.fillnull$aveStep <- NULL
df.complete <- rbind(df.fillnull, subset(df, !is.na(df$steps)))

#weekday and weekend arrays
weekday <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
weekend <- c('Saturday','Sunday')
df.complete$dayofweek <- 'NA'
df.complete[weekdays(as.POSIXct(df.complete[,3]))%in%weekday,4]<-'Weekday'
df.complete[weekdays(as.POSIXct(df.complete[,3]))%in%weekend,4]<-'Weekend'
df.complete.ave<-data.frame(tapply(X = df.complete$steps, INDEX = list(df.complete$interval, df.complete$dayofweek) , 
                                   FUN=function(x) mean(x,na.rm = TRUE)))#calculate average steps
df.complete.ave$rownumber <- rownames(df.complete.ave)

temp <- melt(df.complete.ave,id.vars = 'rownumber')
temp$variable <- as.factor(temp$variable)

ggplot(temp, aes(rownumber, value)) +
    geom_line(aes(group = variable)) +
    ggtitle("") + 
    facet_wrap(~variable, ncol = 1)
