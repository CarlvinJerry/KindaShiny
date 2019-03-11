progressFunction <- function(date){
  #Set decimal places to 1
  options(digits = 2)
  #Load packages--
  library(scales)
  library(lubridate)
  #Check if isleap year
  leapYearChecker <- as.numeric(leap_year(date))
  
  #Day of year--------------------------------------------YEARPROG
  Year = if (leapYearChecker==0){
    dayOfYear= yday(date)
    yearProg <- percent(dayOfYear/365)#*100 #Year progress
    yearProg
  } else {
    dayOfYear= yday(date)
    yearProg <- percent(dayOfYear/366 ) # *100 #Year progress
    yearProg
  }
  
  #Day of quarter------------------------------------------QUARTERPROG
  Quarter = if (leapYearChecker == 0){ #NOT A LEAP YEAR----
    #Number of quarter
    if(quarter(date) == 1){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/90) # *100 #Quarter 1 progress
      quarterProg
    } else if(quarter(date) == 2){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/91) # *100 #Quarter 2 progress
      quarterProg
    } else if(quarter(date) == 3){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/92) # *100 #Quarter 3 progress
      quarterProg
    } else if(quarter(date) == 4){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/92) # *100 #Quarter 4 progress
      quarterProg
    }
  } else if (leapYearChecker == 1){ #YEAR IS A LEAP YEAR----
    #Number of quarter
    if(quarter(date) == 1){
      dayOfQuarter = qday(date)
      quarterProg <-percent(dayOfQuarter/91) # *100 #Quarter 1 progress
      quarterProg
    } else if(quarter(date) == 2){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/91) # *100 #Quarter 2 progress
      quarterProg
    } else if(quarter(date) == 3){
      dayOfQuarter = qday(date)
      quarterProg <- Percent(dayOfQuarter/92 )# *100 #Quarter 3 progress
      quarterProg
    } else if(quarter(date) == 4){
      dayOfQuarter = qday(date)
      quarterProg <- percent(dayOfQuarter/92 )#*100 #Quarter 4 progress
      quarterProg
    }
  }
  
  #Day of month--------------------------------------------MONTH PROG
  dayofMonth = mday(date)
  Month <- percent(dayofMonth/days_in_month(date))# *100 #Month progress
  Month
  
  #Day of week--------------------------------------------WEEK PROG
  dayofWeek = mday(date)
  Week <- percent(wday(date)/7)#*100 #Week progress
  Week
  
  #Hour of day-------------------------------------------DAY PROG
  Day = percent(hour(Sys.time())/24)#*100 #Day Progress
  Day
  
  #Minute of hour------------------------------------------HOUR PROG
  Hour = percent(minute(Sys.time())/60)#*100
  Hour
  
  #Second of minute -----------------------------------------MINUTE PROG
  Minute = percent(second(Sys.time())/60)#*100
  
  #create data frame
  Progressor<-data.frame(Year,Quarter,Month,Week,Day,Hour,Minute,CurrentTime = format(Sys.time(), "%a %d %b %Y %X %Z"))
  Progressor
  
  
  # p <- plot_ly(
  #   y = as.numeric(Progressor[1:7 ])*100,  # First observation only
  #   type = "bar",
  #   # mode = 'lines'
  # )
  # 
  # p
  
}
progressFunction(Sys.time())
#str(progressFunction(Sys.time()))
library(DT)


data<-progressFunction(Sys.time())
as.numeric.factor(data$Year)
as.numeric(as.character(data$Year))[data$Year])
as.character(data$Year)
as.numeric("10%")


percent_vec = paste(1:100, "%", sep = "")
as.numeric(sub("%", "", data$Year))


i=1
while(TRUE){
  
  if (i %% 31536000==0)
  {
    break
  }
  flush.console()
  time <- progressFunction(Sys.Date())
  Year<- as.numeric(sub("%", "",time$Year))
  Quarter <- as.numeric(sub("%", "",time$Quarter))
  Month <- as.numeric(sub("%", "",time$Month))
  Day <- as.numeric(sub("%", "",time$Day))
  Hour <- as.numeric(sub("%", "",time$Hour))
  Minute <- as.numeric(sub("%", "",time$Minute))
  
  df<- cbind(Year,Quarter,Month,Day,Hour,Minute)
  #print(df)
 # as.numeric(sub("%", "",progressFunction(Sys.Date())[1:7][,1:7]))
 plot(df[1:7],type='b',ylim=c(0,100))
  
  Sys.sleep(time=1)
  i=i+1
}

mutate(mtcars, displ_l = disp / 61.0237)