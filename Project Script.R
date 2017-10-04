#please change the file path in line#3 and 143

crimesPath <- "Chicago_Crimes_2012_to_2017.csv"
crimes     <- read.table(file = crimesPath,header = T,sep = ",")
library(lubridate)
time       <- as.character(crimes[,4])
time2      <- strptime(x = time,format = "%m/%d/%Y %I:%M:%S %p")
Date       <- date(time2)
DaysBack   <- max(Date)-Date
H          <- hour(time2)
Day       <- weekdays(time2)
#creating weekend/weekday category
dayCat  <-
    ifelse(Day == "Sunday" | Day== "Monday" | Day == "Tuesday" | Day == "Wednesday" | Day == "Thursday", "Week",
           ifelse(Day == "Friday" | Day == "Saturday", "Weekend", "Broken"))
Month   <- strftime(time2, "%b")
#creating temperature categories
tempCat <- ifelse(Month == "Jan"| Month == "Nov" | Month == "Dec", "Cold",
                  ifelse(Month == "Mar" | Month == "Apr" | Month == "Sep" | Month == "Oct", "Temperate",
                         ifelse(Month == "Feb","February",
                                ifelse( Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug", "Hot", "Broken"))))

timeSlot<-
  ifelse(H  <= 4, '20:00 to 04:00',
         ifelse(H  <= 12, '04:00 to 12:00',
                ifelse(H <= 20, '12:00 to 20:00',
                       ifelse(H <= 24, '20:00 to 04:00',"NA"
                                     ))))
crimes$dayCategory  <- dayCat
crimes$dayCategory  <- as.factor(crimes$dayCategory)
crimes$tempCategory <- tempCat
crimes$tempCategory <- as.factor(crimes$tempCategory)
crimes$timeSlot     <- timeSlot
crimes$DaysBack     <- DaysBack
crimes$WeekDay      <- Day
crimes$WeekDay      <- as.factor(crimes$Weekday)
crimes$Month        <- Month
crimes$Month        <- as.factor(crimes$Month)
#Create names for the district numbers, and then add to dataset as a factor variable.
crimes$DistrictName <- 
  ifelse(crimes$District == 1, 'Central',
         ifelse(crimes$District == 2, 'Wentworth',
                ifelse(crimes$District == 3, 'Grand Crossing',
                       ifelse(crimes$District == 4, 'South Chicago',
                              ifelse(crimes$District == 5, 'Calumet', 
                                     ifelse(crimes$District == 6,'Gresham',
                                            ifelse(crimes$District == 7,'Englewood',
                                                   ifelse(crimes$District == 8, 'Chicago Lawn',
                                                          ifelse(crimes$District == 9, 'Deering',
                                                                 ifelse(crimes$District == 10, 'Ogden',
                                                                        ifelse(crimes$District == 11, 'Harrison',
                                                                               ifelse(crimes$District == 12, 'Near West',
                                                                                      ifelse(crimes$District == 14, 'Shakespeare',
                                                                                             ifelse(crimes$District == 15, 'Austin',
                                                                                                    ifelse(crimes$District == 16,'Jefferson Park',
                                                                                                           ifelse(crimes$District == 17,'Albany Park',
                                                                                                                  ifelse(crimes$District == 18,'Near North',
                                                                                                                         ifelse(crimes$District == 19,'Town Hall',
                                                                                                                                ifelse(crimes$District == 20,'Lincoln',
                                                                                                                                       ifelse(crimes$District == 22,'Morgan Park',
                                                                                                                                              ifelse(crimes$District == 24,'Rogers Park',
                                                                                                                                                     ifelse(crimes$District == 25,'Grand Central','NA'
                                                                                                                                                     ))))))))))))))))))))))
#Create categories from the primary types and add to dataset as a factor variable
crimes$DistricName <- as.factor(crimes$DistrictName)
crimes$Classification <- 
  ifelse(crimes$Primary.Type == 'ARSON' | crimes$Primary.Type == 'HOMICIDE' | crimes$Primary.Type== 'WEAPONS VIOLATION', 'DEADLY',
         ifelse(crimes$Primary.Type == 'CRIMINAL DAMAGE' | crimes$Primary.Type == 'CRIMINAL TRESPASS' |crimes$Primary.Type == 'MOTOR VEHICLE THEFT' |crimes$Primary.Type == 'THEFT', 'PROPERTY',
                ifelse(crimes$Primary.Type == 'ASSAULT', 'AGGRESSIVE',
                       ifelse(crimes$Primary.Type == 'BATTERY', 'AGGRESSIVE',
                              ifelse(crimes$Primary.Type == 'BURGLARY', 'AGGRESSIVE', 
                                     ifelse(crimes$Primary.Type == 'CRIM SEXUAL ASSAULT','AGGRESSIVE',
                                            ifelse(crimes$Primary.Type == 'HUMAN TRAFFICKING','AGGRESSIVE',
                                                   ifelse(crimes$Primary.Type == 'INTIMIDATION', 'AGGRESSIVE',
                                                          ifelse(crimes$Primary.Type == 'KIDNAPPING', 'AGGRESSIVE',
                                                                 ifelse(crimes$Primary.Type == 'OFFENSE INVOLVING CHILDREN', 'AGGRESSIVE',
                                                                        ifelse(crimes$Primary.Type == 'OTHER OFFENSE', 'AGGRESSIVE',
                                                                               ifelse(crimes$Primary.Type == 'PUBLIC PEACE VIOLATION', 'AGGRESSIVE',
                                                                                      ifelse(crimes$Primary.Type == 'ROBBERY', 'AGGRESSIVE',
                                                                                             ifelse(crimes$Primary.Type == 'SEX OFFENSE', 'AGGRESSIVE',
                                                                                                    ifelse(crimes$Primary.Type == 'DECEPTIVE PRACTICE','NONVIOLENT',
                                                                                                           ifelse(crimes$Primary.Type == 'GAMBLING','AGGRESSIVE',
                                                                                                                  ifelse(crimes$Primary.Type == 'CONCEALED CARRY LICENSE VIOLATION','NONVIOLENT',
                                                                                                                         ifelse(crimes$Primary.Type == 'INTERFERENCE WITH PUBLIC OFFICER','NONVIOLENT',
                                                                                                                                ifelse(crimes$Primary.Type == 'LIQUOR LAW VIOLATION','NONVIOLENT',
                                                                                                                                       ifelse(crimes$Primary.Type == 'NARCOTICS','NONVIOLENT',
                                                                                                                                              ifelse(crimes$Primary.Type == 'NON-CRIMINAL','NONVIOLENT',
                                                                                                                                                     ifelse(crimes$Primary.Type == 'NON-CRIMINAL (SUBJECT SPECIFIC)','NONVIOLENT',
                                                                                                                                                              ifelse(crimes$Primary.Type == 'NON - CRIMINAL','NONVIOLENT',
                                                                                                                                                                      ifelse(crimes$Primary.Type == 'OBSCENITY','NONVIOLENT',
                                                                                                                                                                              ifelse(crimes$Primary.Type == 'OTHER NARCOTIC VIOLATION','NONVIOLENT',
                                                                                                                                                                                     ifelse(crimes$Primary.Type == 'PROSTITUTION','NONVIOLENT',
                                                                                                                                                                                            ifelse(crimes$Primary.Type == 'PUBLIC INDECENCY','NONVIOLENT',
                                                                                                                                                                                                    ifelse(crimes$Primary.Type == 'STALKING','NONVIOLENT',
                                                                                                                                                                                                        ifelse(crimes$Primary.Type == '',NA, 'ERROR'    
                                                                                                                                                     )))))))))))))))))))))))))))))
crimes$Classification <- factor(crimes$Classification, levels = c('NONVIOLENT', 'PROPERTY', 'AGGRESSIVE', 'DEADLY'), ordered = T)
keeps  <- c("Year","Latitude","Longitude","timeSlot","Month","DistrictName","Classification","WeekDay","DaysBack","tempCategory","dayCategory")
crimes <- crimes[,keeps]
crimes <-subset.data.frame(crimes, Latitude != "NA")
crimes <-subset.data.frame(crimes, DistrictName != "NA")
crimes <- subset.data.frame(crimes, Classification != "ERROR")

#Code to get aggregates of crimes by categories used for the regression
crimes$countOfCrimes <- 1
library(dplyr)
#Temperature classes instead of months
keeps <- c("Year","Latitude","Longitude","timeSlot","DistrictName","Classification","dayCategory","DaysBack","tempCategory","countOfCrimes")
crimes2 <- crimes[,keeps]
aggregateCrime <- crimes2 %>%
    group_by(Year,tempCategory,Classification,timeSlot,dayCategory,DistrictName) %>%
      summarise_each(funs(sum))
#getting rid of unneeded variables for regression
keeps <- c("Year","timeSlot","tempCategory","DistrictName","Classification","dayCategory","countOfCrimes")
aggregateCrime <- aggregateCrime[,keeps]
aggregateCrime$Year <- aggregateCrime$Year-2011
#creating the four datasets based on the different classification
nonviolent <- subset.data.frame(aggregateCrime, Classification == "NONVIOLENT")
deadly <- subset.data.frame(aggregateCrime, Classification == "DEADLY")
property <- subset.data.frame(aggregateCrime, Classification == "PROPERTY")
aggressive <- subset.data.frame(aggregateCrime, Classification == "AGGRESSIVE")

#four multiple linear regression models based on the four datasets
fitnon <- lm(countOfCrimes ~ Year+tempCategory+DistrictName+dayCategory+timeSlot, data = nonviolent)
fitdead <- lm(countOfCrimes ~ Year+tempCategory+DistrictName+dayCategory+timeSlot, data = deadly)
fitprop <- lm(countOfCrimes ~ Year+tempCategory+DistrictName+dayCategory+timeSlot, data = property)
fitagg <- lm(countOfCrimes ~ Year+tempCategory+DistrictName+dayCategory+timeSlot, data = aggressive)

#saving the coefficients into vectors
nonCoef <-summary(fitnon)$coefficients[,1]
deadCoef <- summary(fitdead)$coefficients[,1]
propCoef <- summary(fitprop)$coefficients[,1]
aggCoef <- summary(fitagg)$coefficients[,1]

Coefficients <- data.frame(nonCoef,deadCoef,propCoef,aggCoef)
setwd("C:/Users/John/Documents/MGMT 590 Project")
write.table(x = Coefficients,file = "CrimeCoefficients.csv",sep = ",",col.names = T)

#Below creates dataset of last x number days of crimes
crimesMAP <- subset(crimes, DaysBack <= 365)
keeps     <- c("Year","Latitude","Longitude","timeSlot","DistrictName","Classification", "WeekDay","Month")
crimesMAP <- crimesMAP[,keeps]
setwd("C:/Users/John/Documents/MGMT 590 Project")
write.table(x = crimesMAP,file = "CrimeMapFile.csv",sep = ",",col.names = T)

