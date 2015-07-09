?read.table

workingDirectory <- "D:\\development\\datasciencecoursera\\4-exploratory-data-analysis"
setwd(workingDirectory)

fileName <- "./household_power_consumption.txt"
hasHeader <- TRUE
saperatorStr <- ";"
stringsAsFactorFlag <- FALSE 
dateFormat <- "%d/%m/%Y"
timeFormat <- "%H:%M:%S"

elecPwrConsumption <- 
  read.table(
    file = fileName, header = hasHeader, 
    sep = saperatorStr, stringsAsFactors = stringsAsFactorFlag)

# TOTAL PLOTS
# globalActivePowerHistPlot
# globalActivePowerLinePlot
# subMeteringsPlot
# voltatePlot
# globalReactivePowerPlot

# PLOT 1 = 
# PLOT 2 =
# PLOT 3 = 
# PLOT 4 =

# Manual validation
class(elecPwrConsumption)# OK - data.frame
ncol(elecPwrConsumption)# OK
nrow(elecPwrConsumption)# OK
names(elecPwrConsumption)# OK
# [1] "Date"                  "Time"                  "Global_active_power"   "Global_reactive_power" "Voltage"               "Global_intensity"     
# [7] "Sub_metering_1"        "Sub_metering_2"        "Sub_metering_3"
str(elecPwrConsumption)# View class of columns

# Investigate on R's Data/Time
?strptime
dateTimeSample <- strptime("16/12/2006", format = "%d/%m/%Y")
class(dateTimeSample)
?as.Date
dateTimeSample2 <- as.Date("16/12/2006", "%d/%m/%Y")
class(dateTimeSample2)

# Subsetting data.frame with Date/Time data
sample <- data.frame(
  x = c(
    strptime("16/12/2006", format = "%d/%m/%Y"),
    strptime("17/12/2006", format = "%d/%m/%Y")), 
  y = c(1, 2))
str(sample)
sample[sample$x >= strptime("16/12/2006", format = "%d/%m/%Y"),]

# Convert one column into different types
dateColumnData <- strptime(elecPwrConsumption$Date, format = "%d/%m/%Y")
length(dateColumnData) == nrow(elecPwrConsumption)# OK

elecPwrConsumption$Date <- strptime(elecPwrConsumption$Date, format = "%d/%m/%Y")

# Subset data by Date
# Boundaries data type needs to be consistent with the one in data.frame
dateUpperBound <- strptime("02/02/2007", format = "%d/%m/%Y")
dateLowerBound <- strptime("01/02/2007", format = "%d/%m/%Y")
elecPwrConsumptionSubSet <-
  elecPwrConsumption[
    elecPwrConsumption$Date >= dateLowerBound &
      elecPwrConsumption$Date <= dateUpperBound, ]

# Validate subset content
str(elecPwrConsumptionSubSet)# OK
str(elecPwrConsumptionSubSet$Date)# OK
str(elecPwrConsumptionSubSet$Time)# OK
unique(elecPwrConsumptionSubSet$Date)# OK
summary(elecPwrConsumptionSubSet$Global_active_power)

# Convert data types into numeric
elecPwrConsumptionSubSet$Date <-
  as.Date(elecPwrConsumptionSubSet$Date)

elecPwrConsumptionSubSet$Global_active_power <-
  as.numeric(elecPwrConsumptionSubSet$Global_active_power)

elecPwrConsumptionSubSet$Voltage <-
  as.numeric(elecPwrConsumptionSubSet$Voltage)

elecPwrConsumptionSubSet$Sub_metering_1 <-
  as.numeric(elecPwrConsumptionSubSet$Sub_metering_1)
elecPwrConsumptionSubSet$Sub_metering_2 <-
  as.numeric(elecPwrConsumptionSubSet$Sub_metering_2)
elecPwrConsumptionSubSet$Sub_metering_3 <-
  as.numeric(elecPwrConsumptionSubSet$Sub_metering_3)

elecPwrConsumptionSubSet$Global_reactive_power <-
  as.numeric(elecPwrConsumptionSubSet$Global_reactive_power)

# Plotting
?plot
summary(elecPwrConsumptionSubSet$Global_active_power)
str(elecPwrConsumptionSubSet)
head(elecPwrConsumptionSubSet, n = 20)
str(elecPwrConsumptionSubSet$Date)

unique(weekdays(elecPwrConsumptionSubSet$Date))

# PLOT 1
?hist
hist(
  elecPwrConsumptionSubSet$Global_active_power, 
  col = "red",
  xlab = "Global Active Power (kilowatts)")

# PLOT 2
str(as.character(elecPwrConsumptionSubSet$Time)) # %H:%M:%S
str(as.character(elecPwrConsumptionSubSet$Date)) # %Y-%m-%d

head(elecPwrConsumptionSubSet$Time, n = 72)

dateTimeStrings <- 
  paste(
    as.character(elecPwrConsumptionSubSet$Date),
    " ",
    as.character(elecPwrConsumptionSubSet$Time),
    sep = "")

head(dateTimeStrings)
dateTimeColumn <- strptime(dateTimeStrings, format = "%Y-%m-%d %H:%M:%S")
str(dateTimeColumn)
?cbind
elecPwrConsumptionSubSet2 <- cbind(elecPwrConsumptionSubSet, dateTimeColumn)
names(elecPwrConsumptionSubSet2)
str(elecPwrConsumptionSubSet2$dateTimeColumn)

plot(
  elecPwrConsumptionSubSet2$Global_active_power 
    ~ 
      elecPwrConsumptionSubSet2$dateTimeColumn,
  type = "l",
  xlab = "Days",
  ylab = "Global Active Power (kilowatts)")

# PLOT 3
?lines
xLimit <- rbind(
  elecPwrConsumptionSubSet2$Sub_metering_1, 
  elecPwrConsumptionSubSet2$Sub_metering_2,
  elecPwrConsumptionSubSet2$Sub_metering_3)
min(xLimit)# 0
max(xLimit)# 38

yLimit <- elecPwrConsumptionSubSet2$dateTimeColumn
str(yLimit)
min(yLimit)# "2007-02-01 MYT"
max(yLimit)# "2007-02-02 23:59:00 MYT"

plot(
  elecPwrConsumptionSubSet2$Sub_metering_1
    ~
      elecPwrConsumptionSubSet2$dateTimeColumn, 
  type = "l",
  xlab = "Days",
  ylab = "Global Active Power (kilowatts)")

lines(
  elecPwrConsumptionSubSet2$Sub_metering_2
    ~
    elecPwrConsumptionSubSet2$dateTimeColumn,
  col = "red")

lines(
  elecPwrConsumptionSubSet2$Sub_metering_3
  ~
    elecPwrConsumptionSubSet2$dateTimeColumn,
  col = "blue")

?legend
legend(
  "topright", 
  legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
  col = c("black", "red", "blue"),
  lty = c(1, 1))

# PLOT 4
# Execute PNG first then followed by PLOT

plot(
  elecPwrConsumptionSubSet2$Voltage
  ~
    elecPwrConsumptionSubSet2$dateTimeColumn, 
  type = "l",
  xlab = "datetime",
  ylab = "Voltage")

plot(
  elecPwrConsumptionSubSet2$Global_reactive_power
  ~
    elecPwrConsumptionSubSet2$dateTimeColumn, 
  type = "l",
  xlab = "datetime",
  ylab = "Global Reactive Power")

with(
  elecPwrConsumptionSubSet2, {
  plot(
    Global_reactive_power ~ dateTimeColumn, 
    type = "l")
})

png(
  filename = "./sample.png", 
  width = 480, height = 480,
    units = "px", bg = "white")
dev.copy(png)
dev.off()

par(mfrow = c(2, 2))
par("mar")
