# Project Functions

### Pre-Process Data & Call Neural Network
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x, min, max) {
  return(x * (max - min) + min)
}

unscale <- function(vector, scale, center) {
  vector * scale  + center
}

calculateRSquared <- function(actual, predicted) {
  return(1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2))
}

getModelInputs <- function(InputData) {
  print("2. Inputs sent to function: Forecast.Electric.Demand()")
  # Extract Time Stemps from Data
  Num.Data.Points <- dim(InputData)[1]
  Time.Stamp <- strptime(InputData$DATE,"%m/%d/%Y %H:%M")

  # Select Training Range
  StartTime <- 1
  TrainRange <- StartTime:Num.Data.Points
  print(paste0("Training data start date: ",Time.Stamp[StartTime]))

  # Extract Hours field from Time.Stamp
  Hours <- as.numeric(format(Time.Stamp, '%H'))

  # Extract Days field from Time.Stamp
  Day.Date <- as.numeric(format(Time.Stamp, '%d'))
  Day.Number <- as.numeric(format(Time.Stamp, '%w'))
  Day.Number[Day.Number == 0] = 7
  Day.Name <- weekdays(Time.Stamp)
  # Modify Hours & Days
  temp <- 12 - Hours
  temp[temp >- 0] = 0
  Hours.Modified <- Hours + 2 * temp
  Day.Number.Modified <- Day.Number
  Day.Number.Modified[Day.Number < 6] = 1
  Day.Number.Modified[Day.Number == 6] = 2
  Day.Number.Modified[Day.Number > 6] = 3
  print("Extracting Hour_of_Day & Day_of_Week fields from the DATE field Time Stamp ")

  print('Extracting holidays')
  # flip the true/false so that we can encode it to be true for non-holidays and
  # false for holidays.  we want to basically exclude holidays from the RNN
  # Holidays <- as.numeric(!isHoliday(timeDate(InputData$DATE)))
  # pres.day <- getNthDayOfWeek(third, Mon, Feb, 2017)
  # pres.day.idx <- grep(pres.day, Time.Stamp)
  # Holidays[pres.day.idx] <- 0
  # the above literally has ZERO effect on the R2 score of the model
  # the below actually does positively affect the model's R2 score

  shouldCalculateHolidays = require('timeDate') & require('RcppBDT')
  if (shouldCalculateHolidays) {
    Holidays <- as.numeric(isHoliday(timeDate(InputData$DATE)))
    pres.day.2017 <- getNthDayOfWeek(third, Mon, Feb, 2017)
    pres.day.2017.idx <- grep(pres.day.2017, Time.Stamp)
    Holidays[pres.day.2017.idx] <- 1
    pres.day.2016 <- getNthDayOfWeek(third, Mon, Feb, 2016)
    pres.day.2016.idx <- grep(pres.day.2016, Time.Stamp)
    Holidays[pres.day.2016.idx] <- 1

    thanksgiving.2016 <- getNthDayOfWeek(fourth, Thu, Nov, 2016)
    thanksgiving.2016.idx <- grep(thanksgiving.2016, Time.Stamp)
    Holidays[thanksgiving.2016.idx] <- 1
  }

  # Choose Data to Process
  Dependent.Ix <- c(2:4) # Select dependent columns

  if (shouldCalculateHolidays) {
    Dependent.Data <- cbind(Hours.Modified, Day.Number.Modified, Holidays, InputData[TrainRange, Dependent.Ix]) # X ()
  } else {
    Dependent.Data <- cbind(Hours.Modified, Day.Number.Modified, InputData[TrainRange, Dependent.Ix]) # X ()
  }
  Dependent.Data <- as.data.frame(Dependent.Data)
  inputs <- as.data.frame(scale(Dependent.Data))
  print("Dependent data tags: ")
  print(names(inputs))
  return(inputs)
}

getModelTargets <- function(InputData) {
  Num.Data.Points <- dim(InputData)[1]
  StartTime <- 1
  TrainRange <- StartTime:Num.Data.Points
  Independent.Ix <- c(5) # Select Independent columns
  Independent.Data <- InputData[TrainRange, Independent.Ix]; # Y (Actual Electric Demand )
  print("Independent data tags:")
  print(names(InputData[Independent.Ix]))
  return(Independent.Data)
}

Forecast.Electric.Demand <- function(InputData)
{
  inputs <- getModelInputs(InputData)
  targets <- getModelTargets(InputData)
  Percent.To.Test <- 0.40 # Split the input data into train and test
  print("Define NuNet Inputs: ")
  print(paste0("Percent of input data to test: ", 100 * Percent.To.Test, " %"))

  # Train NuNet & Get Predictions
  print("Train NuNet & Get Predictions, please wait... ");
  Predicted.Electric.Demand <- TrainNuNet(inputs, targets, Percent.To.Test)
  print("NuNet Training finished!");

  Time.Stamp <- strptime(InputData$DATE,"%m/%d/%Y %H:%M")
  Output <- list(
    "TimeStamp" = Time.Stamp,
    "inputs" = inputs,
    "targets" = scale(targets),
    Predicted.Electric.Demand=Predicted.Electric.Demand,
    Percent.To.Test=Percent.To.Test)

  return(Output)
}


TrainNuNet <- function(inputs, targets, Percent.To.Test) {
  # Normalize the Data
  targets.scale <- max(targets, na.rm=TRUE)
  if (is.null(dim(inputs))) { # Single Column Input
    z <- max(inputs, na.rm=TRUE) # find Max in Single Input Column
    inputs.scale <- z
    inputs.normalized <- inputs / inputs.scale # Normalize Data
    targets.normalized <- targets / targets.scale # Normalize Data
  } else { # Multi Column Input
    z <- apply(inputs, MARGIN = 2, function(x) max(x, na.rm=TRUE)) # find Max in Each Input Column
    inputs.scale <- as.vector(z)
    # inputs.normalized <- sweep(inputs, 2, inputs.scale, `/`) # Normalize Data
    # inputs.normalized <- sweep(inputs.normalized, 2, apply(inputs.normalized, 2, median)) # divide then subtract
    inputs.normalized <- sweep(inputs, 2, apply(inputs, 2, median)) # instead of divide by max
    targets.normalized <- targets / targets.scale # Normalize Data
  }

  # Split the Data into Train and Test
  patterns <- splitForTrainingAndTest(inputs.normalized, targets.normalized, ratio = Percent.To.Test)
  set.seed(13);

  # Train NN to folow Actual
  # The use of an Elman network (Elman 1990) for time series regression.
  model <- elman(
    patterns$inputsTrain,
    patterns$targetsTrain,
    size = c(6, 6),
    learnFuncParams = c(0.1),
    maxit = 500,
    inputsTest = patterns$inputsTest,
    targetsTest = patterns$targetsTest,
    linOut = FALSE)

  NN.fitted.Train <- model$fitted.values * targets.scale
  NN.fitted.Test <- model$fittedTestValues * targets.scale

  Predicted.Electric.Demand <- c(NN.fitted.Train, NN.fitted.Test)

  result <- list(Predicted.Electric.Demand)

  return(result) # Returned object
}

