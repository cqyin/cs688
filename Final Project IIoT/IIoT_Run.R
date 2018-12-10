# RSNNS Energy Consumption Forecast
rm(list=ls()); cat("\014") # clear all
library("RSNNS")
source("Project Functions.R") # This is the Lab version of the code

# Read Data from CSV
Path.2.Files <- file.path("Data","OficeBuildingData.csv") # Path to CSV File
InputData <- read.csv(Path.2.Files,stringsAsFactors = FALSE,
                      blank.lines.skip = TRUE,header=T) # Read CSV File

# Get data exported from FacilityConneX
csv.file <- file.path('Data', 'FCX-Chart.csv')
csv.data <- read.csv(
  csv.file, stringsAsFactors = FALSE, blank.lines.skip = TRUE, header = TRUE)

# Call Forecast Function to Train NN & Obtain Predictions
Output <- Forecast.Electric.Demand(InputData)

# ### ====== Claculate Residuals ====
(R2 <- calculateRSquared(InputData$Electric.Demand..kW., unlist(Output$Predicted.Electric.Demand)))

actual.csv <- csv.data$Optimal.Electric.Demand.kWh..Student.013...Electric.Meter....kWh..
predicted.csv <- csv.data$Predicted.Electric.Demand.kWh..Student.013...Electric.Meter....kWh..
actual.csv <- actual.csv[-1] # remove first entry because in predicted it is NA
predicted.csv <- predicted.csv[-1] # remove NA
(calculateRSquared(actual.csv, predicted.csv))

# Plot Results
Range.to.Plot <- seq(from = round(dim(InputData)[1]*(1-Output$Percent.To.Test)), to = dim(InputData)[1])
Title <- "NN Trained to Model Office building Energy Consumption based on Temperature, Humidity and Dewpoint"
Title <- paste(Title,paste0("(Train End Date is ", Output$TimeStamp[Range.to.Plot[1]]),")")
Title <- paste0(Title,"; R Squared:",round(R2,2))

# ### ====== Plot Results ====
library(plotly)
PlotData <- data.frame(x=Output$TimeStamp,
                       y1=InputData$Electric.Demand..kW.,
                       y2=unlist(Output$Predicted.Electric.Demand))

p <- plot_ly(PlotData, x=~x, y=~y1, name = 'Actual Electric Demand (kW)', type = 'scatter', mode = 'lines')
p <- add_trace(p,y=~y2, name = 'RNN Forecasted Demand (kWh)', mode = 'lines', symbol = I(1), marker = list(size = 5)) # add another line


p <- p %>% layout(title = Title,
                  xaxis = list(title = 'Date',
                               zeroline = TRUE),
                  yaxis = list(title = 'Electric Demand (kW)',
                               range = c(0,max(InputData$Electric.Demand..kW.))))

p # To Plot type "p" in console



