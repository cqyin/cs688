library(googleVis)

# Line Chart

student.data = data.frame(
  Student = c("Alice", "Bob", "Charlie", "Dave", "Ed"),
  Exam = c(90, 80, 60, 85, 75),
  Quiz = c(8, 6, 9, 7, 10))

student.data

chart1 <- gvisLineChart(student.data)
plot(chart1)

chart1$type
chart1$chartid
chart1$html$header
chart1$html$chart
chart1$html$caption
chart1$html$footer

names(chart1$html$chart)

chart1$html$chart['jsChart']
chart1$html$chart['divChart']

# Save chart html to file
cat(chart1$html$chart, file = "chart1.html")

# Line Chart with 2 axis


chart2 <- 
  gvisLineChart(student.data,
                "Student", 
                c("Exam","Quiz"),
                options=list(
                  series="[{targetAxisIndex: 0},
                           {targetAxisIndex:1}]",
                  vAxes="[{title:'Exam'}, {title:'Quiz'}]"
                ))
                  
plot(chart2)

cat(chart2$html$chart, file = "chart2.html")

# chart with options

chart3 <-  
  gvisLineChart(student.data, 
                xvar="Student", 
                yvar=c("Exam","Quiz"),
                options=list(
                  title="Student Scores",
                  titleTextStyle="{color:'red', 
                          fontName:'Courier', 
                          fontSize:16}", 
                  backgroundColor="#D3D3D3",                          
                  vAxis="{gridlines:{color:'red', count:5}}",
                  hAxis="{title:'Student', titleTextStyle:{color:'blue'}}",
                  series="[{color:'green', targetAxisIndex: 0}, 
                           {color: 'orange',targetAxisIndex:1}]",
                  vAxes="[{title:'Exam'}, {title:'Quiz'}]",
                  legend="bottom",
                  curveType="function",
                  width=500, height=300                         
                        ))
plot(chart3)

cat(chart3$html$chart, file = "chart3.html")

# Inline editing

chart4 <-  
  gvisLineChart(student.data, 
                xvar = "Student", 
                yvar = c("Exam","Quiz"),
                options=list(
                  series="[{targetAxisIndex: 0},
                           {targetAxisIndex:1}]",
                  vAxes="[{title:'Exam'}, {title:'Quiz'}]",
                  gvis.editor="Edit me!"))
plot(chart4)

cat(chart4$html$chart, file = "chart4.html")



# Bar Chart

chart5 <- gvisBarChart(student.data)
plot(chart5)

cat(chart5$html$chart, file = "chart5.html")

chart6 <- gvisBarChart(student.data,
                       xvar="Student", 
                       yvar=c("Exam","Quiz"),
                       options=list(isStacked = TRUE))
plot(chart6)

cat(chart6$html$chart, file = "chart6.html")


# Column chart

chart7 <- gvisColumnChart(student.data)
plot(chart7)

cat(chart7$html$chart, file = "chart7.html")

chart8 <- gvisColumnChart(student.data,
                       xvar="Student", 
                       yvar=c("Exam","Quiz"),
                       options=list(isStacked = TRUE))
plot(chart8)

cat(chart8$html$chart, file = "chart8.html")

# Area chart

chart9 <- gvisAreaChart(student.data)

plot(chart9)

cat(chart9$html$chart, file = "chart9.html")

chart10 <- gvisAreaChart(student.data,
                          xvar="Student", 
                          yvar=c("Exam","Quiz"),
                          options=list(isStacked = TRUE))
plot(chart10)

cat(chart10$html$chart, file = "chart10.html")

# Combo chart

chart11 <- 
  gvisComboChart(student.data,
                 xvar="Student",
                 yvar=c("Exam", "Quiz"),
                 options=list(seriesType="bars",
                              series='{1: {type:"line"}}'))
plot(chart11)

cat(chart11$html$chart, file = "chart11.html")

chart12 <- 
  gvisComboChart(student.data,
                 xvar="Student",
                 yvar=c("Exam", "Quiz"),
                 options=list(seriesType="line",
                              series='{1: {type:"bars"}}'))
plot(chart12)

cat(chart12$html$chart, file = "chart12.html")

# Scatter chart

head(women)

chart13 <- gvisScatterChart(women)

plot(chart13)

cat(chart13$html$chart, file = "chart13.html")


chart14 <- 
  gvisScatterChart(women,
                   options=list(
                     legend="none",
                     lineWidth=2, pointSize=10,
                     pointShape="diamond",
                     title="Weight vs Height", 
                     vAxis="{title:'weight (lbs)'}",
                     hAxis="{title:'height (in)'}", 
                     width=400, height=400))
plot(chart14)

cat(chart14$html$chart, file = "chart14.html")

head(iris)

chart15 <- gvisScatterChart(iris[,1:4],
                            options=list(
                              legend="top",
                              hAxis="{title:'Sepal Length'}", 
                              width=600, height=400))
plot(chart15)

cat(chart15$html$chart, file = "chart15.html")

data.iris <- iris

data.iris$Index <- 1:nrow(iris)

head(data.iris, n = 2)

head(data.iris[, c(6, 1:4)], n = 2)

chart15_1 <- gvisScatterChart(data.iris[, c(6, 1:4)],
                            options=list(
                              hAxis="{title:'Index'}", 
                              width=700, height=600))
plot(chart15_1)

cat(chart15_1$html$chart, file = "chart15_1.html")

# Bubble chart

head(Fruits)

chart16 <- 
  gvisBubbleChart(Fruits, 
                  idvar="Fruit", 
                  xvar="Sales", yvar="Expenses",
                  colorvar="Year", sizevar="Profit",
                  options=list(
                    hAxis='{minValue:70, maxValue:120}',
                    width=600, height=400))
plot(chart16)

cat(chart16$html$chart, file = "chart16.html")


# Candlestick chart

chart17 <- 
  gvisCandlestickChart(OpenClose, xvar="Weekday", 
                       low="Low", high="High",
                       open="Open", close="Close",
                        options=list(
                          vAxis='{minValue:0, maxValue:100}',
                          legend='none'))

plot(chart17)

cat(chart17$html$chart, file = "chart17.html")


# Pie chart

chart18 <- gvisPieChart(CityPopularity)

plot(chart18)

cat(chart18$html$chart, file = "chart18.html")

# Donought chart

chart19 <- gvisPieChart(CityPopularity, options=list(
  slices="{1: {offset: 0.3}, 3: {offset: 0.2}}",
  title='City popularity',
  legend='none',
  pieSliceText='label',
  pieHole=0.2,
  width=500, height=500))

plot(chart19)

cat(chart19$html$chart, file = "chart19.html")

chart19_1 <- gvisPieChart(CityPopularity, options=list(
  slices="{1: {offset: 0.3}}",
  title='City popularity',
  legend='none', is3D='true',
  pieSliceText='label',
  pieHole=0.2,
  width=500, height=500))

plot(chart19_1)

cat(chart19_1$html$chart, file = "chart19_1.html")


# Gauge

CityPopularity

chart20 <-  
  gvisGauge(CityPopularity,
            options=list(min=0, max=800))
plot(chart20)

cat(chart20$html$chart, file = "chart20.html")

chart21 <-  
  gvisGauge(CityPopularity,
            options=list(min=0, max=800,
                         redFrom=0, redTo=300, 
                         yellowFrom=300, yellowTo=500,
                         greenFrom=500, greenTo=800, 
                         width=400, height=300))
plot(chart21)

cat(chart21$html$chart, file = "chart21.html")

# Histogram

chart22 <- gvisHistogram(iris[,1:4],
                            options=list(
                              legend="top"))
plot(chart22)

cat(chart22$html$chart, file = "chart22.html")

# Table

head(Stock)

chart23 <- 
  gvisTable(Stock,
            formats=list(Value="#,###"))

plot(chart23)

cat(chart23$html$chart, file = "chart23.html")


# Table with Pages

names(Population)

head(Population[, c(1:4, 6:7)])

Population[1,5]

chart24 <- 
  gvisTable(Population,
            formats=list(Population="#,###",
                         '% of World Population'='#.#%'),
            options=list(page='enable'))
plot(chart24)

cat(chart24$html$chart, file = "chart24.html")



# Organizational chart

data = data.frame(
  Node =  c("A", "B", "C", "D", "E", "F"),
  Parent = c(NA, "A", "A", "A", "C", "C"),
  val = 1:6
)

data

chart25 <- 
  gvisOrgChart(data,
               idvar="Node",
               parentvar="Parent",
               tipvar="val",
               options=list(width=600, height=250,
                            size='large', 
                            allowCollapse=TRUE))

plot(chart25)

cat(chart25$html$chart, file = "chart25.html")


Regions

chart26 <- 
  gvisOrgChart(Regions,
               options=list(width=600, height=450,
                            size='large', 
                            allowCollapse=TRUE))

plot(chart26)

cat(chart26$html$chart, file = "chart26.html")


# Tree map

chart27 <- 
  gvisTreeMap(Regions,
              idvar = "Region", 
              parentvar = "Parent",
              sizevar = "Val",
              colorvar = "Fac",
              options=list(width=450, height=450,
                           fontSize=16))

plot(chart27)

cat(chart27$html$chart, file = "chart27.html")


# Annotation chart

chart28 <- 
  gvisAnnotationChart(Stock,
                      datevar="Date",
                      numvar="Value", 
                      idvar="Device",
                      titlevar="Title", 
                      annotationvar="Annotation")

plot(chart28)

cat(chart28$html$chart, file = "chart28.html")

# Sankey chart
data.SK <- data.frame(
  From=c('GB','FR','IN', 'IN','JP','SG','JFK','JFK','SFO','SFO'),
  To=c(rep('JFK', 3), rep('SFO', 3), rep(c('BOS', 'DC'),2)),
  Weight=c(5,7,6,2,9,4, 10,8, 6,9))

data.SK

chart29 <- 
  gvisSankey(data.SK,
             from="From", to="To", weight="Weight",
             options=list(
               sankey="{link: {color: { fill: '#d799ae' } },
                                node: { color: { fill: '#00ff00' },
                                label: { color: '#871b47' } }}"))
plot(chart29)

cat(chart29$html$chart, file = "chart29.html")


# Geo chart

Exports

chart30 <-
  gvisGeoChart(Exports, 
               locationvar="Country", 
               colorvar="Profit")
               
plot(chart30)

cat(chart30$html$chart, file = "chart30.html")

# US Data state by state

library(datasets)

colnames(state.x77)

rownames(state.x77)

states <- data.frame("State" = rownames(state.x77), 
                     state.x77[, c(1,2)])

head(states)

chart31 <- 
  gvisGeoChart(states, 
               locationvar = "State", 
               colorvar = "Population",
               options=list(region="US",
                            displayMode="regions", 
                            resolution="provinces",
                            width=600, height=400))
plot(chart31)

cat(chart31$html$chart, file = "chart31.html")


# Hurricane Andrew

names(Andrew)

Andrew[1:3, c("LatLong","Speed_kt", "Pressure_mb")]

chart32 <- 
  gvisGeoChart(Andrew, 
               locationvar="LatLong", 
               sizevar="Speed_kt",
               colorvar="Pressure_mb", 
               options=list(region="US",
                            displayMode="Markers", 
                            resolution="provinces",
                 colorAxis="{colors:['red', 'grey']}"))

plot(chart32)

cat(chart32$html$chart, file = "chart32.html")

# Google Maps

Andrew[1:3, c("LatLong","Tip")]

chart33 <- 
  gvisMap(Andrew, 
          locationvar = "LatLong" , 
          tipvar = "Tip", 
          options=list(
            showTip=TRUE, 
            showLine=TRUE, 
            enableScrollWheel=TRUE,
            mapType='terrain', 
            useMapTypeControl=TRUE))

plot(chart33)

cat(chart33$html$chart, file = "chart33.html")

Population[1:10, c("Country", "Population")]

chart33_1 <- 
  gvisMap(Population[1:10,], 
          locationvar = "Country" , 
          tipvar = "Population", 
          options=list(
            showTip=TRUE, 
            mapType='terrain', 
            useMapTypeControl=TRUE))

plot(chart33_1)

cat(chart33_1$html$chart, file = "chart33_1.html")

# Calendar chart

head(Cairo)
range(Cairo$Temp)

chart34 <- 
  gvisCalendar(Cairo,
               datevar="Date", 
               numvar="Temp",
               options=list(
                 title="Daily temperature in Cairo",
                 width=950))
                 
plot(chart34)

cat(chart34$html$chart, file = "chart34.html")

chart34_1 <- 
  gvisCalendar(Cairo,
               datevar="Date", 
               numvar="Temp",
               options=list(
                 title="Daily temperature in Cairo",
                 width=950,
                 colorAxis="{colors:['blue', 'red']}"))

plot(chart34_1)

cat(chart34_1$html$chart, file = "chart34_1.html")



# Timeline chart

data1 <- data.frame(
  Term=c("1","2","3"),
  President=c("Washington", "Adams", "Jefferson"),
  Begin=as.Date(c("1789-03-29", "1797-02-03", "1801-02-03")),
  End=  as.Date(c("1797-02-03", "1801-02-03", "1809-02-03")))

data1

chart35 <- 
  gvisTimeline(data=data1, 
               rowlabel="President",
               start="Begin", end="End")

plot(chart35)

cat(chart35$html$chart, file = "chart35.html")


chart36 <- 
  gvisTimeline(data=data1, 
               rowlabel="President",
               barlabel = "Term",
               start="Begin", end="End")

plot(chart36)

cat(chart36$html$chart, file = "chart36.html")


data2 <- data.frame(
  Position=c(rep("President", 3), rep("VicePres", 3)),
  Name=c("Washington", "Adams", "Jefferson",
          "Adams", "Jefferson", "Burr"),
  Begin=as.Date(rep(c("1789-03-29", "1797-02-03", 
                      "1801-02-03"),2)),
  End=as.Date(rep(c("1797-02-03", "1801-02-03", 
                      "1809-02-03"),2)))

data2

chart37 <- 
  gvisTimeline(data=data2, 
               rowlabel="Name",
               barlabel="Position",
               start="Begin", end="End",
               options=list(
                 height=350))

plot(chart37)

cat(chart37$html$chart, file = "chart37.html")



chart38 <- 
  gvisTimeline(data=data2,
               rowlabel="Name",
               barlabel="Position",
               start="Begin", 
               end="End",
               options=list(
                 timeline="{groupByRowLabel:false}",
                 height=350,
                 backgroundColor='#add8e6',
                 colors="['#cbb69d', '#603913']"))

plot(chart38)

cat(chart38$html$chart, file = "chart38.html")


# Merging charts

exports.chart1 <- 
  gvisGeoChart(Exports, 
               locationvar="Country", 
               colorvar="Profit", 
               options=list(width=300, height=300))

exports.chart2 <- 
  gvisTable(Exports, 
            options=list(width=200, height=300))

chart39 <- gvisMerge(exports.chart1,
                     exports.chart2,
                     horizontal=TRUE) 
plot(chart39)

cat(chart39$html$chart, file = "chart39.html")

#

options.chart <- list(legend='none', width=320, height=130)

chart40.1 <- 
  gvisTable(OpenClose, 
            options=options.chart)

chart40.2 <- 
  gvisCandlestickChart(OpenClose, xvar="Weekday", 
                       low="Low", high="High",
                       open="Open", close="Close",
                       options=options.chart)

chart40.3 <- 
  gvisLineChart(OpenClose, "Weekday", 
                c("Open", "Close"), 
                options=options.chart)

chart40.4 <- 
  gvisColumnChart(OpenClose, "Weekday", 
                  c("Open", "Close"),
                  options=options.chart)

chart40.5 <- 
  gvisAreaChart(OpenClose, "Weekday", 
                c("Open", "Close"),
                options=options.chart)

chart40.6 <- 
  gvisBarChart(OpenClose, "Weekday", 
               c("Open", "Close"),
               options=options.chart)

merged.1 <- 
  gvisMerge(
    gvisMerge(chart40.1, chart40.2, horizontal=TRUE),
    gvisMerge(chart40.3, chart40.4, horizontal=TRUE),
    horizontal=FALSE, tableOptions="bgcolor=\"#AABBCC\"") 

chart40 <- 
  gvisMerge(
    merged.1, 
    gvisMerge(chart40.5, chart40.6, horizontal=TRUE),
    horizontal=FALSE, tableOptions="bgcolor=\"#AABBCC\"") 

plot(chart40)

cat(chart40$html$chart, file = "chart40.html")

## Trenlines

## Trend line demo
# A trendline is a line superimposed on a chart revealing the overall direction 
# of the data. Google Charts can automatically generate trendlines for 
# Scatter Charts, Bar Charts, Column Charts, and Line Charts.
# 
# Fore more details see:
# https://developers.google.com/chart/interactive/docs/gallery/trendlines

## Linear trend line

## Add a trend line to the first series
## ---- LinearTrend ----
chart41 <-
  gvisScatterChart(women, 
                   options=list(
                     trendlines="0"))
plot(chart41)

cat(chart41$html$chart, file = "chart41.html")



## ---- ExponentialTrend ----

chart42 <-
  gvisScatterChart(women, 
                   options=list(trendlines=
                                  "{0: {type: 'exponential',
                                visibleInLegend: 'true',
                                color: 'green',
                                lineWidth: 10,
                                opacity: 0.5}}",
                                legend="top"))

plot(chart42)

cat(chart42$html$chart, file = "chart42.html")



## Multiple trend lines

chart43 <-
  gvisScatterChart(iris[,1:4], 
                   options=list(trendlines=
                                  "{0: { visibleInLegend: 'true', 
                                color: 'blue'},
                                1: { visibleInLegend: 'false', 
                                color: 'red'},
                                2: { visibleInLegend: 'false', 
                                color: 'orange'}
                                }",
      legend="bottom"))

plot(chart43)

cat(chart43$html$chart, file = "chart43.html")


## ---- ColumnChartWithTrendline ----

data3 <- data.frame(
  Col1=c(1,3,4,5,6,8), 
  Col2=c(12,23,32,40,50,65),
  Col3=c(5,6,10,12,15,20))

data3

chart44 <-
  gvisColumnChart(data3,
                  options=list(trendlines="{
                               0: {
                               type: 'exponential',
                               labelInLegend: 'Trend 1',
                               visibleInLegend: true,}, 
                               1:{
                               labelInLegend: 'Trend 2',
                               visibleInLegend: true}
                               }",
            chartArea="{left:50,top:20,
            width:'50%',height:'75%'}"
              ))

plot(chart44)

cat(chart44$html$chart, file = "chart44.html")




