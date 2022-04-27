# 00 main script (scipting_trial1)
# aslinda main degil bu. data hazirlama bolgesi gibi. ama butun datalari da hazirlamiyoruz falan. buna cok gerek yok gibi

# book render -------------------------------------------------------------
bookdown::render_book("index.Rmd")
browseURL("docs/index.html")


# Libraries ---------------------------------------------------------------
require(dplyr)
require(data.table)
require(ggplot2)
require(glmnet)
# require(DT)
# require(caret)
require(plot.matrix)
require(kernlab)
require(tidyr)
# require(stringr)
require(tidyverse)
#
require(knitr)

# Load Data ---------------------------------------------------------------
data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/aliaga.csv"))
# data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/bares.csv"))
# data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/dinar.csv"))
# data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/geycek.csv"))
# data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/soke.csv"))
# data = data.table(read.csv(file = "/Users/canhakan/tez/models_main/stations/soma.csv"))

station_name = 'aliaga'

# Column Names ------------------------------------------------------------
temp = ncol(data)-3
#
newnames = c("date","hour","production")
for(i in c(1:temp)){
    n = paste("loc",i,sep="")
    newnames = c(newnames,n)
}
#
colnames(data) = newnames

# Derived Datasets -----------------------------------------------------------

# train/test
index = floor(nrow(data)*0.75)

train = data[1 : index, ]
test1  = data[(index+1) : nrow(data), ]

# lag +-3
lagged = createLagged(data = train, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test = createLagged(data = test, lags = c(-3:3), other.head = c(1:3), other.tail = 0)

# power 3
powered = createPowered(data = train, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test = createPowered(data = test, powers = c(2:3), other.head = c(1:3), other.tail = 0)

# temporal
temporal = createTemporal(train,6,c(1:3))
temporal.test = createTemporal(test,6,c(1:3))

# lag and power
laggedpowered = createLagged(data = powered, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test = createLagged(data = powered.test, lags = c(-3:3), other.head = c(1:3), other.tail = 0)


# Results Table -----------------------------------------------------------
results = data.table(station = character(),
                     model = character(),
                     train_error = numeric(),
                     test_error = numeric())

results[order(station)]
