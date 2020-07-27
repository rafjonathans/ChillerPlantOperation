library(tidyverse)
library(lubridate)
library(plotly)
library(egg)
library(caret)
library(e1071)
library(ggcorrplot)

start_time <- Sys.time()

# Get the dataset
## First step is to select working directory where the dataset stored
total_file <- list.files(pattern = c("Trend", ".csv"))

datalist <- list()

for (x in 1:length(total_file)) {
  
  # Read dataset
  datalist[[x]] <- read.csv(total_file[x], sep = ";", dec = ",", header = FALSE)
  
  # Split column considering the category in first column
  datalist[[x]] <- split(datalist[[x]], datalist[[x]][1])
  
  # Reduce timestamp
  for (y in 1:length(datalist[[x]])) {
    datalist[[x]][[y]] <- aggregate(datalist[[x]][[y]]$V3,
                                    list(hour = ymd_hms(cut(as.POSIXct(datalist[[x]][[y]]$V2, format = c("%e/%m/%Y %H:%M:%S")), "hour"))),
                                    mean, )
  }
}


## Merge similar variable
Variable <- list()

for (x in 1:length(datalist)){
  for (y in 1:length(datalist[[1]])) {
    if (x == 1) {
      Variable[[y]] <- datalist[[x]][[y]]
    }
    else {
      Variable[[y]] <- add_row(Variable[[y]],
                               datalist[[x]][[y]])
    }    
  }
}

## Remove duplicate hour
for (x in 1:length(Variable)){
  Variable[[x]] <- distinct(Variable[[x]], hour, .keep_all = T)
}

## Merge variable in a list into one table
Variable <- reduce(Variable, left_join, by = "hour")
colnames(Variable) <- c("Time", "OAHum", "OADB", "OAWB", "WCC5_EIT", 
                      "WCC5_EOT", "Variable_1", 'Variable_2', "HVAC_GPM", "HVAC_CHWdP", 
                      "WCC3_RLA", "WCC4_RLA", "WCC5_RLA", "WCC5_CIT", "WCC4_CIT",
                      "WCC3_CIT", "HVAC_CTHZ", "PROC_CTHZ", "PROC_CHWRT", "PROC_CHWST",
                      "HVAC_CHWRT", "HVAC_CHWST", "WCC3_EFF", "WCC4_EFF", "WCC5 EFF",
                      "Variable_3", "Variable_4", "WCC3_COT", "WCC4_COT", "WCC5_COT",
                      "WCC3_EOT", "WCC4_EOT", 'WCC3_EIT', 'WCC4_EIT', 'DailyTRH')


# Missing value 
## Impute missing logging time
timePeriod <- data.frame(Time = seq(min(Variable$Time), max(Variable$Time), by="hour"))
Variable <- left_join(timePeriod, Variable,"Time")


## Explore missing value
valplot <- function(x, value){
  if (is.na(value)) {
    temp <- is.na(x)
  } 
  else {
    temp <- (x == value)
  }
  temp %>%
    data.frame() %>%
    rowid_to_column(var = "X") %>%
    gather(key = "Y", value = "Z", -1) %>%
    
    ggplot(aes(x = X,
               y = Y,
               fill = Z)) +
    geom_tile() +
    labs(x = "Missing Position",
         y = "Variables",
         title = "Missing Value Mapping") +
    theme(panel.background = element_blank(),
          legend.position = "top", 
          plot.title = element_text(hjust = 0.5))
}

plot_missmap <- valplot(Variable, NA)

plot_missmap


## Drop DailyTRH column due to high missing value
Variable <- select(Variable, -c("DailyTRH"))

## Check other variable 
missVar <- Variable %>%
              is.na() %>%
              colSums() > 0

missVar <- names(Variable[,missVar])
# View(Variable[rowSums(is.na(Variable))>0,])

## Impute 0 to missing value and negative value
Variable <- replace(Variable, is.na(Variable), 0)
### As seen in View(), row with missing value also have 0 values at least in one column"

## Replace negative value with 0 
Variable <- replace(Variable, Variable < 0, 0)



# Outliers
## Outliers on ambient environment condition will be replaced by new values
## Outliers on facility operational condition will be handled by standardization. 
## Keeping the outlier information but make the calculation algorithm less sensitive to that


## Explore OA outliers
OA_var <- c("Time", "OAHum","OADB", "OAWB")

OAHum_plot <- ggplot(Variable) + 
                geom_boxplot(aes(y = OAHum)) +
                labs(x= "OA Humidity",
                     y= "Relative Humidity [%]") +
                scale_y_continuous(expand = c(0,0),limits = c(0,120), breaks = seq(0,120,20)) + 
                scale_x_continuous(expand = c(0,0.1)) +
                theme(panel.background = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.x = element_blank(), 
                      panel.grid.major.y = element_line(colour = "grey"),
                      plot.title = element_text(hjust = 0.5, margin=margin(0,0,30,0)))

OADB_plot <- ggplot(Variable) +
              geom_boxplot(aes(y = OADB)) +
              labs(x= "OA Dry Bulb",
                   y= "Dry Bulb Temperature [C]") +
              scale_y_continuous(expand = c(0,0),limits = c(0,60), breaks = seq(0,60,10)) + 
              scale_x_continuous(expand = c(0,0.1)) +
              theme(panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.x = element_blank(), 
                    panel.grid.major.y = element_line(colour = "grey"),
                    plot.title = element_text(hjust = 0.5, margin=margin(0,0,30,0)))

OAWB_plot <- ggplot(Variable) +
              geom_boxplot(aes(y = OAWB)) +
              labs(x= "OA Wet Bulb",
                   y= "Wet Bulb Temperature [C]",
                   title = "Outdoor Air Condition") +
              scale_y_continuous(expand = c(0,0),limits = c(0,60), breaks = seq(0,60,10)) + 
              scale_x_continuous(expand = c(0,0.1)) +
              theme(panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.x = element_blank(), 
                    panel.grid.major.y = element_line(colour = "grey"),
                    plot.title = element_text(hjust = 0.5, margin=margin(0,0,30,0)))

OA_plot <- ggarrange(OADB_plot, OAWB_plot, OAHum_plot, nrow = 1, ncol = 3)

OA_stats <- data.frame(OAHum= boxplot.stats(Variable$OAHum)$stats,
                       OADB= boxplot.stats(Variable$OADB)$stats,
                       OAWB= boxplot.stats(Variable$OAWB)$stats)


## Replace outliers with Q1 and Q4 on OAWB and OADB
Variable$OADB[which(Variable$OADB < OA_stats[1,which(names(OA_stats) == 'OADB')])] <- OA_stats[1,which(names(OA_stats) == 'OADB')]
Variable$OADB[which(Variable$OADB > OA_stats[5,which(names(OA_stats) == 'OADB')])] <- OA_stats[5,which(names(OA_stats) == 'OADB')]

Variable$OAWB[which(Variable$OAWB < OA_stats[1,which(names(OA_stats) == 'OAWB')])] <- OA_stats[1,which(names(OA_stats) == 'OAWB')]
Variable$OAWB[which(Variable$OAWB > OA_stats[5,which(names(OA_stats) == 'OAWB')])] <- OA_stats[5,which(names(OA_stats) == 'OAWB')]


## Replace outliers with Q1 and Q3 on OAHum
Variable$OAHum[which(Variable$OAHum < OA_stats[1,which(names(OA_stats) == 'OAHum')])] <- OA_stats[1,which(names(OA_stats) == 'OAHum')]
Variable$OAHum[which(Variable$OAHum > OA_stats[4,which(names(OA_stats) == 'OAHum')])] <- OA_stats[4,which(names(OA_stats) == 'OAHum')]


## As target variable is total chiller power consumption we need to add WCC
Variable$Total_RLA <- (Variable$WCC3_RLA + Variable$WCC4_RLA + Variable$WCC5_RLA)


## Manage too many 0 values
### In column
colZero <- (colSums(Variable == 0) / nrow(Variable)) %>%
              data.frame(Data = .) 

colZero <- data.frame(Data = colZero[order(colZero,decreasing = T),],
                      row.names = rownames(colZero)[order(colZero,decreasing = T)])
# colZero


### WCC RLA and EFF is possible to be zero during off operation. Drop Variable1 and Variable2
Variable <- Variable[-which(names(Variable) %in% c("Variable_2", "Variable_1"))]


### In rows
rowZero <- boxplot.stats(rowSums(Variable == 0))

mapZero <- valplot(Variable, 0)+
  geom_vline(xintercept = c(which(Variable$Time == ymd_hms(20160101000000)),
                            which(Variable$Time == ymd_hms(20170101000000)),
                            which(Variable$Time == ymd_hms(20180101000000))))

# Check total rows to delete
# sum(rowSums(Variable == 0) > rowZero$stats[5]+1) / nrow(Variable)
# Variable[(rowSums(Variable == 0) > rowZero$stats[5]+1),]


## Delete rows with too many 0
Variable <- Variable[-which(rowSums(Variable == 0) > rowZero$stats[5]),]

# Check zero values removal result
# valplot(Variable, 0)+
#   geom_vline(xintercept = c(which(Variable$Time == ymd_hms(20160101000000)),
#                             which(Variable$Time == ymd_hms(20170101000000)),
#                             which(Variable$Time == ymd_hms(20180101000000))))


## Explore operational outlier
### Visualize total outliers
OP_var <- names(Variable[-which(names(Variable) %in% OA_var)])

a <- c()

for (x in 1:length(OP_var)) {
  a[x] <- length(boxplot.stats(Variable[[OP_var[x]]])$out)
}

OP_outliers <- data.frame(OP_var= OP_var,
                          OutlierPercent = round(a/nrow(Variable)*100, 1))

OP_outliersPlot <- ggplot(OP_outliers, aes(x = OP_var, y = OutlierPercent)) +
                    geom_col() +
                    labs(y = "Outlier Percentage [%]",
                         title = "Operational Data Outlier Proportion") +
                    coord_flip() +
                    scale_y_continuous(expand = c(0,0), limits = c(0,15), breaks = seq(0,15,3)) +
                    theme(panel.background = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid.major.x = element_line(colour = "grey"),
                          plot.title = element_text(hjust = 0.5, margin=margin(0,0,10,0)))


## Standardization
### Check skewness and select transformation method to get normal distribution pattern

VarSkew <- data.frame(direct= abs(sapply(Variable[-1], skewness)),
           logp1= abs(sapply(log1p(Variable[-1]), skewness)),
           squared= abs(sapply((Variable[-1])**2, skewness)),
           cubed= abs(sapply((Variable[-1])**3, skewness)))

normalized_method <- c()
# Normalized_method == 1 means no normalization
# Normalized_method == 2 means log1p transformation
# Normalized_method == 3 means quadratic transformation
# Normalized_method == 4 means cubic transformation

for (x in 1:nrow(VarSkew)){
  normalized_method[x] <- which.min(VarSkew[x,])
  if (normalized_method[x] == 2) {
    Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] <- log1p(Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))])
  }
  if (normalized_method[x] == 3) {
    Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] <- Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] ** 2
  }
  if (normalized_method[x] == 4) {
    Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] <- Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] ** 3
  }
}
# record for normalized method
# normalized_method


## Standardized predictors
# Variable_3 always constant
Variable <- Variable[-which(names(Variable) == "Variable_3")]

dataNOTScaled <- Variable[which(names(Variable) %in% c("Total_RLA", "Time", "WCC3_RLA", "WCC4_RLA", "WCC5_RLA"))]

preProcValues <- preProcess(Variable[-which(names(Variable) %in% c("Total_RLA", "Time", "WCC3_RLA", "WCC4_RLA", "WCC5_RLA"))],
                            method = c("center", "scale"))
dataScaled <- predict(preProcValues,
                      Variable[-which(names(Variable) %in% c("Total_RLA", "Time", "WCC3_RLA", "WCC4_RLA", "WCC5_RLA"))])

allData <- bind_cols(dataNOTScaled, dataScaled)


# Feature selection
## Selection done by manual, by looking to dependent variable correlation to the other independent variables
## Using this
test <- Var2015[-1] %>% 
  cor() %>%
  abs() %>% 
  data.frame()

testSort <- order(test$WCC3_RLA, decreasing = T)
ggplotly(ggcorrplot(test[testSort,testSort], tl.srt = 90) + 
           theme(axis.text.x = element_text(vjust = 0.5)))


# Model
## Modeling and predicting from all data
### Funciton to transform the dependent back

transformBack <- function(numeric, predictedName){
  temp <- normalized_method[rownames(VarSkew) %in% c(predictedName)]
  
  if (temp == 1) {
    numeric
  }
  else if (temp == 2) {
    expm1(numeric) 
  }
  else if (temp == 3) {
    numeric ** 0.5
  }
  else if (temp == 4) {
    numeric ** (1/3)
  }
}

## Model all
set.seed(1234)

trainIndex <- createDataPartition(allData$Total_RLA, times = 1, p = 0.7, list = F)
trainVar <- Variable[trainIndex,]
testVar <- Variable[-trainIndex,]

modelVarWCC3 <- caret::train(WCC3_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar, method = "lm")
predictedVarWCC3 <- predict(modelVarWCC3, testVar[which(names(testVar) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                      transformBack("WCC3_RLA")

modelVarWCC4 <- caret::train(WCC4_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar, method = "lm")
predictedVarWCC4 <- predict(modelVarWCC4, testVar[which(names(testVar) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                      transformBack("WCC4_RLA")
                      
modelVarWCC5 <- caret::train(WCC5_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar, method = "lm")
predictedVarWCC5 <- predict(modelVarWCC5, testVar[which(names(testVar) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                      transformBack("WCC5_RLA")

modelVarPerformance <- data.frame(WCC3 = postResample(pred = predictedVarWCC3, obs = transformBack(testVar$WCC3_RLA, "WCC3_RLA")),
                                  WCC4 = postResample(pred = predictedVarWCC4, obs = transformBack(testVar$WCC4_RLA, "WCC4_RLA")),
                                  WCC5 = postResample(pred = predictedVarWCC5, obs = transformBack(testVar$WCC5_RLA, "WCC5_RLA")))

totalEnergyDiff <- data.frame(WCC3 = round((sum(predictedVarWCC3)/sum(transformBack(testVar$WCC3_RLA, "WCC3_RLA"))-1)*100,2),
                              WCC4 = round((sum(predictedVarWCC4)/sum(transformBack(testVar$WCC4_RLA, "WCC4_RLA"))-1)*100,2),
                              WCC5 = round((sum(predictedVarWCC5)/sum(transformBack(testVar$WCC5_RLA, "WCC5_RLA"))-1)*100,2), 
                              row.names = "PercentTotalEnergyDiff")
  

modelVarPerformance <- bind_rows(modelVarPerformance, totalEnergyDiff)
modelVarPerformance


## Model 2015
Var2015 <- Variable[which(allData$Time >= ymd(20150101) & Variable$Time < ymd(20160101)),]

trainIndex_2015 <- createDataPartition(Var2015$WCC3_RLA, times = 1, p = 0.7, list = F)
trainVar_2015 <- Var2015[trainIndex_2015,]
testVar_2015 <- Var2015[-trainIndex_2015,]

modelVarWCC3_2015 <- caret::train(WCC3_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2015, method = "lm")
predictedVarWCC3_2015 <- predict(modelVarWCC3_2015, testVar_2015[which(names(testVar_2015) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                          transformBack("WCC3_RLA")

modelVarWCC4_2015 <- caret::train(WCC4_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2015, method = "lm")
predictedVarWCC4_2015 <- predict(modelVarWCC4_2015, testVar_2015[which(names(testVar_2015) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                          transformBack("WCC4_RLA")

modelVarWCC5_2015 <- caret::train(WCC5_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2015, method = "lm")
predictedVarWCC5_2015 <- predict(modelVarWCC5_2015, testVar_2015[which(names(testVar_2015) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
                          transformBack("WCC5_RLA")

modelVarPerformance_2015 <- data.frame(WCC3 = postResample(pred = predictedVarWCC3, obs = transformBack(testVar$WCC3_RLA, "WCC3_RLA")),
                                       WCC4 = postResample(pred = predictedVarWCC4, obs = transformBack(testVar$WCC4_RLA, "WCC4_RLA")),
                                       WCC5 = postResample(pred = predictedVarWCC5, obs = transformBack(testVar$WCC5_RLA, "WCC5_RLA")))

totalEnergyDiff_2015 <- data.frame(WCC3 = round((sum(predictedVarWCC3_2015)/sum(transformBack(testVar_2015$WCC3_RLA, "WCC3_RLA"))-1)*100,2),
                              WCC4 = round((sum(predictedVarWCC4_2015)/sum(transformBack(testVar_2015$WCC4_RLA, "WCC4_RLA"))-1)*100,2),
                              WCC5 = round((sum(predictedVarWCC5_2015)/sum(transformBack(testVar_2015$WCC5_RLA, "WCC5_RLA"))-1)*100,2), 
                              row.names = "PercentTotalEnergyDiff")


modelVarPerformance_2015 <- bind_rows(modelVarPerformance_2015, totalEnergyDiff_2015)
modelVarPerformance_2015

data.frame(Predict = predictedVarWCC3_2015,
           Actual = transformBack(testVar_2015$WCC3_RLA, "WCC3_RLA")) %>% 
  rowid_to_column() %>% 
  gather(key = "y", value = "z", -1) %>% 
  ggplot() +
  geom_jitter(aes(rowid, z, col= y)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_blank())
  

## Model 2016
Var2016 <- Variable[which(allData$Time >= ymd(20160101) & Variable$Time < ymd(20170101)),]

trainIndex_2016 <- createDataPartition(Var2016$WCC3_RLA, times = 1, p = 0.7, list = F)
trainVar_2016 <- Var2016[trainIndex_2016,]
testVar_2016 <- Var2016[-trainIndex_2016,]

modelVarWCC3_2016 <- caret::train(WCC3_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2016, method = "lm")
predictedVarWCC3_2016 <- predict(modelVarWCC3_2016, testVar_2016[which(names(testVar_2016) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC3_RLA")

modelVarWCC4_2016 <- caret::train(WCC4_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2016, method = "lm")
predictedVarWCC4_2016 <- predict(modelVarWCC4_2016, testVar_2016[which(names(testVar_2016) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC4_RLA")

modelVarWCC5_2016 <- caret::train(WCC5_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2016, method = "lm")
predictedVarWCC5_2016 <- predict(modelVarWCC5_2016, testVar_2016[which(names(testVar_2016) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC5_RLA")

modelVarPerformance_2016 <- data.frame(WCC3 = postResample(pred = predictedVarWCC3, obs = transformBack(testVar$WCC3_RLA, "WCC3_RLA")),
                                       WCC4 = postResample(pred = predictedVarWCC4, obs = transformBack(testVar$WCC4_RLA, "WCC4_RLA")),
                                       WCC5 = postResample(pred = predictedVarWCC5, obs = transformBack(testVar$WCC5_RLA, "WCC5_RLA")))

totalEnergyDiff_2016 <- data.frame(WCC3 = round((sum(predictedVarWCC3_2016)/sum(transformBack(testVar_2016$WCC3_RLA, "WCC3_RLA"))-1)*100,2),
                                   WCC4 = round((sum(predictedVarWCC4_2016)/sum(transformBack(testVar_2016$WCC4_RLA, "WCC4_RLA"))-1)*100,2),
                                   WCC5 = round((sum(predictedVarWCC5_2016)/sum(transformBack(testVar_2016$WCC5_RLA, "WCC5_RLA"))-1)*100,2), 
                                   row.names = "PercentTotalEnergyDiff")


modelVarPerformance_2016 <- bind_rows(modelVarPerformance_2016, totalEnergyDiff_2016)
modelVarPerformance_2016



## Model 2017
Var2017 <- Variable[which(allData$Time >= ymd(20170101) & Variable$Time < ymd(20180101)),]

trainIndex_2017 <- createDataPartition(Var2017$WCC3_RLA, times = 1, p = 0.7, list = F)
trainVar_2017 <- Var2017[trainIndex_2017,]
testVar_2017 <- Var2017[-trainIndex_2017,]

modelVarWCC3_2017 <- caret::train(WCC3_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2017, method = "lm")
predictedVarWCC3_2017 <- predict(modelVarWCC3_2017, testVar_2017[which(names(testVar_2017) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC3_RLA")

modelVarWCC4_2017 <- caret::train(WCC4_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2017, method = "lm")
predictedVarWCC4_2017 <- predict(modelVarWCC4_2017, testVar_2017[which(names(testVar_2017) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC4_RLA")

modelVarWCC5_2017 <- caret::train(WCC5_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2017, method = "lm")
predictedVarWCC5_2017 <- predict(modelVarWCC5_2017, testVar_2017[which(names(testVar_2017) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC5_RLA")

modelVarPerformance_2017 <- data.frame(WCC3 = postResample(pred = predictedVarWCC3, obs = transformBack(testVar$WCC3_RLA, "WCC3_RLA")),
                                       WCC4 = postResample(pred = predictedVarWCC4, obs = transformBack(testVar$WCC4_RLA, "WCC4_RLA")),
                                       WCC5 = postResample(pred = predictedVarWCC5, obs = transformBack(testVar$WCC5_RLA, "WCC5_RLA")))

totalEnergyDiff_2017 <- data.frame(WCC3 = round((sum(predictedVarWCC3_2017)/sum(transformBack(testVar_2017$WCC3_RLA, "WCC3_RLA"))-1)*100,2),
                                   WCC4 = round((sum(predictedVarWCC4_2017)/sum(transformBack(testVar_2017$WCC4_RLA, "WCC4_RLA"))-1)*100,2),
                                   WCC5 = round((sum(predictedVarWCC5_2017)/sum(transformBack(testVar_2017$WCC5_RLA, "WCC5_RLA"))-1)*100,2), 
                                   row.names = "PercentTotalEnergyDiff")


modelVarPerformance_2017 <- bind_rows(modelVarPerformance_2017, totalEnergyDiff_2017)
modelVarPerformance_2017



## Model 2018
Var2018 <- Variable[which(allData$Time >= ymd(20180101) & Variable$Time < ymd(20190101)),]

trainIndex_2018 <- createDataPartition(Var2018$WCC3_RLA, times = 1, p = 0.7, list = F)
trainVar_2018 <- Var2018[trainIndex_2018,]
testVar_2018 <- Var2018[-trainIndex_2018,]

modelVarWCC3_2018 <- caret::train(WCC3_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2018, method = "lm")
predictedVarWCC3_2018 <- predict(modelVarWCC3_2018, testVar_2018[which(names(testVar_2018) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC3_RLA")

modelVarWCC4_2018 <- caret::train(WCC4_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2018, method = "lm")
predictedVarWCC4_2018 <- predict(modelVarWCC4_2018, testVar_2018[which(names(testVar_2018) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC4_RLA")

modelVarWCC5_2018 <- caret::train(WCC5_RLA ~ HVAC_GPM + WCC3_EOT + WCC3_COT + WCC4_EOT + WCC4_COT + WCC5_EOT + WCC5_COT,data = trainVar_2018, method = "lm")
predictedVarWCC5_2018 <- predict(modelVarWCC5_2018, testVar_2018[which(names(testVar_2018) %in% c("HVAC_GPM", "WCC3_EOT", "WCC3_COT", "WCC4_EOT", "WCC4_COT", "WCC5_EOT", "WCC5_COT"))]) %>% 
  transformBack("WCC5_RLA")

modelVarPerformance_2018 <- data.frame(WCC3 = postResample(pred = predictedVarWCC3, obs = transformBack(testVar$WCC3_RLA, "WCC3_RLA")),
                                       WCC4 = postResample(pred = predictedVarWCC4, obs = transformBack(testVar$WCC4_RLA, "WCC4_RLA")),
                                       WCC5 = postResample(pred = predictedVarWCC5, obs = transformBack(testVar$WCC5_RLA, "WCC5_RLA")))

totalEnergyDiff_2018 <- data.frame(WCC3 = round((sum(predictedVarWCC3_2018)/sum(transformBack(testVar_2018$WCC3_RLA, "WCC3_RLA"))-1)*100,2),
                                   WCC4 = round((sum(predictedVarWCC4_2018)/sum(transformBack(testVar_2018$WCC4_RLA, "WCC4_RLA"))-1)*100,2),
                                   WCC5 = round((sum(predictedVarWCC5_2018)/sum(transformBack(testVar_2018$WCC5_RLA, "WCC5_RLA"))-1)*100,2), 
                                   row.names = "PercentTotalEnergyDiff")


modelVarPerformance_2018 <- bind_rows(modelVarPerformance_2018, totalEnergyDiff_2018)
modelVarPerformance_2018


data.frame(Predict = predictedVarWCC4,
           Actual = transformBack(testVar$WCC4_RLA, "WCC4_RLA")) %>% 
  rowid_to_column() %>% 
  gather(key = "y", value = "z", -1) %>% 
  ggplot() +
  geom_jitter(aes(rowid, z, col= y)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_blank())

bind_rows(modelVarPerformance,
          modelVarPerformance_2015, .id = "id")



