library(tidyverse)
library(lubridate)
library(plotly)
library(egg)
library(caret)
library(e1071)

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
colnames(Variable) <- c("Time", "OAHum", "OADB", "OAWB", "WCC5-EIT", 
                      "WCC5-EOT", "Variable-1", 'Variable-2', "HVAC_GPM", "HVAC_CHWdP", 
                      "WCC3-RLA", "WCC4-RLA", "WCC5-RLA", "WCC5CIT", "WCC4-CIT",
                      "WCC3-CIT", "HVAC-CTHZ", "PROC-CTHZ", "PROC-CHWRT", "PROC-CHWST",
                      "HVAC-CHWRT", "HVAC-CHWST", "WCC3-EFF", "WCC4-EFF", "WCC5 EFF",
                      "Variable-3", "Variable-4", "WCC3-COT", "WCC4-COT", "WCC5-COT",
                      "WCC3-EOT", "WCC4-EOT", 'WCC3-EIT', 'WCC4-EIT', 'DailyTRH')




# Missing value 
## Explore missing value


plot_missmap <- Variable %>%
                    is.na() %>%
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


plot_missmap

## Drop DailyTRH column due to high missing value
Variable <- select(Variable, -c("DailyTRH"))
head(Variable)

## Check other variable 
missVar <- Variable %>%
              is.na() %>%
              colSums() > 0

missVar <- names(Variable[,missVar])
missVar

# View(Variable[rowSums(is.na(Variable))>0,])

## Impute 0 to missing value
### As seen in View(), row with missing value also have 0 values at least in one column"
Variable <- replace(Variable, is.na(Variable), 0)


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

OA_plot
OA_stats


## Replace outliers with Q1 and Q4 on OAWB and OADB
Variable$OADB[which(Variable$OADB < OA_stats[1,which(names(OA_stats) == 'OADB')])] <- OA_stats[1,which(names(OA_stats) == 'OADB')]
Variable$OADB[which(Variable$OADB > OA_stats[5,which(names(OA_stats) == 'OADB')])] <- OA_stats[5,which(names(OA_stats) == 'OADB')]

Variable$OAWB[which(Variable$OAWB < OA_stats[1,which(names(OA_stats) == 'OAWB')])] <- OA_stats[1,which(names(OA_stats) == 'OAWB')]
Variable$OAWB[which(Variable$OAWB > OA_stats[5,which(names(OA_stats) == 'OAWB')])] <- OA_stats[5,which(names(OA_stats) == 'OAWB')]

## Replace outliers with Q1 and Q3 on OAHum
Variable$OAHum[which(Variable$OAHum < OA_stats[1,which(names(OA_stats) == 'OAHum')])] <- OA_stats[1,which(names(OA_stats) == 'OAHum')]
Variable$OAHum[which(Variable$OAHum > OA_stats[4,which(names(OA_stats) == 'OAHum')])] <- OA_stats[4,which(names(OA_stats) == 'OAHum')]


## Explore operational outlier
### Visualize total outliers
OP_var <- names(Variable[-which(names(Variable) %in% OA_var)])
OP_var

Variable[OP_var]

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


## Normalization and standardization
### Check skewness and select normalization method

VarSkew <- data.frame(direct= abs(sapply(Variable[-1], skewness)),
           logp1= abs(sapply(log1p(Variable[-1]), skewness)),
           squared= abs(sapply((Variable[-1])**2, skewness)))

head(Variable[which(colnames(Variable) %in% rownames(VarSkew[1,]))])
head(VarSkew)

for (x in 1:nrow(VarSkew)){
  normalized_method <- which.min(VarSkew[x,])
  if (normalized_method == 2) {
    Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] <- log1p(Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))])
  }
  if (normalized_method == 3) {
    Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] <- Variable[which(colnames(Variable) %in% rownames(VarSkew[x,]))] ** 2
  }
}

data.frame(abs(sapply(Variable[-1], skewness)))

normalized_method
which.min(test[1,])
test
sapply(test[], which.min)

ggplot(Variable, aes(`WCC5-EIT`)) +
  geom_histogram() +
  stat_function()

## Standardization

