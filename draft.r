library(data.table)
library(DescTools)
library(swfscMisc)

install.packages("DescTools")

location <- 'D:\\Demyd\\Personal\\R\\kaggle\\'
filename <- 'application_test.csv'
data_all <- data.table(read.csv(paste(location, filename, sep = "")))
vars <- c('AMT_INCOME_TOTAL',	'AMT_CREDIT',	'AMT_ANNUITY',	'AMT_GOODS_PRICE',	'REGION_POPULATION_RELATIVE',	'HOUR_APPR_PROCESS_START')
data_vars <- data_all[, ..vars]

data_vars$AMT_ANNUITY[!is.na(data_vars$AMT_ANNUITY)] <- 1

gm <- function(x){  x <- x[!is.na(x)]; exp(mean(log(x)))} #https://www.r-bloggers.com/r-geometric-mean/
hm <- function(x){harmonic.mean(x, na.rm=TRUE)} #https://www.r-bloggers.com/geometric-and-harmonic-means-in-r/


mean_summary <- as.data.table(rbind(apply(data_vars, 2, mean, na.rm = TRUE), apply(data_vars, 2, gm), apply(data_vars, 2, hm)))
mean_summary <- cbind(item = c('Arithmetic Mean', 'Geometric Mean', 'Harmonic Mean'), mean_summary)
mean_summary[1, 4] <- mean(data_vars$AMT_ANNUITY[!is.na(data_vars$AMT_ANNUITY)], na.rm = TRUE)

mode_median <- as.data.table(rbind(apply(data_vars, 2, Mode, na.rm = FALSE), apply(data_vars, 2, median, na.rm = FALSE)))
mode_median <- cbind(item = c('mode', 'median'), mode_median)
mode_median[1, 4] <- median(data_vars$AMT_ANNUITY[!is.na(data_vars$AMT_ANNUITY)], na.rm = TRUE)

am_mode <- (mean_summary[1, ..vars] / mode_median[1, ..vars])
am_median <- (mean_summary[1, ..vars] / mode_median[1, ..vars])


boxplot(data_vars$AMT_CREDIT,
        main="Different boxplots per var",
        col="orange",
        border="brown"
)




median(data_vars$AMT_ANNUITY, na.rm = TRUE)

data_vars$AMT_ANNUITY[is.na(data_vars$AMT_ANNUITY)]


