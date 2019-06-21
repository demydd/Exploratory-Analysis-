library(data.table)
library(DescTools)
library(swfscMisc)

#install.packages("DescTools")

location <- 'Your file directory' # put your file directory here
filename <- 'application_test.csv'
data_all <- data.table(read.csv(paste(location, filename, sep = "")))
vars <- c('AMT_INCOME_TOTAL',	'AMT_CREDIT',	'AMT_ANNUITY',	'AMT_GOODS_PRICE',	'REGION_POPULATION_RELATIVE',	'HOUR_APPR_PROCESS_START')
data_vars <- data_all[, ..vars]

gm <- function(x){  x <- x[!is.na(x)]; exp(mean(log(x)))} #https://www.r-bloggers.com/r-geometric-mean/
hm <- function(x){harmonic.mean(x, na.rm=TRUE)} #https://www.r-bloggers.com/geometric-and-harmonic-means-in-r/

# compile mean values
mean_summary <- as.data.table(rbind(apply(data_vars, 2, mean, na.rm = TRUE), apply(data_vars, 2, gm), apply(data_vars, 2, hm)))
mean_summary <- cbind(item = c('Arithmetic Mean', 'Geometric Mean', 'Harmonic Mean'), mean_summary)
mean_summary[1, 4] <- mean(data_vars$AMT_ANNUITY[!is.na(data_vars$AMT_ANNUITY)], na.rm = TRUE)

# compile mode and median values
mode_median <- as.data.table(rbind(apply(data_vars, 2, Mode, na.rm = TRUE), apply(data_vars, 2, median, na.rm = TRUE)))
mode_median <- cbind(item = c('mode', 'median'), mode_median)
mode_median[1, 4] <- median(data_vars$AMT_ANNUITY[!is.na(data_vars$AMT_ANNUITY)], na.rm = TRUE)

# proportion of Arithmetic Mean to Mode and Median
am_mode <- (mean_summary[1, ..vars] / mode_median[1, ..vars])
am_median <- (mean_summary[1, ..vars] / mode_median[1, ..vars])

min_max <- as.data.table(rbind(apply(data_vars, 2, min, na.rm = TRUE), apply(data_vars, 2, max, na.rm = TRUE)))

# Example of quantile
quantile_step <- 0.1
quantile_start <- quantile(data_vars$AMT_INCOME_TOTAL, probs = seq(0, 1, quantile_step), na.rm = FALSE, names = TRUE)[1:(1/quantile_step)]
quantile_end <- quantile(data_vars$AMT_INCOME_TOTAL, probs = seq(0, 1, quantile_step), na.rm = FALSE, names = TRUE)[2:(length(quantile_start) + 1)]
quantile_summary <- rbind(quantile_start, quantile_end)
colnames(quantile_summary) <- paste((1:dim(quantile_summary)[2])*10, '%', sep ="")

#Example of Box plot
boxplot(data_vars$AMT_CREDIT,
        main="Different boxplots per var",
        col="orange",
        border="brown"
)
dev.off()

#variance
qty <- dim(data_vars)[1]
range <- min_max[2] - min_max[1]
d <- apply(as.data.table(vars), 1, function(x){sum(abs(data_vars[as.vector(!is.na(data_vars[, ..x])), ..x] - unlist(mean_summary[1,..x]))) / sum(!is.na(data_vars[, ..x]))})
stanDev <- apply(as.data.table(vars), 1, function(x){sd(unlist(data_vars[as.vector(!is.na(data_vars[, ..x])), ..x]))})

Oscillator_ratio <- range/mean_summary[1, ..vars] * 100
Linear_variance_ratio <- d/mean_summary[1, ..vars] * 100
Variance_ration <- stanDev/mean_summary[1, ..vars] * 100


