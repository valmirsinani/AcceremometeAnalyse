
#six basic features(1.Average, 2.Standard Deviation, 3.Average Absolute Difference, 4.Average Resultant Acceleration, 5.Time Between Peaks, 6.Binned Distribution) )

print("-------Calculate standard deviation----------")
# Calculate standard deviation
acceleration <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

data <- acceleration[, c("x", "y", "z")]

standard_deviation_x <- sd(acceleration$x)
standard_deviation_y <- sd(acceleration$y)
standard_deviation_z <- sd(acceleration$z)
 par(mfrow = c(3, 4)) # Divide the plot area into a 2x3 gri
cat('sd_x',standard_deviation_x)
plot(standard_deviation_x, main = "standard_deviation_x", xlab = "Data", ylab = "standard_deviation_x")
cat('sd_y',standard_deviation_y)
plot(standard_deviation_y, main = "standard_deviation_y", xlab = "Data", ylab = "standard_deviation_x")
cat('sd_z',standard_deviation_z)
plot(standard_deviation_z, main = "standard_deviation_z", xlab = "Data", ylab = "standard_deviation_z")

print("-------Calculate the average----------")
# Calculate the average
average_x <- mean(data$x)
average_y <- mean(data$y)
average_z <- mean(data$z)

 
cat('av_x',average_x)
 plot(average_x, main = "average_x", xlab = "Data", ylab = "average_x")
cat('av_y',average_y)
plot(average_y, main = "average_y", xlab = "Data", ylab = "average_y")
cat('av_z',average_z)
plot(average_z, main = "average_z", xlab = "Data", ylab = "average_z")

print("-------Average Absolute Difference----------")
#Average Absolute Difference

abs_diff_x <- abs(data$x - average_x)
abs_diff_y <- abs(data$y - average_y)
abs_diff_z <- abs(data$z - average_z)

cat('aad_x', mean(abs_diff_x))
plot(abs_diff_x, main = "abs_diff_x", xlab = "Data", ylab = "abs_diff_x")
cat('aad_y', mean(abs_diff_y))
plot(abs_diff_y, main = "abs_diff_y", xlab = "Data", ylab = "abs_diff_y")
cat('aad_z', mean(abs_diff_z))
plot(abs_diff_z, main = "abs_diff_z", xlab = "Data", ylab = "abs_diff_z")
print("-------Average Resultant Acceleration----------")
#Average Resultant Acceleration

resultant_acceleration <- sqrt(acceleration$x^2 + acceleration$y^2 + acceleration$z^2)
avg_resultant_acceleration <- mean(resultant_acceleration)
cat('ara=',avg_resultant_acceleration)
plot(avg_resultant_acceleration, main = "avg_resultant_acceleration", xlab = "Data", ylab = "avg_resultant_acceleration")

print("-------Time Between Peaks----------")
#Time Between Peaks
 
library('lubridate')
op <- options(digits.secs=0)
  
datetime_dt <- as.POSIXct(acceleration$dt, format = "%Y-%m-%dT%H:%M:%OS")
datetime_dt_format <-minute(datetime_dt) #paste(hour(datetime_dt), minute(datetime_dt),substr(second(datetime_dt), 1, 2) ,sep="")
 

peaks_x <- diff(sign(diff(acceleration$x, na.pad = FALSE))) < 0
peaks_y <- diff(sign(diff(acceleration$y, na.pad = FALSE))) < 0
peaks_z <- diff(sign(diff(acceleration$z, na.pad = FALSE))) < 0

peak_timestamps <- datetime_dt_format[c(peaks_x, peaks_y, peaks_z)]


time_between_peaks <- diff(peak_timestamps)
plot(time_between_peaks, main = "time_between_peaks", xlab = "Data", ylab = "time_between_peaks")

# Sample acceleration data
acceleration_x <- as.numeric(data$x)
acceleration_y <- as.numeric(data$y)
acceleration_z <- as.numeric(data$z)

# Combine all axes into a single vector
all_accelerations <- c(acceleration_x, acceleration_y, acceleration_z)
 
# Create a histogram with specified number of bins
num_bins <- 10
histogram <- hist(all_accelerations, breaks = num_bins, plot = FALSE)

# Print the histogram information
plot(histogram, main = "histogram", xlab = "Data", ylab = "histogram")


