

install.packages("pracma")  # Install the pracma package if not already installed
library(pracma)
accel_data <- read.csv("<dir>/magnetomererdatasWithDidDT.csv", sep = ',')



# Step 3: Calculate the autocorrelation
accel_y <- accel_data$y  # Assuming acceleration_y is the column of interest
autocorr <- acf(accel_y, plot = FALSE)

# Step 4: Find peaks in the autocorrelation
threshold <- 0.2
peaks <- c()
lag_values <- seq(1, length(autocorr$acf))



for (i in lag_values) {
  ret<-  max(autocorr$acf[(i+1):length(autocorr$acf)])
  print(paste(ret,autocorr$acf[i]))
  if(toString(ret) =='NA') {
     print(ret) 
    next
    }
if (autocorr$acf[i] > threshold && autocorr$acf[i] == max(autocorr$acf[(i+1):length(autocorr$acf)])) {
    
    peaks <- c(peaks, i)
  }else{
    
  }
}

num_peaks <- length(peaks)
# Step 6: Visualize the autocorrelation and detected peaks
plot(autocorr$acf, type = "h", xlab = "Lag", ylab = "Autocorrelation")
points(peaks, autocorr$acf[peaks], col = "red")
