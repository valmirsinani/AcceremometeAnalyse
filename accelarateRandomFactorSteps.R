
#Load and preprocess accelerometer data 
accelerometer_data <- read.csv("<dir>/magnetomererdatasWithDidDT.csv", sep = ',')

#print(range(accelerometer_data))
#print(sort(accelerometer_data$z, decreasing=TRUE))


# Preprocess accelerometer_data...

# Calculate magnitude of acceleration
acceleration_magnitude <- sqrt(accelerometer_data$x^2 + accelerometer_data$y^2 + accelerometer_data$x^2)
plot(as.ts( acceleration_magnitude))
  

# Set threshold for step detection
threshold <- 1.2 * mean(acceleration_magnitude)  # Adjust the multiplier as needed
threshold_run <- 1.5 * mean(acceleration_magnitude)  # Adjust the multiplier as needed


# Initialize step count and state variables
step_count <- 0
previous_state <- "below"
current_state <- ""


# Detect walking steps
countSteps <- function(pthreshold){
  step_count <- 0 #init value
  previous_state <- "below" #init value
  current_state <- "" #init value
  
  for (i in 2:length(acceleration_magnitude)) {
    if (acceleration_magnitude[i] > pthreshold) {      current_state <- "above"    }
    else {      current_state <- "below"    }
    
    if (previous_state == "below" && current_state == "above") {      step_count <- step_count + 1    }
    
    previous_state <- current_state
  }
  return(step_count)
}
 
walkedspaps<-countSteps(threshold)
 

# Detect runing steps # Print the total step count
runspaps<-countSteps(threshold_run)
 
# Detect stairs steps
step_count <- 0
for (i in 2:length(acceleration_magnitude)) {
  if (acceleration_magnitude[i] > threshold) {    current_state <- "above"  } 
  else {    current_state <- "below"  }
  if (previous_state == "below" && current_state == "above") {
    # Check for a large drop in acceleration indicating a stair step
    if (acceleration_magnitude[i-1] - acceleration_magnitude[i] > threshold) {step_count <- step_count + 1}
  } 
  previous_state <- current_state
}
 c<- c(walkedspaps,runspaps,step_count)
# Print the total step count
print(step_count) 
accel_features <- accel_data[, c("x", "y", "z")]

plot(as.ts( accel_features))


df <- data.frame(work=c[1],run=c[2],stairs=c[3])
plot(as.ts( df))

#View(df)
 
 
 
