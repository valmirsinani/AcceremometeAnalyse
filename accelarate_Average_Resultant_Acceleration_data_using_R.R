

data <-  read.csv("<dir>T/magnetomererdatasWithDidDT.csv", sep = ',')


# Calculate the magnitude of acceleration for each data point
data$acceleration <- sqrt(data$x^2 + data$y^2 + data$z^2)

average_resultant_acceleration <- mean(data$acceleration)

print(average_resultant_acceleration)




