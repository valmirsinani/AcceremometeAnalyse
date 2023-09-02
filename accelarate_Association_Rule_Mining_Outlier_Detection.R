

accel_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

data <- accel_data[, c("x", "y", "z")]

 

boxplot(data)


# R program to illustrate
# Linear Regression

accel_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')


# Height vector
x <- accel_data$x

# Weight vector
y <-accel_data$y
 par(mfrow = c(1, 1)) # Divide the plot area into a 2x3 gri

# Create a linear regression model
model <- lm(y~x)
 

# Find the weight of a person
# With height 182
df <- data.frame(x = 182)
res <- predict(model, df)
cat("\nPredicted value of a person
            with height = 182")
 
 

# Plot
plot(x, y, main = "Height vs Weight
                Regression model")
 
 