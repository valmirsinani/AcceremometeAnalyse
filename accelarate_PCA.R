# Load required packages
library(stats)
library(ggplot2)

# Load the accelerometer data
data <-  read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',' )#,nrows=2000)


# Data preprocessing
# Remove missing values
data <- na.omit(data)

# Extract the accelerometer readings (assuming X, Y, Z axes are columns in the dataset)
accel_data <- data[, c("x", "y", "z")]

# Standardize the variables
accel_scaled <- scale(accel_data)

# Perform PCA
pca_result <- princomp(accel_scaled, cor = TRUE)

# Explained variance ratio
variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Scree plot
scree_plot <- ggplot(data = data.frame(PC = 1:length(variance_ratio), Variance = variance_ratio), aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Principal Component", y = "Variance Explained") +
  ggtitle("Scree Plot") +
  theme_minimal()

plot(scree_plot)


a <- "17:24:00"

b <- strptime(a, format = "%H:%M:%S")
library(lubridate)
hour(b)
minute(b) 

