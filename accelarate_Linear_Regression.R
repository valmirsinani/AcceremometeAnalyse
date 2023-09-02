


# R program to illustrate
# Linear Regression

# Height vector
x <- c(153, 169, 140, 186, 128,
       136, 178, 163, 152, 133)


# Weight vector
y <- c(64, 81, 58, 91, 47, 57,
       75, 72, 62, 49)


# Create a linear regression model
model <- lm(y~x)

print(model)

df <- data.frame(x = 182)
res <- predict(model, df)
cat("\nPredicted value of a person
            with height = 182")

print(res)

# Plot
plot(x, y, main = "Height vs Weight
                Regression model")


abline(lm(y~x))

