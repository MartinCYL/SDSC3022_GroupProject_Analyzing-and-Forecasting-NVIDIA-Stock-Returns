

# Data Prepossessing
data <- read.csv('C:/Users/HUAWEI/Desktop/car data.csv')
data_clean <- na.omit(data)
set.seed(123)
index <- sample(1:nrow(data_clean), 0.8 * nrow(data_clean))
training_set <- data_clean[index, ]
test_set <- data_clean[-index, ]



#Data Prepossessing : find outliers
par(mfrow=c(2,2))
boxplot(training_set$Year, main="Boxplot for Year", ylab="Year")
boxplot(training_set$Present_Price, main="Boxplot for Present Price", ylab="Present Price")
boxplot(training_set$Kms_Driven, main="Boxplot for Kms Driven", ylab="Kms Driven")
boxplot(training_set$Selling_Price, main="Boxplot for Selling Price", ylab="Selling Price")



#Data Prepossessing : remove outliers
for(col in c("Year", "Present_Price", "Kms_Driven")){
  Q1 <- quantile(training_set[[col]], .25)
  Q3 <- quantile(training_set[[col]], .75)
  IQR <- Q3 - Q1
  upper_bound <- Q3 + 1.5 * IQR
  lower_bound <- Q1 - 1.5 * IQR
  training_set <- training_set[training_set[[col]] >= lower_bound & training_set[[col]] <= upper_bound, ]
}
Q1_sp <- quantile(training_set$Selling_Price, .25)
Q3_sp <- quantile(training_set$Selling_Price, .75)
IQR_sp <- Q3_sp - Q1_sp
upper_bound_sp <- Q3_sp + 1.5 * IQR_sp
lower_bound_sp <- Q1_sp - 1.5 * IQR_sp
training_set <- training_set[training_set$Selling_Price >= lower_bound_sp & training_set$Selling_Price <= upper_bound_sp, ]



# Remove bad leverage points
model <- lm(Selling_Price ~ Year + Present_Price + Kms_Driven, data = training_set)
standardized_resids <- rstandard(model)
training_set$standardized_resids <- standardized_resids
training_set <- training_set[abs(training_set$standardized_resids) <= 2, ]
training_set$standardized_resids <- NULL



#linear regression : simple linear regression
model_year <- lm(Selling_Price ~ Year, data = training_set)
summary(model_year)

model_pp <- lm(Selling_Price ~ Present_Price, data = training_set)
summary(model_pp)

model_kd <- lm(Selling_Price ~ Kms_Driven, data = training_set)
summary(model_kd)



#linear regression : multiple linear regression
model1 <- lm(Selling_Price ~ Present_Price + Kms_Driven, data = training_set)
summary(model1)

model2 <- lm(Selling_Price ~ Present_Price + Year, data = training_set)
summary(model2)

model <- lm(Selling_Price ~ Year + Present_Price + Kms_Driven, data = training_set)
summary(model)




#multiple linear regression : compare different models
anova(model1,model)
anova(model2,model)



#multiple linear regression : Added Variable Plots
avPlots(model)




#multiple linear regression : Calculate VIF
vif_values <- vif(model)
print(vif_values)




#test set,0.95 confidence interval and the prediction interval
predictions <- predict(model, newdata=test_set, interval="confidence")
predictions_pred <- predict(model, newdata=test_set, interval="prediction")

test_set$predicted <- predictions[, "fit"]
test_set$lower_ci <- predictions[, "lwr"]
test_set$upper_ci <- predictions[, "upr"]
test_set$lower_pi <- predictions_pred[, "lwr"]
test_set$upper_pi <- predictions_pred[, "upr"]

ggplot(test_set, aes(x=predicted, y=Selling_Price)) +
  geom_point() +
  geom_line(aes(y=lower_ci), color="blue", linetype="dashed") +
  geom_line(aes(y=upper_ci), color="blue", linetype="dashed") +
  geom_line(aes(y=lower_pi), color="red", linetype="dotted") +
  geom_line(aes(y=upper_pi), color="red", linetype="dotted") +
  labs(title="Confidence and Prediction Intervals", x="Predicted", y="Actual")

ci_within_count <- sum(test_set$Selling_Price >= test_set$lower_ci & test_set$Selling_Price <= test_set$upper_ci)
pi_within_count <- sum(test_set$Selling_Price >= test_set$lower_pi & test_set$Selling_Price <= test_set$upper_pi)
total_test_observations <- nrow(test_set)
ci_within_ratio <- ci_within_count / total_test_observations
pi_within_ratio <- pi_within_count / total_test_observations

ci_within_ratio
pi_within_ratio