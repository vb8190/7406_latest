library(nnet)

setwd("C:/Users/josep/Documents/Data Mining and Statistical Learning/Project/renthop")

rh_data.train <- read.table(
	file = "Final_Project_Variables - train.csv", sep=",", header=TRUE)

### drop observation, listing_id, adjacency variables unused for regression
rh_reg.train <- rh_data.train[,3:560]


### multiple linear regression to test for linear relationship
test <- lm(as.formula(rh_reg.train), data = rh_reg.train)
summary(test)

# Residual standard error: 0.527 on 48794 degrees of freedom
# Multiple R-squared:  0.2994,    Adjusted R-squared:  0.2914 
# F-statistic: 37.43 on 557 and 48794 DF,  p-value: < 2.2e-16

# get predictions
test.pred <- ifelse(test$fitted.values < 1,1,
	ifelse(test$fitted.values > 3, 3, round(test$fitted.values)))

mean(test.pred != rh_reg.train$interest_code)

# [1] 0.2989342

# original data comparison

original <- lm(interest_code ~ log_price_sq + log_price + bedrooms + bathrooms,
	data = rh_reg.train)
summary(original)

# get predictions
original.pred <- ifelse(original$fitted.values < 1,1,
	ifelse(original$fitted.values > 3, 3, round(original$fitted.values)))

# Residual standard error: 0.5865 on 49347 degrees of freedom
# Multiple R-squared:  0.1224,    Adjusted R-squared:  0.1223 
# F-statistic:  1721 on 4 and 49347 DF,  p-value: < 2.2e-16

mean(original.pred != rh_reg.train$interest_code)

# [1] 0.3637948

# Start trimming variables by getting rid of interactions with p-values > 0.1
# This is not ideal, but there are so many variables, R chokes running 
# stepwise. Stepwise is also not ideal, but other methods would tax R even 
# more. I may try some other methods when I convert to Python

interest_code <- rh_reg.train[,1]
Xs <- rh_reg.train[,2:558]

# dropping the p-value for the intercept 
p_vals <- coef(summary(test))[-1,4]

# ensuring non-interacting variables stay
p_vals[1:47] <- p_vals[1:47]/10

# dropping interaction variables with significance > 0.1
rh_reduce.train <- cbind(interest_code,Xs[,p_vals <= 0.1])
dim(rh_reduce.train)
# [1] 49352   255

test <- lm(as.formula(rh_reduce.train), data = rh_reduce.train)
summary(test)

# Residual standard error: 0.5276 on 49097 degrees of freedom
# Multiple R-squared:  0.2933,    Adjusted R-squared:  0.2897 
# F-statistic: 80.23 on 254 and 49097 DF,  p-value: < 2.2e-16

# get predictions
test.pred <- ifelse(test$fitted.values < 1,1,
	ifelse(test$fitted.values > 3, 3, round(test$fitted.values)))

mean(test.pred != rh_reduce.train$interest_code)

# [1] 0.302318

###Poisson Regression
fit.PR <- multinom(as.formula(rh_reduce.train),maxit = 1000,data = rh_reduce.train)

# iter 760 value 30283.034847
# final  value 30283.034847 

pred.PR <- predict(fit.PR,rh_reduce.train)
mean(pred.PR != rh_reduce.train$interest_code)

# [1] 0.2724104

# original data comparison

original.PR <- multinom(interest_code ~ log_price_sq + log_price + 
	bedrooms + bathrooms,maxit = 1000,data = rh_reduce.train)

predO.PR <- predict(original.PR,rh_reduce.train)
mean(predO.PR != rh_reduce.train$interest_code)

# [1] 0.3021154

# reduce by stepwise
StepwiseAIC.lm <- step(test, trace = 0)
summary(StepwiseAIC.lm)

# Residual standard error: 0.5276 on 49115 degrees of freedom
# Multiple R-squared:  0.2931,    Adjusted R-squared:  0.2897 
# F-statistic: 86.31 on 236 and 49115 DF,  p-value: < 2.2e-16

summary(StepwiseAIC.lm)$call

reg_formula <- interest_code ~ days_between_adj_5 + log_price_adj_3 + 
    interest_adj_13 + bathrooms_adj_4 + descript_len_adj_11 + 
    bedrooms_adj_3 + feature_count + descript_len + photo_count + 
    bedrooms + bathrooms + log_price + log_price_sq + log_price__interest_adj_13 + 
    website + one + two + three + four + five + six + seven + 
    Doorman + Dogs_Allowed + Cats_Allowed + Pets_Allowed + Balcony + 
    Outdoors + Internet_Mention + Loft + New + No_fee + Gym + 
    Storage + Sunlight + Pool + Laundry_Room + Wash_Dry + Prewar + 
    AirCon + days_between_adj_5__website + days_between_adj_5__one + 
    days_between_adj_5__two + days_between_adj_5__four + days_between_adj_5__seven + 
    days_between_adj_5__Outdoors + days_between_adj_5__Internet_Mention + 
    days_between_adj_5__Gym + days_between_adj_5__Pool + days_between_adj_5__Dishwasher + 
    days_between_adj_5__Prewar + log_price_adj_3__one + log_price_adj_3__six + 
    log_price_adj_3__seven + log_price_adj_3__Doorman + log_price_adj_3__Cats_Allowed + 
    log_price_adj_3__Pets_Allowed + log_price_adj_3__Hardwood + 
    log_price_adj_3__No_fee + log_price_adj_3__Storage + log_price_adj_3__Pool + 
    log_price_adj_3__Laundry_Room + log_price_adj_3__Wash_Dry + 
    log_price_adj_3__Prewar + interest_adj_13__website + interest_adj_13__two + 
    interest_adj_13__three + interest_adj_13__Doorman + interest_adj_13__Hardwood + 
    interest_adj_13__Internet_Mention + interest_adj_13__No_fee + 
    interest_adj_13__Sunlight + interest_adj_13__Laundry_Room + 
    interest_adj_13__Dishwasher + interest_adj_13__Prewar + feature_count_adj_25__one + 
    feature_count_adj_25__three + feature_count_adj_25__five + 
    feature_count_adj_25__six + feature_count_adj_25__Dogs_Allowed + 
    feature_count_adj_25__Outdoors + feature_count_adj_25__Internet_Mention + 
    feature_count_adj_25__No_fee + feature_count_adj_25__Gym + 
    feature_count_adj_25__Storage + feature_count_adj_25__Sunlight + 
    bathrooms_adj_4__one + bathrooms_adj_4__six + bathrooms_adj_4__Doorman + 
    bathrooms_adj_4__Outdoors + bathrooms_adj_4__Gym + bathrooms_adj_4__Sunlight + 
    bathrooms_adj_4__Laundry_Room + descript_len_adj_11__one + 
    descript_len_adj_11__five + descript_len_adj_11__Cats_Allowed + 
    descript_len_adj_11__Elevator + descript_len_adj_11__Pool + 
    descript_len_adj_11__Laundry_Room + descript_len_adj_11__Wash_Dry + 
    descript_len_adj_11__Dishwasher + descript_len_adj_11__Prewar + 
    bedrooms_adj_3__one + bedrooms_adj_3__three + bedrooms_adj_3__six + 
    bedrooms_adj_3__seven + bedrooms_adj_3__Doorman + bedrooms_adj_3__Cats_Allowed + 
    bedrooms_adj_3__Pets_Allowed + bedrooms_adj_3__No_fee + bedrooms_adj_3__Gym + 
    bedrooms_adj_3__Sunlight + bedrooms_adj_3__Pool + bedrooms_adj_3__Laundry_Room + 
    bedrooms_adj_3__Wash_Dry + bedrooms_adj_3__Prewar + mgmt_list_count__website + 
    mgmt_list_count__Cats_Allowed + mgmt_list_count__Pets_Allowed + 
    mgmt_list_count__Elevator + mgmt_list_count__Internet_Mention + 
    mgmt_list_count__New + mgmt_list_count__Gym + mgmt_list_count__Storage + 
    mgmt_list_count__Sunlight + mgmt_list_count__Dishwasher + 
    mgmt_list_count__Prewar + feature_count__website + feature_count__one + 
    feature_count__two + feature_count__three + feature_count__four + 
    feature_count__five + feature_count__six + feature_count__seven + 
    feature_count__Doorman + feature_count__Cats_Allowed + feature_count__Loft + 
    feature_count__No_fee + feature_count__Pool + feature_count__Wash_Dry + 
    descript_len__Doorman + descript_len__New + descript_len__No_fee + 
    descript_len__Laundry_Room + descript_len__Dishwasher + descript_len__Prewar + 
    photo_count__website + photo_count__one + photo_count__two + 
    photo_count__three + photo_count__four + photo_count__five + 
    photo_count__six + photo_count__Internet_Mention + photo_count__No_fee + 
    photo_count__Sunlight + photo_count__Laundry_Room + photo_count__Dishwasher + 
    bedrooms__website + bedrooms__two + bedrooms__three + bedrooms__four + 
    bedrooms__six + bedrooms__Doorman + bedrooms__Cats_Allowed + 
    bedrooms__Pets_Allowed + bedrooms__Elevator + bedrooms__Outdoors + 
    bedrooms__No_fee + bedrooms__Laundry_Room + bedrooms__Dishwasher + 
    bedrooms__Prewar + bathrooms__two + bathrooms__three + bathrooms__four + 
    bathrooms__five + bathrooms__six + bathrooms__seven + bathrooms__Balcony + 
    bathrooms__No_fee + log_price__website + log_price__one + 
    log_price__three + log_price__four + log_price__five + log_price__six + 
    log_price__seven + log_price__Doorman + log_price__Dogs_Allowed + 
    log_price__Cats_Allowed + log_price__Pets_Allowed + log_price__Balcony + 
    log_price__Hardwood + log_price__Internet_Mention + log_price__New + 
    log_price__No_fee + log_price__Laundry_Room + log_price__Prewar + 
    log_price__AirCon + log_price_sq__website + log_price_sq__one + 
    log_price_sq__three + log_price_sq__four + log_price_sq__five + 
    log_price_sq__six + log_price_sq__seven + log_price_sq__Doorman + 
    log_price_sq__Cats_Allowed + log_price_sq__Pets_Allowed + 
    log_price_sq__Balcony + log_price_sq__Internet_Mention + 
    log_price_sq__New + log_price_sq__No_fee + log_price_sq__Laundry_Room + 
    log_price_sq__Prewar + log_price_sq__AirCon + log_price__interest_adj_13__website + 
    log_price__interest_adj_13__two + log_price__interest_adj_13__three + 
    log_price__interest_adj_13__four + log_price__interest_adj_13__six + 
    log_price__interest_adj_13__Doorman + log_price__interest_adj_13__Hardwood + 
    log_price__interest_adj_13__Internet_Mention + log_price__interest_adj_13__No_fee + 
    log_price__interest_adj_13__Sunlight + log_price__interest_adj_13__Laundry_Room + 
    log_price__interest_adj_13__Dishwasher + log_days_between__two + 
    log_days_between__three + log_days_between__four + log_days_between__seven + 
    log_days_between__No_fee + log_days_between__Laundry_Room + 
    log_days_between__Prewar

###Poisson Regression
fit.PR <- multinom(reg_formula,maxit = 1000,data = rh_reduce.train)

# iter 690 value 30308.267995
# final  value 30308.220055

pred.PR <- predict(fit.PR,rh_reduce.train)
mean(pred.PR != rh_reduce.train$interest_code)

# [1] 0.2721065 vs previous [1] 0.2724104 vs original [1] 0.3021154

###k-Fold Cross Validation###

###loop begin###

# add your data's designation inside the quotes
data_string <- "rh_reduce.train"
k <- 10
n <- dim(eval(parse(text = data_string)))[1]

randpick <- sample(1:n,n,replace = FALSE)

TestSize <- n/k
remainder <- 0

Err.PR <- NULL
LogLoss.PR <- NULL
O_Err.PR <- NULL
O_LogLoss.PR <- NULL

for (i in 1:k){
	FoldSize <- floor(TestSize)
	CVbegin <- ((i-1)*FoldSize) + 1 + floor(remainder)
	remainder <- remainder + TestSize - FoldSize
	CVend <- i*FoldSize + floor(remainder)
	CV.test <- eval(parse(text = data_string))[randpick[CVbegin:CVend],] 
	CV.train <- eval(parse(text = data_string))[-randpick[CVbegin:CVend],] 

###Poisson Regression
fit.PR <- multinom(reg_formula,maxit = 1000,data = CV.train)

### predict categories
pred.PR <- predict(fit.PR,CV.test)
Iter_Err <- mean(pred.PR != CV.test$interest_code)

Err.PR[i] <- Iter_Err 

### predict probabilities
pred_prob.PR <- predict(fit.PR,CV.test,type = "probs")

Iter_Loss <- (ifelse(CV.test$interest_code == 1,log(pred_prob.PR[,1]),
	ifelse(CV.test$interest_code == 2, log(pred_prob.PR[,2]),
	log(pred_prob.PR[,3]))))

LogLoss.PR[i] <- -mean(Iter_Loss) 

# original data comparison

original.PR <- multinom(interest_code ~ log_price_sq + log_price + 
	bedrooms + bathrooms,maxit = 1000,data = CV.train)

### predict categories
predO.PR <- predict(original.PR,CV.test)
O_Iter_Err <- mean(predO.PR != CV.test$interest_code)


O_Err.PR[i] <- O_Iter_Err 

### predict probabilities
predO_prob.PR <- predict(original.PR,CV.test,type = "probs")

O_Iter_Loss <- (ifelse(CV.test$interest_code == 1,log(predO_prob.PR[,1]),
	ifelse(CV.test$interest_code == 2, log(predO_prob.PR[,2]),
	log(predO_prob.PR[,3]))))

O_LogLoss.PR[i] <- -mean(O_Iter_Loss) 


print("Interation")
print(i)
print("Interation Err")
print(Iter_Err)
print("Interation Log Loss")
print(-mean(Iter_Loss))

}

###loop end###

mean(Err.PR)

#[1] 0.2765293 

mean(LogLoss.PR)

# [1] 0.626938

# vs Original data results below

mean(O_Err.PR)

#[1] 0.3022228

mean(O_LogLoss.PR)

[1] 0.7219528

# Write regression variables to file

formula_str <- as.character(reg_formula)

column_names <- unlist(strsplit(formula_str[3],"[ + ]"))
column_names <- column_names[column_names != '' & column_names != '\n']

rh_data.comb <- read.table(
	file = "Final_Project_Variables - combined.csv", sep=",", header=TRUE)
rh_data.out <- rh_data.comb[,1:3]

for (i in 1:length(column_names)){
	rh_data.out <- cbind(rh_data.out,
	rh_data.comb[,which(colnames(rh_data.comb) == column_names[i])])
}

colnames(rh_data.out)[4:239] <- column_names

write.table(rh_data.out,file = "FP_Regression_Variables - combined.csv", sep=",", 
	row.names = FALSE, col.names = TRUE)


