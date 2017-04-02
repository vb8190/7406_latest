# This is the code I was using to find the best combination of adjacency variables

setwd("C:/Users/josep/Documents/Data Mining and Statistical Learning/Project/renthop")

renthop.comb <- read.table(file = "combined_renthop.csv", sep=",", header=TRUE)

renthop.comb$log_price <- log(renthop.comb$price)
renthop.comb$days_between <- log(renthop.comb$days_between)

adjacent.comb <- read.table(file = "renthop_adjacency_vars.csv", sep=",", header=TRUE)

adjacent.train <- adjacent.comb[!is.na(adjacent.comb$interest_code),]

var_prefix <- c('days_between_adj_','feature_count_adj_',
	'descript_len_adj_','photo_count_adj_', 'log_price_adj_',
	'bedrooms_adj_', 'bathrooms_adj_', 'interest_adj_')

for(i in 1:length(var_prefix)){
	formula_str <- "interest_code ~ "
	formula_str <- paste(formula_str, var_prefix[i],15,sep='')
	test <- lm(eval(parse(text = formula_str)), data = adjacent.train)
	print(formula_str)
	print(summary(test)$adj.r.squared)
}

var_prefix <- c('days_between_adj_', 'log_price_adj_', 'interest_adj_')

iterators <- c('i_db','i_lp','i_i')

results <- NULL

for(i_db in 1:30){
for(i_lp in 1:30){
for(i_i in 1:30){
	formula_str <- "interest_code ~ "
	for(i in 1:length(var_prefix)){
		if(i == 1){
		formula_str <- paste(formula_str, var_prefix[i],
			eval(parse(text = iterators[i])),sep='')
		}else{
		formula_str <- paste(formula_str," + ", var_prefix[i],
			eval(parse(text = iterators[i])),sep='')
		}
	}
	test <- lm(eval(parse(text = formula_str)), data = adjacent.train)
	result <- c(formula_str,summary(test)$adj.r.squared)
	results <- rbind(results, result)
}}
print(i_db)
}

results <- data.frame(results)
results2 <- results[order(-as.numeric(as.character(results$X2))),]
results2[1:10,]

#3673 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 0.110942594804427
#973  interest_code ~ days_between_adj_2 + log_price_adj_3 + interest_adj_13 0.110910698732306
#3703 interest_code ~ days_between_adj_5 + log_price_adj_4 + interest_adj_13 0.110904439049688
#1003 interest_code ~ days_between_adj_2 + log_price_adj_4 + interest_adj_13 0.110881586627471
#3674 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_14 0.110862156204504
#974  interest_code ~ days_between_adj_2 + log_price_adj_3 + interest_adj_14 0.110856376970898
#4573 interest_code ~ days_between_adj_6 + log_price_adj_3 + interest_adj_13 0.110852714860295
#2773 interest_code ~ days_between_adj_4 + log_price_adj_3 + interest_adj_13  0.11084872617546
#1873 interest_code ~ days_between_adj_3 + log_price_adj_3 + interest_adj_13 0.110847933446336
#4603 interest_code ~ days_between_adj_6 + log_price_adj_4 + interest_adj_13  0.11081948479397


var_prefix <- c('feature_count_adj_', 'photo_count_adj_', 'bathrooms_adj_')

iterators <- c('i_fc','i_pc','i_bath')

results <- NULL

for(i_fc in 1:30){
for(i_pc in 1:30){
for(i_bath in 1:30){
	formula_str <- "interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13"
	for(i in 1:length(var_prefix)){
		formula_str <- paste(formula_str," + ", var_prefix[i],
			eval(parse(text = iterators[i])),sep='')
	}
	test <- lm(eval(parse(text = formula_str)), data = adjacent.train)
	result <- c(formula_str,summary(test)$adj.r.squared)
	results <- rbind(results, result)
}}
print(i_fc)
}

results <- data.frame(results)
results2 <- results[order(-as.numeric(as.character(results$X2))),]
results2[1:10,]
                                                                                                                                   X1
#21604  interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4
#22174 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_20 + bathrooms_adj_4
#22084 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_17 + bathrooms_adj_4
#22294 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_24 + bathrooms_adj_4
#22204 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_21 + bathrooms_adj_4
#22384 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_27 + bathrooms_adj_4
#22414 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_28 + bathrooms_adj_4
#22114 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_18 + bathrooms_adj_4
#19804  interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_23 + photo_count_adj_1 + bathrooms_adj_4
#23404  interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_27 + photo_count_adj_1 + bathrooms_adj_4
#                     X2
#21604 0.111641725479828
#22174 0.111641233095243
#22084 0.111640725746275
#22294 0.111640144434387
#22204 0.111639513603565
#22384 0.111639408100889
#22414 0.111639269745378
#22114 0.111639208580439
#19804 0.111639205517928
#23404 0.111638847830924

var_prefix <- c('descript_len_adj_','bedrooms_adj_')

iterators <- c('i_dl', 'i_bed')

results <- NULL

for(i_dl in 1:30){
for(i_bed in 1:30){
	formula_str <- "interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4"
	for(i in 1:length(var_prefix)){
		formula_str <- paste(formula_str," + ", var_prefix[i],
			eval(parse(text = iterators[i])),sep='')
	}
	test <- lm(eval(parse(text = formula_str)), data = adjacent.train)
	result <- c(formula_str,summary(test)$adj.r.squared)
	results <- rbind(results, result)
}}

results <- data.frame(results)
results2 <- results[order(-as.numeric(as.character(results$X2))),]
results2[1:10,]

#303 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_11 + bedrooms_adj_3
#633 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_22 + bedrooms_adj_3
#603 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_21 + bedrooms_adj_3
#663 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_23 + bedrooms_adj_3
#363 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_13 + bedrooms_adj_3
#543 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_19 + bedrooms_adj_3
#513 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_18 + bedrooms_adj_3
#393 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_14 + bedrooms_adj_3
#153  interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_6 + bedrooms_adj_3
#333 interest_code ~ days_between_adj_5 + log_price_adj_3 + interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + bathrooms_adj_4 + descript_len_adj_12 + bedrooms_adj_3
#                   X2
#303 0.114570716750797
#633 0.114565506630566
#603 0.114565232228085
#663 0.114564263051348
#363 0.114563562562232
#543   0.1145627003336
#513 0.114561547853258
#393  0.11455860902944
#153 0.114557642704433
#333 0.114556946591941

###adjacent variable test

test <- lm(interest_code ~ days_between_adj_5 + log_price_adj_3 + 
	interest_adj_13 + feature_count_adj_25 + photo_count_adj_1 + 
	bathrooms_adj_4 + descript_len_adj_11 + bedrooms_adj_3,
	data = adjacent.train)

summary(test)




