library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(C50)
library(ROCR)
library(randomForest)

contraceptive <- read_csv("/Users/rodolfoocampo/Documents/Mineria/Datos/Examen1/cmc.csv");

names(contraceptive) <- c("wifes_age","wifes_education","husbands_education",
                          "num_children_born","wifes_religion","wifes_now_working",
                          "husbands_occupation","standard_of_living_index",
                          "media_exposure","contraceptive_method_used")

contraceptive$wifes_religion <- factor(contraceptive$wifes_religion)
contraceptive$wifes_now_working <- factor(contraceptive$wifes_now_working)
contraceptive$husbands_occupation <- factor(contraceptive$husbands_occupation)
contraceptive$media_exposure <- factor(contraceptive$media_exposure)

contraceptive$contraceptive_method_used <- ifelse(contraceptive$contraceptive_method_used == 1, 0,
                                                  ifelse(contraceptive$contraceptive_method_used == 2, 
                                                         1, 1))

contraceptive$contraceptive_method_used <- factor(contraceptive$contraceptive_method_used)
head(contraceptive)

maxs <- sapply(select(contraceptive, -wifes_religion, -wifes_now_working, -husbands_occupation,-media_exposure,-contraceptive_method_used), function(x) max(x))

mins <- sapply(select(contraceptive, -wifes_religion, -wifes_now_working, -husbands_occupation,-media_exposure,-contraceptive_method_used), function(x) min(x))

means <- sapply(select(contraceptive, -wifes_religion, -wifes_now_working, -husbands_occupation, -media_exposure,-contraceptive_method_used), function(x) mean(x))

sds <- sapply(select(contraceptive, -wifes_religion, -wifes_now_working, -husbands_occupation, -media_exposure,-contraceptive_method_used), function(x) sd(x))

medians <- sapply(select(contraceptive, -wifes_religion, -wifes_now_working, -husbands_occupation, -media_exposure,-contraceptive_method_used), function(x) median(x))

dp <- data.frame(Max = maxs, Min = mins, Mean = means, SD = sds, Median = medians)

kable(dp)


house_data <- select(house_data, -id)

unique_cat <-  sapply(select(contraceptive, -wifes_age, -wifes_education, -husbands_education, -num_children_born, -standard_of_living_index), 
                      function(x) unique(x))



ggplot(contraceptive, aes(x=contraceptive_method_used, fill=contraceptive_method_used)) +
  geom_histogram(stat="count") + labs(x="Use of Contraceptive") +
  theme_minimal() 

ggplot(contraceptive, aes(x=as.factor(wifes_religion), fill=as.factor(wifes_religion))) +
  geom_histogram(stat="count") + labs(x="Wife's religion") +
  theme_minimal() 

ggplot(contraceptive, aes(x=as.factor(wifes_now_working) , fill=as.factor(wifes_now_working) )) +
  geom_histogram(stat="count") + labs(x="Wife's now working") +
  theme_minimal() 

ggplot(contraceptive, aes(x=as.factor(contraceptive$media_exposure) , fill=as.factor(contraceptive$media_exposure) )) + labs(x="Media Exposure") +
  geom_histogram(stat="count") +
  theme_minimal() 

set.seed(139909)

train_rows <- sample(dim(contraceptive)[1], size=round(dim(contraceptive)[1]*0.75,0), replace=F)
train_set <- contraceptive[train_rows,]

test_set <-contraceptive[-train_rows,]

validation_rows <- sample(dim(train_set)[1], size=round(dim(train_set)[1]*0.0345,0), replace=F)
validation_set <- train_set[validation_rows,]
train_set <- train_set[-validation_rows,]

dim(test_set)[1]

comparison_tree <- data.frame(nodes=numeric(), auc=numeric())
for (i in 1:30){
  node_size = i
  tree_model <- C5.0(contraceptive_method_used ~., data=train_set, trial_size=10,   control=C5.0Control(minCases=i))
  predictions_test <- predict(tree_model, newdata=test_set, type="prob")
  prediction_test_obj <- prediction(predictions_test[,2], test_set$contraceptive_method_used)
  performance_test_obj <- performance(prediction_test_obj, x.measure="fpr", measure="tpr")
  auc_val <- performance(prediction_test_obj,measure="auc")
  comparison_tree[i,1] <- node_size
  comparison_tree[i,2] <- auc_val@y.values[[1]]
}


max_auc_tree <- max(comparison_tree[,2]) 
optimal_nodes_tree <- filter(comparison_tree, comparison_tree$auc==max_auc_tree)

optimal_nodes_tree
max_auc_tree

tree_model <- C5.0(contraceptive_method_used ~., data=train_set, trial_size=10, control=C5.0Control(minCases=optimal_nodes_tree))

predictions_validation <- predict(tree_model, newdata=validation_set, type="prob")
prediction_validation_obj <- prediction(predictions_validation[,2], validation_set$contraceptive_method_used)
performance_validation_obj <- performance(prediction_validation_obj, x.measure="fpr", measure="tpr")
plot(performance_validation_obj)
abline(a=0, b=1, col="red")
auc_val <- performance(prediction_validation_obj,measure="auc")
auc_val@y.values[[1]]

