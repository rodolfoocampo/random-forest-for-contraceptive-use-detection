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

comparison_rf <- data.frame(nodes=numeric(), auc=numeric())
for (i in 1:30){
  node_size = i
  rf_model <- randomForest(contraceptive_method_used ~., data=train_set,ntree=100, nodesize=node_size,   importance=T)
  rf_prediction <- predict(rf_model, newdata=test_set, type="prob")
  rf_prediction_test_obj <- prediction(rf_prediction[,2], test_set$contraceptive_method_used)
  rf_performance_test_obj <- performance(rf_prediction_test_obj, x.measure="fpr",  measure="tpr")
  auc_val <- performance(rf_prediction_test_obj,measure="auc")
  auc_val@y.values[[1]]
  comparison_rf[i,1] <- node_size
  comparison_rf[i,2] <- auc_val@y.values[[1]]
}


max_auc_rf <- max(comparison_rf[,2]) 
optimal_point_rf <- filter(comparison_rf, comparison_rf$auc==max_auc_rf)
optimal_nodes_rf <- optimal_point_rf[,1]
rf_model <- randomForest(contraceptive_method_used ~., data=train_set,ntree=100, nodesize=optimal_nodes_rf, importance=T)


rf_prediction <- predict(rf_model, newdata=test_set, type="prob")
rf_prediction_test_obj <- prediction(rf_prediction[,2], test_set$contraceptive_method_used)

rf_performance_test_obj <- performance(rf_prediction_test_obj, x.measure="fpr", measure="tpr")
plot(rf_performance_test_obj)
abline(a=0, b=1, col="red")
auc_val <- performance(rf_prediction_test_obj,measure="auc")
auc_val@y.values[[1]]
train_set2 <- select(train_set, -wifes_now_working, -wifes_religion, -husbands_occupation)

val_set2 <- select(validation_set, -wifes_now_working, -wifes_religion, -husbands_occupation)

test_set2 <- select(test_set, -wifes_now_working, -wifes_religion, -husbands_occupation)


comparison_rf2 <- data.frame(nodes=numeric(), auc=numeric())
for (i in 1:30){
  node_size = i
  rf_model2 <- randomForest(contraceptive_method_used ~., data=train_set2,ntree=100, nodesize=node_size, importance=T)
  
  
  rf_prediction2 <- predict(rf_model2, newdata=test_set2, type="prob")
  rf_prediction_test_obj2 <- prediction(rf_prediction2[,2], test_set2$contraceptive_method_used)
  
  rf_performance_test_obj2 <- performance(rf_prediction_test_obj2, x.measure="fpr", measure="tpr")
  
  auc_val_rf2 <- performance(rf_prediction_test_obj2,measure="auc")
  auc_val_rf2@y.values[[1]]
  comparison_rf2[i,1] <- node_size
  comparison_rf2[i,2] <- auc_val_rf2@y.values[[1]]
}

max_auc_rf2 <- max(comparison_rf2[,2]) 
optimal_point_rf2 <- filter(comparison_rf2, comparison_rf2$auc==max_auc_rf2)
optimal_nodes_rf2 <- optimal_point_rf2[,1]

rf_model2 <- randomForest(contraceptive_method_used ~., data=train_set2,ntree=100, nodesize=optimal_nodes_rf2, importance=T)
varImpPlot(rf_model2)

rf_prediction2 <- predict(rf_model2, newdata=test_set2, type="prob")
rf_prediction_test_obj2 <- prediction(rf_prediction2[,2], test_set$contraceptive_method_used)

rf_performance_test_obj2 <- performance(rf_prediction_test_obj2, x.measure="fpr", measure="tpr")
plot(rf_performance_test_obj2)
abline(a=0, b=1, col="red")
auc_val_rf2 <- performance(rf_prediction_test_obj2,measure="auc")
auc_val_rf2@y.values[[1]]
comparison_tree2 <- data.frame(nodes=numeric(), auc=numeric())
for (i in 1:30){
  node_size = i
  tree_model2 <- C5.0(contraceptive_method_used ~., data=train_set2, trial_size=10, control=C5.0Control(minCases=i))
  predictions_test2 <- predict(tree_model2, newdata=test_set2, type="prob")
  prediction_test_obj2 <- prediction(predictions_test2[,2], test_set2$contraceptive_method_used)
  performance_test_obj2 <- performance(prediction_test_obj2, x.measure="fpr", measure="tpr")
  auc_val2 <- performance(prediction_test_obj2,measure="auc")
  auc_val2@y.values[[1]]
  comparison_tree2[i,1] <- node_size
  comparison_tree2[i,2] <- auc_val2@y.values[[1]]
}


max_auc_tree2 <- max(comparison_tree2[,2]) 
optimal_nodes_tree2 <- filter(comparison_tree2, comparison_tree2$auc==max_auc_tree2)

max_auc_tree2
optimal_nodes_tree2
comparison_rf3 <- data.frame(nodes=numeric(), trees= numeric(), auc=numeric())
for (i in 5:35){
  node_size = i
  for(j in 80:120){
    num_tress = j
    rf_model3 <- randomForest(contraceptive_method_used ~., data=train_set2,ntree=num_tress, nodesize=node_size, importance=T)
    rf_prediction3 <- predict(rf_model3, newdata=test_set2, type="prob")
    rf_prediction_test_obj3 <- prediction(rf_prediction3[,2], test_set2$contraceptive_method_used)
    
    rf_performance_test_obj3 <- performance(rf_prediction_test_obj3, x.measure="fpr", measure="tpr")
    
    auc_val_rf3 <- performance(rf_prediction_test_obj3,measure="auc")
    vector <- c(nodes=node_size, tress=num_tress, auc=auc_val_rf3@y.values[[1]])
    comparison_rf3 <- rbind(comparison_rf3, vector)
    
  }
}
comparison_rf3 <- data.frame(comparison_rf3)
names(comparison_rf3) <- c("nodes","trees","auc")
max_auc_rf3 <- max(comparison_rf3[,3]) 
optimal_point_rf3 <- filter(comparison_rf3, comparison_rf3$auc==max_auc_rf3)
optimal_nodes_rf3 <- optimal_point_rf3[,1]
optimal_trees_rf3 <- optimal_point_rf3[,2]
rf_model3 <- randomForest(contraceptive_method_used ~., data=train_set2,ntree=optimal_trees_rf3, nodesize=optimal_nodes_rf3, importance=T)
varImpPlot(rf_model3)

rf_prediction3 <- predict(rf_model3, newdata=test_set2, type="prob")
rf_prediction_test_obj3 <- prediction(rf_prediction3[,2], test_set2$contraceptive_method_used)

rf_performance_test_obj3 <- performance(rf_prediction_test_obj3, x.measure="fpr", measure="tpr")
plot(rf_performance_test_obj3)
abline(a=0, b=1, col="red")
auc_val_rf3 <- performance(rf_prediction_test_obj3,measure="auc")
auc_val_rf3@y.values[[1]]
perf_tn_fn <- performance(rf_prediction_test_obj3, measure="tnr", x.measure = "fnr")

roc_table <- data.frame(cutoff=rf_performance_test_obj3@alpha.values[[1]],
                        tp=rf_performance_test_obj3@y.values[[1]],
                        fp=rf_performance_test_obj3@x.values[[1]],
                        tn=perf_tn_fn@y.values[[1]],
                        fn=perf_tn_fn@x.values[[1]])

negatives <- filter(test_set2, contraceptive_method_used==0) %>% nrow()
positives <- filter(test_set2, contraceptive_method_used==1) %>% nrow()

costs <- data.frame(cutoff=roc_table$cutoff, cost = (roc_table$fp*100*negatives + roc_table$fn*60*positives) )
savings <-  data.frame(cutoff=roc_table$cutoff, saved = (roc_table$tp*20*positives + roc_table$tn*10*negatives) )


total <- data.frame(cost=costs, saved = savings)

total <- filter(total, total$cost.cost<10000)

net_profit <- data.frame( net_profit = total[,4] - total[,2], cutoff = total$cost.cutoff)

max_profit <- max(net_profit$net_profit)


optimal_cutoff <- filter(net_profit, net_profit$net_profit == max_profit )
names(optimal_cutoff) <- c("Maximum Profit", "Corresponding Cutoff")
kable(optimal_cutoff)

final_numbers <- filter(total, total$cost.cutoff == optimal_cutoff[,2])
kable(final_numbers)

final_error <- filter(roc_table, roc_table$cutoff == optimal_cutoff[,2])
kable(final_error)