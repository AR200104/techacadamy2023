install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("ggpolypath")
install.packages("caret")
install.packages("tree")
install.packages("randomForest")
install.packages("umap")
install.packages("xgboost")

library(dplyr)
library(readxl)
library(ggplot2)
library(maps)
library(ggpolypath)
library(caret)
library(tree)
library(randomForest)
library(umap)
library(xgboost)

class <-read.csv("data/class.csv")
index <- read.csv("data/index.csv")
zoo1 <- read_excel("data/zoo1.xlsx")
zoo2 <- read.csv("data/zoo2.csv")
zoo3 <- read.csv("data/zoo3.csv")


#---------------DATA VISUALIZATION---------------#


#Value of fish species per country

fish <- select(index, c("IUCN", "Species", "Country", "Value")) %>%
  filter(Species == "Fish", IUCN == "TOT_KNOWN") %>%
  select(Country, Value)

fish_plot <- ggplot(data = fish, aes(x = Country, y = Value)) + 
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Value") +
  ggtitle("Value of fish species") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)))

print(fish_plot)


#Value of endangered species in Germany

endangered_ger <- select(index, c("IUCN", "Species", "COU", "Value")) %>%
  filter(IUCN=="VULNERABLE", COU == "DEU") %>%
  select(Species, Value)

endangered_ger_plot <- ggplot(data = endangered_ger, aes(x = Species, y = Value)) +
  geom_bar(stat = "identity") +
  xlab("Species") + 
  ylab("Value") +
  ggtitle("Value of endangered species in germany") + 
  theme(axis.title.x = element_text(angle = 0, hjust = 1))

print(endangered_ger_plot)  


#Number of endangered species in Germany without invertebrates

endangered_ger_wo_invertebrates <- slice_head(endangered_ger,n = 5)

endangered_ger_wo_invertebrates_plot <- ggplot(data = endangered_ger_wo_invertebrates, 
                                               aes(x = Species, y = Value))+
  geom_bar(stat = "identity") +
  xlab("Species") + 
  ylab("Value") +
  ggtitle("Value of endangered species without invertebrates in germany") + 
  theme(axis.title.x = element_text(angle = 0, hjust = 1))

print(endangered_ger_wo_invertebrates_plot)


#---------------MERGING DATA---------------#


zoo1_temp <- zoo1
not_change <- c("animal_name", "legs", "class_type") # these columns should not be changed

for (i in 1:ncol(zoo1_temp)) {
  if (names(zoo1_temp)[i] %in% not_change){
    next
  }
  else {
    zoo1_temp[i] <- lapply(zoo1_temp[i], as.logical)
    class(zoo1_temp[i])
  }
}

zoo2_temp <- zoo2

for (i in 1:ncol(zoo2_temp)) {
  if (names(zoo2_temp)[i] %in% not_change){
    next
  }
  else {
    zoo2_temp[i] <- lapply(zoo2_temp[i], as.logical)
    class(zoo2_temp[i])
  }
}

zoo3_temp <- zoo3

for (i in 1:ncol(zoo3_temp)) {
  if (names(zoo3_temp)[i] %in% not_change){
    next
  }
  else {
    zoo3_temp[i] <- lapply(zoo3_temp[i], as.logical)
    class(zoo3_temp[i])
  }
}

zoo_complete <- bind_rows(zoo1_temp, zoo2_temp, zoo3_temp)


# select necessary columns
class_selection <- select(class, c("Class_Number", "Class_Type")) %>%
  rename(class_type = Class_Number)
zoo_dataset <- inner_join(zoo_complete, class_selection, by = "class_type")



world_map <- map_data("world")
# get critical endangered mammals
endangered_world <- select(index, "IUCN", "SPEC", "Country", "Value") %>%
  filter(IUCN == "CRITICAL", SPEC == "MAMMAL") %>%
  select(Country, Value)

#change country names so they will be displayed
endangered_world[19, "Country"] <- "Slovakia"
endangered_world[23, "Country"] <- "Turkey"
endangered_world[24, "Country"] <- "UK"

#rename column for join
colnames(endangered_world)[1] <- "region"

# merge data

#endangered_world_map <- merge(world_map, endangered_world, by.x = "region", by.y = "Country", all.x = TRUE) OLD

endangered_world_map <- inner_join(world_map, endangered_world, by = "region")

ggplot() +
  geom_polygon(data = endangered_world_map, aes(x = long, y = lat, group = group, fill = Value)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  geom_path(data = world_map, aes(x = long, y = lat, group = group), color = "darkgrey", linewidth = 0.1) +
  theme(panel.background = element_rect(fill="white")) + 
  labs(fill = "Number of \nendangered mammals:") +
  coord_equal()




zoo_partition <- createDataPartition(zoo_dataset$class_type, p = 0.7, list = FALSE)
zoo_training <- zoo_dataset[zoo_partition,]
zoo_test <- zoo_dataset[-zoo_partition,]

my_tree <- tree(factor(zoo_training$Class_Type) ~ .-animal_name-Class_Type-class_type, data = zoo_training)
plot(my_tree, 
     main = "Zoo decision tree", 
     type = "uniform")
text(my_tree)



#---------------Classification Analysis + Random Forest---------------#


# data for confusion_matrix
pred <- predict(my_tree, zoo_test, type = "class")

confusion_matrix <- confusionMatrix(pred, factor(zoo_test$Class_Type))

# plot confusion_matrix
ggplot(data = as.data.frame(confusion_matrix$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Tree")
  

# Random forest

forest <- randomForest(factor(zoo_training$Class_Type) ~ .-animal_name-Class_Type-class_type, data = zoo_training, ntree = 100)
pred_forest <- predict(forest, zoo_test, type = "class")

confusion_matrix_forest <- confusionMatrix(pred_forest , factor(zoo_test$Class_Type))

# plot confusion_matrix
ggplot(data = as.data.frame(confusion_matrix_forest$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("RandomForest")


varImpPlot(forest)


#---------------Dimensional Reduction---------------#



# prepare data for umap
zoo_training_umap <- subset(zoo_training, select= -c(animal_name, class_type, Class_Type))
labels <- subset(zoo_training, select = Class_Type)


umap_fit <- umap(zoo_training_umap)
umap_df <- as.data.frame(umap_fit$layout)
umap_final <- cbind(umap_df, labels)

#plot(umap_df$V1,umap_df$V2, type = "p" )

ggplot(umap_final, aes(x = V1, y = V2, color = Class_Type)) +
  geom_point() +
  ggtitle("UMAP Plot")


#training on umap data

zoo_partition_umap <- createDataPartition(umap_final$Class_Type, p = 0.7, list = FALSE)
zoo_training_umap <- umap_final[zoo_partition_umap,]
zoo_test_umap <- umap_final[-zoo_partition_umap,]


umap_forest <- randomForest(factor(Class_Type)~ ., zoo_training_umap)

pred_forest_umap <- predict(umap_forest, zoo_test_umap, type = "class")

confusion_matrix_umap_forest <- confusionMatrix(pred_forest_umap , factor(zoo_test_umap$Class_Type))

# plot confusion_matrix
ggplot(data = as.data.frame(confusion_matrix_umap_forest$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Random Forest UMAP")

#Might vary because of other data representation



#---------------KNN---------------#



zoo_train_knn <-subset(zoo_training, select = -c(animal_name, class_type))
zoo_test_knn <- subset(zoo_test, select = -c(animal_name, class_type))

#only odd values from 1 to 25 used for knn
k_values <- seq(from = 1, to = 25, by = 2)

knn_zoo <- train(Class_Type ~ ., data = zoo_train_knn, method = "knn", preProcess = c("center", "scale"), tuneGrid = data.frame(k = k_values))
knn_zoo
plot(knn_zoo, main = "Accuracy based on # of neighbors")

pred_knn <- predict(knn_zoo, newdata = zoo_test_knn, type = "raw")


confusion_matrix_knn <- confusionMatrix(pred_knn , factor(zoo_test_knn$Class_Type))

# plot confusion_matrix
ggplot(data = as.data.frame(confusion_matrix_knn$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("KNN")



#---------------Bonus---------------#
#no char data
zoo_train_xgbst <-subset(zoo_training, select = -c(animal_name, Class_Type))
zoo_test_xgbst <- subset(zoo_test, select = -c(animal_name, Class_Type))

zoo_xgbst <- xgboost(data = as.matrix(zoo_train_xgbst), label = zoo_train_xgbst$class_type, max.depth = 10, nrounds =20, objective = "multi:softmax", num_class = 8)

pred_xgbst <- predict(zoo_xgbst, as.matrix(zoo_test_xgbst))


confusion_matrix_xgbst <- confusionMatrix(factor(pred_xgbst) , factor(zoo_test_xgbst$class_type))

# plot confusion_matrix
ggplot(data = as.data.frame(confusion_matrix_xgbst$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("XGBoost")












