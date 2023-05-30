library(caret)
library(tree)
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)


tree_model <- tree(Species ~ ., data = iris)
print(tree_model)
text(tree_model)
plot(tree_model)
