library(caret)
library(keras)

path <- getwd()

### load the R.data (available in the github repository), this include the 6 models, a list of the generation times used, and a list the data structure of each species.
load("data.RData")

setwd(path)
obs <- dget("observed.txt")


sp.names <- c("Campephilus principalis",
              "Colinus virginianus",
              "Conuropsis carolinensis",
              "Ectopistes migratorius",
              "Megascops asio",
              "Dryobates villosus",
              "Tympanuchus cupido cupido",
              "Vermivora bachmanii",
              "Zenaida macroura")



x <- species
result <- list()
for(u in 1:length(gentimes)){
  
setwd(paste(path,"/",names(gentimes)[u], sep=""))

prob <- NULL

for(i in 1:length(x)){
  
  y <- list.files(pattern = x[i])
  sims <- NULL
  index <- NULL
  inn <- 0
  
  for(j in c("_BN_","_CS_","_EXP_","_BN2_","_CS2_","_EXP2_")){
    s <- read.table(y[grep(j, y)], header=T)
    index <- c(index, rep(inn, nrow(s)))
    inn <- inn + 1
    sims <- rbind(sims, s[, grep("s_", colnames(s))])
  }
  
  sims <- as.matrix(sims)
  
  sims[is.na(sims)] <- 0
  
  trainIndex <- createDataPartition(index, p = .8,
                                    list = FALSE,
                                    times = 1)
  
  train <- list(sims[trainIndex[,1],], index[trainIndex])
  test <- list(sims[-trainIndex[,1],], index[-trainIndex])
  
  names(train) <- c("x","y")
  names(test) <- c("x","y")
  
  nrow(test$x)
  
  proc.data <- list(train, test)
  names(proc.data) <- c("train", "test")
  
  library(keras)
  c(train_images, train_labels) %<-% proc.data$train
  c(test_images, test_labels) %<-% proc.data$test
  
  
  train_images <- scale(train_images)
  #train_images <- train_images/max(train_images)
  # Use means and standard deviations from training set to normalize test set
  col_means_train <- attr(train_images, "scaled:center")
  col_stddevs_train <- attr(train_images, "scaled:scale")
  test_images <- scale(test_images, center = col_means_train, scale = col_stddevs_train)
  
  model <- keras_model_sequential() %>%
      layer_dense(units = 32, activation = "relu", input_shape = c(dim(train_images)[2])) %>%
      layer_dense(units = 32, activation = "relu") %>%
      layer_dense(units = 6, activation = "softmax")
    
    model %>% compile(
      optimizer = 'adam',
      loss = 'sparse_categorical_crossentropy',
      metrics = c('accuracy'))
  
  
  epochs <- 300
  history <- model %>% fit(
    train_images,
    train_labels,
    epochs = epochs,
    batch_size = 50000,
    validation_split = 0.1)
  
  
  score <- model %>% evaluate(test_images, test_labels)
  
  TAB2 <- scale(t(data.frame(obs[[i]][1:12])), center = col_means_train, scale = col_stddevs_train)
  predictions <- model %>% predict(TAB2)
  prob <- rbind(prob, c(predictions,score))
  print(prob)
}
rownames(prob) <- x
colnames(prob) <- c("_BN_","_CS_","_EXP_","_BN2_","_CS2_","_EXP2_", "loss", "accuracy")

result[[u]] <- prob
}

names(result) <- names(gentimes)

print(result)

dput(result, "model_class_result.txt")
