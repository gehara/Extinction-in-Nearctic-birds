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





RES <- list()
suported <- list()
suported$"1" <- c(2,1,6,6,6,3,2,6,6)
suported$'2' <- c(2,1,6,6,6,3,2,6,6)
suported$'3' <- c(5,5,6,6,6,3,5,6,6)
suported$'lit' <- c(6,1,6,6,6,3,5,3,6)
npars <- list()
npars$'1' <- c(1,3,5,5,5,3,1,5,5)
npars$'2' <- c(1,3,5,5,5,3,1,5,5)
npars$'3' <- c(5,5,5,5,5,3,5,5,5)
npars$'lit' <- c(5,3,5,5,5,3,5,3,5)

x <- species

estimate<-function(){
numeric.w <- 0
for(w in c("1", "2", "3", "lit"))
#for(w in c("3"))
  {
  numeric.w <- numeric.w + 1
  setwd(paste(path,"/", w, sep=""))

  res.sp <- list()
  for(i in 1:length(x)){

    y <- list.files(pattern = x[i])

    modelos <- c("_BN_","_CS_","_EXP_","_BN2_","_CS2_","_EXP2_")

    s <- read.table(y[grep(modelos[suported[[numeric.w]][i]], y)], header=T)

    predic <- s[, grep("s_", colnames(s))]
    outcome <- s[, -grep("s_", colnames(s))]

    predic <- as.matrix(predic)
    outcome <- as.matrix(outcome)

    predic[is.na(predic)] <- 0

    trainIndex <- sample(1:nrow(predic), .88*nrow(predic))

    prob <- NULL
    for(u in 1:npars[[numeric.w]][i]){

      train <- list(predic[trainIndex,], outcome[trainIndex, u])
      test <- list(predic[-trainIndex,], outcome[-trainIndex, u])

      names(train) <- c("x","y")
      names(test) <- c("x","y")

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

      regress <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu",
                    input_shape = dim(train_images)[2]) %>%
        layer_dense(units = 32, activation = "relu") %>%
        #layer_dense(units = 32, activation = "relu") %>%
        layer_dense(units = 1, activation = "relu")

      regress %>% compile(
        loss = "mape",
        optimizer = optimizer_rmsprop(),
        metrics = list("mean_absolute_percentage_error"))

      regress %>% summary()

      epochs <- 1500
      history <- regress %>% fit(
      train_images,
      train_labels,
      epochs = epochs,
      batch_size = 10000,
      validation_split = 0.1)

      c(loss, mape) %<-% (regress %>% evaluate(test_images, test_labels, verbose = 0))

      test_pred <- regress %>% predict(test_images)

      cor.coef <- cor.test(test_pred, test_labels)$estimate
      linear.coef <- lm(test_pred ~ test_labels)$coefficients[2]

      TAB2 <- scale(t(data.frame(obs[[i]][1:12])), center = col_means_train, scale = col_stddevs_train)
      predictions <- regress %>% predict(TAB2)
      prob <- rbind(prob, c(predictions, mape, linear.coef, cor.coef))
    }
    rownames(prob) <- colnames(outcome)[1:npars[[numeric.w]][i]]
    colnames(prob) <- c("Estimate","MAE", "linear.coef","cor.coef")
    res.sp[[i]] <- prob
  }
  names(res.sp) <- x
RES[[numeric.w]] <- res.sp
}
return(RES)
}

result <- estimate()
names(result) <- c("1","2","3","lit")
print(result)
dput(result, "result.txt")




