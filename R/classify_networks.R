#########################################
## Machine learning classification
## Mattia Girardi
## 15.01.2021
########################################

#' Classify networks
#'
#' @param master network measures
#' @param partitions list of partitions
#' @return accuracy of classification
#' @export
#' @import data.table
classify.networks <- function(master, partitions, fit = formula(fit)){
  # create accuracy vector
  accuracy <- c()
  for(i in 1:ncol(partitions)){
    # create training data
    master_training <- master[partitions[, i],]

    # create master data
    master_test <- master[-partitions[, i],]

    # train classifier
    default_glm_mod <- train(
      form = fit,
      data = master_training,
      method = "glm",
      family = "binomial"
    )

    # predict networks of test data
    prediction <- predict(default_glm_mod, newdata = master_test)

    # create confusion matrix
    result <- table(master_test$NetworkDomain, prediction)

    # compute accuracy
    accuracy[length(accuracy) + 1] <- (result[1]+result[4])/(nrow(master_test))
  }
  return(data.table(Accuracy = mean(accuracy), SD = sd(accuracy)))
}
