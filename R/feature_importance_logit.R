#########################################
## Feature importance logistic regression
## Mattia Girardi
## 15.01.2021
########################################

#' Classify networks
#'
#' @param master measures
#' @param partitions list of partitions
#' @return accuracy of classification
#' @export
#' @import data.table
feature.importance.logit <- function(master, fit = formula(fit)){
  # logistic regression
  logit_glm <- glm(form = fit,
                   family = binomial, 
                   data = master
  )
  # save regression output
  coeff <- data.frame(summary(logit_glm)$coefficients)
  
  # drop intercept
  res <- data.table(Variable = rownames(coeff), coeff)[-1]
  
  # order by absolute value of estimate
  res <- res[order(abs(res$Estimate), decreasing = T)]
  
  # change name of estimate column
  names(res)[2] <- "Standardized Coeffiecient"
  
  # create odds ratio column
  res$'Odds Ratio' <- sapply(res$'Standardized Coeffiecient', exp)
  
  # order levels by value of standardized coefficient
  res$Variable <-  factor(res$Variable, levels = rev(res$Variable))
  
  # convert data to long format
  res <- melt(res[, c(1, 2, 6)], id.vars = "Variable")
  
  # plot feature importance
  ggplot(res, aes(x = Variable, y = value, fill = variable)) +
    geom_col(width=0.7, position = "dodge") + coord_flip() +
    scale_fill_manual(values = c("seagreen2", "dodgerblue2")) +
    scale_y_continuous(limits=c(-2,2.5), breaks=c(-2, -1, 0, 1, 2)) +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text.x.bottom = element_text(size = 12),
          axis.text.y.left = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray85",
                                          size = 0.5),
          panel.grid.minor = element_line(colour = "gray85",
                                          size = 0.5),
          axis.ticks = element_blank(),
          legend.position = c(0.85, 0.25),
          legend.background = element_rect(size = 0.1, colour = "Black"),
          legend.key = element_blank(),
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.text = element_text(color = "gray20", size = 12)) +
    geom_hline(yintercept = 1, color = "dodgerblue3", size=1, linetype = "dotted")+
    geom_hline(yintercept = 0, color = "gray20", size=0.5)
}