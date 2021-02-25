#########################################
## Diffusion results functions
## Mattia Girardi
## 15.01.2021
########################################

#' Plot diffusion results
#'
#' @param files diffusion results
#' @return plot of diffusion results
#' @export
#' @import data.table
plot.diffusion.results <- function(files, results_list = list(),
                                   plot_list = list()){
  for(i in 1:length(files)){
    # create list of results
    for(j in 1:length(files)){
      # save spec name
      spec <- paste("D", sub(".*/(.+)% s.*", "\\1", files[j]),
                    sub(".*_(.+)% p.*", "\\1", files[j]),
                    sub(".*_(.+)% t.*", "\\1", files[j]), sep = "_")
      
      # read file
      result <- fread(files[j])
      
      # transform Social,Offline and Social,Online into factor variables with 2 levels
      for(n in 1:nrow(result)){
        if(result[n, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
          result[n, "NetworkDomain"] <- gsub(".*", "Social", result[n, "NetworkDomain"])
        } else {
          result[n, "NetworkDomain"] <- gsub(".*", "Non-Social", result[n, "NetworkDomain"])
        }
      }
      
      # assign file to spec name
      assign(spec, result)
      
      # create result list
      results_list[[spec]] <- result
    }
    
    # save title names
    titles <- c()
    for(name in 1:length(files)){
      title <- files[name]
      title %<>%
        gsub("output/diffusion/consolidated/", "", .) %>%
        gsub("_", ", ", .) %>%
        gsub("prob", "probability of transmission", .) %>%
        gsub(".csv", "", .)
      titles[length(titles) + 1] <- title
    }
    
    # change levels of network domain
    results_list[[i]]$NetworkDomain <- factor(results_list[[i]][, NetworkDomain],
                                              levels=c("Social", "Non-Social"))
    
    # save ggplot to plot list
    plot_list[[length(plot_list) + 1]] <- ggplot(results_list[[i]], aes(x = Nodes, y = Mean)) +
      geom_point(aes(color = NetworkDomain)) + scale_x_log10(breaks = c(100, 1000, 10000)) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + ylab("Number of Iterations") +
      scale_color_manual(values = c("orangered1", "gray20")) + labs(title = sprintf("%s", titles[i])) +
      facet_wrap(NetworkDomain ~ ., scales = "free_x",nrow = 2) + theme(panel.background = element_blank(),
                                                                        panel.grid.major = element_line(colour = "gray85", size = 1),
                                                                        panel.grid.minor = element_line(colour = "gray85"),
                                                                        axis.ticks = element_blank(),
                                                                        axis.title.y = element_text(size = 20),
                                                                        axis.title.x = element_text(size = 25),
                                                                        axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                                        axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                                        legend.position = "none",
                                                                        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", vjust = 5),
                                                                        strip.background = element_rect(
                                                                          fill="white"),
                                                                        strip.text = element_text(size = 20, face = "bold"),
                                                                        axis.line = element_line(),
                                                                        axis.title.x.bottom = element_blank(),
                                                                        plot.margin=unit(c(2,2,2,2),"cm")
      )
    if(i == length(titles)){
      # plot results
      n <- length(plot_list)
      nCol <- floor(sqrt(n))
      
      # arrange plot grid
      do.call("grid.arrange", c(plot_list, ncol = nCol))
    }
  }
}


#' Plot diffusion spread
#'
#' @param files diffusion results
#' @return plot of diffusion spread
#' @export
#' @import data.table
plot.diffusion.spread <- function(files, results_list = list(),
                                  plot_list = list()){
  for(i in 1:length(files)){
    # create list of results
    for(j in 1:length(files)){
      # save spec name
      spec <- paste("D", sub(".*/(.+)% s.*", "\\1", files[j]),
                    sub(".*_(.+)% p.*", "\\1", files[j]),
                    sub(".*_(.+)% t.*", "\\1", files[j]), sep = "_")
      
      # read file
      result <- fread(files[j])
      
      # transform Social,Offline and Social,Online into factor variables with 2 levels
      for(n in 1:nrow(result)){
        if(result[n, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
          result[n, "NetworkDomain"] <- gsub(".*", "Social", result[n, "NetworkDomain"])
        } else {
          result[n, "NetworkDomain"] <- gsub(".*", "Non-Social", result[n, "NetworkDomain"])
        }
      }
      
      # compute spread
      result$Spread <- (apply(result[,5:14], 1, max) - apply(result[,5:14], 1, min))/result[, 3]
      
      # assign file to spec name
      assign(spec, result)
      
      # save title names
      titles <- c()
      for(name in 1:length(files)){
        title <- files[name]
        title %<>%
          gsub("output/diffusion/consolidated/", "", .) %>%
          gsub("_", ", ", .) %>%
          gsub("prob", "probability of transmission", .) %>%
          gsub(".csv", "", .)
        titles[length(titles) + 1] <- title
      }
      
      # create result list
      results_list[[spec]] <- result
    }
    
    
    # change levels of network domain
    results_list[[i]]$NetworkDomain <- factor(results_list[[i]][, NetworkDomain],
                                              levels=rev(c("Social", "Non-Social")))
    # save ggplot to plot list
    plot_list[[length(plot_list) + 1]] <- ggplot(results_list[[i]], aes(x = Nodes, y = Spread, color = NetworkDomain)) + geom_point() +
      scale_x_log10() + facet_wrap(NetworkDomain~.) + scale_y_continuous(limits = c(0, 50)) +
      scale_color_manual(values = c("gray20", "orangered1")) + ylab("Relative spread") +
      labs(title = sprintf("%s", titles[i])) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_line(colour = "gray85", size = 1),
            panel.grid.minor = element_line(colour = "gray85"),
            axis.ticks = element_blank(),
            axis.title.y = element_text(size = 25, face = "plain"),
            axis.title.x = element_text(size = 25, face = "plain"),
            axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
            axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5, vjust = 5),
            strip.background = element_rect(
              fill="white"),
            strip.text = element_text(size = 20),
            axis.line = element_line(),
            axis.title.x.bottom = element_blank(),
            plot.margin=unit(c(2,2,2,2),"cm")
      )
    
    if(i == length(titles)){
      # plot results
      n <- length(plot_list)
      nCol <- floor(sqrt(n)) + 1
      
      # arrange plot grid
      do.call("grid.arrange", c(plot_list, ncol = 3))
    }
  }
}


#' Feature importance; diffusion
#'
#' @param master diffusion results
#' @param files diffusion results files
#' @param fit diffusion regression formula
#' @return corrplot feature importance rank
#' @export
#' @import data.table
feature.importance.diffusion <- function(master, files, fit = formula(fit)){
  
  # diffusion regression
  diffusion_lm <- lm(form = fit,
                     data = master)
  
  # save diffusion summaries and assign them to variable
  response <- summary(diffusion_lm)
  summaries <- list()
  for(i in 1:length(response)){
    var <- paste("R", i, sep = "")
    res <- data.frame(response[[i]]$coefficients)[-1,]
    summaries[[var]] <- res
  }
  
  # create estimates table
  for(r in 1:length(summaries)){
    if(r == 1){
      estimate <- data.table(Measures = rownames(summaries[[r]]), summaries[[r]]$Estimate)
    } else {
      estimate <- cbind(estimate, summaries[[r]]$Estimate)
    }
  }
  
  # change column names
  colnames(estimate)[-1] <- colnames(master_diffusion[, 24:(22 + ncol(estimate))])
  
  # order estimates by absolute value
  estimate <- estimate[order(abs(estimate[, 2]), decreasing = T)]
  
  # create long data
  estimate_table <- melt(estimate, id.vars = "Measures")
  
  # plot importance
  ggplot(estimate_table, aes(x = reorder(Measures, abs(value)), y = value,
                             fill = variable)) + geom_col(position = "dodge") +
    coord_flip() + labs(fill = "Simulation") +
    scale_fill_manual(values = c("mediumpurple2", "seagreen2", "springgreen3", "seagreen4", "darkgreen",
                                 "skyblue2", "dodgerblue1")) +
    theme(axis.title = element_blank(),
          axis.text.x.bottom = element_text(size = 12),
          axis.text.y.left = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray85",
                                          size = 0.5),
          panel.grid.minor = element_line(colour = "gray85",
                                          size = 0.5),
          axis.ticks = element_blank(),
          legend.position = c(0.85, 0.4),
          legend.background = element_rect(size = 0.1, colour = "Black"),
          legend.key = element_blank(),
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.text = element_text(color = "gray20", size = 12),
          legend.title = element_text(size = 12)) +
    guides(fill = guide_legend(reverse=TRUE))
}


#' Rank correlation
#'
#' @param master diffusion results
#' @return feature importance plot
#' @export
#' @import data.table
feature.rank.corrplot <- function(master, fit = formula(fit)){
  # diffusion regression
  diffusion_lm <- lm(form = fit,
                     data = master)
  
  # save diffusion summaries and assign them to variable
  response <- summary(diffusion_lm)
  summaries <- list()
  for(i in 1:length(response)){
    var <- paste("R", i, sep = "")
    res <- data.frame(response[[i]]$coefficients)[-1,]
    res <- data.table(ID = 1:(nrow(res)), Variable = rownames(res), data.table(res))
    res <- res[order(abs(res$Estimate), decreasing = T)]
    summaries[[var]] <- res
  }
  
  # create ranks table
  for(r in 1:length(summaries)){
    if(r == 1){
      ranks <- data.frame(summaries[[r]]$ID[1:5])
    } else {
      ranks <- cbind(ranks, summaries[[r]]$ID[1:5])
    }
  }
  
  # change column names
  colnames(ranks) <- colnames(master_diffusion[, 24:(23 + ncol(ranks))])
  
  #### Jaccard index
  jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
  }
  
  # prepare correlation matrix
  rank_corr_plot <- matrix(nrow = ncol(ranks), ncol = ncol(ranks))
  # compute Jaccard index
  for(i in 1:ncol(ranks)){
    for(j in 1:ncol(ranks)){
      a <- ranks[, i]
      b <- ranks[, j]
      names(a) <- names(b) <- c("a", "b")
      rank_corr_plot[i, j] <- jaccard(a, b)
    }
  }
  # rename axis
  colnames(rank_corr_plot) <- rownames(rank_corr_plot) <- colnames(ranks)
  
  # compute Spearman's rank correlation
  cor <- cor(ranks, method = "spearman")
  
  # merge correlationa
  rank_corr_plot[lower.tri(rank_corr_plot)] <- cor[lower.tri(cor)]
  
  # create corrplot
  corrplot(rank_corr_plot, method = "number", tl.col = "black", tl.offset = 0.4,
           cl.align.text = "l", tl.srt = 90, addgrid.col = "black", number.cex = 1.5,
           tl.cex = 1.5)
}



