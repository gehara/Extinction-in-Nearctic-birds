# load ggplot
library(ggplot2)

# save directory path to object
path <- getwd()

### load the R.data (available in the github repository), this include the 6 models, a list of the generation times used, and a list the data structure of each species.
load("data.RData")

### load observed summary statistics
observed <- dget("observed.txt")

# species names for the plot
sp.names <- c("Campephilus principalis",
              "Colinus virginianus",
              "Conuropsis carolinensis",
              "Ectopistes migratorius",
              "Megascops asio",
              "Dryobates villosus",
              "Tympanuchus cupido cupido",
              "Vermivora bachmanii",
              "Zenaida macroura")

### this function plots first 10 PCs for all species. This will generate a long pdf.
PCA.of.models1 <- function(name) {
  
  pdf(paste("PCA1_",name,".pdf",sep=""), paper="a4r", width=10, pointsize=10)
  for(i in 1:length(species)){
    
    
    setwd(paste(path,"/", name, sep=""))
    EXP2.sim <- read.delim(paste("SIMS_EXP2_",species[i],".txt",sep=""))[1:200, ]
    BN2.sim <- read.delim(paste("SIMS_BN2_",species[i],".txt",sep=""))[1:200, ]
    CS2.sim <- read.delim(paste("SIMS_CS2_",species[i],".txt",sep=""))[1:200, ]
    BN.sim <- read.delim(paste("SIMS_BN_",species[i],".txt",sep=""))[1:200, ]
    CS.sim <- read.delim(paste("SIMS_CS_",species[i],".txt",sep=""))[1:200, ]
    EXP.sim <- read.delim(paste("SIMS_EXP_",species[i],".txt",sep=""))[1:200, ]
    
    data <- c(rep("EXP",nrow(EXP.sim)),
              rep("BN",nrow(BN.sim)),
              rep("CS",nrow(CS.sim)))
    
    data2 <-  c(rep("EXP2",nrow(EXP2.sim)),
                rep("BN2",nrow(BN2.sim)),
                rep("CS2",nrow(CS2.sim)))
    
    models <- rbind(EXP.sim[,6:ncol(EXP.sim)],
                    BN.sim[,6:ncol(BN.sim)],
                    CS.sim[,4:ncol(CS.sim)])
    
    
    models2 <- rbind(EXP2.sim[,8:ncol(EXP2.sim)],
                     BN2.sim[,8:ncol(BN2.sim)],
                     CS2.sim[,8:ncol(CS2.sim)])
    
    obs <- observed[[i]][names(observed[[i]]) %in% colnames(models)]
    
    models2 <- models2[colnames(models2) %in% colnames(models)]
    
    index <- c(data, data2)
    models <- rbind(models, models2)
    
    labels <- unique(index)
    labels <- sort(c(labels, "obs"))
    sizes <- rep(2, length(labels))
    sizes[which(labels == "obs")] <- 10
    shapes <- rep(16, length(labels))
    shapes[which(labels == "obs")] <- 8
    data.PCA <- index[complete.cases(models)]
    models.PCA <- models[complete.cases(models), ]
    PCA <- prcomp(rbind(models.PCA, obs), center = T, scale. = T, 
                  retx = T)
    scores <- data.frame(PCA$x[, 1:ncol(PCA$x)])
    PC <- colnames(scores)[1:10]
    
    plotPCA <- function(PCS) {
      PCS <- rlang::sym(PCS)
      p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = !!PCS)) + 
        ggplot2::theme(legend.position = "none") + 
        ggplot2::geom_point(ggplot2::aes(colour = c(data.PCA,"obs"), size = c(data.PCA, "obs"), shape = c(data.PCA,"obs"))) + 
        ggplot2::scale_shape_manual(values = shapes) + 
        ggplot2::scale_size_manual(values = sizes) + 
        ggplot2::scale_color_brewer(palette = "Set1") +
        
        if (PCS == "PC2") ggplot2::theme(legend.position = "top", legend.direction = "horizontal", legend.title = ggplot2::element_blank()) 
      
      return(p)
    }
    P <- NULL
    for (j in 2:10) {
      P[[j]] <- plotPCA(PC[j])
    }
    gridExtra::grid.arrange(P[[2]], P[[3]], P[[4]], P[[5]], P[[6]], 
                            P[[7]], P[[8]], P[[9]], P[[10]], nrow = 3, top = sp.names[i])
    print(i) 
  }
  dev.off()
  
}

### this function plots the first two PCS of each speacies in a single page.
PCA.of.models2 <- function(name) {
  
  pdf(paste("PCA2_",name,".pdf",sep=""), paper="a4r", width=10, pointsize=10)
  P <- NULL
  for(i in 1:length(species)){
    
    
    setwd(paste(path,"/", name, sep=""))
    EXP2.sim <- read.delim(paste("SIMS_EXP2_",species[i],".txt",sep=""))[1:200, ]
    BN2.sim <- read.delim(paste("SIMS_BN2_",species[i],".txt",sep=""))[1:200, ]
    CS2.sim <- read.delim(paste("SIMS_CS2_",species[i],".txt",sep=""))[1:200, ]
    BN.sim <- read.delim(paste("SIMS_BN_",species[i],".txt",sep=""))[1:200, ]
    CS.sim <- read.delim(paste("SIMS_CS_",species[i],".txt",sep=""))[1:200, ]
    EXP.sim <- read.delim(paste("SIMS_EXP_",species[i],".txt",sep=""))[1:200, ]
    
    data <- c(rep("EXP",nrow(EXP.sim)),
              rep("BN",nrow(BN.sim)),
              rep("CS",nrow(CS.sim)))
    
    data2 <-  c(rep("EXP2",nrow(EXP2.sim)),
                rep("BN2",nrow(BN2.sim)),
                rep("CS2",nrow(CS2.sim)))
    
    models <- rbind(EXP.sim[,6:ncol(EXP.sim)],
                    BN.sim[,6:ncol(BN.sim)],
                    CS.sim[,4:ncol(CS.sim)])
    
    
    models2 <- rbind(EXP2.sim[,8:ncol(EXP2.sim)],
                     BN2.sim[,8:ncol(BN2.sim)],
                     CS2.sim[,8:ncol(CS2.sim)])
    
    obs <- observed[[i]][names(observed[[i]]) %in% colnames(models)]
    
    models2 <- models2[colnames(models2) %in% colnames(models)]
    
    models <- rbind(models, models2)
    index <- c(data, data2)
    
    labels <- unique(index)
    labels <- sort(c(labels, "obs"))
    sizes <- rep(2, length(labels))
    sizes[which(labels == "obs")] <- 10
    shapes <- rep(16, length(labels))
    shapes[which(labels == "obs")] <- 8
    data.PCA <- index[complete.cases(models)]
    models.PCA <- models[complete.cases(models), ]
    
    PCA <- prcomp(rbind(models.PCA, obs), center = T, scale. = T, 
                  retx = T)
    scores <- data.frame(PCA$x[, 1:ncol(PCA$x)])
    PC <- colnames(scores)[1:2]
    plotPCA <- function(PCS) {
      PCS <- rlang::sym(PCS)
      p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = !!PCS)) + 
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_point(ggplot2::aes(colour = c(data.PCA, "obs"), size = c(data.PCA, "obs"), shape = c(data.PCA, "obs"))) +
        ggplot2::scale_shape_manual(values = shapes) +
        ggplot2::scale_size_manual(values = sizes) +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggtitle(label = sp.names[i]) +
        theme(plot.title = element_text(face="italic")) +
        if (i == 1) 
          ggplot2::theme(legend.position = "top", legend.direction = "horizontal", 
                         legend.title = ggplot2::element_blank())
      return(p)
    }
    
    P[[i]] <- plotPCA(PC[2])
    
    
  }
  
  gridExtra::grid.arrange(P[[1]], P[[2]], P[[3]], P[[4]], P[[5]], 
                          P[[6]], P[[7]], P[[8]], P[[9]], nrow = 3)
  
  dev.off()
}

## this will loop through all generation times
for(i in 1:length(gentimes)) {
  setwd(paste(path,"/", names(gentimes)[i],sep=""))
  PCA.of.models1(name = names(gentimes)[i])
}

## this will loop through all generation times
for(i in 1:length(gentimes)) {
  setwd(paste(path,"/", names(gentimes)[i],sep=""))
  PCA.of.models2(name = names(gentimes)[i])
}
