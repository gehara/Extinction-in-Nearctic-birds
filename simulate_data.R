### install and load required packages
#install.packages("devtools")
library(devtools)
install_github("gehara/PipeMaster@master")
library(PipeMaster)

### save path to object
path <- getwd()

# create output folders for each generation time
dir.create(paste(path,"/",1,sep=""))
dir.create(paste(path,"/",2,sep=""))
dir.create(paste(path,"/",3,sep=""))
dir.create(paste(path,"/","lit",sep=""))

### load the R.data (available in the github repository), this include the 6 models, a list of the generation times used, and a list the data structure of each species.
load("data.RData")

############ this function automates the simulations of the 6 models for all 9 species

simulate.data <- function(EXP, EXP2, CS, CS2, BN, BN2, ncores, nsim.blocks,
                                 block.size, gentimes, folder){
  
  
  for(i in 1:length(species)){
    
    ### this converts the times to generations according to the generation times
    EXP2$flags$en$time[,5] <- round(c(100,500000)/gentimes[i],0)
    EXP2$flags$en$time[,4] <- round(c(50,10000)/gentimes[i],0)
    
    BN2$flags$en$time[,5] <- round(c(100,500000)/gentimes[i],0)
    BN2$flags$en$time[,4] <- round(c(50,10000)/gentimes[i],0)
    
    CS2$flags$en$time[,5] <- round(100/gentimes[i],0)
    CS2$flags$en$time[,4] <- round(50/gentimes[i],0)
    
    EXP$flags$en$time[,5] <- round(500000/gentimes[i],0)
    EXP$flags$en$time[,4] <- round(10000/gentimes[i],0)
    
    BN$flags$en$time[,5] <- round(500000/gentimes[i],0)
    BN$flags$en$time[,4] <- round(10000/gentimes[i],0)
    
    
    ### this updates the data structure of each species 
    EXP$loci<-data.structure[[i]]$loci
    EXP$I<-data.structure[[i]]$I
    
    BN2$loci<-data.structure[[i]]$loci
    BN2$I<-data.structure[[i]]$I
    
    BN$loci<-data.structure[[i]]$loci
    BN$I<-data.structure[[i]]$I
    
    CS2$loci<-data.structure[[i]]$loci
    CS2$I<-data.structure[[i]]$I
    
    CS$loci<-data.structure[[i]]$loci
    CS$I<-data.structure[[i]]$I
    
    
    #### simulation function from PipeMaster r-package. See github.com/gehara/PipeMaster for more details on this
   sim.msABC.sumstat(EXP2, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("EXP2",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
    
   sim.msABC.sumstat(BN2, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("BN2",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
    
   sim.msABC.sumstat(CS2, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("CS2",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
    
   sim.msABC.sumstat(EXP, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("EXP",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
    
   sim.msABC.sumstat(BN, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("BN",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
    
   sim.msABC.sumstat(CS, path = paste(path,"/",folder, sep=""),
                      nsim.blocks = nsim.blocks, use.alpha = F,
                      output.name = paste("CS",species[i],sep="_"),
                      append.sims = F, ncores=ncores, block.size = block.size)
  }
  
}

### this will loop the simulations over all generation times. !!!Select the number of cores you want to use !!!
### The total number of simulations is a product of ncores x block.size x nsim.blocks. Do not use a block size larger than 1000 or computation performance will go down.

for(i in 1:length(gentimes)) {
  simulate.data(EXP, EXP2, CS, CS2, BN, BN2, ncores = 40, block.size = 10,
                       nsim.blocks = 1, gentimes = gentimes[[i]], folder = names(gentimes)[i])
  
}
