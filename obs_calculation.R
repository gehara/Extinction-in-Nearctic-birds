EXP <- dget("/home/marcelo/Dropbox/Documents/projects_ongoing/ExtinctBirds-Demographic-Modeling/Exp.txt")

species <- list.files("/home/marcelo/Dropbox/Documents/projects_ongoing/ExtinctBirds-Demographic-Modeling/sequences")
pop.assign <- list()
obs <- list()
for(i in 1:length(species)){
  pop.assign[[i]]<-read.delim(paste("/home/marcelo/Dropbox/Documents/projects_ongoing/ExtinctBirds-Demographic-Modeling/pop.assign/",species[i],sep=""))
  pop.assign[[i]][,2]<-1
  obs[[i]] <- obs.sumstat.ngs(EXP,path.to.fasta = paste("/home/marcelo/Dropbox/Documents/projects_ongoing/ExtinctBirds-Demographic-Modeling/sequences/",species[i],sep=""),
                              pop.assign=pop.assign[[i]])
}