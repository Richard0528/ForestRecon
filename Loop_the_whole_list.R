##Use bia_glo_data function
  ##and loop throught whole datasets

shape.vals<-seq(1,3,.25)
scale.vals<-seq(20,50,10)
#shape.vals<-seq(1,3,1)
#scale.vals<-seq(20,50,20)
filepath<-"../../ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
source("../ForestReconGitRepo/ForestRecon/BIA_GLO_func.R")
source("../ForestReconGitRepo/ForestRecon/Boxplot_func.R")
for (ii in 1:length(shape.vals))
{
  for (jj in 1:length(scale.vals))
    {
    load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))

    biaglo1.list<-biaglo.fn(n.density=length(tph))

    load(paste(filepath,"Clumped4000/ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))

    biaglo2.list<-biaglo.fn(n.density=length(tph))

    biaglo.list<-lapply(names(biaglo1.list),function(x) rbind(biaglo1.list[[x]],biaglo2.list[[x]]))
    names(biaglo.list)<-names(biaglo1.list)

    ##if you want to save the file, uncomment command line below.
    ##it will take a long time
    #save(biaglo.list,file=paste("CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    }
}
