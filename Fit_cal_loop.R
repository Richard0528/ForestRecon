##Clumped Data
##Use Fit_cal_function to loop throught everything

#shape.vals<-seq(1,3,1)
#scale.vals<-seq(20,50,20)
shape.vals<-seq(1,3,.25)
scale.vals<-seq(20,50,10)

# for MCK computer
filepath<-"../../Results/WithPCQDiam/Clumped/" 
source("../ForestRecon/cal_function.R")
source("../ForestRecon/BIA_GLO_func.R")
#filepath<-"../ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"

# for Richard's
setwd("/Users/richardyang/documents/GIT/ForestRecon")
filepath<-"../../ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
source("BIA_GLO_func.R")
source("cal_function.R")

options.control<-c("true_tph","BIA_MSE","BIA_RME","BIA_RMAE","GLO_MSE","GLO_RME","GLO_RMAE","BOX_tph")

for (ii in 1:length(shape.vals))
{
  for (jj in 1:length(scale.vals))
  {
    load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    ##biaglo.list for 1000
    biaglo1.list<-biaglo.fn(n.density=length(tph))
    
    #sim.density.section
    Combined1_sim.list <- sim.density.section
    
    #sim.density.section10
    Combined1_sim10.list <- sim.density.section10
    
    #sim.density.section15
    Combined1_sim15.list <- sim.density.section15
    
    load(paste(filepath,"Clumped4000/ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    ##biaglo.list for 4000
    biaglo2.list<-biaglo.fn(n.density=length(tph))
    
    #sim.density.section
    Combined2_sim.list <- sim.density.section
    #merge
    Combined_sim.list <- lapply(c(1:10),function(x) rbind(Combined1_sim.list[[x]],Combined2_sim.list[[x]]))
    names(Combined_sim.list)<-names(Combined1_sim.list)
    
    ##merge for biaglo.list
    biaglo.list<-lapply(names(biaglo1.list),function(x) rbind(biaglo1.list[[x]],biaglo2.list[[x]]))
    names(biaglo.list)<-names(biaglo1.list)
    
    #sim.density.section10
    Combined2_sim10.list <- sim.density.section10
    #merge
    Combined_sim10.list <- lapply(c(1:10),function(x) rbind(Combined1_sim10.list[[x]],Combined2_sim10.list[[x]]))
    names(Combined_sim10.list)<-names(Combined1_sim10.list)
    
    #sim.density.section15
    Combined2_sim15.list <- sim.density.section15
    #merge
    Combined_sim15.list <- lapply(c(1:10),function(x) rbind(Combined1_sim15.list[[x]],Combined2_sim15.list[[x]]))
    names(Combined_sim15.list)<-names(Combined1_sim15.list)
    
    #MSE
#    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=)
#    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=biaglo.list[[1]],bySection = FALSE)
    # call this .df, modify all of the other calls
    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=biaglo.list)#,bySection = FALSE)
    #save(MSE_Fit_cal.list,file=paste("MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    MSE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list,est_section=biaglo.list)
    #save(MSE_Fit10_cal.list,file=paste("MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    MSE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list,est_section=biaglo.list)
    #save(MSE_Fit15_cal.list,file=paste("MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    MSE_Fitall_cal.list<- list(MSE_Fit_cal.list, MSE_Fit10_cal.list, MSE_Fit15_cal.list)
    
    #RME
    RME_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list, est_section=biaglo.list, fit_cal = "RME")
   # save(RME_Fit_cal.list,file=paste("RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RME_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list, est_section=biaglo.list, fit_cal = "RME")
  #  save(RME_Fit10_cal.list,file=paste("RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RME_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list, est_section=biaglo.list, fit_cal = "RME")
   # save(RME_Fit15_cal.list,file=paste("RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RME_Fitall_cal.list<- list(RME_Fit_cal.list, RME_Fit10_cal.list, RME_Fit15_cal.list)
    
    #RMAE
    RMAE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit_cal.list,file=paste("RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RMAE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list,est_section=biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit10_cal.list,file=paste("RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RMAE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list,est_section=biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit15_cal.list,file=paste("RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RMAE_Fitall_cal.list<- list(RMAE_Fit_cal.list, RMAE_Fit10_cal.list, RMAE_Fit15_cal.list)
    
    # save when you want to
    # save(MSE_Fit_cal.list, MSE_Fit10_cal.list, MSE_Fit15_cal.list, RME_Fit_cal.list, RME_Fit10_cal.list, RME_Fit15_cal.list,
    #      RMAE_Fit_cal.list, RMAE_Fit10_cal.list, RMAE_Fit15_cal.list, file=paste("AllFitStats_CombinedShape",shape.vals[ii],
    #                                                                               "Scale",scale.vals[jj],".RData",sep=""))
    
    ## for save as pdf
    # pdf(file=paste("ClumpedFitStatsScale",scale.vals[jj],"Shape",shape.vals[ii],".pdf",sep=""))
    # for(mm in options.control)
    #   Plot.fn(mm)
    # dev.off()
    
  }
}

