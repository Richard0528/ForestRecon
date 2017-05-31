
##MSE
##RME
##RMAE
# first a line to load the current workspace
#shape.vals<-seq(1,3,.25)
#scale.vals<-seq(20,50,10)
filepath<-"../../ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
load(paste(filepath,"ClumpedShape1Scale20.RData",sep = ""))
#load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))

Fit_cal_func.fn<-function(ncol=4, nrow=10, sim_section=sim.density.section, est_section=est.density.section.bia, nsim=4000, fit_cal="MSE")
{
  cur_matrix.df = data.frame(matrix(NA, ncol = ncol, nrow = nrow))
  for (i in 1:10)
  {
    sim.tph.all = sim_section[[i]][,5]
    for (j in 1:4)
    {
        if (fit_cal == "MSE"){
          cur_matrix.df[i,j] = sum((est_section[[i]][,j] - sim.tph.all)^2)/nsim
        } else if (fit_cal == "RME"){
          cur_matrix.df[i,j] = sum((est_section[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
        } else if (fit_cal == "RMAE"){
          cur_matrix.df[i,j] = sum(abs(est_section[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
        }
    }
  }
  colnames(cur_matrix.df) = c("section1", "section2", "section3", "section4")
  return(cur_matrix.df)
}
