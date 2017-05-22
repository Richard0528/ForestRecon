
#cal_stats.fn = function()
##MSE
MSEsection.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
sim.tph.all = rowSums(sim.density.section[[tph.id]])/4
for (i in 1:10)
{
  for (j in 1:4)
  {
    MSEsection.df[i,j] = sum((est.density.section.bia[[i]][,j] - sim.tph.all)^2)/nsim
  }
}
colnames(MSEsection.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

MSEsection15.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    MSEsection15.df[i,j] = sum((est.density.section.bia[[i]][,j] - mean(sim.density.section15[[i]][,j]))^2)/nsim
  }
}
colnames(MSEsection15.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

MSEsection10.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    MSEsection10.df[i,j] = sum((est.density.section.bia[[i]][,j] - mean(sim.density.section10[[i]][,j]))^2)/nsim
  }
}
colnames(MSEsection10.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

##RME
RMEsection.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMEsection.df[i,j] = sum((est.density.section.bia[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
  }
}
colnames(RMEsection.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

RMEsection15.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMEsection15.df[i,j] = sum((est.density.section.bia[[i]][,j] - mean(sim.density.section15[[i]][,j]))/sim.tph.all)/nsim
  }
}
colnames(RMEsection15.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

RMEsection10.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMEsection10.df[i,j] = sum((est.density.section.bia[[i]][,j] - mean(sim.density.section10[[i]][,j]))/sim.tph.all)/nsim
  }
}
colnames(RMEsection10.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

##RMAE
RMAEsection.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMAEsection.df[i,j] = sum(abs(est.density.section.bia[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
  }
}
colnames(RMAEsection.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

RMAEsection15.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMAEsection15.df[i,j] = sum(abs(est.density.section.bia[[i]][,j] - mean(sim.density.section15[[i]][,j]))/sim.tph.all)/nsim
  }
}
colnames(RMAEsection15.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')

RMAEsection10.df = data.frame(matrix(NA, ncol = 4, nrow = 10))
for (i in 1:10)
{
  for (j in 1:4)
  {
    RMAEsection10.df[i,j] = sum(abs(est.density.section.bia[[i]][,j] - mean(sim.density.section10[[i]][,j]))/sim.tph.all)/nsim
  }
}
colnames(RMAEsection10.df) = c('section 1', 'section 2', 'section 3', 'seciont 4')


  