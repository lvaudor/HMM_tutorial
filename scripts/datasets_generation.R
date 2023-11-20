setwd("C:/Documents and Settings/lvaudor/Bureau/tutoRial/TUTOS/HMM/tuto_HMM_dev/chapitres")

datawood<- read.csv("data_original/donnees_herve_arrangees.csv", sep=";", dec=",")
wood=datawood$nbre.embacle
obsWood=rep("nothing",length(wood))
obsWood[which(wood>10)]="wood"

write.table(data.frame(PK=datawood$PK,obsWood),"data/wood.csv",sep=";", row.names=FALSE)


######################################################################################

dataDurance <- read.csv("data_original/dataElise_Durance.txt", sep=";")
attach(dataDurance)

logit=function(p){
  p[p<=0]=1e-3
  p[p>=1]=0.999
  log(p/(1-p))
}
datacp=data.frame(
  x=m_dist_axe,
  f.slope=log(m_pente),
  f.sinuosity=log(m_dist_axe/m_dist_dir),
  f.vall_bott_W=log(m_lrgm_fdv),
  f.wat_cov=logit(m_surf_eau/m_surf_ba),
  f.act_chan_cov=log(m_surf_ba/m_surf_bv),
  f.act_chan_vW=log(m_eiqba+1)
)

write.table(datacp,"data/dataDurance.csv",sep=";", row.names=FALSE)