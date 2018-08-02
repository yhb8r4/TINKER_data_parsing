library(ggplot2)
library(xlsx)
library(data.table)
library(gdata)

ELEC<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/ELEC.txt'
EXCH<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/EXCH.txt'
FFCOV<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/FFCOV.txt'
FFE<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/TOTAL.txt'
FFELEC<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/FFELEC.txt'
FFVDW<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/FFVDW.txt'
KINE<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/KINETIC.txt'
POL<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/POLAR.txt'
TEMP<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/TEMP.txt'
TOTAL<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/TOTAL.txt'
INVARIANT<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/INVARIANT.txt'
DISP<-'/Users/ybui/Documents/SCRAPS/Rscript/5WATERS/DISP.txt'

simlength<-1000
col.names<-c('EFP.ELEC', 'EFP.EXCH', 'FF.ENERGY', 'FF.COVALENT', 'FF.ELEC', 'FF.VDW', 'KINETIC', 'EFP.POLAR', 'TEMP', 'FF.TOTAL', 'TIMESTEP', 'INVARIANT','EFP.DISP')

df_elec<-read.table(ELEC, header=FALSE, sep="", nrow=simlength )
df_exch<-read.table(EXCH, header=FALSE, sep="", nrow=simlength)
df_FFCOV<-read.table(FFCOV, header=FALSE, sep="", nrow=simlength )
df_FFE<-read.table(FFE, header=FALSE, sep="", nrow=simlength)
df_FFELEC<-read.table(FFELEC, header=FALSE, sep="", nrow=simlength )
df_FFVDW<-read.table(FFVDW, header=FALSE, sep="", nrow=simlength)
df_KINE<-read.table(KINE, header=FALSE, sep="", nrow=simlength )
df_POL<-read.table(POL, header=FALSE, sep="", nrow=simlength)
df_TEMP<-read.table(TEMP, header=FALSE, sep="", nrow=simlength )
df_TOTAL<-read.table(TOTAL, header=FALSE, sep="", nrow=simlength)
df_INVARIANT<-read.table(INVARIANT, header=FALSE, sep="", nrow=simlength)
df_DISP<-read.table(DISP, header=FALSE, sep="", nrow=simlength)

df_main<-as.data.frame(matrix(nrow=simlength, ncol=13))

df_main[1]<-df_elec[3]
df_main[2]<-df_exch[4]
df_main[3]<-df_FFCOV[4]
df_main[4]<-df_FFE[3]
df_main[5]<-df_FFELEC[4]
df_main[6]<-df_FFVDW[4]
df_main[7]<-df_KINE[3]
df_main[8]<-df_POL[3]
df_main[9]<-df_TEMP[3]
df_main[10]<-df_TOTAL[3]
df_main[11]<-seq(1:1000)
df_main[12]<-df_INVARIANT[2]
df_main[13]<-df_DISP[3]

colnames(df_main, do.NULL=TRUE, prefix="col")
colnames(df_main)<-col.names

df_main[df_main==0] <- NA
df_main = na.omit(df_main)

timestep<- df_main$TIMESTEP
efp_elec <- df_main$EFP.ELEC
efp_xr <- df_main$EFP.EXCH
efp_pol <- df_main$EFP.POLAR
efp_disp <- df_main$EFP.DISP
efp_total<- df_main$EFP.ELEC + df_main$EFP.EXCH + df_main$EFP.POLAR + df_main$EFP.DISP
efp_pol_elec <- df_main$EFP.POLAR + df_main$EFP.ELEC
efp_xr_disp <- df_main$EFP.EXCH + df_main$EFP.DISP
ff_elec_vdw <- df_main$FF.ELEC + df_main$FF.VDW
ff_covalent <- df_main$FF.COVALENT
ff_elec <- df_main$FF.ELEC
ff_vdw <- df_main$FF.VDW

df = cbind.data.frame(timestep, efp_total, efp_elec,efp_xr, efp_pol, efp_disp, efp_pol_elec, efp_xr_disp, ff_elec_vdw, ff_covalent, ff_elec, ff_vdw)


#function to plot non-covalent interactions.
gen.efp.ff <- function(x) {
  TIMESTEP<-x$timestep
  efp_total <-x$efp_total
  efp_pol_elec <-x$efp_pol_elec
  efp_xr_disp<-x$efp_xr_disp
  TITLE <- deparse(substitute(x))
  
  p<-ggplot(x)
  p<- p + geom_line(data=x, aes(x=timestep,y=efp_total, color='efp_total'))
  #    p<- p + geom_line(data=x, aes(x=timestep,y=efp_pol_elec, color='efp_pol_elec'))
  p<- p + geom_line(data=x, aes(x=timestep,y=efp_elec, color='efp_elec'))
  #  p<- p + geom_line(data=x, aes(x=timestep,y=efp_disp, color='efp_disp'))
  #  p<- p + geom_line(data=x, aes(x=timestep,y=efp_pol, color='efp_pol'))
  #  p<- p + geom_line(data=x, aes(x=timestep,y=efp_xr, color='efp_xr'))
  #    p<- p + geom_line(data=x, aes(x=timestep,y=efp_xr_disp, color='efp_xr_disp'))
  p<- p + geom_point(data=x, aes(x=timestep,y=ff_elec_vdw, color='ff_elec_vdw'))
  #  p<- p + geom_point(data=x, aes(x=timestep,y=ff_covalent, color='ff_covalent'))
  p<- p + geom_point(data=x, aes(x=timestep,y=ff_elec, color='ff_elec'))
  p<- p + geom_point(data=x, aes(x=timestep,y=ff_vdw, color='ff_vdw'))
  p<- p + ggtitle(TITLE)
  p<- p + xlab("TIMESTEP (fs)")
  p<- p + ylab("ENERGY (Hartree)")
  p<- p + theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.text=element_text(size=15), legend.title=element_text(size=15))
  #  p<- p + xlim(2500, 5000)
  #  p<- p + ylim(-0.02, 0.002)
  #  p<- p + ylim(-50, 0)
  #  p<- p + ylim(-5,5)
  #  p<- p + ylim(-50,.025)
  print(p)  
}

#function to plot covalent and non-covalent interactions. 
gen.energy <- function(x) {
  TIMESTEP<-x$TIMESTEP
  TOTAL <-x$POTENTIAL
  KINETIC <-x$KINETIC
  INVARIANT<-x$INVARIANT
  TITLE <- deparse(substitute(x))
  
  p<-ggplot(x)
  p<- p + geom_line(data=x, aes(x=TIMESTEP,y=KINETIC, color='KINETIC'))
  p<- p + geom_line(data=x, aes(x=TIMESTEP,y=TOTAL, color='TOTAL'))
  p<- p + geom_line(data=x, aes(x=TIMESTEP,y=INVARIANT, color='INVARIANT'))
  #    p<- p + geom_point(data=x, aes(x=TIMESTEP,y=FF.ENERGY, color='FF.ENERGY'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP,y=ELEC, color='efp.ELEC'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP,y=EXCH, color='efp.EXCH'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP,y=POLAR, color='efp.POLAR'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP,y=DISP, color='efp.DISP'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP, y=FF.ELEC, color='FF.ELEC'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP, y=FF.VDW, color='FF.VDW'))
  #  p<- p + geom_point(data=x, aes(x=TIMESTEP, y=FF.COVALENT, color='FF.COVALENT'))
  p<- p + ggtitle(TITLE)
  p<- p + xlab("TIMESTEP (fs)")
  p<- p + ylab("ENERGY (Hartree)")
  #  p<- p + xlim(2400, 2450)
  #  p<- p + ylim(-.7, -0.8)
  #  p<- p + ylim(0.017150, 0.017175)
  print(p)
}

#function to plot temperature.
#gen.temperature <- function(x){
#  TIMESTEP<-x$TIMESTEP
#  TEMP <- x$TEMPERATURE
#  TITLE <- deparse(substitute(x))

#  p<-ggplot(x, aes(x=TIMESTEP, y=TEMP))
#  p<- p + geom_line(data=x, aes(x=TIMESTEP,y=TEMP, color='TEMP'))
#  p<- p + ggtitle(TITLE)
#  p<- p + xlab("TIMESTEP (fs)")
#  p<- p + ylab("TEMPERATURE (K)")

#  print(p)
#}


