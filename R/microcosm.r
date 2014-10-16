# Project: Microcosm Diversity Experiment started 9-2007
# Author: David LeBauer
# Data:  
# Analyses: 
# 

# NOTES
# df = dataframe
# M = matrix
# V = vector


## WORKING DIRECTORY
setwd(dir = "C:/Users/dlebauer/Documents/GitHub/lebauer2010ldr/data/" )
## Linux:
#setwd (dir = "~/Research/Microcosm/Rcode" )

##############
# Libraries
#############
library(nlme)
library(lattice)
library('doBy')
library('SIN')
library('alr3')

###### 
# functions
######


#############
# Data: Import, Merge, subset to df: dat.all and dat.live
#############

#df######## 
# dat.mono
# raw functional trait data from Monoculture Experiment (3 reps / spp except 6 for Collybia)
#########

dat.mono <-
  read.csv ( "dat.mono.csv", header = TRUE )
  # name mglitter iso.let iso.num block    ppo     bg    nag totalCO2 ugC_mg

#df########
# mono.traits
# 
#########

#df#########
# mono.traits
# find means from dat.mono and merge into mono.traits
 bg.i <- summaryBy( bg ~ iso.let,
   na.rm=TRUE,
   data=dat.mono,        
   FUN=c(mean,sd)
   )
 ppo.i <-  summaryBy( ppo ~ iso.let,
   na.rm=TRUE,
   data=dat.mono,        
   FUN=c(mean,sd)
   )
 nag.i <-  summaryBy( nag ~ iso.let,
   na.rm=TRUE,
   data=dat.mono,        
   FUN=c('mean','sd')
   )
 ugC.i <-  summaryBy( ugC_mg ~ iso.let,
   na.rm=TRUE,
   data=dat.mono,        
   FUN=c(mean,sd)
   )
 foo <- merge(bg.i,ppo.i)
 foo<- merge(foo,nag.i)

mono.traits <- merge(foo,ugC.i)

#########
# Diversity Experiment
#########

dat.enz <-
  read.csv  ( "Microcosm_enz_final.csv", header = TRUE )
  # id substrate vial BG PPO

#df #####
# dat.all
# Calculations were done in Excel, but could be done through matrix manipulation within dat.all and subsets like spp, nsp, and obs.c.tot
######

dat.dcdt <-
  read.csv ("Microcosm_dcdt_final.csv", header = TRUE)

  #id substrate vial rep	spdiversity	fndiv	g_litter	tube
  #A B C D E F G H I J K L M N O P
  #t1 dt.1 t2 dt.2 t3 dt.3  t4 dt.4 t5 dt.5 t6 dt.6 t7 dt.7
  #dc.1	dc.2 dc.3 dc.4 dc.5 dc.6 dc.7 c.total

  #colnames =  "id"  "substrate"  "vial"  "tube"
  #"rep"  "spdiversity"  "fndiv"  "obs.c.tot"
  #"Dt" "Dmax" "g_litter" "sumplogp")

dat.iso.id <-
 read.csv ( "isolate.id.csv", header = TRUE )
 # id vial microcosm.tray iso.all iso1 iso2 iso3 iso4 iso5 iso6 iso7 iso8


 foo <- merge ( dat.enz , dat.dcdt , by = "id")
dat.all <- merge (dat.iso.id, foo , by = "id")
 dat.all$Dt <- as.numeric (dat.all$Dt)
 dat.all$Dt.2 <- as.numeric (dat.all$Dt.2)
 dat.all$obs.c.tot<-as.numeric(dat.all$obs.c.tot)
 dat.all$c.total <- dat.all$obs.c.tot/dat.all$g_litter
 dat.all$substrate <- dat.all$substrate.x
write.csv ( dat.all, "dat.all.csv" )
#attach(dat.all)

#df########
# dat.live 
# dat.dead
# remove microcosms with low yield, e.g. dead microcosms
    # MUST DEAL WITH THESE SEPARATELY...
    #WHICH ONES DIDNT GROW ?
dat.live <- dat.all[dat.all$obs.c.tot>20,]
write.csv ( dat.live, "dat.live.csv" )
attach ( dat.live )

#M########
## spp
# matrix of 1/0 for presence/absence of sp.i
  spp <- cbind (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)

#M########
# nsp
# total species in a given row
  nsp <- rowSums( spp ,  na.rm = FALSE, dims = 1)

#df#############
# dat.mono2
# bring names in to second set of monoculture data
 sp.code <- unique(dat.mono1 [,c(1,3)]) # create matrix of letter / name combo
 dat.mono2 <- merge ( dat.all[which (spdiversity==1),],sp.code) #monoculture data from expt2 



#############
# Analysis
############# 
#
#

##############
# Check Data
##############
# Distributions 


# 1. compare histograms of total CO2 production across all microcosms, and CO2 production across microcosms that yielded significantly more than 0, specifically, I removed tubes with less than 20 obs.c.tot (units = ug C?)

#fig######
# dist. of decomp responses with and without "dead" vials.
# conclusion: normal enough, with high sample size, suatable for regression
par ( mfrow = c(1,2))
hist(dat.all$obs.c.tot, breaks=20, main = "dat.all")
hist(dat.live$obs.c.tot, breaks=20, main = "dat.live")


#fig########
#  
# histograms
# good enough for large sample size (?)
par (mfrow = c (2,2))
hist(obs.c.tot, breaks=20)
hist(Dt, breaks=20)
hist(log10(ppo), breaks=20)
hist(log10(bg), breaks=20)


#fig########
# 
# enz. vs. decomposition
# no relationship

par (mfrow = c(2,1))
plot(log10(ppo[1:190]+100)-2,obs.c.tot[1:190])
plot(log10(bg+10),obs.c.tot)



#MONOCULTURES

png(filename = "monoculture.png", width = 960, height = 480, units = "px",
         pointsize = 12, bg = "white")

par ( mfrow = c(1,2))
# graph original monoculture data
  attach (dat.mono)
  x<- rank(bg)# log10(bg+100)-2
  y<- rank(ppo)#log10(ppo+100)-2

  xlabel <- "bg"
  ylabel <- "ppo"

  plot ( x, y ,
    type = 'n',
    xlab = xlabel,
    ylab = ylabel)
  text(x, y,
   labels= iso.let)



x<- log10( bg)
y<- log10( ppo)
  plot ( x,y,
    type = 'n',
    xlab = xlabel,
    ylab = ylabel)
  text(x, y,
    labels= iso.let)

           
dev.off()


# second monoculture data set
#attach ( dat.mono2)
   # x <-rank(BG[which(substrate.x=="a")])
   # y <- rank(PPO[which(substrate.x=="a")])
x<- log10( BG)
y<- log10( PPO)
  plot ( x,y,
    type = 'n',
    xlab = xlabel,
    ylab = ylabel)
  text(x, y,
    labels= iso.let)


