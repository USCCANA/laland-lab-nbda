load("starlingSolve1.2")

oadata<-c("nG1T1","nG1T2","nG1T4","nG1T5","nG1T6","nG2T1","nG2T2","nG2T3","nG2T4","nG2T5","nG2T6","nG3T1","nG3T2","nG3T3","nG3T4","nG3T5")

#Fit an additive OADA model to all the starling diffusions, with ILVs 1 and 4

model5<-addFit(oadata,asocialVar=c(1,4))
summary(model5)

#To find confidence intervals for s, we specify the first parameter in the list with which=1
#And specify a range over which we would like to plot it, and the number of points to be plotted with resolution
#We also need to specify the model name, data and the ILVs

plotProfLikelihood(range=c(0,10),which=1,resolution=30,model=model5,data=oadata,asocialVar=c(1,4))

#This makes it clear that the confidence intervals include zero at the lower end, and the upper bound is between 2 3 and 5
#So we ask for the upper bound in that range using upperInt=c(3,5)

profLikCI(1,upperInt=c(3,5),model=model5,data=oadata,asocialVar=c(1,4))

#Now the same process for TADA
#This should work for both gammaTadaFit and tadaFit

#Combine diffusions into one file:
tadata<-combineTadaData(c("ntG1T1","ntG1T2","ntG1T4","ntG1T5","ntG1T6","ntG2T1","ntG2T2","ntG2T3","ntG2T4","ntG2T5","ntG2T6","ntG3T1","ntG3T2","ntG3T3","ntG3T4","ntG3T5"))


model6<-gammaTadaFit(tadata)
summary(model6)

plotProfLikelihood(range=c(0,2),which=1,resolution=30,model=model6,data=tadata)

#This makes it clear that the lower bound is between 0 and 0.2, and the upper bound between 0.4 and 0.6
#So we ask for the upper bound in that range using upperInt=c(3,5)

profLikCI(1,lowerInt=c(0,0.2),upperInt=c(0.4,0.6),model=model6,data=tadata)

#Example use for baseline rate
plotProfLikelihood(range=c(5e3,6e4),which=2,resolution=30,model=model6,data=tadata)

#This makes it clear that the lower bound is between 0 and 1e4, and the upper bound between 4e4 and 5e4- find these points


profLikCI(2,lowerInt=c(0,1e4),upperInt=c(4e4,5e4),model=model6,data=tadata)

