
#######Soil Experiment.########
library(additivityTests)

#Uploading Data File
soilEx <- read.csv(file.choose())
summary(soilEx)
attach(soilEx)


#Johnson - GrayBill
critical.values(nrow(c), ncol(c), 0.01)

#Settings up Factors.

Soil <- as.factor(Soil)
treatcode <- as.factor(TreatCode)
Treatment <- as.factor(Treatment)

summary(soilEx)


#Two Way Anova

res.aov2 <- aov(Yield ~ treatment + soil, data = soilEx)
summary(res.aov2)

interaction.plot(Soil,Treatment, Yield, data = soilEx)
interaction.plot(treatment,soil, Yield)

#########Project ###################


library(additivityTests)

##########Testing - Data - 1 Soil Experiment ###############

##Treatment As Factors
##Soil Type as Factors

##Yield As Response

#Building Matrix of Soil - Stack.
c = matrix(
  c(11.1, 15.3, 22.7, 23.8,25.6,31.2,25.8,32.6,40.8,52.1,52.8,63.1,59.5,55.3,63.3,65,58.8,61.4,41.1,78.1,60.2),
  nrow=7,
  ncol=3
)


#Hidden
mandel.test(c)
mtukey.test(c)
tukey.test(c)

#Not Hidden
tusell.test(c)
lbi.test(c)
johnson.graybill.test(c)


#########Testing - Data - 2 - Wine Fermentation ##########


wine <- read.csv(file.choose())
attach(wine)

####Three Factors ####

#Grape Variety (Grape Var) at 4 Levels : Concord, Iona, S9549, S5409
#Yest Strain (YeastStr) at 5 Levels: VY09, VY21, VY74, VY72
#Temperature(Temp of Fermentation) at 3 levels: 80, 50 ,80 

###Two Response Variables ###

#Alcohol % - AlcPct
#Total Alcohol Content (TAC)


##Doing 2 stacks for each response variable.


###AlcPCT #####

##Creaint sub plots

par(mfrow=c(2,1))
interaction.plot(GrapeVar,Temp, AlcPct)
interaction.plot(YeastStr,GrapeVar, AlcPct)

##Nothing unsual Dont Test
res.aov3 <- aov(AlcPct ~ Temp + YeastStr)
summary(res.aov3)


##Rejection on both
res.aov <- aov(AlcPct ~ Temp + GrapeVar)
summary(res.aov)
interaction.plot(GrapeVar,Temp, AlcPct)

###New Stack for Data above
temp_grap <- matrix(
            c(7.6,7.9,7.9,8.0,7.8,10.1,9.8,9.6,8.9,8.1,11.4,10,11.3,10.6,10,10.3,9.9,9.7,10.2,8.7,
              8.0,8.2,8.2,8.1,7.9,10.4,10.3,9.9,10.4,10.4,11.6,11.4,11.5,11.6,11.4,10.4,10.4,10.2,10.2,10.3)
            ,nrow = 20,
            ncol = 3
)


mandel.test(temp_grap)
mtukey.test(temp_grap)
tukey.test(temp_grap)

#Not Hidden
tusell.test(temp_grap)
lbi.test(temp_grap)
johnson.graybill.test(temp_grap)




##Rejection on Grape Variable.
res.aov2 <- aov(AlcPct ~ GrapeVar + YeastStr)
summary(res.aov2)
interaction.plot(GrapeVar,YeastStr, AlcPct)


###New Stack Testing - Treating Each-Strain and Temp as 1 variable, Straing ~ GrapeVar
test <- matrix(
        c(7.6,8.1,8.0,7.9,8.1,8.2,7.9,8.2,8.2,8.0,8.3,8.1,7.8,8.0,7.9,10.1,10.2,10.4,9.8,
          10.1,10.3,9.6,10.3,9.9,8.9,10.1,10.4,8.1,10.3,10.4,11.4,11.6,11.6,10.0,11.2,11.4,
          11.3,11.6,11.5,10.6,11.7,11.6,10.0,11.5,11.4,10.3,10.4,10.4,9.9,10.6,10.4,9.7,10.1,10.2,10.2,10.3,10.2,8.7,9.7,10.3
        ),
        nrow = 15,
        ncol = 4
)

mandel.test(test)
tusell.test(test)
lbi.test(test)
johnson.graybill.test(test)

#Not Hidden
mtukey.test(test)
tukey.test(test)




####TAC - total alcohol content ###

par(mfrow=c(2,1))
interaction.plot(GrapeVar,Temp, TAC)
interaction.plot(YeastStr,GrapeVar, TAC)

nres1.aov <- aov(TAC ~ Temp + YeastStr)
nres2.aov <- aov(TAC ~ Temp + GrapeVar)
nres3.aov <- aov(TAC ~ GrapeVar + YeastStr)
summary(nres1.aov) #Rejection on Temp 
summary(nres2.aov) #Double Rejection
summary(nres3.aov) #Rejection on Grape Var



###Temp vs YeastStr

interaction.plot(Temp,YeastStr, TAC)
interaction.plot(YeastStr,Temp, TAC)


temp_y2 <- matrix(
  c(1.05,1.19,0.98,1.17,1.06,1.83,1.00,1.19,1.15,1.22,1.03,1.35,1.06,1.26,1.03,1.14,1.09,1.23,0.93,1.47,
    0.98,1.14,0.72,1.10,1.04,1.14,0.96,1.12,0.98,1.14,0.78,1.13,0.97,1.15,0.79,1.12,1.00,1.11,0.87,1.14,
    1.11,1.36,0.94,1.17,1.29,1.42,0.93,1.01,1.15,1.44,0.90,1.02,1.03,1.20,0.84,1.03,1.18,1.33,0.90,1.09)
    ,
  nrow = 20,
  ncol = 3
)

#No Rejection
mandel.test(temp_y2)
tusell.test(temp_y2)
lbi.test(temp_y2)
johnson.graybill.test(temp_y2)
mtukey.test(temp_y2)
tukey.test(temp_y2)


###Temp vs GrapVar###

interaction.plot(Temp,GrapeVar, TAC)
interaction.plot(GrapeVar,Temp, TAC)

temp_G2 <- matrix(
  c(1.05,1.06,1.15,1.06,1.09,1.19,1.83,1.22,1.26,1.23,0.98,1.00,1.03,1.03,0.93,
    1.17,1.19,1.35,1.14,1.47,0.98,1.04,0.98,0.97,1.00,1.14,1.14,1.14,1.15,1.11,
    0.72,0.96,0.78,0.79,0.87,1.10,1.12,1.13,1.12,1.14,1.11,1.29,1.15,1.03,1.18,
    1.36,1.42,1.44,1.20,1.33,0.94,0.93,0.90,0.84,0.90,1.17,1.01,1.02,1.03,1.09)
  ,
  nrow = 20,
  ncol = 3
)

#All Rejection
mandel.test(temp_G2)
tusell.test(temp_G2)
lbi.test(temp_G2)
johnson.graybill.test(temp_G2)
mtukey.test(temp_G2)
tukey.test(temp_G2)



###GrapVar vs Yeast


interaction.plot(YeastStr,GrapeVar, TAC)
interaction.plot(GrapeVar,YeastStr, TAC)

y_g <- matrix(
  c(1.05,0.98,1.11,1.06,1.04,1.29,1.15,0.98,1.15,
    1.06,0.97,1.03,1.09,1.00,1.18,1.19,1.14,1.36,1.83,1.14,
    1.42,1.22,1.14,1.44,1.26,1.15,1.20,1.23,1.11,1.33,0.98,
    0.72,0.94,1.00,0.96,0.93,1.03,0.78,0.90,1.03,0.79,0.84,
    0.93,0.87,0.90,1.17,1.10,1.17,1.19,1.12,1.01,1.35,1.13,
    1.02,1.14,1.12,1.03,1.47,1.14,1.09),
    nrow = 15,
    ncol = 4
)


#No Rejection
mandel.test(y_g)
mtukey.test(y_g)
tukey.test(y_g)


#Rejected
lbi.test(y_g)
johnson.graybill.test(y_g)
tusell.test(y_g)


####Poisson####

ps <- read.csv(file.choose())

ps2 <-read.csv(file.choose())


attach(ps2)
interaction.plot(Poison,Treatments, Time)

interaction.plot(Treatments,Poison,Time)
pm <- matrix(c(0.31,0.82,0.43,0.45,0.36,0.92,0.44,0.56,0.22
               ,0.30,0.23,0.30), nrow = 4, ncol = 3)


#Rejected
mtukey.test(pm)
tukey.test(pm)
mandel.test(pm)


#not Rejected
lbi.test(pm)
johnson.graybill.test(pm)
tusell.test(pm)

