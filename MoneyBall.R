rm(list = ls())
# import library ------------------
library(VineCopula)
library(copula)
library(MASS)
library(plot3D)
library(goftest)
library("compiler", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
# change the cmd -----------------------------
setwd("~/Documents/M052040033/Meeting/MLB/Rcode/MLBdata")

# import data---------------------------------
BPV <- read.csv("Batting Player Value.csv")
PPV <- read.csv("Pitching Player Value.csv")
PSB <- read.csv("Player Standard Batting.csv")
PSP <- read.csv("Player Standard Pitching.csv")
PSF <- read.csv("Player Standard Fielding.csv")
VA2016 <- read.csv("2016.csv")

# fix the variable class ------------
BPV$Name <- as.character(BPV$Name)
PPV$Name <- as.character(PPV$Name)
PSB$Name <- as.character(PSB$Name)
PSP$Name <- as.character(PSP$Name)
PSF$Name <- as.character(PSF$Name)
VA2016$Pitcher <- as.character(VA2016$Pitcher)
VA2016$Batter <- as.character(VA2016$Batter)
VA2016$Event <- as.character(VA2016$Event)
VA2016$Distance..ft. <- as.numeric(VA2016$Distance..ft.)

BPV <- na.omit(BPV)
PPV <- na.omit(PPV)
PSB <- na.omit(PSB)
PSP <- na.omit(PSP)
PSF <- na.omit(PSF)
VA2016 <- na.omit(VA2016)

VA2016$Angle <- VA2016$Angle + 90
Event2016 <- unique(VA2016$Event)

#VA2016$TVelocity <- log(-VA2016$Velocity + max(VA2016$Velocity)+2)

# Event class data -------------------------
Single.All <- VA2016[which(VA2016$Event == "Single"),]            #24705
Flyout.All <- VA2016[which(VA2016$Event == "Flyout"),]            #19014
HomeRun.All <- VA2016[which(VA2016$Event == "Home Run"),]         #5398
Lineout.All <- VA2016[which(VA2016$Event == "Lineout"),]          #10189
Double.All <- VA2016[which(VA2016$Event == "Double"),]            #7748
Groundout.All <- VA2016[which(VA2016$Event == "Groundout"),]      #26952
GroundedDP.All <- VA2016[which(VA2016$Event == "Grounded Into DP"),] #3202
DoublePlay.All <- VA2016[which(VA2016$Event == "Double Play"),]   #402
PopOut.All <- VA2016[which(VA2016$Event == "Pop Out"),]           #4337
SacFly.All <- VA2016[which(VA2016$Event == "Sac Fly"),]           #1127
Forceout.All <- VA2016[which(VA2016$Event == "Forceout"),]        #3072
FieldError.All <- VA2016[which(VA2016$Event == "Field Error"),]   #1257
Triple.All <- VA2016[which(VA2016$Event == "Triple"),]            #816
SacFlyDP.All <- VA2016[which(VA2016$Event == "Sac Fly DP"),]      #20
FieldersChoiceOut.All <- VA2016[which(VA2016$Event == "Fielders Choice Out"),] #199
BatterInterference.All <- VA2016[which(VA2016$Event == "Batter Interference"),]#2
TriplePlay.All <- VA2016[which(VA2016$Event == "Triple Play"),]                #6
Faninterference.All <- VA2016[which(VA2016$Event == "Fan interference"),]      #40
FieldersChoice.All <- VA2016[which(VA2016$Event == "Fielders Choice"),]        #199
BuntGroundout.All <- VA2016[which(VA2016$Event == "Bunt Groundout"),]          #1




# build Cross table----------------------
Pitcher2016 <- unique(VA2016$Pitcher)
Batter2016 <- unique(VA2016$Batter)
PBtable <- as.data.frame(matrix(data = 0,nrow = length(Batter2016), ncol = length(Pitcher2016)));rownames(PBtable) <- Batter2016; colnames(PBtable) <- Pitcher2016
EBtable <- as.data.frame(matrix(data = 0,nrow = length(Batter2016), ncol = length(Event2016)));rownames(EBtable) <- Batter2016; colnames(EBtable) <- Event2016


for(i in 1:nrow(VA2016)){
  PBtable[which(rownames(PBtable) == VA2016[i,2]),which(colnames(PBtable) == VA2016[i,1])] <- PBtable[which(rownames(PBtable) == VA2016[i,2]),which(colnames(PBtable) == VA2016[i,1])] + 1
  EBtable[which(rownames(EBtable) == VA2016[i,2]),which(colnames(EBtable) == VA2016[i,6])] <- EBtable[which(rownames(EBtable) == VA2016[i,2]),which(colnames(EBtable) == VA2016[i,6])] + 1
}

BatterScore <- apply(X = PBtable, MARGIN = 1, FUN = sum)
plot(sort(BatterScore), type = "l")

EBtable$Sum <- apply(X = EBtable, MARGIN = 1, FUN = sum)
EBtable$Score <- apply(X = EBtable[,c(1,3,5,13)], MARGIN = 1, FUN = sum)
EBtable$ScoreRate <- EBtable$Score/EBtable$Sum
plot(sort(EBtable$ScoreRate), type = "l")

GoodBtable <- EBtable[which(EBtable$Sum >= 200),]
TopBtable <- GoodBtable[which(GoodBtable$ScoreRate >= 0.375),]

Top10Batter <- rownames(TopBtable)
Top10data <- data.frame()
for(B in 1:length(Top10Batter)){
  Top10data <- rbind(Top10data, VA2016[which(VA2016$Batter == Top10Batter[B]),])
}
order(apply(X = EBtable[1:20], MARGIN = 2, FUN = sum),decreasing = T)
Top10data15 <- data.frame()
Event2016.15 <- Event2016[c(1:13,15)]
for(B in 1:length(Event2016.15)){
  Top10data15 <- rbind(Top10data15, Top10data[which(Top10data$Event == Event2016.15[B]),])
}


# copula model------
plot(VA2016$Velocity, VA2016$Angle, pch = 16, cex = 0.5, col = "blue")
plot(VA2016$TVelocity, VA2016$Angle, pch = 16, cex = 0.5) #VA2016$TVelocity <- log(-VA2016$Velocity + abs(max(VA2016$Velocity))+1.0001)
hist(VA2016$Velocity, breaks = 150, probability = T)
hist(VA2016$Angle, breaks = 200, probability = T)
hist(VA2016$TVelocity, breaks = 150, probability = T)

  Vel.dis <- fitdistr(x = VA2016$TVelocity, densfun = "normal")
  
  hist(VA2016$TVelocity, breaks = 200, col = "blue", probability = T)
  hist(x = rnorm(n = nrow(VA2016), mean = Vel.dis$estimate[1],  sd =  Vel.dis$estimate[2] ), probability = T, breaks = 200, add = T, col = "gold", density = 20)
  
  cvm.test(VA2016$TVelocity, "pnorm", Vel.dis$estimate[1],Vel.dis$estimate[2])
  
  
  Ang.dis <- fitdistr(x = VA2016$Angle, densfun = "logistic")
  
  hist(VA2016$Angle, breaks = 200, col = "blue", probability = T)
  hist(x = rlogis(n = nrow(VA2016), location = Ang.dis$estimate[1], scale = Ang.dis$estimate[2]), probability = T, breaks = 200, add = T, col = "gold", density = 20)
  
  cvm.test(VA2016$Velocity, "plogis", Ang.dis$estimate[1],Ang.dis$estimate[2])
  # pseudo observation ------
  U <- pobs(VA2016$TVelocity)
  V <- pobs(VA2016$Angle)
  plot(U,V, pch = 16, cex = 0.1)
  
  FS <- c(1,2,3,4,5,6,7,8,9,10,13,14,16,17,18,19,20,23,24,26,27,28,29,30,33,34,36,37,38,39,40,104,114,124,134,204,214,224,234)
  FSL <- data.frame()
  for(f in 1:length(FS)){
    VA.CopSelect <- BiCopSelect(u1 = U, u2 = V, familyset = FS[f], selectioncrit = "AIC")  
    FSL[f,1] <- VA.CopSelect$familyname
    FSL[f,2] <- VA.CopSelect$par
    FSL[f,3] <- VA.CopSelect$par2
    FSL[f,4] <- VA.CopSelect$logLik
  }
  colnames(FSL) <- c("familyname", "par", "par2", "logLik");FSL
  f.num <- FS[which.max(FSL$logLik)]
  
  VA.CopSelect <- BiCopSelect(u1 = U, u2 = V, familyset = NA, selectioncrit = "AIC")
  VA2016.Cop <- copulaFromFamilyIndex(family = VA.CopSelect$family, par = VA.CopSelect$par, par2 = VA.CopSelect$par2)
  contourplot2(VA2016.Cop, dCopula, nlevels = 30,main = c(VA.CopSelect$familyname),method = "flattest",labels = T,labcex = 2,col.regions = jet.col(30), xlab = "u", ylab = "v")
  VA2016Cop_dist <- mvdc(copula = VA2016.Cop, margins = c("norm", "logis"),
                         paramMargins = list(
                           list(mean = Vel.dis$estimate[1], sd = Vel.dis$estimate[2]),
                           list(location = Ang.dis$estimate[1], scale = Ang.dis$estimate[2])
                           )
                        )
  contourplot2(x = VA2016Cop_dist,  FUN = dMvdc, xlim = c(min(VA2016$TVelocity), max(VA2016$TVelocity)), ylim = c(min(VA2016$Angle), max(VA2016$Angle)),  nlevels = 25, method = "flattest",labels = T,labcex = 2,col.regions = jet.col(25))
  
  # simulation and test-----------
  VAsim <- rMvdc(n = nrow(VA2016), mvdc = VA2016Cop_dist)
  plot(VA2016$TVelocity, VA2016$Angle, pch = 16, cex = 0.2, col = "blue")
  points(VAsim, pch = 16, cex = 0.2, col = "gold")
  
  simV <- -(exp(VAsim[,1]) - 2 - max(VA2016$Velocity)) 
  
  plot(VA2016$Velocity, VA2016$Angle, pch = 16, cex = 0.2, col = "blue")
  points(simV,VAsim[,2], pch = 16, cex = 0.2, col = "gold")
  
  
  # top10 data -------------
  Top10data15
  
  plot(Top10data15$Velocity,Top10data15$Angle, pch = 16, cex = 0.5, col = "blue") 
  hist(Top10data15$Velocity, breaks = 100, probability = T)
  hist(Top10data15$Angle, breaks = 100, probability = T)
  
  M1 <- Top10data15$Velocity
  M1.par <- fitdistr(Top10data15$Velocity, "weibull")
  
  hist(M1, breaks = 100, probability = T, col = "blue")
  hist(x = rweibull(n = length(M1), shape = M1.par$estimate[1], scale = M1.par$estimate[2]), breaks = 100, probability = T, add = T, col = "gold")
  
  ks.test(x = M1, null = "pweibull", M1.par$estimate[1], M1.par$estimate[2])
  
  M2 <- Top10data15$Angle
  M2.par <- fitdistr(Top10data15$Angle, "logistic")  
  
  hist(M2, breaks = 100, probability = T, col = "blue")
  hist(x = rlogis(n = length(M2), location  = M2.par$estimate[1], scale = M2.par$estimate[2]), breaks = 100, probability = T, add = T, col = "gold")
  
  ks.test(x = M2, null = "plogis", M2.par$estimate[1], M2.par$estimate[2])
  
  # pseudo observation ------
  U15 <- pobs(M1)
  V15 <- pobs(M2)
  plot(U15,V15)
  
  VA15.CopSelect <- BiCopSelect(u1 = U15, u2 = V15, familyset = NA, selectioncrit = "logLik")
  VA15.Cop <- copulaFromFamilyIndex(family = VA15.CopSelect$family, par = VA15.CopSelect$par, par2 = VA15.CopSelect$par2)
  contourplot2(VA15.Cop, dCopula, nlevels = 30,main = c(VA15.CopSelect$familyname),method = "flattest",labels = T,labcex = 2,col.regions = jet.col(30), xlab = "u", ylab = "v")
  VA15.Cop_dist <- mvdc(copula = VA15.Cop, margins = c("weibull", "logis"),
                         paramMargins = list(
                          list(shape = M1.par$estimate[1], scale = M1.par$estimate[2]),
                          list(location = M2.par$estimate[1], scale = M2.par$estimate[2])
                          )
                        )
  contourplot2(x = VA15.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  nlevels = 25, method = "flattest",labels = T,labcex = 2,col.regions = jet.col(25))
  plot(M1,M2, pch = 16, cex = 0.3)
  
  VA15.Cop_Sim <- rMvdc(n = nrow(Top10data15), mvdc = VA15.Cop_dist)  
  points(VA15.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  
  # HR copula --------------
  HomeRun.top <- Top10data15[which(Top10data15$Event == "Home Run"),]

  plot(HomeRun.top$Velocity, HomeRun.top$Angle, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(HomeRun.top$Velocity, breaks = 50, col = "blue", probability = T)
  hist(HomeRun.top$Angle, breaks = 50, col = "blue", probability = T)
  
  M1.HR <- HomeRun.top$Velocity
  M2.HR <- HomeRun.top$Angle
  
  M1.HR.par <- fitdistr(M1.HR, "normal")
  cvm.test(M1.HR, "pnorm", M1.HR.par$estimate[1],M1.HR.par$estimate[2])
  
  M2.HR.par <- fitdistr(M2.HR, densfun = "gamma" )
  ks.test(M2.HR, "pgamma", M2.HR.par$estimate[1], M2.HR.par$estimate[2] , alternative = "less")
  hist(x = rgamma(n = length(M2.HR), shape = M2.HR.par$estimate[1], rate = M2.HR.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
    # pseudo observation---------
    U.HR <- pobs(M1.HR)
    V.HR <- pobs(M2.HR)
    plot(U.HR, V.HR, pch = 16, cex = 0.5)
    
    HR.CopSelect <- BiCopSelect(u1 = U.HR, u2 = V.HR, familyset = NA, selectioncrit = "AIC")
    HR.Cop <- copulaFromFamilyIndex(family = HR.CopSelect$family, par = HR.CopSelect$par, par2 = HR.CopSelect$par2)
    HR.Cop_dist <- mvdc(copula = HR.Cop, margins = c("norm", "gamma"),
                        paramMargins = list(
                          list(mean = M1.HR.par$estimate[1], sd = M1.HR.par$estimate[2]),
                          list(shape = M2.HR.par$estimate[1], rate = M2.HR.par$estimate[2])
                          )
                        )
    contourplot2(x = HR.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
    plot(M1.HR, M2.HR, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
    
    HR.Cop_Sim <- rMvdc(n = nrow(HomeRun.top), mvdc = HR.Cop_dist)  
    points(HR.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  #Single ------------
  Single.top <- Top10data15[which(Top10data15$Event == "Single"),]
  plot(Single.top$Velocity, Single.top$Angle, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(Single.top$Velocity, breaks = 50, col = "blue")
  hist(Single.top$Angle, breaks = 50, col = "blue")
    
  M1.S <- Single.top$Velocity
  M2.S <- Single.top$Angle
    
  M1.S.par <- fitdistr(M1.S, "weibull")
  cvm.test(x = M1.S, null = "pweibull", M1.S.par$estimate[1],M1.S.par$estimate[2] )
  hist(Single.top$Velocity, breaks = 50, col = "blue",probability = T)
  hist(x = rweibull(n = length(M1.S), shape = M1.S.par$estimate[1], scale = M1.S.par$estimate[2]), breaks = 50, col = "gold", add = T, probability = T)
  
  M2.S.par <- fitdistr(M2.S, "logistic")
  cvm.test(M2.S, "plogis", M2.S.par$estimate[1],M2.S.par$estimate[2])
    # pseudo observation---------
    U.S <- pobs(M1.S)
    V.S <- pobs(M2.S)
    plot(U.S, U.S, pch = 16, cex = 0.5)
    
    S.CopSelect <- BiCopSelect(u1 = U.S, u2 = V.S, familyset = NA, selectioncrit = "AIC")
    S.Cop <- copulaFromFamilyIndex(family = S.CopSelect$family, par = S.CopSelect$par, par2 = S.CopSelect$par2)
    S.Cop_dist <- mvdc(copula = S.Cop, margins = c("weibull", "logis"),
                        paramMargins = list(
                          list(shape = M1.S.par$estimate[1], scale = M1.S.par$estimate[2]),
                          list(location = M2.S.par$estimate[1], scale = M2.S.par$estimate[2])
                        )
    )
    contourplot2(x = S.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
    plot(M1.S, M2.S, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
    
    S.Cop_Sim <- rMvdc(n = nrow(Single.top), mvdc = S.Cop_dist)  
    points(S.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
    
    
  
  # Double copula -------
  Double.top <- Top10data15[which(Top10data15$Event == "Double"),]            #7748
    
  plot(Double.top$Velocity, HomeRun.top$Angle, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(Double.top$Velocity, breaks = 50, col = "blue", probability = T)
  hist(Double.top$Angle, breaks = 50, col = "blue", probability = T)
  
  M1.D <- Double.top$Velocity
  M2.D <- Double.top$Angle
  
  M1.D.par <- fitdistr(M1.D, "weibull")
  ks.test(M1.D, "pweibull", M1.D.par$estimate[1],M1.D.par$estimate[2])
  
  M2.D.par <- fitdistr(M2.D, densfun = "logistic" )
  ks.test(M2.D, "plogis", M2.D.par$estimate[1], M2.D.par$estimate[2] , alternative = "less")
  hist(x = rlogis(n = length(M2.D), location = M2.D.par$estimate[1], scale = M2.D.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  # pseudo observation---------
  U.D <- pobs(M1.D)
  V.D <- pobs(M2.D)
  plot(U.D, V.D, pch = 16, cex = 0.5)
  
  D.CopSelect <- BiCopSelect(u1 = U.D, u2 = V.D, familyset = NA, selectioncrit = "AIC")
  D.Cop <- copulaFromFamilyIndex(family = D.CopSelect$family, par = D.CopSelect$par, par2 = D.CopSelect$par2)
  D.Cop_dist <- mvdc(copula = D.Cop, margins = c("weibull", "logis"),
                      paramMargins = list(
                        list(shape = M1.D.par$estimate[1], scale = M1.D.par$estimate[2]),
                        list(location = M2.D.par$estimate[1], scale = M2.D.par$estimate[2])
                      )
  )
  contourplot2(x = D.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
  plot(M1.D, M2.D, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  
  D.Cop_Sim <- rMvdc(n = nrow(Double.top), mvdc = D.Cop_dist)  
  points(D.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  # Triple copula -------
  Triple.top <- Top10data15[which(Top10data15$Event == "Triple"),]            #7748
  
  plot(Triple.top$Velocity, HomeRun.top$Angle, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(Triple.top$Velocity, breaks = 50, col = "blue", probability = T)
  hist(Triple.top$Angle, breaks = 50, col = "blue", probability = T)
  
  M1.T <- Triple.top$Velocity
  M2.T <- Triple.top$Angle
  
  M1.T.par <- fitdistr(M1.T, "weibull")
  cvm.test(M1.T, "pweibull", M1.T.par$estimate[1],M1.T.par$estimate[2])
  
  M2.T.par <- fitdistr(M2.T, densfun = "logistic" )
  ks.test(M2.T, "plogis", M2.T.par$estimate[1], M2.T.par$estimate[2] , alternative = "less")
  hist(x = rlogis(n = length(M2.T), location = M2.T.par$estimate[1], scale = M2.T.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  # pseudo observation---------
  U.T <- pobs(M1.T)
  V.T <- pobs(M2.T)
  plot(U.T, V.T, pch = 16, cex = 0.5)
  
  T.CopSelect <- BiCopSelect(u1 = U.T, u2 = V.T, familyset = NA, selectioncrit = "AIC")
  T.Cop <- copulaFromFamilyIndex(family = T.CopSelect$family, par = T.CopSelect$par, par2 = T.CopSelect$par2)
  T.Cop_dist <- mvdc(copula = T.Cop, margins = c("weibull", "logis"),
                     paramMargins = list(
                       list(shape = M1.T.par$estimate[1], scale = M1.T.par$estimate[2]),
                       list(location = M2.T.par$estimate[1], scale = M2.T.par$estimate[2])
                     )
  )
  contourplot2(x = T.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
  plot(M1.T, M2.T, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  
  T.Cop_Sim <- rMvdc(n = nrow(Triple.top), mvdc = T.Cop_dist)  
  points(T.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  #SDTH data ------------------------------
  SDTH.top <- Top10data15[which(Top10data15$Event == "Single"| 
                                  Top10data15$Event == "Double"|
                                  Top10data15$Event == "Triple"|
                                  Top10data15$Event == "Home Run" ),]            #7748
  
  plot(SDTH.top$Velocity, SDTH.top$Angle, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(SDTH.top$Velocity, breaks = 50, col = "blue", probability = T)
  hist(SDTH.top$Angle, breaks = 50, col = "blue", probability = T)
  
  M1.SDTH <- SDTH.top$Velocity
  M2.SDTH <- SDTH.top$Angle
  
  M1.SDTH.par <- fitdistr(M1.SDTH, "weibull")
  ks.test(M1.SDTH, "pweibull", M1.SDTH.par$estimate[1],M1.SDTH.par$estimate[2])
  hist(x = rweibull(n = length(M1.SDTH), shape = M1.SDTH.par$estimate[1], scale = M1.SDTH.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  
  M2.SDTH.par <- fitdistr(M2.SDTH, densfun = "logistic" )
  cvm.test(M2.SDTH, "plogis", M2.SDTH.par$estimate[1], M2.SDTH.par$estimate[2])
  hist(x = rlogis(n = length(M2.SDTH), location = M2.SDTH.par$estimate[1], scale = M2.SDTH.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  # pseudo observation---------
  U.SDTH <- pobs(M1.SDTH)
  V.SDTH <- pobs(M2.SDTH)
  plot(U.SDTH, V.SDTH, pch = 16, cex = 0.5)
  
  SDTH.CopSelect <- BiCopSelect(u1 = U.SDTH, u2 = V.SDTH, familyset = NA, selectioncrit = "AIC")
  SDTH.Cop <- copulaFromFamilyIndex(family = SDTH.CopSelect$family, par = SDTH.CopSelect$par, par2 = SDTH.CopSelect$par2)
  SDTH.Cop_dist <- mvdc(copula = SDTH.Cop, margins = c("weibull", "logis"),
                     paramMargins = list(
                       list(shape = M1.SDTH.par$estimate[1], scale = M1.SDTH.par$estimate[2]),
                       list(location = M2.SDTH.par$estimate[1], scale = M2.SDTH.par$estimate[2])
                     )
  )
  contourplot2(x = SDTH.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
  plot(M1.SDTH, M2.SDTH, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  
  SDTH.Cop_Sim <- rMvdc(n = nrow(SDTH.top), mvdc = SDTH.Cop_dist)  
  points(SDTH.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  #DTH data ------------------------------
  DTH.top <- Top10data15[which(Top10data15$Event == "Double"|
                                Top10data15$Event == "Triple"|
                                Top10data15$Event == "Home Run" ),]            #7748
  
  plot(DTH.top$Velocity, DTH.top$Angle, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  hist(DTH.top$Velocity, breaks = 50, col = "blue", probability = T)
  hist(DTH.top$Angle, breaks = 50, col = "blue", probability = T)
  
  M1.DTH <- DTH.top$Velocity
  M2.DTH <- DTH.top$Angle
  
  M1.DTH.par <- fitdistr(M1.DTH, "weibull")
  ks.test(M1.DTH, "pweibull", M1.DTH.par$estimate[1],M1.DTH.par$estimate[2])
  hist(x = rweibull(n = length(M1.DTH), shape = M1.DTH.par$estimate[1], scale = M1.DTH.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  
  M2.DTH.par <- fitdistr(M2.DTH, densfun = "logistic" )
  cvm.test(M2.DTH, "plogis", M2.DTH.par$estimate[1], M2.DTH.par$estimate[2])
  hist(x = rlogis(n = length(M2.DTH), location = M2.DTH.par$estimate[1], scale = M2.DTH.par$estimate[2]), breaks = 50, add = T, col = "gold", probability = T)
  
  # pseudo observation---------
  U.DTH <- pobs(M1.DTH)
  V.DTH <- pobs(M2.DTH)
  plot(U.DTH, V.DTH, pch = 16, cex = 0.5)
  
  DTH.CopSelect <- BiCopSelect(u1 = U.DTH, u2 = V.DTH, familyset = NA, selectioncrit = "AIC")
  DTH.Cop <- copulaFromFamilyIndex(family = DTH.CopSelect$family, par = DTH.CopSelect$par, par2 = DTH.CopSelect$par2)
  DTH.Cop_dist <- mvdc(copula = DTH.Cop, margins = c("weibull", "logis"),
                        paramMargins = list(
                          list(shape = M1.DTH.par$estimate[1], scale = M1.DTH.par$estimate[2]),
                          list(location = M2.DTH.par$estimate[1], scale = M2.DTH.par$estimate[2])
                        )
  )
  contourplot2(x = DTH.Cop_dist,  FUN = dMvdc, xlim = c(min(M1), max(M1)), ylim = c(min(M2), max(M2)),  cuts = 5, method = "flattest",n.grid = 50, labels = F,labcex = 10,col.regions = jet.col(10))
  plot(M1.DTH, M2.DTH, xlim = c(min(Top10data15$Velocity), max(Top10data15$Velocity)), ylim = c(min(Top10data15$Angle), max(Top10data15$Angle)), col = "blue", pch = 16, cex = 0.3, main = "Home Run")
  
  DTH.Cop_Sim <- rMvdc(n = nrow(DTH.top), mvdc = DTH.Cop_dist)  
  points(DTH.Cop_Sim, pch = 16, cex = 0.3, col = "gold")
  
  # interval probability ----------
  Pinterval <- function(a,b,c,d,mvdc){
    pMvdc(c(a,c), mvdc) - pMvdc(c(a,d), mvdc) - pMvdc(c(b,c), mvdc) + pMvdc(c(b,d), mvdc)
  }
  Pinterval(120,80,130,110,HR.Cop_dist)
  
  
  
  
  
  
  