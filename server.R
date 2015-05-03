library(shiny)
library(rgdal) 
library(rgeos) 
library(mapproj) 
library(maptools) 
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)

FSU_Reg<-readOGR(dsn = "data/FSU.shp", layer = "FSU",encoding = "utf8", stringsAsFactors = FALSE, verbose = FALSE)

Agglo<-readOGR(dsn = "data/DARIUS_points.shp" , layer = "DARIUS_points", encoding = "utf8", stringsAsFactors = FALSE, verbose = FALSE)

Shape <- "Shape"
assign(Shape, FSU_Reg)

name_col <- as.data.frame(colnames(Agglo@data))
name_col$NumCol <- rownames(name_col)
colnames(name_col) <- c("name", "NumCol")

selectParamToRun <- function(calibration, n_trait){
  x <- calibration
  x <- subset(x, overflow == 0)
  x <- subset(x,nTrait == n_trait)
  sortedX <- x[order(x$distribution) , ]
  return(sortedX[1,])
}


plotAttribute <- function(attribute, Attcolor, Attnames) {
  title(paste(attribute," cities", sep=""))
  legend("topleft", legend=Attnames, bty="n", fill=Attcolor, cex=0.8,title = "City status")
  arrows(par()$usr[1] + 100000, 
         par()$usr[3] + 100000,
         par()$usr[1] + 1000000, 
         par()$usr[3] + 100000,
         lwd = 2, code = 3,
         angle = 90, length = 0.05)
  text(par()$usr[1] + 505000, 
       par()$usr[3] + 270000, 
       "1000 km", cex = 0.8)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  mariuscsv <- reactiveValues(mariusperiod1 = "data/default-marius1959-1989.csv", mariusperiod2 = "data/default-marius1989-2010.csv")
  
  output$map1 <- renderPlot({ 
    Year <- subset(name_col, name == input$Census_year)
    Year <- as.numeric(Year[1,2])
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1, ann=FALSE, axes=FALSE)
    plot(Agglo, pch = 21,
         cex = input$sizefactor1 * sqrt(Agglo@data[, Year ]/ pi), 
         col = "gray30", bg = "dodgerblue", lwd = 0.5,
         add = TRUE)
    title(paste("City population | ", substr(input$Census_year, 4, 7), sep=""))
    leg <- c(15000, 1000, 100, 10)
    legend("topleft",legend = leg, pch = 21,
           col = "gray30", pt.bg = "dodgerblue",
           pt.cex = input$sizefactor1 * sqrt(leg / pi),
           bty = "n", cex=0.8, title = "Population in agglomerations (* 1000)")
    arrows(par()$usr[1] + 100000, 
           par()$usr[3] + 100000,
           par()$usr[1] + 1000000, 
           par()$usr[3] + 100000,
           lwd = 2, code = 3,
           angle = 90, length = 0.05)
    text(par()$usr[1] + 505000, 
         par()$usr[3] + 270000, 
         "1000 km", cex = 0.8)  
  })
  
  
  output$map1bis <- renderPlot({ 
  Year1 <- subset(name_col, name == input$Census_year1)
  Year1 <- as.numeric(Year1[1,2])
  Year2 <- subset(name_col, name == input$Census_year2)
  Year2 <- as.numeric(Year2[1,2])
  NumYear1 <- as.numeric(substr(input$Census_year1, 4, 7))
  NumYear2 <- as.numeric(substr(input$Census_year2, 4, 7))
  InterCensusTime <- NumYear2 - NumYear1
   Agglo@data$Change <- 100 * (((Agglo@data[,Year2] / Agglo@data[,Year1]) ^ (1/InterCensusTime)) -1)
  vPal9 <- rev(brewer.pal(n = 8, name = "RdBu"))
  vcut <- c(-5, -1, -0.5, 0, 0.5, 1, 2, 5, 15)
  Agglo@data$VarCut<- as.character(cut(Agglo@data$Change,
                                       breaks = vcut,
                                       labels = vPal9,
                                       include.lowest = TRUE,
                                       right = FALSE))
  vLegendBox <- as.character(levels(cut(Agglo@data$Change,
                                        breaks = vcut,
                                        include.lowest = TRUE,
                                        right = FALSE)))
  par(mar = c(0,0,1,0))
  plot(Shape, col="gray69", border="white", lwd = 1)
  
   plot(Agglo, pch = 21,
       cex = input$sizefactor1 * sqrt(Agglo@data[,Year2] / pi), 
       col = "gray30", bg = Agglo@data$VarCut, lwd = 0.5,add = TRUE)
  title(paste("Change in Population | ", NumYear1, "-", NumYear2, sep=""))
  leg <- c(15000, 1000, 100, 10)
  legend("topleft",legend = leg, pch = 21,
         col = "white", pt.bg = "grey",
         pt.cex = input$sizefactor1 * sqrt(leg / pi),
         bty = "n", cex=0.8, title = paste("Population in ", NumYear2, sep=""))
  legend("topright", legend=vLegendBox, bty="n", fill=vPal9, cex=0.8,title = "Annual growth rate (%)")
  arrows(par()$usr[1] + 100000, 
         par()$usr[3] + 100000,
         par()$usr[1] + 1000000, 
         par()$usr[3] + 100000,
         lwd = 2, code = 3,
         angle = 90, length = 0.05)
  text(par()$usr[1] + 505000, 
       par()$usr[3] + 270000, 
       "1000 km", cex = 0.8)
})
  

output$map1ter <- renderPlot({ 
  attribute <- input$attribute
  
   if (attribute == "Regional Capital") {
    Agglo@data$AttCol <- ifelse(Agglo@data$Capital == 1, "red", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$Capital == 1, 0.8, 0.05)
  #  Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("red", "black")
    Attnames <- c("Capital city", "Other city")
  par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
    
  }
  
  if (attribute == "National Capital") {
    Agglo@data$AttCol <- ifelse(Agglo@data$Nat.Capita == 1, "coral", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$Nat.Capita == 1, 1.5, 0.05)
  #  Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("coral", "black")
    Attnames <- c("Capital city", "Other city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
  
  if (attribute == "Mono Industry") {
    Agglo@data$AttCol <- ifelse(Agglo@data$MONOGOROD == 1, "darkmagenta", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$MONOGOROD == 1, 0.6, 0.05)
    Attcolor <- c("darkmagenta", "black")
    Attnames <- c("Mono Industry city (Russia only)", "Other city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
  
  if (attribute == "Oil&Gas located") {
    Agglo@data$AttCol <- ifelse(Agglo@data$Hydrocarbo == 1, "deepskyblue4", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$Hydrocarbo == 1, 0.6, 0.05)
  #  Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("deepskyblue4", "black")
    Attnames <- c("Oil&Gas city", "Other city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
  
  if (attribute == "Coal Located") {
    Agglo@data$AttCol <- ifelse(Agglo@data$Coal == 1, "dodgerblue3", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$Coal == 1, 0.6, 0.05)
  #  Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("dodgerblue3", "black")
    Attnames <- c("Coal city", "Other city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
  
  if (attribute == "Airport") {
    Agglo@data$AttCol <- ifelse(Agglo@data$AIRPORT == 1, "deeppink3", "black")
    Agglo@data$AttCex <- ifelse(Agglo@data$AIRPORT == 1, 0.6, 0.05)
   # Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("deeppink3", "black")
    Attnames <- c("Airport city", "Other city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
  
  if (attribute == "East/West") {
    Agglo@data$AttCol <- ifelse(Agglo@data$West.East == "W", "goldenrod", "aquamarine3")
    Agglo@data$AttCex <- ifelse(Agglo@data$West.East == "W", 0.3, 0.3)
    #Agglo@data$AttCex <- Agglo@data$AttCex * input$sizefactor1
    Attcolor <- c("goldenrod", "aquamarine3")
    Attnames <- c("Western city", "Eastern city")
    par(mar = c(0,0,1,0))
    plot(Shape, col="gray69", border="white", lwd = 1)
    plot(Agglo, pch = 21, cex = Agglo@data$AttCex, 
         col = Agglo@data$AttCol, bg = Agglo@data$AttCol, add = TRUE)
    plotAttribute(attribute, Attcolor, Attnames)
  }
})

  output$graph1 <- renderPlot({ 
    significant <- input$pvalue / 100
    current_generation <- "400000"
    
    calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
    calibration <- subset(calibration, distribution != "Inf")
    
    calibration$Bonus_ <- calibration$fr.geocites.gugus.balance.Bonus
    calibration$Cost_ <- calibration$fr.geocites.gugus.transaction.FixedCostTransaction
    calibration$Resources_ <- calibration$fr.geocites.marius.SubSurfaceResources
    calibration$Redistribution_ <- calibration$fr.geocites.marius.DoubleRedistribution
    calibration$Transition_ <- calibration$fr.geocites.gugus.urbanisation.UrbanTransition
    calibration$X1959_1989_ <- calibration$fr.geocites.marius.From1959To1989
    
    model <- lm(distribution ~ Bonus_ + 
                  Cost_ +
                  Resources_ + 
                  Redistribution_ +
                  Transition_ +
                  X1959_1989_, 
                data=calibration)
  
    summary(model)
    coef <- summary(model)$coefficient
    data <- as.data.frame(coef[,1])
    sign <- as.data.frame(coef[,4])
    colnames(data) <- c("DistanceToData")
    colnames(sign) <- c("Significant")
    sign$Significant <- ifelse(sign$Significant < significant, "1.yes", "2.no")
    sign$Mechanism <- rownames(sign)
    data$Mechanism <- rownames(data)
    data <- cbind(data, sign)
    
    p <- ggplot(aes(x=Mechanism, fill=Significant), data=data)
    plot1 <- p + geom_bar(aes(y=DistanceToData), stat="identity") +
      scale_fill_manual(values = c("dodgerblue", "gray30")) + 
      theme(axis.text=element_text(size=12) ,
            axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1))
    plot1
    
  })
  
  
output$graphinteraction <- renderPlot({ 
  significant <- input$pvalue / 100
  current_generation <- "400000"
  
  selectedMec <- input$mechanismInteractions
  interactingMec <- input$mechanismsInteractions
  
  calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
  calibration <- subset(calibration, distribution != "Inf")
  
  calibration$Bonus_ <- calibration$fr.geocites.gugus.balance.Bonus
  calibration$Cost_ <- calibration$fr.geocites.gugus.transaction.FixedCostTransaction
  calibration$Resources_ <- calibration$fr.geocites.marius.SubSurfaceResources
  calibration$Redistribution_ <- calibration$fr.geocites.marius.DoubleRedistribution
  calibration$Transition_ <- calibration$fr.geocites.gugus.urbanisation.UrbanTransition
  calibration$X1959_1989_ <- calibration$fr.geocites.marius.From1959To1989
  
 if (selectedMec != interactingMec) {
 
  if (selectedMec == "Bonus") calibration$Mech1 <- calibration$Bonus_
  if (selectedMec == "Fixed Costs") calibration$Mech1 <- calibration$Cost_ 
  if (selectedMec == "Resources") calibration$Mech1 <- calibration$Resources_
  if (selectedMec == "Redistribution") calibration$Mech1 <- calibration$Redistribution_
  if (selectedMec == "Urban Transition") calibration$Mech1 <- calibration$Transition_
  
  if (interactingMec == "Bonus") calibration$Mech2 <- calibration$Bonus_
  if (interactingMec == "Fixed Costs") calibration$Mech2 <- calibration$Cost_ 
  if (interactingMec == "Resources") calibration$Mech2  <- calibration$Resources_
  if (interactingMec == "Redistribution") calibration$Mech2  <- calibration$Redistribution_
  if (interactingMec == "Urban Transition") calibration$Mech2  <- calibration$Transition_
  
    model <- lm(distribution ~ Mech1 + 
                  Mech1 * Mech2 + 
                  Mech2 +
                                            X1959_1989_, 
                                          data=calibration)
 
  summary(model)
  coef <- summary(model)$coefficient
  data <- as.data.frame(coef[,1])
  sign <- as.data.frame(coef[,4])
  colnames(data) <- c("DistanceToData")
  colnames(sign) <- c("Significant")
  sign$Significant <- ifelse(sign$Significant < significant, "1.yes", "2.no")
  sign$Mechanism <- rownames(sign)
  data$Mechanism <- rownames(data)
  data <- cbind(data, sign)
  
  p <- ggplot(aes(x=Mechanism, fill=Significant), data=data)
  plot1 <- p + geom_bar(aes(y=DistanceToData), stat="identity") +
    scale_fill_manual(values = c("dodgerblue", "gray30")) + 
    theme(axis.text=element_text(size=12) ,
          axis.title=element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1))
  plot1
 }
})


  
 output$print1 <- renderDataTable({
   
   current_generation <- "400000"
   n_trait <- input$ntrait
   
   calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
   calibration$nTrait <- rowSums(calibration == "true") - 1
   
   
      if (input$period  == "1959-1989") calibration <- subset(calibration, fr.geocites.marius.From1959To1989 == "true") 
     if (input$period  == "1989-2010") calibration <- subset(calibration, fr.geocites.marius.From1989To2010 == "true") 
     
     
   simToRun <- selectParamToRun(calibration, n_trait)
   dist <- ncol(simToRun)-3
   dead <- ncol(simToRun)-1
   Eval <- as.data.frame(t(simToRun[1,dist:dead]))
   Eval$EvaluationGoals <- Eval[,1]
   Eval[,1] <- c("Distance To Data", "Cities without wealth", "Overflow")
   colnames(Eval) <- c("Evaluation Goals", "Evaluation value (to minimize)")
   Eval
   }, options = list(paging = FALSE, lengthMenu = FALSE, searching = FALSE, info = FALSE ))
 

output$print1ter <- renderDataTable({
  current_generation <- "400000"
  n_trait <- input$ntrait
  
  calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
  calibration$nTrait <- rowSums(calibration == "true") - 1
  
  
  if (input$period  == "1959-1989") calibration <- subset(calibration, fr.geocites.marius.From1959To1989 == "true") 
  if (input$period  == "1989-2010") calibration <- subset(calibration, fr.geocites.marius.From1989To2010 == "true") 
  
  
  simToRun <- selectParamToRun(calibration, n_trait)
  
  Parameters <- as.data.frame(t(simToRun[1,8:20]))
  colnames(Parameters) <- "Calibrated Value"
  Parameters$UsedInTheModel <- c(rep("NO",13))
  Parameters$Mechanism <- c(rep("",13))
  Parameters[1:6,"Mechanism"] <- "Generic Model"
  Parameters[1:6,"UsedInTheModel"] <- "YES"
  
  if (simToRun[,1]  == "true") {
    Parameters[7,"UsedInTheModel"]  <- "YES"
    Parameters[7,"Mechanism"]  <- "Bonus"}
  if (simToRun[,2]  == "true") {
    Parameters[8,"UsedInTheModel"]  <- "YES"
    Parameters[8,"Mechanism"]  <- "Fixed Cost Transaction"}
  if (simToRun[,3]  == "true") {
    Parameters[9:10,"UsedInTheModel"]  <- "YES"
    Parameters[9:10,"Mechanism"]  <- "SubSurface Resources"
  }
  if (simToRun[,4]  == "true") {
    Parameters[11:12,"UsedInTheModel"]  <- "YES"
    Parameters[11:12,"Mechanism"]  <- "Double Redistribution"}
  if (simToRun[,5]  == "true") {
    Parameters[13,"UsedInTheModel"]  <- "YES"
  Parameters[13,"Mechanism"]  <- "Urban Transition"}
  
  Param <- as.data.frame(rownames(Parameters))
  Param$values <- Parameters[,1]
  Param$UsedInTheModel <- Parameters[,2]
  Param$mechanism <-Parameters[,3]
  colnames(Param) <- c("Parameter name", "Calibrated Value", "Used in the model", "Associated Mechanism")
  
  Param
},options = list(paging = FALSE, lengthMenu = FALSE, searching = FALSE, info = FALSE ))

 

output$print1bis <- renderDataTable({
  
  current_generation <- "400000"
  n_trait <- input$ntrait
  significant <- input$pvalue / 100
  calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
  calibration$nTrait <- rowSums(calibration == "true") - 1
  
  
  if (input$period  == "1959-1989") calibration <- subset(calibration, fr.geocites.marius.From1959To1989 == "true") 
  if (input$period  == "1989-2010") calibration <- subset(calibration, fr.geocites.marius.From1989To2010 == "true") 
  
  
  simToRun <- selectParamToRun(calibration, n_trait)
  mechanisms <- as.data.frame(input$period)
  colnames(mechanisms) <- "Period"
  if (simToRun[,1]  == "true") mechanisms$BonusForExchanges <- "ON"
  if (simToRun[,2]  == "true") mechanisms$FixedCosts <- "ON"
  if (simToRun[,3]  == "true") mechanisms$ResourceExtraction <- "ON"
  if (simToRun[,4]  == "true") mechanisms$Redistribution <- "ON"
  if (simToRun[,5]  == "true") mechanisms$UrbanTransition <- "ON"
  
  mecha <- as.data.frame(t(mechanisms))
  mecha$State <- mecha[,1]
  mecha[,1] <- rownames(mecha)
  colnames(mecha)<- c("Model structure", "Structure State")
  mecha
},  options = list(paging = FALSE, lengthMenu = FALSE, searching = FALSE, info = FALSE ))

 
 output$cppst <-renderPrint ({
   input$go
   if (input$runingperiod  == "1959-1989") time <- "fr.geocites.marius.From1959To1989"
   if (input$runingperiod  == "1989-2010") time <- "fr.geocites.marius.From1989To2010"
   listmec <- input$mechanisms    
   
   
   if(input$go == 1) {
     
      if (input$choice_run == "Best Calibrated Model") {
          current_generation <- "400000"
           calibration <- read.csv(paste("data/population", current_generation, ".csv", sep=""), dec=".", sep=",")
     
           if (input$runingperiod  == "1959-1989") calibration <- subset(calibration, fr.geocites.marius.From1959To1989 == "true") 
            if (input$runingperiod  == "1989-2010") calibration <- subset(calibration, fr.geocites.marius.From1989To2010 == "true") 
    
              x <- calibration
              x <- subset(x, overflow == 0)
              if (any(listmec == "Bonus")) x <- subset(x,fr.geocites.gugus.balance.Bonus == "true")
              if (any(listmec == "Fixed Costs")) x <- subset(x,fr.geocites.gugus.transaction.FixedCostTransaction == "true")
              if (any(listmec == "Resources")) x <- subset(x,fr.geocites.marius.SubSurfaceResources == "true")
              if (any(listmec == "Redistribution")) x <- subset(x,fr.geocites.marius.DoubleRedistribution == "true")
              if (any(listmec == "Urban Transition")) x <- subset(x,fr.geocites.gugus.urbanisation.UrbanTransition == "true")
             sortedX <- x[order(x$distribution) , ]
             simToRun <- sortedX[1,]
            mec <- ""
     if (simToRun$fr.geocites.gugus.balance.Bonus == "true") mec <- paste(mec, "fr.geocites.gugus.balance.Bonus,", sep="")
     if (simToRun$fr.geocites.gugus.transaction.FixedCostTransaction == "true") mec <- paste(mec, "fr.geocites.gugus.transaction.FixedCostTransaction,", sep="")
     if (simToRun$fr.geocites.marius.SubSurfaceResources == "true") mec <- paste(mec, "fr.geocites.marius.SubSurfaceResources,", sep="")
     if (simToRun$fr.geocites.marius.DoubleRedistribution == "true") mec <- paste(mec, "fr.geocites.marius.DoubleRedistribution,", sep="")
     if (simToRun$fr.geocites.gugus.urbanisation.UrbanTransition == "true") mec <- paste(mec, "fr.geocites.gugus.urbanisation.UrbanTransition,", sep="")
     
     param <- paste("economicMultiplier=",simToRun$economicMultiplier,
                    ",populationToWealthExponent=",simToRun$populationToWealthExponent,
                    ",sizeEffectOnSupply=",simToRun$sizeEffectOnSupply,
                    ",sizeEffectOnDemand=",simToRun$sizeEffectOnDemand,
                    ",wealthToPopulationExponent=",simToRun$wealthToPopulationExponent,
                    ",distanceDecay=",simToRun$distanceDecay,
                    ",bonusMultiplier=",simToRun$bonusMultiplier,
                    ",fixedCost=",simToRun$fixedCost,
                    ",oilAndGazEffect=",simToRun$oilAndGazEffect,
                    ",coalEffect=",simToRun$coalEffect,
                    ",territorialTaxes=",simToRun$territorialTaxes,
                    ",capitalShareOfTaxes=",simToRun$capitalShareOfTaxes,
                    ",ruralMultiplier=",simToRun$ruralMultiplier,
                    sep="")
     
        copypaste <- paste("http://localhost:8081/run/marius1?mechanisms=", mec, time, "&parameters=", param, sep="")
       }
   
   
   if (input$choice_run == "Customised Model") {

         mec <- ""
        if (any(listmec == "Bonus")) mec <- paste(mec, "fr.geocites.gugus.balance.Bonus,", sep="")
       if (any(listmec == "Fixed Costs")) mec <- paste(mec, "fr.geocites.gugus.transaction.FixedCostTransaction,", sep="")
         if (any(listmec == "Resources")) mec <- paste(mec, "fr.geocites.marius.SubSurfaceResources,", sep="")
        if (any(listmec == "Redistribution")) mec <- paste(mec, "fr.geocites.marius.DoubleRedistribution,", sep="")
        if (any(listmec == "Urban Transition")) mec <- paste(mec, "fr.geocites.gugus.urbanisation.UrbanTransition,", sep="")
     
      param <- paste("economicMultiplier=",input$economicMultiplier,
                    ",populationToWealthExponent=",input$populationToWealthExponent,
                    ",sizeEffectOnSupply=",input$sizeEffectOnSupply,
                    ",sizeEffectOnDemand=",input$sizeEffectOnDemand,
                    ",wealthToPopulationExponent=",input$wealthToPopulationExponent,
                    ",distanceDecay=",input$distanceDecay,
                    ",bonusMultiplier=",input$bonusMultiplier,
                    ",fixedCost=",input$fixedCost,
                    ",oilAndGazEffect=",input$oilAndGazEffect,
                    ",coalEffect=",input$coalEffect,
                    ",territorialTaxes=",input$territorialTaxes,
                    ",capitalShareOfTaxes=",input$capitalShareOfTaxes,
                    ",ruralMultiplier=",input$ruralMultiplier,
                    sep="")
     
            copypaste <- paste("http://localhost:8081/run/marius1?mechanisms=", mec, time, "&parameters=", param, sep="")
          }

      download.file(copypaste, destfile = paste("data/marius", input$runingperiod, ".csv", sep=""), method = "curl")
   
   
   if (input$runingperiod  == "1959-1989") mariuscsv$mariusperiod1 <- paste("data/marius", input$runingperiod, ".csv", sep="")
   if (input$runingperiod  == "1989-2010") mariuscsv$mariusperiod2 <- paste("data/marius", input$runingperiod, ".csv", sep="")   
  
   "Done!"
   
   }
   
 })
 

output$modelcombi <- renderPrint({
  if (input$runingperiod  == "1959-1989") time <- " From 1959 To 1989"
  if (input$runingperiod  == "1989-2010") time <- " From 1989 To 2010"
  listmec <- input$mechanisms    
  mod <- "Generic Model"
  if (any(listmec == "Bonus")) mod <- paste(mod, " + Bonus", sep="")
  if (any(listmec == "Fixed Costs")) mod <- paste(mod, " + FixedCostTransaction", sep="")
  if (any(listmec == "Resources")) mod <- paste(mod, " + SubSurfaceResources", sep="")
  if (any(listmec == "Redistribution")) mod <- paste(mod, " + DoubleRedistribution", sep="")
  if (any(listmec == "Urban Transition")) mod <- paste(mod, " + UrbanTransition", sep="")
  paste("Model combination : ", input$choice_run, " | ", mod, time, sep="")
})

output$modelcombi2 <- renderPrint({
  if (input$runingperiod  == "1959-1989") time <- " From 1959 To 1989"
  if (input$runingperiod  == "1989-2010") time <- " From 1989 To 2010"
  listmec <- input$mechanisms    
  mod <- "Generic Model"
  if (any(listmec == "Bonus")) mod <- paste(mod, " + Bonus", sep="")
  if (any(listmec == "Fixed Costs")) mod <- paste(mod, " + FixedCostTransaction", sep="")
  if (any(listmec == "Resources")) mod <- paste(mod, " + SubSurfaceResources", sep="")
  if (any(listmec == "Redistribution")) mod <- paste(mod, " + DoubleRedistribution", sep="")
  if (any(listmec == "Urban Transition")) mod <- paste(mod, " + UrbanTransition", sep="")
  paste("Model combination : ", input$choice_run, " | ", mod, time, sep="")
})

output$modelcombi3 <- renderPrint({
  if (input$runingperiod  == "1959-1989") time <- " From 1959 To 1989"
  if (input$runingperiod  == "1989-2010") time <- " From 1989 To 2010"
  listmec <- input$mechanisms    
  mod <- "Generic Model"
  if (any(listmec == "Bonus")) mod <- paste(mod, " + Bonus", sep="")
  if (any(listmec == "Fixed Costs")) mod <- paste(mod, " + FixedCostTransaction", sep="")
  if (any(listmec == "Resources")) mod <- paste(mod, " + SubSurfaceResources", sep="")
  if (any(listmec == "Redistribution")) mod <- paste(mod, " + DoubleRedistribution", sep="")
  if (any(listmec == "Urban Transition")) mod <- paste(mod, " + UrbanTransition", sep="")
  paste("Model combination : ", input$choice_run, " | ", mod, time, sep="")
})

 output$graph2 <- renderPlot({
   
   beginyear <- substr(input$runingperiod, 1, 4)
   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   if (input$runingperiod  == "1959-1989") {
   mariusstep0 <- subset(marius, step == 0)
   mariusstep11 <- subset(marius, step == 11)
   mariusstep20 <- subset(marius, step == 20)
   mariusstep30 <- subset(marius, step == 30)
   step0 <- mariusstep0[order(-mariusstep0$population) , ]
   step0 <- step0[,3]
   step11 <- mariusstep11[order(-mariusstep11$population) , ]
   step11 <- step11[,3]
   step20 <- mariusstep20[order(-mariusstep20$population) , ]
   step20 <- step20[,3]
   step30 <- mariusstep30[order(-mariusstep30$population) , ]
   step30 <- step30[,3]
   ranks <- 1:1145
   
   darius <- subset(Agglo@data, Pop1959 > 0)
   
   zipf0 <- as.data.frame(cbind (ranks, step0))
   colnames(zipf0) <- c("ranks", "size")
   sizes1970 <- darius[order(-darius$Pop1970) , ]
   sizes1970 <- sizes1970$Pop1970
   zipf1970 <- as.data.frame(cbind (ranks, sizes1970))
   colnames(zipf1970) <- c("ranks", "size")
   zipf11 <- as.data.frame(cbind (ranks, step11))
   colnames(zipf11) <- c("ranks", "size")
   sizes1979 <- darius[order(-darius$Pop1979) , ]
   sizes1979 <- sizes1979$Pop1979
   zipf1979 <- as.data.frame(cbind (ranks, sizes1979))
   colnames(zipf1979) <- c("ranks", "size")
   zipf20 <- as.data.frame(cbind (ranks, step20))
   colnames(zipf20) <- c("ranks", "size")
   sizes1989 <- darius[order(-darius$Pop1989) , ]
   sizes1989 <- sizes1989$Pop1989
   zipf1989 <- as.data.frame(cbind (ranks, sizes1989))
   colnames(zipf1989) <- c("ranks", "size")
   zipf30 <- as.data.frame(cbind (ranks, step30))
   colnames(zipf30) <- c("ranks", "size")
   zipf <- rbind(zipf0, zipf1970, zipf11, zipf1979, zipf20, zipf1989, zipf30 )
   date <- c(rep("1959", 1145),rep("1970", 1145), rep("sim1970", 1145),
             rep("1979", 1145), rep("sim1979", 1145), rep("1989", 1145), rep("sim1989", 1145))
   zipf <- as.data.frame(cbind (zipf, date))
   cols <- c( "sim1989" = "dodgerblue","1989" = "gray70","sim1979" = "dodgerblue3", "1979"= "gray60", "sim1970" = "dodgerblue4", "1970"= "gray50", "1959" = "gray30") 
 }
   
   if (input$runingperiod  == "1989-2010") {
     
     mariusstep0 <- subset(marius, step == 0)
     mariusstep13 <- subset(marius, step == 13)
     mariusstep21 <- subset(marius, step == 21)
      step0 <- mariusstep0[order(-mariusstep0$population) , ]
     step0 <- step0[,3]
     step13 <- mariusstep13[order(-mariusstep13$population) , ]
     step13 <- step13[,3]
     step21 <- mariusstep21[order(-mariusstep21$population) , ]
     step21 <- step21[,3]
         ranks <- 1:1822
     
     darius <- subset(Agglo@data, Pop1989 > 0)
   
     zipf0 <- as.data.frame(cbind (ranks, step0))
     colnames(zipf0) <- c("ranks", "size")
     sizes2002 <- darius[order(-darius$Pop2002) , ]
     sizes2002 <- sizes2002$Pop2002
     zipf2002 <- as.data.frame(cbind (ranks, sizes2002))
     colnames(zipf2002) <- c("ranks", "size")
     zipf13 <- as.data.frame(cbind (ranks, step13))
     colnames(zipf13) <- c("ranks", "size")
     sizes2010 <- darius[order(-darius$Pop2010) , ]
     sizes2010 <- sizes2010$Pop2010
     zipf2010 <- as.data.frame(cbind (ranks, sizes2010))
     colnames(zipf2010) <- c("ranks", "size")
     zipf21 <- as.data.frame(cbind (ranks, step21))
     colnames(zipf21) <- c("ranks", "size")
     zipf <- rbind(zipf0, zipf2002, zipf13, zipf2010, zipf21)
     date <- c(rep("1989", 1822),rep("2002", 1822), rep("sim2002", 1822),
               rep("2010", 1822), rep("sim2010", 1822))
     zipf <- as.data.frame(cbind (zipf, date))
     cols <- c( "sim2010" = "dodgerblue","2010" = "gray70","sim2002" = "dodgerblue3", "2002"= "gray50", "1989" = "gray30") 
     
   }
   
   p <-ggplot(zipf, aes(x=ranks, y=size, group=date, fill=date, colour=date)) 
   p + scale_y_log10(breaks=c(10, 100, 1000, 10000)) +
     scale_x_log10(breaks=c(1, 10, 100, 1000)) + 
     xlab("Rank") + ylab("Population (x1000)") +
     geom_point() + geom_line() +
     scale_colour_manual(values=cols) +
   theme(axis.text=element_text(size=12) ,
         axis.title=element_text(size=14),
         axis.text.x = element_text(angle = 45, hjust = 1))
 #+    ggtitle(paste("Simulated\n", sep="")) 
   
 })
 
 
 output$graphObsSim <- renderPlot({
   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   if (input$runingperiod  == "1959-1989") {
      mariusstep30 <- subset(marius, step == 30)
     darius <- subset(Agglo@data, Pop1959 > 0)  
     SimObs <- data.frame(darius, mariusstep30[match(darius$AROKATO,mariusstep30$arokato), ])
     SimObs$simulated <- SimObs$population 
     SimObs$observed <- SimObs$Pop1989 
   }
   
   if (input$runingperiod  == "1989-2010") {
      mariusstep21 <- subset(marius, step == 21)
      darius <- subset(Agglo@data, Pop1989 > 0)  
      SimObs <- data.frame(darius, mariusstep21[match(darius$AROKATO,mariusstep21$arokato), ])
      SimObs$simulated <- SimObs$population 
      SimObs$observed <- SimObs$Pop2010
   }
   
   p <-ggplot(SimObs, aes(x=observed, y=simulated)) 
   p + scale_y_log10(breaks=c(10, 100, 1000, 10000)) +
     scale_x_log10(breaks=c(10, 100, 1000, 10000)) + 
     xlab("Observed Population") + ylab("Simulated Population") +
     geom_point() + 
     #geom_line() +
     scale_colour_manual(values=c("dodgerblue", "black", "white")) +
     theme(axis.text=element_text(size=12) ,
           axis.title=element_text(size=14),
           axis.text.x = element_text(angle = 45, hjust = 1))
   #+    ggtitle(paste("Simulated\n", sep="")) 
   
   
 })
  
  output$map2 <- renderPlot({
   par(mar = c(0,0,1,0))
   plot(Shape, col="gray69", border="white", lwd = 1)

   cutoff <- input$cutoff
   
   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   if (input$runingperiod  == "1959-1989") {
     observed <- input$year_sima
     mariusstep0 <- subset(marius, step == 0)
     mariusstep11 <- subset(marius, step == 11)
     mariusstep20 <- subset(marius, step == 20)
     mariusstep30 <- subset(marius, step == 30)

  if (observed == "Pop1989") table <- mariusstep30
   if (observed == "Pop1979") table <- mariusstep20
   if (observed == "Pop1970") table <- mariusstep11
   if (observed == "Pop1959") table <- mariusstep0
  
 
   }
  
  if (input$runingperiod  == "1989-2010") {
    observed <- input$year_simb
    mariusstep0 <- subset(marius, step == 0)
    mariusstep13 <- subset(marius, step == 13)
    mariusstep21 <- subset(marius, step == 21)
    
    if (observed == "Pop2010") table <- mariusstep21
    if (observed == "Pop2002") table <- mariusstep13
    if (observed == "Pop1989") table <- mariusstep0
 
  }
  
   Year <- subset(name_col, name == observed)
   Year <- as.numeric(Year[1,2])
   
   Agglo@data <- data.frame(Agglo@data, table[match(Agglo@data$AROKATO,table$arokato), ])
   Agglo <- Agglo[!is.na(Agglo@data$population), ]
   Agglo@data <- Agglo@data[order(-Agglo@data[,Year]) , ]
   Agglo@data$Residuals <- log(Agglo@data[,Year]) - log(Agglo@data$population) 
   Agglo <- Agglo[Agglo@data$Residuals > cutoff | Agglo@data$Residuals < -cutoff, ]
   
   my_palette <- colorRampPalette(c("dodgerblue", "seashell", "coral"))
   vPal9 <- my_palette(8)
   vcut <- c(-1000, -100, -10, -1, 0, 1, 10, 100, 1000)
   Agglo@data$VarCut<- as.character(cut(Agglo@data$Residuals,
                                        breaks = vcut,
                                        labels = vPal9,
                                        include.lowest = TRUE,
                                        right = FALSE))
   vLegendBox <- as.character(levels(cut(Agglo@data$Residuals,
                                         breaks = vcut,
                                         include.lowest = TRUE,
                                         right = FALSE)))
   plot(Agglo, pch = 21,
        cex = input$sizefactor1 * sqrt(Agglo@data[,Year]/ pi), 
        col = "gray9", bg = Agglo@data$VarCut, lwd = 0.1,
        add = TRUE)
   title(paste("Maximum residuals | ", substr(observed, 4, 7), sep=""))
   leg <- c(15000, 1000, 100, 10)
   legend("topleft",legend = leg, pch = 21,
          col = "gray30", pt.bg = "gray90",
          pt.cex = input$sizefactor1 * sqrt(leg / pi),
          bty = "n", cex=0.8, title = "Simulated Population in agglomerations (* 1000)")
   legend("topright", legend=vLegendBox, bty="n", fill=vPal9, cex=0.8,title = "log(Observed) - log(Simulated) Population")
   arrows(par()$usr[1] + 100000, 
          par()$usr[3] + 100000,
          par()$usr[1] + 1000000, 
          par()$usr[3] + 100000,
          lwd = 2, code = 3,
          angle = 90, length = 0.05)
   text(par()$usr[1] + 505000, 
        par()$usr[3] + 270000, 
        "1000 km", cex = 0.8)  
 })
 
 output$table_pos_res <- renderDataTable({
   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   if (input$runingperiod  == "1959-1989") {
     observed <- input$year_sima
     mariusstep0 <- subset(marius, step == 0)
     mariusstep11 <- subset(marius, step == 11)
     mariusstep20 <- subset(marius, step == 20)
     mariusstep30 <- subset(marius, step == 30)
     
     if (observed == "Pop1989") table <- mariusstep30
     if (observed == "Pop1979") table <- mariusstep20
     if (observed == "Pop1970") table <- mariusstep11
     if (observed == "Pop1959") table <- mariusstep0
     
}
   
   if (input$runingperiod  == "1989-2010") {
     observed <- input$year_simb
     mariusstep0 <- subset(marius, step == 0)
     mariusstep13 <- subset(marius, step == 13)
     mariusstep21 <- subset(marius, step == 21)
     
     if (observed == "Pop2010") table <- mariusstep21
     if (observed == "Pop2002") table <- mariusstep13
     if (observed == "Pop1989") table <- mariusstep0
     
}
   
   
   Year <- subset(name_col, name == observed)
   Year <- as.numeric(Year[1,2])
   
   Agglo@data <- data.frame(Agglo@data, table[match(Agglo@data$AROKATO,table$arokato), ])
   
   Agglo <- Agglo[!is.na(Agglo@data$population), ]
   
   Agglo@data <- Agglo@data[order(-Agglo@data[,Year]) , ]
   Agglo@data$Residuals <- log(Agglo@data[,Year]) - log(Agglo@data$population) 
   
   res <- Agglo@data[order(-Agglo@data$Residuals) , ]
   residuals <-  cbind(res$name, res$country, round(res[,Year],0), round(res$population,0))
   colnames(residuals) <- c("City","Republic", "Observed thousands", "Simulated thousands")
   residuals
 })
 
 output$table_neg_res <- renderDataTable({

   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   if (input$runingperiod  == "1959-1989") {
     observed <- input$year_sima
     mariusstep0 <- subset(marius, step == 0)
     mariusstep11 <- subset(marius, step == 11)
     mariusstep20 <- subset(marius, step == 20)
     mariusstep30 <- subset(marius, step == 30)
     
     if (observed == "Pop1989") table <- mariusstep30
     if (observed == "Pop1979") table <- mariusstep20
     if (observed == "Pop1970") table <- mariusstep11
     if (observed == "Pop1959") table <- mariusstep0
}
   
   if (input$runingperiod  == "1989-2010") {
     observed <- input$year_simb
     mariusstep0 <- subset(marius, step == 0)
     mariusstep13 <- subset(marius, step == 13)
     mariusstep21 <- subset(marius, step == 21)
     
     if (observed == "Pop2010") table <- mariusstep21
     if (observed == "Pop2002") table <- mariusstep13
     if (observed == "Pop1989") table <- mariusstep0
          
 }
   
   Year <- subset(name_col, name == observed)
   Year <- as.numeric(Year[1,2])
   
   Agglo@data <- data.frame(Agglo@data, table[match(Agglo@data$AROKATO,table$arokato), ])
   
   Agglo <- Agglo[!is.na(Agglo@data$population), ]
   
   Agglo@data <- Agglo@data[order(-Agglo@data[,Year]) , ]
   Agglo@data$Residuals <- log(Agglo@data[,Year]) - log(Agglo@data$population) 
   
   res <- Agglo@data[order(-Agglo@data$Residuals) , ]
   residuals <-  cbind(res$name, res$country, round(res[,Year],0), round(res$population,0))
   colnames(residuals) <- c("City","Republic", "Observed thousands", "Simulated thousands")
  residuals
 })
   
 output$graph3 <- renderPlot({

   if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
   if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
   
   marius <- read.csv(mariusfile,sep=",",dec=".")
   
   
   if (input$runingperiod  == "1959-1989") {
     observed <- input$year_sim2a
     mariusstep0 <- subset(marius, step == 0)
     mariusstep11 <- subset(marius, step == 11)
     mariusstep20 <- subset(marius, step == 20)
     mariusstep30 <- subset(marius, step == 30)
     
     if (observed == "Pop1989") table <- mariusstep30
     if (observed == "Pop1979") table <- mariusstep20
     if (observed == "Pop1970") table <- mariusstep11
     if (observed == "Pop1959") table <- mariusstep0
  }
   
   if (input$runingperiod  == "1989-2010") {
     observed <- input$year_sim2b
     mariusstep0 <- subset(marius, step == 0)
     mariusstep13 <- subset(marius, step == 13)
     mariusstep21 <- subset(marius, step == 21)
     
     if (observed == "Pop2010") table <- mariusstep21
     if (observed == "Pop2002") table <- mariusstep13
     if (observed == "Pop1989") table <- mariusstep0
     
}
   
     
   Year <- subset(name_col, name == observed)
   Year <- as.numeric(Year[1,2])
   
   Agglo@data <- data.frame(Agglo@data, table[match(Agglo@data$AROKATO,table$arokato), ])
   
   Agglo <- Agglo[!is.na(Agglo@data$population), ]
   
   Agglo@data <- Agglo@data[order(-Agglo@data[,Year]) , ]
   Agglo@data$Residuals <- log(Agglo@data[,Year]) - log(Agglo@data$population) 
   Agglo@data$Population <- Agglo@data[,Year]
   res <- Agglo@data
   res$OilGas <- res$Hydrocarbo
   res$Western <- res$West.East
   res$MonoIndustry <- res$MONOGOROD
   model <- lm(Residuals ~ Western + 
                 Capital +
                 OilGas + 
                 Coal +
                 MonoIndustry +
                 log(Population), 
               data=res)
   summary(model)
   coef <- summary(model)$coefficient
   
   data <- as.data.frame(coef[,1])
   sign <- as.data.frame(coef[,4])
   colnames(data) <- c("Residual")
   colnames(sign) <- c("Significant")
   sign$Significant <- ifelse(sign$Significant < 0.01, "1.yes", "2.no")
   sign$UrbanAttribute <- rownames(sign)
   data$UrbanAttribute <- rownames(data)
   data <- cbind(data, sign)
   
   p <- ggplot(aes(x=UrbanAttribute, fill=Significant), data=data)
   plot1 <- p + geom_bar(aes(y=Residual), stat="identity") +
     scale_fill_manual(values = c("dodgerblue", "gray30")) + 
     theme(axis.text=element_text(size=12) ,
           axis.title=element_text(size=14),
           axis.text.x = element_text(angle = 45, hjust = 1))
   
   plot1
   
 })
 
output$graph3interaction <- renderPlot({ 
  selectedAtt <- input$Attribute1
  interactingAtt <- input$Attribute2
  
  if (selectedAtt != interactingAtt) {
    
    if (input$runingperiod  == "1959-1989") mariusfile <- mariuscsv$mariusperiod1 
    if (input$runingperiod  == "1989-2010") mariusfile <- mariuscsv$mariusperiod2
    
    marius <- read.csv(mariusfile,sep=",",dec=".")
    
  
  if (input$runingperiod  == "1959-1989") {
    observed <- input$year_sim2a
    mariusstep0 <- subset(marius, step == 0)
    mariusstep11 <- subset(marius, step == 11)
    mariusstep20 <- subset(marius, step == 20)
    mariusstep30 <- subset(marius, step == 30)
    
    if (observed == "Pop1989") table <- mariusstep30
    if (observed == "Pop1979") table <- mariusstep20
    if (observed == "Pop1970") table <- mariusstep11
    if (observed == "Pop1959") table <- mariusstep0
  }
  
  if (input$runingperiod  == "1989-2010") {
    observed <- input$year_sim2b
    mariusstep0 <- subset(marius, step == 0)
    mariusstep13 <- subset(marius, step == 13)
    mariusstep21 <- subset(marius, step == 21)
    
    if (observed == "Pop2010") table <- mariusstep21
    if (observed == "Pop2002") table <- mariusstep13
    if (observed == "Pop1989") table <- mariusstep0
    
  }
  
  
  Year <- subset(name_col, name == observed)
  Year <- as.numeric(Year[1,2])
  
  Agglo@data <- data.frame(Agglo@data, table[match(Agglo@data$AROKATO,table$arokato), ])
  
  Agglo <- Agglo[!is.na(Agglo@data$population), ]
  
  Agglo@data <- Agglo@data[order(-Agglo@data[,Year]) , ]
  Agglo@data$Residuals <- log(Agglo@data[,Year]) - log(Agglo@data$population) 
  Agglo@data$Population <- Agglo@data[,Year]
  res <- Agglo@data
  res$OilGas <- res$Hydrocarbo
  res$Western <- res$West.East
  res$MonoIndustry <- res$MONOGOROD
  
  
  if (selectedAtt == "Location") res$Attribute1 <- res$Western
  if (selectedAtt == "Status") res$Attribute1 <- res$Capital
  if (selectedAtt == "Oil&Gas") res$Attribute1 <- res$OilGas
  if (selectedAtt == "Coal") res$Attribute1 <- res$Coal
  if (selectedAtt == "Specialisation") res$Attribute1 <- res$MonoIndustry
  if (selectedAtt == "Size") res$Attribute1 <- log(res$Population)
 
  if (interactingAtt == "Location") res$Attribute2 <- res$Western
  if (interactingAtt == "Status") res$Attribute2 <- res$Capital
  if (interactingAtt == "Oil&Gas") res$Attribute2 <- res$OilGas
  if (interactingAtt == "Coal") res$Attribute2 <- res$Coal
  if (interactingAtt == "Specialisation") res$Attribute2 <- res$MonoIndustry
  if (interactingAtt == "Size") res$Attribute2 <- log(res$Population)
  
  
  model <- lm(Residuals ~ Attribute1 + 
                Attribute1 * Attribute2 +
                Attribute2, 
              data=res)
 
  summary(model)
  coef <- summary(model)$coefficient
  
  data <- as.data.frame(coef[,1])
  sign <- as.data.frame(coef[,4])
  colnames(data) <- c("Residual")
  colnames(sign) <- c("Significant")
  sign$Significant <- ifelse(sign$Significant < 0.01, "1.yes", "2.no")
  sign$UrbanAttribute <- rownames(sign)
  data$UrbanAttribute <- rownames(data)
  data <- cbind(data, sign)
  
  p <- ggplot(aes(x=UrbanAttribute, fill=Significant), data=data)
  plot1 <- p + geom_bar(aes(y=Residual), stat="identity") +
    scale_fill_manual(values = c("dodgerblue", "gray30")) + 
    ggtitle("Interaction between attributes") +
    theme(axis.text=element_text(size=12) ,
          axis.title=element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  }
})

})
