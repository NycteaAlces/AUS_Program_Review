ui <- fluidPage(verbatimTextOutput(("debug")))

options(shiny.fullstacktrace = TRUE)

shinyServer(function(input, output,session) {#----

  #Prepare the user-input slots -- dynamic/reactive
 # DB <- reactive(input$MegaDB$datapath)
  # GIS <- reactive(input$WMU_Shp)

  #truncvalue <- reactive(as.double(input$truncation[1]))

  AEP_Budget <- reactive({
    if(is.null(input$AEPBudget))
      return(0)
    else
      return(as.double(input$AEPBudget))
  })

  # output$debug <- renderPrint({
  #   sessionInfo()
  # })
  #
  # printLogJs <- function(x, ...) {
  #
  #   logjs(x)
  #
  #   T
  # }




#OL <- eventReactive(list(input$MegaDB$datapath,input$AEPBudget, input$EMSDBudget, input$ForecastYr, input$TopUp ), { ####----

  OL <- eventReactive(list(input$AEPBudget, input$EMSDBudget, input$ForecastYr, input$TopUp), { ####----

  AUS <- na.omit(read.csv(text=getURL("https://raw.githubusercontent.com/NycteaAlces/AUS_Program_Review/master/Data/R_Survey_Status_2018_full_Success_V5.csv"),  header=T))
   # AUS <- na.omit(read.csv(paste(input$MegaDB$datapath))) #remove nulls
    ForecastYr <- input$ForecastYr
    first_int <- AUS[which(AUS$Interval==1),] #select only year 1 budget requests
    first_int <- first_int[which(!is.na(first_int$Budget)),] #remove pesky nulls
    first_int$DeficitV2 <- as.numeric(first_int$YearSince-first_int$Years)
    first_int$EMSum <- 0
    first_int$Event <- 0
    AnnBudg <- as.numeric(input$AEPBudget) #Define
    EMBudg <- as.numeric(input$EMSDBudget)

    Schedule <- data.frame(Region= as.character(), WMU = as.character(), Event=as.numeric(), CumSum = as.numeric(), Budget=as.numeric(),Year=as.numeric(), Deficit=as.numeric())
    DefCnt <- data.frame(DefNum=as.numeric(), Year=as.numeric())
    PredOut <- data.frame(Year=as.numeric(), Prop50 = as.numeric())

    Risk50 <- data.frame(Prop = c(.7,3.5,8.05,13.05,18.60,22.65,26.00,28.70,31.75,34.30,36.75,38.65,40.90,43.10,44.65), Years=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))#Relationship based on provincial observed risk of 50% change in populations between successive surveys
    fit50 <- lm(Prop ~ log(Years), data=Risk50)
    Risk20 <- data.frame(Prop=c(21.65,36.55,45.30,51.50,55.5,58.7,61.55,63.90,65.70,67,68.1,69.25,70.35,71.55,72.45), Years=c(2:16))
    fit20 <- lm(Prop ~ log(Years), data=Risk20)
    #create vectors to store PredOut
    Years.PredOut <- c()
    Predicted.PredOut <- c()
    Predicted20.PredOut <- c()
    PredOut <- data.frame(Years=as.numeric(), fit50=as.numeric())
    PredOut20 <- data.frame(Years=as.numeric(), fit20=as.numeric())

   # Pred50.cnt <- c()
  #  Pred20.cnt <- c()

    print(summary(fit20))

 if (input$TopUp==T){
      for(y in 2020:ForecastYr){
        first_int <- first_int[order(-first_int$DeficitV2),]
        current.sum.OPS = 0
        current.sum.EM = 0
        current.sum = 0
  #      nvect.df <- data.frame(YearSince = as.numeric())
        for(c in 1:nrow(first_int)){
          current.sum = current.sum + as.numeric(first_int$Budget[c])

          if( first_int$EOMSD[c]=="EMSD"){

            current.sum.EM = current.sum.EM +  as.numeric(first_int$Budget[c])

            if (current.sum.EM <= EMBudg){

              first_int$Event[c] <- y
              first_int$CumSum[c] <- current.sum
              first_int$EMSum[c] <- current.sum.EM
              first_int$YearSince[c] <- 0
            } else if ((current.sum.OPS + as.numeric(first_int$Budget[c])) <= AnnBudg){

              current.sum.OPS <- current.sum.OPS + as.numeric(first_int$Budget[c])
              first_int$Event[c] <- y
              first_int$CumSum[c] <- current.sum
              first_int$OpsSum[c] <- current.sum.OPS
              first_int$YearSince[c] <- 0}
            else {

              x <- first_int[which(first_int$Event == y),]
              z <- data.frame(Region= x$Region, WMU = x$WMU, Event=x$Event, CumSum = x$CumSum, Budget=x$Budget,Year=x$Year, Deficit=x$DeficitV2)
              break
            }}
          else {
              current.sum.OPS = current.sum.OPS +  as.numeric(first_int$Budget[c])
              if (current.sum.OPS <= AnnBudg){

                first_int$Event[c] <- y
                first_int$CumSum[c] <- current.sum
                first_int$OpsSum[c] <- current.sum.OPS
                first_int$YearSince[c] <- 0
              } else if (current.sum.EM <= (EMBudg-18)){
                       next
                }else  {

                x <- first_int[which(first_int$Event == y),]
                z <- data.frame(Region= x$Region, WMU = x$WMU, Event=x$Event, CumSum = x$CumSum, Budget=x$Budget,Year=x$Year, Deficit=x$DeficitV2)
                break
                }}}

        Schedule <- rbind(z, Schedule)
        first_int$YearSince <-   (first_int$YearSince + 1)
        first_int$DeficitV2 <- (as.numeric(first_int$YearSince)-as.numeric(first_int$Years))

        DefCnt.it <- data.frame(cbind(nrow(first_int[which(first_int$DeficitV2 > 0),]), as.numeric(y)))
        DefCnt <- rbind(DefCnt.it, DefCnt)
        nvect.df <- data.frame(Years=as.numeric(first_int$YearSince))
        #  print(paste("The mean predicted percent of units >50% different is ", mean(predict(fit50, newdata=nvect.df$Years)),"."))
        Years.PredOut <- append(Years.PredOut, as.numeric(y))
        Predicted.PredOut <- append(Predicted.PredOut, as.numeric(mean(predict(fit50, newdata=nvect.df))))
        Predicted20.PredOut <- append(Predicted20.PredOut, as.numeric(mean(predict(fit20, newdata=nvect.df))))




        }


    }


   else { #checkbox not clicked
     for(y in 2020:ForecastYr){
       first_int <- first_int[order(-first_int$DeficitV2),]
       current.sum.OPS = 0
       current.sum.EM = 0
       current.sum = 0
       for(c in 1:nrow(first_int)){
         current.sum = current.sum + as.numeric(first_int$Budget[c])
         if( first_int$EOMSD[c]=="EMSD"){
           current.sum.EM = current.sum.EM +  as.numeric(first_int$Budget[c])

           if (current.sum.EM <= EMBudg){
             first_int$Event[c] <- y
             first_int$CumSum[c] <- current.sum
             first_int$EMSum[c] <- current.sum.EM
             first_int$YearSince[c] <- 0
           } else if ((current.sum.OPS + 20) <= AnnBudg){
                next
          } else {
             x <- first_int[which(first_int$Event == y),]
             z <- data.frame(Region= x$Region, WMU = x$WMU, Event=x$Event, CumSum = x$CumSum, Budget=x$Budget,Year=x$Year, Deficit=x$DeficitV2)
             break
           }}
         else {
           current.sum.OPS = current.sum.OPS +  as.numeric(first_int$Budget[c])
           if (current.sum.OPS <= AnnBudg){
             first_int$Event[c] <- y
             first_int$CumSum[c] <- current.sum
             first_int$OpsSum[c] <- current.sum.OPS
             first_int$YearSince[c] <- 0
           } else if (current.sum.EM <= (EMBudg-18)){
             next
           }else  {
             x <- first_int[which(first_int$Event == y),]
             z <- data.frame(Region= x$Region, WMU = x$WMU, Event=x$Event, CumSum = x$CumSum, Budget=x$Budget,Year=x$Year, Deficit=x$DeficitV2)
             break
           }}}

       Schedule <- rbind(z, Schedule)
       first_int$YearSince <-   (first_int$YearSince + 1)
       first_int$DeficitV2 <- (as.numeric(first_int$YearSince)-as.numeric(first_int$Years))

       DefCnt.it <- data.frame(cbind(nrow(first_int[which(first_int$DeficitV2 > 0),]), as.numeric(y)))
       DefCnt <- rbind(DefCnt.it, DefCnt)

       nvect.df <- data.frame(Years=as.numeric(first_int$YearSince))
       #  print(paste("The mean predicted percent of units >50% different is ", mean(predict(fit50, newdata=nvect.df$Years)),"."))
       Years.PredOut <- append(Years.PredOut, as.numeric(y))
       Predicted.PredOut <- append(Predicted.PredOut, as.numeric(mean(predict(fit50, newdata=nvect.df))))
       Predicted20.PredOut <- append(Predicted20.PredOut, as.numeric(mean(predict(fit20, newdata=nvect.df))))

       }}

      Schedule <- as.data.frame(Schedule)
      Schedule.V2 <- reshape(data.frame(Region = Schedule$Region, WMU=Schedule$WMU, Event=Schedule$Event, Budget=Schedule$Budget), idvar = c("Region", "WMU"), timevar= "Event", direction = "wide", sep = "-")
      Deficit.V1 <- reshape(data.frame(Region = Schedule$Region, WMU=Schedule$WMU, Event=Schedule$Event, Budget=Schedule$Deficit), idvar = c("Region", "WMU"), timevar= "Event", direction = "wide", sep = "-")
      colnames(Schedule.V2) <- c("Region", "WMU", as.numeric(input$ForecastYr):2020)
      Schedule.V3 <- Schedule.V2[,c(1,2, ncol(Schedule.V2):3)]
      colnames(Deficit.V1) <- c("Region", "WMU", as.numeric(input$ForecastYr):2020)
      colnames(DefCnt) <- c("DefCnt","Year")
      y <- match(Schedule.V2, cbind(first_int$EOSD, first_int$WMU))#CReate Schedule.V2 with EOMSD column
      PredOut <- data.frame(Years=Years.PredOut,fit50=Predicted.PredOut)
      PredOut20 <- data.frame(Years=Years.PredOut,fit20=Predicted20.PredOut)

      list(Schedule = Schedule.V2,
           Schedule.V2 = Schedule,
           Deficit.V1 = Deficit.V1,
           Schedule=Schedule,
           first_int=first_int,
           DefCnt = DefCnt,
           y=y,
           PredOut = PredOut,
           PredOut20=PredOut20)

      })


output$AUS_Sched = DT::renderDataTable(OL()$Schedule, options = list(lengthChange=FALSE, pageLength = 20), caption=paste("Table 1. Resulting schedule of surveys if the average funding value is ",input$AEPBudget, ". ") )
output$DefForecast <- renderPlot(ggplot (data = OL()$DefCnt,aes(x=Year, y=DefCnt)) + geom_point(colour = "red", alpha=I(0.5) )+geom_text(aes(label=DefCnt))+geom_line(data=OL()$DefCnt, aes(x=Year,y=DefCnt))+ylim(0, max(OL()$DefCnt$DefCnt))+labs(title=paste("Number of survey units that are in deficit based on the proposed funding amounts of $",input$AEPBudget, "000 from AEP, and $", input$EMSDBudget, "000 from EMSD."), y="Number of survey units that are in deficit", x="Forecasted year"))
output$PerfPlot <- renderPlot(ggplot(OL()$first_int,aes(DeficitV2, fill=factor(Region))) + geom_histogram(color="grey", aes(alpha=EOMSD)) + facet_wrap(~Region) + geom_vline(xintercept=0, color = "red",size=1) + labs(title= paste("Performance measure: delivery on target survey frequency in ", input$ForecastYr), subtitle="Reporting on the number of surveys that have met the target (red line), by Region", y="Number of survey priorities", x="Number of years that surveys are overdue (positive), or where objectives met (zero or negative)"))
output$Schedule <- renderPlot(ggplot(data=OL()$Schedule.V2, aes(y=Budget,x=Event, fill=Region))+geom_bar(stat="identity")+geom_text(size=2, position=position_stack(vjust=0.5),aes(label=WMU)) + labs(title= paste("Schedule of surveys based on the user-specified funding levels of AEP and EMSD budgets are, respectively, ", input$AEPBudget, " and ", input$EMSDBudget, "." ), y="Cumulative Annual Budget", x="Forecasted Year"))#+theme_minimal()+scale_fill_brewer(palette = "Paired")+theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5)))
output$SchedulebyReg <- renderPlot(ggplot(data=OL()$Schedule.V2, aes(y=Budget,x=Event, fill=Region))+geom_bar(stat="identity")+facet_wrap(~Region) +geom_text(size=2, position=position_stack(vjust=0.5),aes(label=WMU)))
output$PredOut <- renderPlot(ggplot(data=OL()$PredOut, aes(y=fit50, x=Years))+geom_point(shape=1) + geom_smooth(method="lm") + coord_trans(x="log10") + labs(title="Predicted proportion (%) of provincial WMUs that will exceed 50% change in moose since the last survey was conducted.", y="Proportion of provincial WMU (Percent) predicted to exceed 50% change since last survey"))#+geom_Smooth(method=log(Years)))
output$PredOut20 <- renderPlot(ggplot(data=OL()$PredOut20, aes(y=fit20, x=Years))+geom_point(shape=1)+ geom_smooth(method="lm") + coord_trans(x="log10") + labs(title="Predicted proportion (%) of provincial WMUs that will exceed 20% change in moose since the last survey was conducted.", y="Proportion of provincial WMU (Percent) predicted to exceed 20% change since last survey"))
#output$PredCnt20 <- renderPlot(ggplot(data=OL()$PredCount, aes(y=Cnt20, x=Years))+geom_point(shape=1))

})
