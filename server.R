ui <- fluidPage(verbatimTextOutput(("debug")))

shinyServer(function(input, output,session) {#----

  #Prepare the user-input slots -- dynamic/reactive
 # DB <- reactive(input$MegaDB$datapath)
  # GIS <- reactive(input$WMU_Shp)

  #truncvalue <- reactive(as.double(input$truncation[1]))

  AEP_Budget <- reactive({
    if(is.null(input$AEPBudget))
      return(1200)
    else
      return(as.double(input$AEPBudget))
  })

  output$debug <- renderPrint({
    sessionInfo()
  })


OL <- eventReactive(list(input$MegaDB$datapath,input$AEPBudget, input$ForecastYr), { ####----

    AUS <- na.omit(read.csv(paste(input$MegaDB$datapath))) #remove nulls
    ForecastYr <- input$ForecastYr
    first_int <- AUS[which(AUS$Interval==1),] #select only year 1 budget requests
    first_int <- first_int[which(!is.na(first_int$Budget)),] #remove pesky nulls
    first_int$DeficitV2 <- as.numeric(first_int$YearSince-first_int$Years)
    AnnBudg <- as.numeric(input$AEPBudget) #Define

    Schedule <- data.frame(Region= as.character(), WMU = as.character(), Event=as.numeric(), CumSum = as.numeric(), Budget=as.numeric(),Year=as.numeric(), Deficit=as.numeric())
    DefCnt <- data.frame(DefNum=as.numeric(), Year=as.numeric())
     for(y in as.numeric(format(Sys.Date(),"%Y")):ForecastYr){
      first_int <- first_int[order(-first_int$DeficitV2),]
      current.sum = 0
      for(c in 1:nrow(first_int)){
        current.sum = current.sum + as.numeric(first_int$Budget[c])
        if (current.sum <= AnnBudg){
          first_int$Event[c] <- y
          first_int$CumSum[c] <- current.sum
          first_int$YearSince[c] <- 0
        } else {
          x <- first_int[which(first_int$Event == y),]
          z <- data.frame(Region= x$Region, WMU = x$WMU, Event=x$Event, CumSum = x$CumSum, Budget=x$Budget,Year=Current_year, Deficit=x$DeficitV2)
          break
        }
      }
      Schedule <- rbind(z, Schedule)
      first_int$YearSince <-   (first_int$YearSince + 1)
      first_int$DeficitV2 <- (as.numeric(first_int$YearSince)-as.numeric(first_int$Years))

      DefCnt.it <- data.frame(cbind(nrow(first_int[which(first_int$DeficitV2 > 0),]), as.numeric(y)))
      DefCnt <- rbind(DefCnt.it, DefCnt)

    }


    Schedule <- as.data.frame(Schedule)

    Schedule.V2 <- reshape(data.frame(Region = Schedule$Region, WMU=Schedule$WMU, Event=Schedule$Event, Budget=Schedule$Budget), idvar = c("Region", "WMU"), timevar= "Event", direction = "wide", sep = "-")
    Deficit.V1 <- reshape(data.frame(Region = Schedule$Region, WMU=Schedule$WMU, Event=Schedule$Event, Budget=Schedule$Deficit), idvar = c("Region", "WMU"), timevar= "Event", direction = "wide", sep = "-")

    colnames(Schedule.V2) <- c("Region", "WMU", as.numeric(input$ForecastYr):as.numeric(format(Sys.Date(),"%Y")))
    Schedule.V3 <- Schedule.V2[,c(1,2, ncol(Schedule.V2):3)]
    colnames(Deficit.V1) <- c("Region", "WMU", as.numeric(input$ForecastYr):as.numeric(format(Sys.Date(),"%Y")))
    colnames(DefCnt) <- c("DefCnt","Year")
    print(DefCnt)
    list(Schedule = Schedule.V2,
         Deficit.V1 = Deficit.V1,
         Schedule=Schedule,
         first_int=first_int,
         DefCnt = DefCnt
         )
})

output$AUS_Sched = DT::renderDataTable(OL()$Schedule, options = list(lengthChange=FALSE, pageLength = 20), caption=paste("Table 1. Resulting schedule of surveys if the average funding value is ",input$AEPBudget) )
output$DefForecast <- renderPlot(ggplot () + geom_point(data = OL()$DefCnt, aes(x=Year, y=DefCnt), colour = "red", alpha=I(0.5) )+geom_line(data=OL()$DefCnt, aes(x=Year,y=DefCnt)))
output$PerfPlot <- renderPlot(ggplot(OL()$first_int,aes(DeficitV2, fill=factor(Region))) + geom_histogram(color="grey", aes(alpha=EOMSD)) + facet_wrap(~Region) + geom_vline(xintercept=0, color = "red",size=1)+labs(title= paste("Performance measure: delivery on target survey frequency in ", input$ForecastYr), subtitle="Reporting on the number of surveys that have met the target (red line), by Region", y="Number of survey priorities", x="Number of years that surveys are overdue (positive), or where objectives met (zero or negative)"))



})

