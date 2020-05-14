library(shinythemes)
library(shiny)
library(dplyr)
library(mice)
library(randomForest)
library(tidyr)
library(caret)
library(ggplot2)
library(tidyverse)
library(gghighlight)
library(RColorBrewer)
library(reticulate)
library(randomForest)
library(caret)




load("rf.rda")
# set the environment for python
use_python("C:/ProgramData/Anaconda3/python.exe")
source_python('python_ref.py')

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  f_data <- read.csv("financial_data.csv")
  
  # regroup
  A_level <- c("A","A-","A+","AA","AA-","AAA","AA+")
  B_level <- c("BBB","BBB-","BBB+","BB+","BB","BB-","B","B-","B+")
  C_level <- c("CCC","CCC-","CCC+")
  f_data$Rating <- NA
  f_data$Rating[which(f_data$S.P.Rating %in% A_level)] <- "A"
  f_data$Rating[which(f_data$S.P.Rating %in% B_level)] <- "B"
  f_data$Rating[which(f_data$S.P.Rating %in% C_level)] <- "C"
  f_data$industry <- as.character(f_data$industry)
  
  
  industry <- as.matrix(unlist(lapply(strsplit(f_data$industry,"\\s|\\&|\\-|\\/|\\,|\\,"), function(l) l[[1]])),ncol=1)
  head<-as.data.frame(table(industry)) %>% arrange(desc(Freq)) %>% head(5) %>% 
    select(industry) %>% unlist() %>% droplevels()
  industry[which(!(industry %in% head))] <- "others"
  f_data$industry <- industry
  all_feature <- c("fye","sales.1","foreign.scr","fcf","ltm.sales.gr","ltm.ni.gr","ebitda.int","debt.t..bv","cash.int",
                   "interest","ret.on.cap","cfo.debt","debt.ebitda","cfo.interest","ebitda.int.1","ebit.sales","debt.t.asts",
                   "cfo.capex","sales.3yr.avg.growth","ebitda.gr","ni.growth","ni.5yr.gr")
  all_group <- c("industry","Rating","country")
  
  my_theme <- function(){
    nb.cols <- 20
    mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(nb.cols)
    list(
      theme_light(),
      scale_fill_manual(values = mycolors),
      theme(legend.position = "bottom")
    )
  }
  
  requirement_data <- read.csv("requirement.csv")
  
  output$requirement_table = DT::renderDataTable(
    requirement_data,
    options = list(scrollX = TRUE)
  )
  
  sentiment_data <- read.csv("sentiment_analysis.csv")
  
  output$sentiment_table = DT::renderDataTable(
    sentiment_data,
    options = list(scrollX = TRUE)
  )
  
  # run python script
  output$sentiment<-renderPrint({
    testMethod(input$name)
  })
  output$balabala<-renderText({
    foo<-balabala(input$name)
    paste("Currently working on",foo)
  })
  output$headline<-renderText({
    topline(input$name)
  })
  
  data = reactive({
      df <- data.frame(sic=input$sic, countryba=as.factor(input$countryba), fye=input$fye,
                       sales.1=input$sales1, foreign.scr=input$foreign_scr,
                       fcf=input$fcf, ltm.sales.gr=input$ltm_sales_gr, ltm.ni.gr=input$ltm_ni_gr,
                       sales.3yr.avg.growth = input$sales_3yr_avg_growth, ebitda.gr=input$ebitda_gr,
                       ni.growth=input$ni_growth, ni.5yr.gr=input$ni.5yr.gr, ebitda.int=input$ebitda.int,
                       debt.t..bv=input$debt.t..bv, cash.int=input$cash.int, interest=input$interest,
                       ret.on.cap=input$ret.on.cap, cfo.debt=input$cfo.debt, debt.ebitda=input$debt.ebitda,
                       cfo.interest=input$cfo.interest, ebitda.int.1=input$ebitda.int.1, ebit.int=input$ebit.int,
                       ebit.sales=input$ebit.sales, debt.t.asts=input$debt.t.asts, cfo.capex=input$cfo.capex)
      levels(df$countryba) = c("US", 'other')
      return(df)
  })
  
  pred <- reactive({
    # make predictions
    prediction <- predict(rf, data(), type = "response")

    # append predictions
    prediction <- as.data.frame(list(prediction))
    colnames(prediction) = c('prediction')
    return(prediction)
  })
  
  output$table <- renderTable({
    if (is.null(data())) {return()}
    print(pred())
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
  output$boxPlot <- renderPlot({
    temp<-f_data%>%select(name,input$feature,input$group)
    lim<-quantile(temp[,2],c(0.1,0.9),na.rm=T)
    ggplot(temp,aes_string(x=input$group,y=input$feature,fill=input$group))+geom_boxplot()+
      scale_y_continuous(limits = lim)+
      labs(x=input$group,y=input$feature,title="Boxplot with highlighting in black")+my_theme()+
      gghighlight(name==input$name,unhighlighted_colour = "slategray1")+
      geom_label(aes(label=name),label.size = 0.5)
    
  })
  output$comparePlot<-renderPlot({
    foo<-f_data%>%select(name,input$feature)%>%filter(name==input$name|name%in%input$compare)
    ggplot(foo,aes(x=name,y=foo[,2],fill=name))+
      geom_bar(stat = "identity",width=0.5)+
      labs(title="Comparison between companies",y=colnames(foo)[2])+
      coord_flip()+my_theme()
  })
  
  df <- reactive({
    # read the data from input
    df = read.csv(input$upload_data$datapath, header = TRUE)
    
    # select only useful variables
    df = df[,c("sic", "fye", "sales.1", "foreign.scr", "fcf", "ltm.sales.gr", "ltm.ni.gr", 
               "sales.3yr.avg.growth", "ebitda.gr", "ni.growth", "ni.5yr.gr", "ebitda.int", 
               "debt.t..bv", "cash.int", "interest", "ret.on.cap", "cfo.debt", "debt.ebitda", "cfo.interest", "ebitda.int.1",
               "ebit.int", "ebit.sales", "debt.t.asts", "cfo.capex", "countryba")]
    
    return(df)
  })
  
  results <- reactive({
    # null check
    if(is.null(input$upload_data)){
      return(NULL)
    }
    
    # a message to show it is making predictions
    withProgress(message = 'Just one second',{
      prediction <- rf %>% predict(df() , type = "response")
      
    # add the prediction to the original dataframe
    df_with_pred <- cbind(df(), prediction)
    return(df_with_pred)
    })
  })
  
  # download predictions
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    })
  
  # show predictions
  output$pred_table = DT::renderDataTable(
    results(),
    options = list(scrollX = TRUE)
  )

}
