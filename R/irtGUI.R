


irtGUI<-function(){


  ui <- dashboardPage(skin = "blue",
                      dashboardHeader(title = "ITEM RESPONSE THEORY ANALYSIS",titleWidth = 500),
                      dashboardSidebar(
                        sidebarMenu(id="sidebarmenu",
                                    menuItem("About Package",tabName = "info",icon = icon("info-circle")),
                                    menuItem("Browse",tabName = "upload",icon = icon("upload"),selected = TRUE),
                                    menuItem("Data Generator",tabName = "generator",icon = icon("graduation-cap")),
                                    menuItem("Model Fit",tabName = "modelfit",icon = icon("thumbs-up")),
                                    menuItem("IRT Assumptions",textOutput("new"),startExpanded = FALSE,icon = icon("bar-chart"),

                                             menuSubItem("Dimensionality",tabName = "boyut",icon = icon("pie-chart")),
                                             menuSubItem("Local Independence Test",tabName = "yerelb",icon = icon("pie-chart"))) ,
                                    menuItem("Item Fit",tabName = "itemfit",icon = icon("thumbs-up")),
                                    menuItem("Item Parameter Estimation",tabName = "param",icon = icon("cogs")),

                                    menuItem("Ability Estimation",tabName = "theta",icon = icon("graduation-cap")),
                                    menuItem("Wright Map",tabName = "wright",icon = icon("graduation-cap")),

                                    menuItem(tabName="karakteristik","Item Characteristic Curve",startExpanded = FALSE, icon = icon("line-chart"))


                        )),
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "info",
                                  fluidRow(
                                    box(title = "About Package", solidHeader = TRUE, status = "info",htmlOutput("info"),width = 12)
                                  )),
                          tabItem(tabName = "upload",
                                  fluidRow(
                                    box(title = "Informations About Application",status = "success",solidHeader = TRUE,width = 12,strong("Please Read This Instructions:"),br()," Your dataset must be in .xls or .xlsx format. Item names should be included in the first line of your data set. In addition, the individual id or name should not be used.",br()," After uploading your data set, you should select your preferred IRT model and click the analyze button. On this page, you can preview your data loaded into the program.
                                        ",br(),"Then, by clicking on the functions in the side menu, you can get your desired analysis results.",hr(),strong("Functions of Side Menu Tabs:"),br(),strong("Item Fit: "),"In this menu, you can see the item fit values of the model you selected.",br(),strong("Item Parameter Estimations: "),"Item parameters of the preferred model are presented with their standard errors."
                                        ,br(),strong("Ability Estimations: "),"Estimated ability parameters and standard errors for each individual are presented.",br(),strong("Wright Map: "),"A wright Map can be obtained, where you can examine the distribution of item difficulties and the ability distribution of individuals in the group on the same graph.",br(),strong("IRT Assumptions: "),"The dimensionality and local independence assumptions of the IRT can be tested.",br(),strong("Item Characteristic and Information Curves: "),"Item characteristic and item information curves for each item in the data set can be drawn.
                                        Graphs can be obtained separately for each item or for all items at the same time."),
                                    box(title = "Browse Data",status = "warning",solidHeader = TRUE,width = 6,
                                        fileInput("dataset", label="Browse Data",placeholder="File",buttonLabel = "Add",accept = c("xlsx","xls","csv")
                                        ),uiOutput(outputId="check")),
                                    box("Note: The Graded Response Model and Generalized Partial Credit Model are suitable for polytomous datasets.",br(), "For a better model selection, you can use the model fit sub-menu.",title="Choose IRT Model",solidHeader = TRUE,status = "warning",width = 6,
                                        uiOutput(outputId="modselect"),submitButton("Analyze")),
                                    box(title="Data Preview",solidHeader = TRUE,dataTableOutput("preview"),status = "info",width = 12)

                                  )),
                          tabItem(
                            tabName = "generator",
                            fluidRow(column(width = 3,
                                            box(title = "Select Conditions",status = "warning",solidHeader = TRUE,width = 12,
                                                submitButton("Generate Data"),
                                                uiOutput(outputId="methodselect"),
                                                uiOutput(outputId="samplesize"),
                                                uiOutput(outputId="itemnumber"),
                                                uiOutput(outputId="thetamean"),
                                                uiOutput(outputId="thetasd"),
                                                uiOutput(outputId="aparmin"),
                                                uiOutput(outputId="aparmax"),
                                                uiOutput(outputId="bparmean"),
                                                uiOutput(outputId="bparsd"),
                                                uiOutput(outputId="cparmin"),
                                                uiOutput(outputId="cparmax")
                                            )),
                                     column(width = 9,
                                            box(title="Information","On this page, you can simulate your dichotomous data, based on logistic models of IRT under the conditions you specified. You can specify the IRT Model, sample size, item number, item and ability parameter properties. After selecting your conditions, you can generate your data set by clicking the Generate Data button. The data set produced can be displayed automatically in the Simulated Data box.",br(),"You can download the data set you have generated by clicking the Download Simulated Data button. To make analyzes with this data set, you need to check the I want to use simulated data option in the Browse submenu, select the IRT model and click the analyze button.", solidHeader = TRUE,status = "success",width = 12),
                                            box(title="Simulated Data",solidHeader = TRUE,downloadButton('downloaddata','Download Simulated Data'),verbatimTextOutput("generator"),status = "info",width = 12)
                                     )
                            )),
                          tabItem(
                            tabName = "modelfit",
                            fluidRow(
                              box(title="Information","Several different model fit indices, the AIC, BIC, log-likelihood are presented in the output. The smaller these fit indices, the better the model fits the data. The Anova function also performs a likelihood ratio test based on the log likelihood values from the two models. Two times the absolute difference in log-likelihood values of the two models has a chi-square distribution, with degrees of freedom equal to the difference in the number of estimated parameters between the two models, under the null hypothesis that there is no difference in model fit between the two models. In other words, the model with more parameters is assumed to fit the data no better than the model with fewer parameters. If the p-value for the chi-square statistics is less than .05, then we can conclude that the more complex model fits better than the simpler model ",strong("(Desjardin & Bulut, 2018, Handbook of Educational Measurement and Psychometrics Using R)."), solidHeader = TRUE,status = "success",width = 10),
                              box(title="Fit Indexes",tableOutput("modelfit")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 10)


                            )),
                          tabItem(
                            tabName = "wright",
                            fluidRow(
                              box(title="Wright Map",plotOutput("wright")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 8),
                              box(title="Information","To perform this analysis, you must have estimated the abilities from the side menu.",br(),"The Wright Map provides a picture of an exam by placing the difficulty of the exam items on the same measurement scale as the ability of the individuals.  This provides the user with a comparison of candidates and items, to better understand how appropriately the test measured.
                                        The Wright Map is organized as two vertical histograms. The left side shows candidates and the right side shows items. The left side of the map shows the distribution of the measured ability of the candidates from most able at the top to least able at the bottom.  The items on the right side of the map are distributed from the most difficult at the top to the least difficult at the bottom.", solidHeader = TRUE,status = "success",width = 4)

                           )),
                          tabItem(
                            tabName = "itemfit",
                            fluidRow(
                              column(width = 7,
                                     box(title="Fit Indexes",tableOutput("itemfit")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 12)),
                              column(width = 5,
                                     box(title="Choose IRT Model to Test Item Fit",solidHeader = TRUE,status = "warning",width = 12,
                                         uiOutput(outputId="modselectitem"),submitButton("Analyze")),
                                     box(title="Information","To obtain Item Fit Indices, select the model to be tested in the box above and start the analysis.",br(),"If the p.S_X2 values in the output are less than .05, it can be interpreted that the item is not fitted with the model.", solidHeader = TRUE,status = "success",width = 12))

                            )),
                          tabItem(
                            tabName = "param",
                            fluidRow(
                              box(title="Item Parameter Estimations",downloadButton('downloadparam','Download Estimated Item Parameters'),dataTableOutput("param")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 10)

                            )),

                          tabItem(
                            tabName = "theta",
                            fluidRow(column(width = 8,
                                            box(title="Ability Estimations",downloadButton('downloadtheta','Download Estimated Abilities'),dataTableOutput("theta")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 12)),
                                     column(width = 4,
                                            box(title="Estimation Method",solidHeader = TRUE,uiOutput(outputId="abilitymethod"),submitButton("Choose Method and Start Estimation"),status = "warning",width = 12),

                                            box(title="Ability Statistics",tableOutput("thetastats")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 12))

                            )),
                          tabItem(
                            tabName = "boyut",
                            fluidRow(

                              box(title="Scree Plot",solidHeader = TRUE,status = "info",width = 8,
                                  plotOutput("screeplot")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5)),
                              box(title="Eigen Values",solidHeader = TRUE,status = "info",width = 4,
                                  dataTableOutput("eigens")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5))


                            )
                          ),
                          tabItem(
                            tabName = "yerelb",
                            fluidRow(

                              box(title="Select Method",solidHeader = TRUE,uiOutput(outputId="localmethod"),submitButton("Start Analyze"),status = "warning",width = 4),
                              box(title="Information","0.20 is the critical value for the residual correlation values obtained from Yen's Q3 test. For item pairs above this value, it can be said that it violates local independence.
                                 Chen and Thissen (1997).",br(),"The G2 index is using the 3.84 (p= .05) or 6.63 (p= .01) cutpoints of chi-square distribution of one degree of freedom (Chen & Thissen, 1997).
", solidHeader = TRUE,status = "success",width = 8),
                              box(title="Results",downloadButton('localdownload','Download Result Matrix'),verbatimTextOutput("local")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),solidHeader = TRUE,status = "info",width = 12)

                            )),

                          tabItem(tabName = "karakteristik",
                                  fluidRow(
                                    box(title="Choose Item",solidHeader = TRUE,status = "warning",width = 12,
                                        uiOutput(outputId="maddeler"),submitButton("view")),
                                    box(title="Item Characteristic Curves",solidHeader = TRUE,downloadButton('downloadicc','Download ICC'), plotOutput("karakter")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),status = "primary",width = 6),
                                    box(title="Item Information Curves",solidHeader = TRUE,downloadButton('downloadinfo','Download IIC'), plotOutput("bilgi")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),status = "primary",width = 6)


                                  ))

                        )
                      ))

  server <- function(input, output,session) {

    mydata<-reactive({

      if(input$check==TRUE){

        data<-generate()
      }

      else{
        inFile <- input$dataset
        if (is.null(inFile))
          return("Please upload data")
        dataset<- read_xlsx(inFile$datapath, sheet=1)
        data<-as.data.frame(dataset)}
      data
    })

    output$maddeler <- renderUI({

      selectInput(inputId="boyutvariable",
                  label="Choose an item",
                  choices=c("All",1:length(mydata())), selected=NULL)
    })

    output$modselect<-renderUI({
      selectInput(inputId ="modselect",selected=NULL,label = NULL,choices = c("Rasch Model","2 PLM","3 PLM","Graded Response Model","Generalized Partial Credit Model"))
    })

    output$check<-renderUI({
      checkboxInput(inputId ="check",label = "I Want to Use Generated Data. To use this option, you must first generate data using the side menu. After the data is generated, you can return to this page, select your model and start the analysis.",value = FALSE)
    })

    output$modselectitem<-renderUI({
      selectInput(inputId ="modselectitem",selected=NULL,label = NULL,choices = c("Rasch Model","2 PLM","3 PLM","Graded Response Model","Generalized Partial Credit Model"))
    })

    output$thetaselect<-renderUI({
      selectInput(inputId ="thetaselect",label = NULL,choices = c("Rasch Model","2 PLM","3 PLM","Graded Response Model","Generalized Partial Credit Model"))
    })


    output$methodselect<-renderUI({
      selectInput(inputId ="methodselect",label = NULL,choices = c("Rasch Model","2 PLM","3 PLM"))
    })

    output$localmethod<-renderUI({
      selectInput(inputId ="localmethod",label = NULL,choices = c("Yen's Q3 Statistic","Chen & Thissen- G2 Statistic","Jack-Knife Statistic"),selected = "Chen & Thissen- G2 Statistic")
    })

    output$abilitymethod<-renderUI({
      selectInput(inputId ="abilitymethod",label = NULL,choices = c("ML","EAP","MAP"),selected = "EAP")
    })

    output$aparmin<-renderUI({
      sliderInput(inputId ="aparmin",label = "Min value of a parameters",min = 0,max = 2, step = 0.1,value = 0.5)
    })

    output$aparmax<-renderUI({
      sliderInput(inputId ="aparmax",label = "Max value of a parameters",min = 0,max = 3, step = 0.1,value = 1.5)
    })

    output$bparmean<-renderUI({
      sliderInput(inputId ="bparmean",label = "Mean of b parameters",min = -3,max = 3, step = 0.1,value = 0)
    })

    output$bparsd<-renderUI({
      sliderInput(inputId ="bparsd",label = "Sd of b parameters",min = 0,max = 3, step = 0.1,value = 1)
    })

    output$cparmin<-renderUI({
      sliderInput(inputId ="cparmin",label = "min value of c parameters",min = 0,max = 0.5, step = 0.05,value = 0)
    })

    output$cparmax<-renderUI({
      sliderInput(inputId ="cparmax",label = "max value of c parameters",min = 0,max = 0.5, step = 0.05,value = 0.25)
    })

    output$thetamean<-renderUI({
      sliderInput(inputId ="thetamean",label = "Mean Theta",min = -0.5,max = 0.5, step = 0.1,value = 0)
    })

    output$thetasd<-renderUI({
      sliderInput(inputId ="thetasd",label = "Sd Theta",min = 0.5,max = 1.5, step = 0.1,value = 1)
    })

    output$itemnumber<-renderUI({
      sliderInput(inputId ="itemnumber",label = "Number of items",min = 10,max = 100, step = 1,value = 20)
    })

    output$samplesize<-renderUI({
      sliderInput(inputId ="samplesize",label = "Sample size",min = 100,max = 2000, step = 50,value = 500)
    })

    generate<-reactive({

      if(is.null(input$samplesize)==TRUE)
        return("Please Choose Your Conditions and Click Generate Data Button.")

      n<-input$itemnumber
      theta<-rnorm(n=input$samplesize,mean = input$thetamean,sd=input$thetasd)

      b<-rnorm(n,mean=input$bparmean,sd=input$bparsd)

      a<-runif(n,min = input$aparmin,max=input$aparmax)

      c<-runif(n,min=input$cparmin,max=input$cparmax)


      response<-matrix(NA,ncol = n, nrow=input$samplesize)

      if(input$methodselect=="Rasch Model"){

        frasch <- function(b,theta) {1/(1+exp(-(theta-b)))}


        for( i in 1:input$samplesize ) {
          for( j in 1:n ) {
            response[i,j]<-ifelse(frasch( b=b[j], theta=theta[i]) < runif(1) , 0 ,1)

          }
        }

      }

      if(input$methodselect=="2 PLM"){

        ftwopl <- function(a,b,theta) {1/(1+exp(-(1.7)*a*(theta-b)))}


        for( i in 1:input$samplesize ) {
          for( j in 1:n ) {
            response[i,j]<-ifelse(ftwopl(a=a[j], b=b[j], theta[i]) < runif(1) , 0 ,1)

          }
        }

      }

      if(input$methodselect=="3 PLM"){

        fthreepl <- function(a,b,c,theta) {   c+(1-c)*(1/(1+exp(-(1.7)*a*(theta-b))))}


        for( i in 1:input$samplesize ) {
          for( j in 1:n ) {
            response[i,j]<-ifelse(fthreepl(a=a[j], b=b[j],c=c[j], theta[i]) < runif(1) , 0 ,1)

          }
        }

      }
      response<-as.data.frame(response)
      response
    })

    output$generator<-renderPrint({
      generate()
    })

    parametre<-reactive({



      if(is.null(input$modselect))
        return(NULL)

      data<- mydata()

      if(input$modselect=="Rasch Model"){


        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, printSE=TRUE)
        SE<-c()
        bpar<-c()
        for(i in 1:ncol(data)){
          SE[i]<- coef.gpcm[[i]][2,2]
          bpar[i]<- coef.gpcm[[i]][1,2]
        }
        item<-1:ncol(data)
        table<- cbind(item,bpar,SE)
        tablo<-as.data.frame(table)

        colnames(tablo)<-c("item","b ","SE")
      }


      if(input$modselect=="2 PLM"){
        if(all(data<2)==FALSE)
        {return("Data must be dichotomous in this model.")}

        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE,printSE=TRUE)
        aSE<-c()
        bSE<-c()
        apar<-c()
        bpar<-c()
        for(i in 1:ncol(data)){
          apar[i]<- coef.gpcm[[i]][1,1]
          bpar[i]<- coef.gpcm[[i]][1,2]
          aSE[i]<- coef.gpcm[[i]][2,1]
          bSE[i]<- coef.gpcm[[i]][2,2]
        }
        item<-1:ncol(data)
        tablo<-cbind(item,apar,aSE,bpar,bSE)
        tablo<-as.data.frame(tablo)
        colnames(tablo)<-c("item","a ","SE","b","SE")
      }
      if(input$modselect=="3 PLM"){
        if(all(data<2)==FALSE)
        {return("Data must be dichotomous in this model.")}

        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, printSE=TRUE)
        aSE<-c()
        bSE<-c()
        cSE<-c()
        apar<-c()
        bpar<-c()
        cpar<-c()
        for(i in 1:ncol(data)){
          apar[i]<- coef.gpcm[[i]][1,1]
          bpar[i]<- coef.gpcm[[i]][1,2]
          cpar[i]<- coef.gpcm[[i]][1,3]
          aSE[i]<- coef.gpcm[[i]][2,1]
          bSE[i]<- coef.gpcm[[i]][2,2]
          cSE[i]<- coef.gpcm[[i]][2,3]
        }

        item<-1:ncol(data)
        tablo<-as.data.frame(cbind(item,apar,aSE,bpar,bSE,cpar,cSE))
        colnames(tablo)<-c("item","a","SE","b ","SE","c ","SE")
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE,SE=TRUE)
        tablo<- coef.gpcm$item

      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE,SE=TRUE)
        tablo<- coef.gpcm$item

      }


      tablo<-as.data.frame(round(tablo,3))
    })

    basicresult<-reactive({

      data<-mydata()

      if(input$modselect=="Rasch Model"){

        res<-mirt::mirt(data,1,itemtype = "Rasch")
      }


      if(input$modselect=="2 PLM"){

        res<-mirt::mirt(data,1,itemtype = "2PL")
      }


      if(input$modselect=="3 PLM"){

        res<-mirt::mirt(data,1,itemtype = "3PL")
      }



      if(input$modselect=="Graded Response Model"){

        res<-mirt::mirt(data,1,itemtype = "graded")
      }


      if(input$modselect=="Generalized Partial Credit Model"){

        res<-mirt::mirt(data,1,itemtype = "gpcm")
      }

      res

    })

    output$param<-renderDataTable({
      parametre()
    })

    output$modelfit<-renderTable({

      if(is.null(input$modselect)==TRUE)
        return(NULL)

      data<- mydata()


      if(all(data<2)==TRUE){


        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)

        AICrasch<-results.gpcm@Fit$AIC
        BICrasch<-results.gpcm@Fit$BIC
        loglikelihoodrasch<-results.gpcm@Fit$logLik

        raschfit<-c(AICrasch,BICrasch,loglikelihoodrasch)



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)

        AIC2pl<-results.gpcm@Fit$AIC
        BIC2pl<-results.gpcm@Fit$BIC
        loglikelihood2pl<-results.gpcm@Fit$logLik

        ikiplfit<-c(AIC2pl,BIC2pl,loglikelihood2pl)

        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)

        AIC3pl<-results.gpcm@Fit$AIC
        BIC3pl<-results.gpcm@Fit$BIC
        loglikelihood3pl<-results.gpcm@Fit$logLik

        ucplfit<-c(AIC3pl,BIC3pl,loglikelihood3pl)


        index<-as.data.frame(rbind(raschfit,ikiplfit,ucplfit))

        name<-c("Rasch","2PLM","3PLM")
        index<-as.data.frame(cbind(name,index))
        colnames(index)<-c("Model","AIC","BIC","loglikelihood")
        index
      }

      if(all(data<2)==FALSE){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

        AICgraded<-results.gpcm@Fit$AIC
        BICgraded<-results.gpcm@Fit$BIC
        loglikelihoodgraded<-results.gpcm@Fit$logLik

        gradedfit<-c(AICgraded,BICgraded,loglikelihoodgraded)



        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)

        AICgpcm<-results.gpcm@Fit$AIC
        BICgpcm<-results.gpcm@Fit$BIC
        loglikelihoodgpcm<-results.gpcm@Fit$logLik

        gpcmfit<-c(AICgpcm,BICgpcm,loglikelihoodgpcm)




        index<-as.data.frame(rbind(gradedfit,gpcmfit))

        name<-c("Graded Response Model","GPCM")
        index<-as.data.frame(cbind(name,index))
        colnames(index)<-c("Model","AIC","BIC","loglikelihood")
        index
      }

      index

    })

    output$itemfit<-renderTable({


      if(is.null(input$modselect)==TRUE)
        return(NULL)

      data<-mydata()

      if( is.null(input$modselectitem)==TRUE)
        return(NULL)

      if(input$modselectitem=="Rasch Model"){
        result<-mirt(data,model = 1,itemtype = "Rasch")
        res<-itemfit(result)}

      if(input$modselectitem=="2 PLM"){
        result<-mirt(data,model = 1,itemtype = "2PL")
        res<-itemfit(result)}

      if(input$modselectitem=="3 PLM"){
        result<-mirt(data,model = 1,itemtype = "3PL")
        res<-itemfit(result)}

      if(input$modselectitem=="3 PLM"){
        result<-mirt(data,model = 1,itemtype = "3PL")
        res<-itemfit(result)}

      if(input$modselectitem=="Graded Response Model"){
        result<-mirt(data,model = 1,itemtype = "graded")
        res<-itemfit(result)}

      if(input$modselectitem=="Generalized Partial Credit Model"){
        result<-mirt(data,model = 1,itemtype = "gpcm")
        res<-itemfit(result)}

      res

    })

    thetaa<-reactive({

      if(is.null(input$modselect)==TRUE)
        return(NULL)

      if(is.null(input$abilitymethod)==TRUE)
        return(NULL)


      data<- mydata()
      if(input$modselect=="Rasch Model"){

        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE,method=input$abilitymethod),3)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }


      if(input$modselect=="2 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE,method = input$abilitymethod),3)

        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }
      if(input$modselect=="3 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE,method = input$abilitymethod),3)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE,method = input$abilitymethod),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE,method = input$abilitymethod),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")

      }

      abilities

    })

    output$theta<-renderDataTable({
      thetaa()
    })

    output$wright<-renderPlot({

      if(is.null(input$modselect)==TRUE)
        return(NULL)

      theta<-thetaa()

      theta<-as.vector(theta[,2])
      if(input$modselect=="Rasch Model"|input$modselect=="2 PLM"|input$modselect=="3 PLM"){
        p<-parametre()
        par<-as.vector(p$b)
      }

      if(input$modselect=="Graded Response Model"|input$modselect=="Generalized Partial Credit Model"){
        p<-parametre()

        par<-p[,2:ncol(p)]
      }



      plt<- wrightMap(theta, par, item.side = itemClassic
                      , item.prop = .5, main.title = "Wright Map",thr.lab.cex = 1.5)
      plt
    })

    output$screeplot<-renderPlot({


      if(is.null(input$modselect)==TRUE)
        return(NULL)

      Response<- mydata()
      fa.parallel(Response, fm = 'minres', fa = 'fa')

    })

    output$eigens<-renderDataTable({

      if(is.null(input$modselect)==TRUE)
        return(NULL)

      Response<- mydata()
      a<-fa.parallel(Response, fm = 'minres', fa = 'fa')

      sayi<-1:ncol(Response)
      sonuc<-as.data.frame(cbind(sayi,round(a[[1]],4)))
      colnames(sonuc)<-c("Factor No","Eigen Value")
      sonuc

    })

    infocurve<-reactive({

      if(is.null(input$modselect)==TRUE)
        return(NULL)


      if(is.null(input$boyutvariable))
        return(NULL)


          data<-mydata()



        if(input$boyutvariable=="All"){

          res<-basicresult()
          plot<- plot(res, type = "info", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{
          res<-basicresult()
          plot<- itemplot(res,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}


      plot

    })

    output$bilgi<-renderPlot({
      infocurve()
    })

    icc<-reactive({

      if(is.null(input$modselect)==TRUE)
        return(NULL)

      data<- mydata()
      if(is.null(input$boyutvariable))
        return(NULL)

      res<-basicresult()

        if(input$boyutvariable=="All"){
          plot<- plot(res, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(res,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}

      plot

    })

    output$karakter<-renderPlot({
      icc()
    })

    localind<-reactive({


      if(is.null(input$modselect)==T)
        return("Please Upload Your Data and Start Analze in Main Page")

      if(is.null(input$localmethod)==T)
        return("Please Select Your Method And Start Analyze.")


      res<-basicresult()

      if(input$localmethod=="Yen's Q3 Statistic"){
        output<-mirt::residuals(res,type="Q3")}

      if(input$localmethod=="Chen & Thissen- G2 Statistic"){
        output<-mirt::residuals(res,type="LDG2")}

      if(input$localmethod=="Jack-Knife Statistic"){
        output<-mirt::residuals(res,type="JSI")}

      output<-round(output,3)
      output


    })

    output$local<-renderPrint({

      localind()

    })

    output$info<-renderText({
      paste(p(strong('Package:'), "IRTshiny"),p(strong('Background Packages:'), "mirt,","irtoys,","psych"),
            p(strong('Package Description:'), "Performing IRT analysis such as parameter estimation, ability estimation, item and model fit analyse, local independence assumption, dimensionality assumption, characteristic, and information curves under various models with a user-friendly shiny interface."),

            p(strong('Package Author:'), "Huseyin Yildiz"),
            p(strong('e-mail:'), tags$a(href="mailto:huseyinyildiz35@gmail.com", "huseyinyildiz35@gmail.com"))

      )
    })

    output$preview<-renderDataTable({
      if(is.null(input$dataset)==T)
        return(NULL)

      mydata()
    })

    output$thetastats<-renderTable({
      if(is.null(input$modselect)==TRUE)
        return(NULL)

      if(is.null(input$abilitymethod)==TRUE)
        return(NULL)

      theta<-thetaa()

      reliability<-marginal_rxx(basicresult())

      indiv<-paste("Number of Individuals: ",round(nrow(theta),3))
      meant<-paste("Mean Theta: ",round(mean(theta[,2],na.rm = TRUE),3))
      sdt<-paste("Standart Deviation of Thetas: ",round(sd(theta[,2],na.rm = TRUE),3))
      reli<-paste("Marginal Reliability: ",round(reliability,3))
      meanter<-paste("Mean of Theta Standart Errors: ",round(mean(theta[,3],na.rm = TRUE),3))
      mint<-paste("Minimum Theta: ",round(min(theta[,2],na.rm = TRUE),3))
      maxt<-paste("Maximum Theta: ",round(max(theta[,2],na.rm = TRUE),3))

      stats<-rbind(indiv,meant,sdt,reli,meanter,maxt,mint)
      colnames(stats)<-c("Stats")
      stats


    })

    output$downloaddata <- downloadHandler(
      filename = function() {
        paste("dataset", ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(generate(), file,row.names = FALSE)
      }
    )

    output$downloadparam <- downloadHandler(
      filename = function() {
        paste("parameters", ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(parametre(), file,row.names = FALSE)
      }
    )

    output$downloadtheta <- downloadHandler(
      filename = function() {
        paste("abilities", ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(thetaa(), file,row.names = FALSE)
      }
    )

    output$localdownload <- downloadHandler(
      filename = function() {
        paste("localindipendence", ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(localind(), file,row.names = FALSE)
      }
    )

    output$downloadicc <- downloadHandler(
      filename = function(){
        paste("icc", '.pdf', sep = "")
      },
      content = function(file){
        pdf(file, width = 5, height = 5)
        print(icc())
        dev.off()
      })


    output$downloadinfo <- downloadHandler(
      filename = function(){
        paste("infocurve", '.pdf', sep = "")
      },
      content = function(file){
        pdf(file, width = 5, height = 5)
        print(infocurve())
        dev.off()
      })
  }
  shinyApp(ui, server)
}
