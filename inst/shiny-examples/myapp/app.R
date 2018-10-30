#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinyAce)
library(shinyjs)

ui <- fluidPage(

   # Application title
   titlePanel("T-tests"),
   h6(" T-tests Authored by:", tags$img(src ="K.JPG", height=100, width=100)),

   verbatimTextOutput("preface"),
   tags$img(src = "T.png"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput("datause", label = "CHOOSE WHETHER TO USE DATA FROM THE SHEET OR NOT",
                    choices = c("usedata","donotusedata"), selected = "donotusedata"),

        sliderInput("los", label = "Enter the level of significance:",
                    min = 0.01, max = 0.1, value = 0.05, step = 0.01),
        selectInput("Tail", label = "Choose among less(lower tail),greater(upper tail),two.sided(Two tail)",
                    choices = c("less","greater","two.sided"), selected = "two.sided"),

        conditionalPanel(condition = "input.datause =='donotusedata'",
                         selectInput("Type", label = "choose among onesample,twosample(Independent Sample)",
                                     choices = c("onesample","twosample"), selected = "onesample")),

         conditionalPanel(condition = "input.datause =='usedata'",
                         selectInput("Type1", label = "choose among onesample,twosample(IndependentSample),paired(Dependent Sample)",
                                     choices = c("onesample","twosample","paired"), selected = "onesample"),
                         textInput("mean1","Enter hypothesized population mean for one sample",30),

                         uiOutput("vx"),
                         uiOutput("vx1"),
                        uiOutput("vy")

                        ),
        conditionalPanel(condition = "input.Type == 'onesample' && input.datause =='donotusedata'",
                         textInput("mean","Enter hypothesized population mean",30),
                         textInput("sd","Enter sample standard deviation",1),
                         textInput ("average","Enter sample average",28),
                         textInput ("samplesize","Enter sample size",25) ),
        conditionalPanel(condition = "input.Type == 'twosample'&& input.datause =='donotusedata'",
                         textInput("average1","Enter sample average of group 1",30),
                         textInput("average2","Enter sample average of group 2",29),
                         textInput ("sd1","Enter sample standard deviation of group 1",1),
                         textInput ("sd2","Enter sample standard deviation of group 2",1),
                         textInput ("samplesize1","Enter sample size of group 1",25),
                         textInput ("samplesize2","Enter sample size of group 2",16),
                         selectInput("variancecheck", label = "Variances Equal?",
                                     choices = c("TRUE","FALSE"), selected = "FALSE"))
),
        mainPanel(
          helpText("Copy paste data with variable names from Excel"),
                aceEditor("text", value=
                    "Before	After	Gender
83	87	Male
                    89	88	Male
                    93	91	Male
                    77	77	Female
                    86	93	Female
                    79	83	Female
                    79	84	Female
                    92	94	Male
                    96	98	Female
                    ",  mode="r", theme="white"),

         verbatimTextOutput("distPlot")
          )
   )
)


# Define server logic required to d raw a histogram
server <- function(input, output) {
  output$preface <-renderPrint({

    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))

  })
  output$vx <- renderUI({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    dataframe$extra<- as.factor("extra1")
    a =  data.frame(split(names(dataframe),sapply(dataframe, function(x) paste(class(x), collapse=" "))))
    selectInput("variablex","Select the quantitative variable",choices = unique(a[,-1]),selected = unique(a[,-1]) )


  })
  output$vx1 <- renderUI({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    dataframe$extra<- as.factor("extra1")
    a =  data.frame(split(names(dataframe),sapply(dataframe, function(x) paste(class(x), collapse=" "))))
    selectInput("variablex1","Select another quantitative variable(Only for Paired t-test)",choices = unique(a[,-1]),selected = unique(a[,-1]) )


  })
  output$vy <- renderUI({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    a =  data.frame(split(names(dataframe),sapply(dataframe, function(x) paste(class(x), collapse=" "))))
    selectInput("variabley","Select the qualitative/grouping variable (Only for Two Sample/Independent Sample Tests",choices = unique(a$factor),selected = unique(a$factor) )


  })
   output$distPlot <- renderPrint({
      options(scipen = 999)
      if(input$datause == "usedata")
      {
        if(input$Type1 == "onesample")
        {
          get.text <- reactive({input$text})
          if(is.null(get.text())){return ()}
          dataframe<- read.table(text = get.text(),header = TRUE)
          if (is.null(input$variablex)) return()
          indexnumber =  grep(input$variablex,colnames(dataframe))
          x = data.frame(dataframe[,indexnumber])
          results = t.test(x,alternative = input$Tail,mu = as.numeric(input$mean1),conf.level = (1- input$los))
          print(results)
        }
        if(input$Type1 == "twosample")
        {
          get.text <- reactive({input$text})
          if(is.null(get.text())){return ()}
          dataframe<- read.table(text = get.text(),header = TRUE)
          if (is.null(input$variablex)) return()
          indexnumberx =  grep(input$variablex,colnames(dataframe))
          indexnumbery =  grep(input$variabley,colnames(dataframe))

          x = data.frame(dataframe[,indexnumberx],dataframe[,indexnumbery])
          f =  split(x[,1],x[,2])
          dataset1 = data.frame(x1 = f[1])
          x1 = select(dataset1,1)
          cat(sprintf("\nThe two groups x1 and x2 are as follows\n"))
          cat(sprintf(colnames(x1)))
          cat(sprintf("\n"))
          dataset2 = data.frame(x2 = f[2])

          x2 = select(dataset2,1)

          cat(sprintf(colnames(x2)))
          results = t.test(x1 ,x2, alternative = input$Tail,conf.level = (1-input$los),var.equal = FALSE)
          print(results)
        }
        if(input$Type1 == "paired")
          {
          get.text <- reactive({input$text})
          if(is.null(get.text())){return ()}
          dataframe<- read.table(text = get.text(),header = TRUE)
          if (is.null(input$variablex)) return()
          indexnumberx =  grep(input$variablex,colnames(dataframe))
          indexnumberx1 =  grep(input$variablex1,colnames(dataframe))
          x = data.frame( Before = dataframe[,indexnumberx])
          #print(head(x))
          attach(x)
          x1 = data.frame(After = dataframe[,indexnumberx1])
          #print(head(x1))
          attach(x1)

         results = t.test(Before,After, alternative = input$Tail,conf.level = (1-input$los),paired = TRUE)
         print(results)
          }
      }
     if(input$datause == "donotusedata")
     {
     if(input$Type == "onesample")
     {
     rnorm2 = function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
     data_set = rnorm2(n= as.numeric(input$samplesize), mean = as.numeric(input$average), sd = as.numeric(input$sd))

     results = t.test(data_set,alternative = input$Tail,mu = as.numeric(input$mean),conf.level = (1- input$los))

     a =  results

     if(a$alternative == "greater")
     {
       cat(sprintf("\n Ho :  Mu <= %g\n",a$null.value))
       cat(sprintf("\n Ha :  Mu > %g\n",a$null.value))
     }
     if(a$alternative == "less")
     {
       cat(sprintf("\n Ho :  Mu >= %g\n",a$null.value))
       cat(sprintf("\n Ha :  Mu < %g\n",a$null.value))
     }
     if(a$alternative == "two.sided")
     {
       cat(sprintf("\n Ho :  Mu = %g\n",a$null.value))
       cat(sprintf("\n Ha :  Mu <> %g\n",a$null.value))
     }
     print(a)
     cat(sprintf("\n Test Result Interpretation\n"))
     if(a$p.value<input$los)
     {cat(sprintf("There is enough evidence to reject Null Hypothesis"))}
     if(a$p.value>input$los)
     {cat(sprintf("There is not enough evidence to reject Null Hypothesis"))}
     }
     if(input$Type == "twosample")
     {
       rnorm2 = function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
     data_set1 = rnorm2(n= as.numeric(input$samplesize1), mean = as.numeric(input$average1), sd = as.numeric(input$sd1))
     data_set2 = rnorm2(n= as.numeric(input$samplesize2),mean = as.numeric(input$average2), sd = as.numeric(input$sd2))
      results = t.test(data_set1,data_set2, alternative = input$Tail,conf.level = (1-input$los),var.equal = as.logical(input$variancecheck))
      a =  results

      if(a$alternative == "greater")
      {
        cat(sprintf("\n Ho :  Mu1 <= Mu2\n"))
        cat(sprintf("\n Ha :  Mu1 > Mu2 \n"))
      }
      if(a$alternative == "less")
      {
        cat(sprintf("\n Ho :  Mu1 >= Mu2\n"))
        cat(sprintf("\n Ha :  Mu1 < Mu2 \n"))
      }
      if(a$alternative == "two.sided")
      {
        cat(sprintf("\n Ho :  Mu1 = Mu2\n"))
        cat(sprintf("\n Ha :  Mu1 <> Mu2 \n"))
      }
      print(a)
      cat(sprintf("\n Test Result Interpretation\n"))
      if(a$p.value<input$los)
      {cat(sprintf("There is enough evidence to reject Null Hypothesis"))}
      if(a$p.value>input$los)
      {cat(sprintf("There is not enough evidence to reject Null Hypothesis"))}
     }
}
   })


   outputOptions(output, 'vx', suspendWhenHidden = FALSE)
   outputOptions(output, 'vy', suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)

