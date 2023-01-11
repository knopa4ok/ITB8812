
library(shiny);
library(shinythemes);
library(corrplot)
library(car)
library(caret)
library(psych)
library(earth)
library(doParallel)
library(ggplot2)
library(DT)
library(rlang)
library(shinycssloaders)

# Andmete laadimine
store <- env()
data=read.csv('Andmestik.csv', header = T)
data$Type = as.factor(data$Type)

VeaSummary_exp<- function (data,lev=NULL,model=NULL){
  data$obs <- exp(data$obs)
  data$pred <- exp(data$pred)
  out <- c(
    RMSE=sqrt(mean((data$obs-data$pred)^2)),
    Rsquared=as.numeric(cor(data$obs,data$pred)^2))
  names(out) <- c("RMSE","Rsquared")
  out
}

ui <- fluidPage(
  # Application title
  navbarPage("Klaasi kvaliteet", theme = shinytheme("lumen"),
             tabPanel("Andmed", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Andmete kirjeldamine"),
                          tags$ul(
                            tags$li(
                              tags$b("RI"),
                              " - murdumisnäitaja"
                            ),
                            tags$li(
                              tags$b("Na"),
                              " - naatrium (ühiku mõõt: massiprotsent vastavas oksiidis)"
                            ),
                            tags$li(
                              tags$b("Mg"),
                              " - magneesium"
                            ),
                            tags$li(
                              tags$b("Al"),
                              " - alumiinium"
                            ),
                            tags$li(
                              tags$b("Si"),
                              " - räni"
                            ),
                            tags$li(
                              tags$b("K"),
                              " - kaalium"
                            ),
                            tags$li(
                              tags$b("Ca"),
                              " - kaltsium"
                            ),
                            tags$li(
                              tags$b("Ba"),
                              " - baarium"
                            ),
                            tags$li(
                              tags$b("Fe"),
                              " - raud"
                            ),
                            tags$li(
                              tags$b("Type of glass (class attribute):"),
                              tags$ol(
                                tags$li("Ehitise klaas (float)"),
                                tags$li("Ehitise klaas (non float)"),
                                tags$li("Sõiduki klaas (float)"),
                                tags$li("Sõiduki klaas (non float)"),
                                tags$li("Konteinerid"),
                                tags$li("Lauanõud"),
                                tags$li("Esituled"),
                              )
                            ),
                          )
                        ),
                        mainPanel(
                          titlePanel("Andmestiku kirjeldus"),
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            tags$div(
                              tags$p("Antud projekti jaoks oli valitud “Klassi klassfikatsioon” andmestik, mis sisaldab andmeid klaasis sisaldavatest elementidest, klaasi murdumisnäitajatest ning klaasi tüübidest. Antud andmed oli kätte saadud kriminoloogia uuringute jaoks. Kuriteopaigal mahajäetud klaasi on võimalik kasutada tõendina juhul kui klaas on õigesti tuvastatud. Anmestik koosneb 10 tunnustest ning 214 objektidest (täpsem andmestiku kirjeldus on toodud töö teises osas “Andmete kirjeldamine ja esmaanalüüs”). Uuringu jaoks olid võetud kõik andmestikus olevad objektid. Andmed csv vormingus on võetud kaggle.com veebilehelt."),
                              tags$p("Viide",
                                     tags$a(href="https://www.kaggle.com/uciml/glass?select=glass.csv", "https://www.kaggle.com/uciml/glass?select=glass.csv")
                              )
                            ),
                            column(2,
                                   selectInput("RI",
                                               "RI:",
                                               c("All",
                                                 unique(as.character(data$RI))))
                            ),
                            column(1,
                                   selectInput("Na",
                                               "Na:",
                                               c("All",
                                                 unique(as.character(data$Na))))
                            ),
                            column(1,
                                   selectInput("Mg",
                                               "Mg:",
                                               c("All",
                                                 unique(as.character(data$Mg))))
                            ),
                            column(1,
                                   selectInput("Al",
                                               "Al:",
                                               c("All",
                                                 unique(as.character(data$Al))))
                            ),
                            column(1,
                                   selectInput("Si",
                                               "Si:",
                                               c("All",
                                                 unique(as.character(data$Si))))
                            ),
                            column(1,
                                   selectInput("K",
                                               "K:",
                                               c("All",
                                                 unique(as.character(data$K))))
                            ),
                            column(1,
                                   selectInput("Ca",
                                               "Ca:",
                                               c("All",
                                                 unique(as.character(data$Ca))))
                            ),
                            column(1,
                                   selectInput("Ba",
                                               "Ba:",
                                               c("All",
                                                 unique(as.character(data$Ba))))
                            ),
                            column(1,
                                   selectInput("Fe",
                                               "Fe:",
                                               c("All",
                                                 unique(as.character(data$Fe))))
                            ),
                            column(2,
                                   selectInput("Type",
                                               "Type:",
                                               c("All",
                                                 unique(as.character(data$Type))))
                            ),
                            
                          ),
                          # Create a new row for the table.
                          DT::dataTableOutput("andmestiku_kirjeldus")%>% withSpinner(type = 5),
                        )
                      ),
             ),
             tabPanel("Andmete esmaanalüüs", fluid = TRUE,
                      mainPanel(
                        titlePanel("Klaasi murdumisnäitaja sõltuvust arvtunnustest"),
                        tabsetPanel(
                          type='tabs',
                          tabPanel('Elementide hajuvusdiagramm', 
                                   tags$h4(
                                     "Hajuvusdiagrammi seoste mustritest saab järeldada, et kõige tugevam lineaarne sõltuvus on tunnuste",
                                     tags$b("RI"),
                                     "ja ",
                                     tags$b("Ca"),
                                     "vahel. Saab ära märkida, et ",
                                     tags$b("K, Ba"),
                                     "ja ",
                                     tags$b("Fe"),
                                     "on vasakpoolse asümmeetriaga tunnused pika sabaga ja erinditega paremalt."),
                                   plotOutput('esmaanaluus_hajuvusdiagramm', height='500', width='auto')%>% withSpinner(type = 5)),
                          tabPanel('Elementide korrelatsioonimaatriks 1', 
                                   tags$h4("Ka korrelatsiooni maatriksi järgi kõige tugevam lineaarne sõltuvus on tunnuste ",
                                           tags$b("RI"),
                                           "ja ",
                                           tags$b("Ca"),
                                           "vahel, nende vahelise korrelatsionikordaja väärtuseks on ",
                                           tags$b("0.81"),
                                           ", mis tähendab, et nende tunnuste vahel on tugev kasvav lineaarne sõltuvus."),
                                   plotOutput('esmaanaluus_korrelatsioonimaatriks1', height='500',width='auto')%>% withSpinner(type = 5)),
                          tabPanel('Elementide korrelatsioonimaatriks 2', 
                                   tags$h4(
                                     "Korrelatsioonimaatriksist saab välja lugeda, et omavahel korrileeruvad paarid on ",
                                     tags$b("RI"),
                                     "- ",
                                     tags$b("Ca"),
                                     ",",
                                     tags$b("Ba "),
                                     "- ",
                                     tags$b("Al"),
                                     " ja ",
                                     tags$b("Al"),"- ",
                                     tags$b("K"),
                                     ", mis on vastavalt ", 
                                     tags$b("R = 0.81"),
                                     ", ",
                                     tags$b("R = 0.48 "), 
                                     "ja", 
                                     tags$b(" R = 0.33. "),
                                     "Teised lineaarsed seoses on kas nõrgad või väga nõrgad.",
                                   ),
                                   plotOutput('esmaanaluus_korrelatsioonimaatriks2', height='500',width='auto')%>% withSpinner(type = 5)),
                        ),
                      )
             ),
             tabPanel("MLR mudel", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('mlr_sidebar_buttons')
                        ),
                        mainPanel(
                          tabsetPanel(id='mlr_tabset_id',
                                      tabPanel('Diagnoostilised graafikud', value = '1',
                                               tags$h4(textOutput("mlr_diagnostic_title")),
                                               plotOutput("mlr_diagnostic", height='500',width='auto')%>% withSpinner(type = 5)
                                      ),
                                      tabPanel('Mudeli prognoosid', value = '2',
                                               tags$h4(textOutput("mlr_prognoosid_title")),
                                               plotOutput("mlr_prognoosid", height='500',width='auto') %>% withSpinner(type = 5)
                                      )
                          )
                        ),
                      )
             ),
             tabPanel("MARS mudel", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('mars_sidebar_selectbox')
                        ),
                        mainPanel(
                          tabsetPanel(id='mars_tabset_id', selected = 1,
                                      tabPanel('Diagnoostilised graafikud', value = 1,
                                               tags$h4(textOutput("mars_diagnostic_title")),
                                               plotOutput("mars_diagnostic", height='500',width='auto')%>% withSpinner(type = 5)
                                      ),
                                      tabPanel('Prognoosid', value = 2,
                                               tags$h4(textOutput("mars_prognoosid_title")),
                                               plotOutput("mars_prognoosid", height='500',width='auto') %>% withSpinner(type = 5)
                                      ),
                                      tabPanel('Visualiseerimine', value = 3,
                                               tags$h4(textOutput("mars_visualiseerimine_title")),
                                               plotOutput("mars_visualiseerimine", height='500',width='auto') %>% withSpinner(type = 5)
                                      ),
                                      tabPanel('Kokkuvõtte', value = 4,
                                               tags$h4(textOutput("mars_kokkuvotte_title")),
                                               uiOutput("mars_kokkuvotte", height='200',width='auto'),
                                               plotOutput("mars_kokkuvotte_plot", height='500',width='auto')%>% withSpinner(type = 5)
                                      )
                          )
                        ),
                      ),
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  print('======= START ========')
  print(timestamp())
  # Andmed - Andmete ettevalmistamine
  data_scale = data;
  data_scale[,names(Filter(is.numeric, data[,-1]))]=lapply(data[,names(Filter(is.numeric, data[,-1]))],scale)
  # Andmed
  output$andmestiku_kirjeldus <- DT::renderDataTable(DT::datatable({
    
    if (input$RI != "All") {
      data <- data[data$RI == input$RI,]
    }
    if (input$Na != "All") {
      data <- data[data$Na == input$Na,]
    }
    if (input$Mg != "All") {
      data <- data[data$Mg == input$Mg,]
    }
    if (input$Al != "All") {
      data <- data[data$Al == input$Al,]
    }
    if (input$Si != "All") {
      data <- data[data$Si == input$Si,]
    }
    if (input$K != "All") {
      data <- data[data$K == input$K,]
    }
    if (input$Ca != "All") {
      data <- data[data$Ca == input$Ca,]
    }
    if (input$Ba != "All") {
      data <- data[data$Ba == input$Ba,]
    }
    if (input$Fe != "All") {
      data <- data[data$Fe == input$Fe,]
    }
    if (input$Type != "All") {
      data <- data[data$Type == input$Type,]
    }
    data
  }))

  # Andmete esmaanaluus
  output$esmaanaluus_hajuvusdiagramm <- renderPlot({ #2
    par(mfrow=c(1,1))
    pairs.panels(Filter(is.numeric,data))
  })%>%bindCache('esmaanaluus_hajuvusdiagramm')
  output$esmaanaluus_korrelatsioonimaatriks1 <- renderPlot({ #3
    corrplot(cor(Filter(is.numeric, data)),method='number')
  })%>%bindCache('esmaanaluus_korrelatsioonimaatriks1')
  output$esmaanaluus_korrelatsioonimaatriks2 <- renderPlot({ #4
    dataCor <- cor(Filter(is.numeric,data))
    corrplot.mixed(dataCor, lower.col = "black", number.cex = 0.9,upper = "ellipse", tl.cex=0.8,cl.cex=0.8,cl.ratio=0.4)
  })%>%bindCache('esmaanaluus_korrelatsioonimaatriks2')

  gc()
  #Mälu puhastus
  
  # Andmete ettevalmistus
  print("Andmete ettevalmistus")
  print(timestamp())
  
  n = nrow(data)
  RNGkind(sample.kind = "Rounding")
  set.seed(123)
  sample_index= sample(1:n,size=n,replace=F)
  
  train = sample_index[1:floor(n*.80)]
  
  data_train = data_scale[train,]
  data_train_trans <- data_train;
  data_test = data_scale[-train,]
  
  mlr = lm(RI~., data = data_train) # MLR mudeli konstrueerimine
  mlr_step = step(mlr, trace=0) # Sammregressioon optimaalse mudeli leidmiseks
  mlr_trans = lm(RI~., data = data_train_trans) 
  mlr_step_trans = step(mlr_trans, trace=0);
  
  lambda <- sapply(data_train[,names(Filter(is.numeric, data_train))[-1]],function(x){powerTransform(x, family="yjPower")$roundlam})
  
  data_train_trans[,names(Filter(is.numeric, data_train))[-1]] <- yjPower(data_train[,names(Filter(is.numeric, data_train))[-1]], lambda)
  data_train_trans$RI <- log(data_train$RI)
  
  mlr_poly <- lm(RI ~ Na + Mg + Al + Si + Ca + poly(Ba,2) + Type, data=data_train)
  mlr_poly <- step(mlr_poly, trace=0)
  
  out <- which(abs(mlr_poly$residuals/sd(mlr_poly$residuals))>3 & hatvalues(mlr_poly)>0.10)
  data_train_out <- data_train[-out,]
  
  print("Andmete ettevalmistus STOP")
  print(timestamp())
  
  # MLR
  print("MLR START")
  print(timestamp())
  output$mlr_sidebar_buttons <- renderUI({
    req(input$mlr_tabset_id);
    if(input$mlr_tabset_id == '1'){
      radioButtons('mlr_diagnostic_plot_type', selected = 1,
                   label = h3('Valige MLR graafiku tüüp'),
                   choices = list(
                     "MLR diagnoostika" = 1, 
                     "Jääkide analüüs" = 2, 
                     "MLR komponeentide selgitamine" = 3,
                     "Polünoommudeli jääkide analüüs" = 4,
                     "Jääkide analüüs" = 5)
      )
    }
    else if(input$mlr_tabset_id == '2'){
      radioButtons('mlr_prognoosid_plot_type',selected = 1,
                   label = h3('Valige MLR graafiku tüüp'),
                   choices = list(
                     "Mudeli prognoos" = 1, 
                     "Teisendatud mudeli prognoos" = 2, 
                     "Polünoommudeli prognoos" = 3)
      )
    }
  })
  output$mlr_diagnostic <- renderPlot({
    # function definition
    VeaSummary<- function (data,lev=NULL,model=NULL){ 
      out <- c(
        RMSE=sqrt(mean((data$obs-data$pred)^2)),
        Rsquared=as.numeric(cor(data$obs,data$pred)^2)
      )
      names(out) <- c("RMSE","Rsquared")
      out
    } 
    
    # Check if variables already defined, to increase perfomance with cache using
    if(!exists("data_test_trans")){
      data_test_trans <- data_test
      data_test_trans[,names(Filter(is.numeric, data_test))[-1]] <- yjPower(data_test[,names(Filter(is.numeric, data_test))[-1]], lambda)
      data_test_trans$RI <- log(data_test$RI)
    }
    
    if(is.null(store$diagnostic_plot_type) || store$diagnostic_plot_type == input$mlr_diagnostic_plot_type){

      system.time({
        train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10, summaryFunction = VeaSummary_exp)
        mlr_trans <- train(RI ~ Na + Mg + Al + Si + K + Ca + Ba + Fe + Type, data=data_train_trans, method = "lm", trControl = train.control,metric="RMSE")
      })
      
      env_cache(store, 'mlr_trans', mlr_trans)
    }else{
      mlr_trans = store$mlr_trans
    }
    # Return requested plot
    req(input$mlr_diagnostic_plot_type);
    if(input$mlr_diagnostic_plot_type == "1"){ #5
      par(mfrow=c(2,2))
      plot(lm(mlr_step))  
    } 
    else if(input$mlr_diagnostic_plot_type == "2"){ #11
      par(mfrow=c(2,2))
      plot(lm(mlr_step_trans)) 
    }
    else if(input$mlr_diagnostic_plot_type == "3"){ #14
      crPlots(mlr_step) 
    }
    else if(input$mlr_diagnostic_plot_type == "4"){ #16
      par(mfrow=c(2,2))
      plot(mlr_poly)
    }
    else if(input$mlr_diagnostic_plot_type == "5"){ #18
      mlr_out <- lm(RI ~ Na + Mg + Al + Si + Ca + poly(Ba,2) + Type, data=data_train_out) 
      mlr_step_out <- step(mlr_out, trace=0)
      ## Andmete visualiseerimine
      par(mfrow=c(2,2))
      plot(lm(mlr_step_out))
    }
  })%>% bindCache('mlr_diagnostic',input$mlr_diagnostic_plot_type)
  output$mlr_diagnostic_title <- renderText({
    req(input$mlr_diagnostic_plot_type);
    if(input$mlr_diagnostic_plot_type == "1"){
      return("Regressioonanalüüsi eeldused ei ole täidetud. Mudelis esinevad erindid ja omapärased väärtused. Mudeli jääkides esineb mittelineaarsus ning jääkide hajuvus ei ole homogeenne (heteroskedastilisus), kõik see tähendab, et mudel on võimalik parandada, kasutades argumentide ja funktsioontunnuse Y teisendamist.")
    } 
    else if(input$mlr_diagnostic_plot_type == "2"){
      return("Jääkide analüüs näitab, et mudel ei eristu palju eelmisest mudelist.")
    }
    else if(input$mlr_diagnostic_plot_type == "3"){
      return("Kuna argumentide teisendamine ei aidanud mudelit parandada, kasutame edaspidi mudeli parandamiseks lihtsa MLR mudelit. Mudeli täpsust võib proovida parandada, kasutades mudelis mittelineaarseid komponente.")
    }
    else if(input$mlr_diagnostic_plot_type == "4"){
      return("Jääkide analüüs näitab, et mudel vastab eeldustele.")
    }
    else if(input$mlr_diagnostic_plot_type == "5"){
      return("Jääkide analüüs näitab, et punased jooned on praktiliselt horisontaalsed ja mudel vastab eeldustele.")
    }
    
  }) %>% bindCache('mlr_diagnostic_title',input$mlr_prognoosid_plot_type)
  output$mlr_prognoosid <- renderPlot({ #6,12,15
    req(input$mlr_prognoosid_plot_type);
    if(input$mlr_prognoosid_plot_type == "1"){ #6
      plot(data_train$RI,mlr_step$fitted.values,xlab="Actual RI",ylab="Predicted RI")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth(data_train$RI,mlr_step$fitted.values), col='blue', lwd=2)
    } 
    else if(input$mlr_prognoosid_plot_type == "2"){ #12
      plot(exp(data_train_trans$RI), exp(mlr_step_trans$fitted.values), xlab="Actual RI",ylab="Predicted RI")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth(exp(data_train_trans$RI),exp(mlr_step_trans$fitted.values)), col='blue', lwd=2)
    }
    else if(input$mlr_prognoosid_plot_type == "3"){ #15
      plot((data_train$RI), (mlr_poly$fitted.values), xlab="Actual Value",ylab="Predicted Value")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth((data_train$RI),(mlr_poly$fitted.values)), col='blue', lwd=2)
    }
    
    
  }) %>% bindCache('mlr_prognoosid',input$mlr_prognoosid_plot_type)
  output$mlr_prognoosid_title <- renderText({
    req(input$mlr_prognoosid_plot_type);
    if(input$mlr_prognoosid_plot_type == "1"){
      return("Võrdleme mudeli prognoose tunnuse Y tegelike väärtustega fitted/predicted vs actual diagrammil. Ideaalis punktid peavad paiknema y=x punasel joonel. Scatterploti siledav sinine joon näitab tegelikku tendentsi.")
    } 
    else if(input$mlr_prognoosid_plot_type == "2"){
      return("Võrdleme teisendatud mudeli prognoose tunnuse Y tegelike väärtustega:")
    }
    else if(input$mlr_prognoosid_plot_type == "3"){
      return("Võrdleme Polünoommudeli prognoose tunnuse Y tegelike väärtustega:")
    }
  })
  print("MLR END")
  # MARS
  print("MARS START")
  print(timestamp())
  if(!exists('mars1')){
    system.time({
      RNGkind(sample.kind = "Rounding")
      set.seed(123)
      mars1 = earth(RI~.,data=data_train,ncross=5,nfold=5,keepxy=T)
    })
    gc()
  }
  if(is.null(store$cv_mars2)){
    system.time({
      RNGkind(sample.kind = "Rounding")
      set.seed(123)
      mars2 = earth(log(RI)~.,data=data_train,ncross=5,nfold=5,keepxy=T)
      env_cache(store, 'mars2', mars2)
      rm('mars2')
    })
  }
  if(is.null(store$cv_mars27)){
    hyper_grid <- expand.grid(degree=1, nprune=2:30)
    Mycluster = makeCluster(detectCores() -1)
    registerDoParallel(Mycluster)
    registerDoSEQ()
    
    system.time({
      RNGkind(sample.kind = "Rounding")
      set.seed(123)
      train.control <- trainControl(method = "repeatedcv",number = 5, repeats = 5,
                                    summaryFunction = VeaSummary_exp)
      cv_mars27 <- train(log(RI)~.,data=data_train,
                       method = "earth",
                       metric = "RMSE",
                       trControl = train.control,
                       tuneGrid = hyper_grid)
    })
    
    env_cache(store, 'cv_mars27', cv_mars27)
    rm('cv_mars27')
    stopCluster(Mycluster)
  }
  if(is.null(store$cv_mars3)){
    mars3 = earth(log(RI)~.,data=data_train,degree=3,nprune=46)
    env_cache(store, 'mars3', mars3)
    rm('mars3')
  }
  gc()
  output$mars_sidebar_selectbox <- renderUI({
    req(input$mars_tabset_id);
    if(input$mars_tabset_id == '1'){
      selectInput('mars_diagnostic_plot_type', selected = 1,
                  label = h3('Valige MARS graafiku tüüp'),
                  choices = list(
                    "MARS-mudeli diagnostika" = 1, 
                    "Ristvalideerimine" = 2, 
                    "MARS-mudeli diagnostika degree-1" = 3,
                    "MARS-mudeli diagnostika degree-3" = 4)
      )
    }
    else if(input$mars_tabset_id == '2'){
      selectInput('mars_prognoosid_plot_type',selected = 1,
                  label = h3('Valige MARS graafiku tüüp'),
                  choices = list(
                    "Tunnuse Y prognoosi võrdlemine" = 1, 
                    "MARS-mudeli prognoos1" = 2, 
                    "MARS-mudeli prognoos2" = 3)
      )
    }
    else if(input$mars_tabset_id == '3'){
      tags$div(selectInput('mars_visualiseerimine_plot_type',selected = 1,
                           label = h3('Valige MARS visualiseerimise graafiku tüüp'),
                           choices = list(
                             "Parameetrite tulemuste visualiseerimine(RMSE)1" = 1, 
                             "Parameetrite tulemuste visualiseerimine(RMSE)2" = 2)
      ),
      uiOutput("mars_visualiseerimine_sliders") 
      )
    }
    else if(input$mars_tabset_id == '4'){
      tags$img(
        width="90%",
        src='http://filosoofiakirjutamine.weebly.com/uploads/5/5/1/2/5512308/1672096.gif?320')
    }
  })
  output$mars_diagnostic <- renderPlot({ #22,23,28,31
    req(input$mars_diagnostic_plot_type);
    if(input$mars_diagnostic_plot_type == "1"){ #22
      plot(mars1) 
    }
    if(input$mars_diagnostic_plot_type == "2"){ #23
      plot(store$mars2)
    }
    if(input$mars_diagnostic_plot_type == "3"){ #28
      plot(store$cv_mars27$finalModel)
    }
    if(input$mars_diagnostic_plot_type == "4"){ #31
      plot(store$mars3)
    }
  }) %>% bindCache('mars_diagnostic',input$mars_diagnostic_plot_type)
  output$mars_prognoosid_title <- renderText({
    req(input$mars_prognoosid_plot_type);
    if(input$mars_prognoosid_plot_type == 1){
      return("Võrdleme mudeli prognoose tunnuse Y tegelike väärtustega. MARS mudelil on samad probleemid nagu parimal MLR mudelil.")
    } 
    else if(input$mars_prognoosid_plot_type == 2){
      return("Treenimisandmetel MARS degree=1 mudel annab halvema tulemuse võrreldes parima MLR mudeliga.")
    }
    else if(input$mars_prognoosid_plot_type == 3){
      return("MARS degree=3 mudel on kõige täpsem võrreldes eelmiste mudelitega.")
    }
  })
  output$mars_prognoosid <- renderPlot({ #24,29,32
    req(input$mars_prognoosid_plot_type);
    if(input$mars_prognoosid_plot_type == 1){ #24
      plot(data_train$RI,exp(store$mars2$fitted.values),xlab="Actual Value",ylab="Predicted Value")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth(data_train$RI,exp(store$mars2$fitted.values)), col='blue', lwd=2)
    } 
    else if(input$mars_prognoosid_plot_type == 2){ #29
      plot(data_train$RI,exp(store$cv_mars27$finalModel$fitted.values),xlab="Actual Value",ylab="Predicted Value")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth(data_train$RI,exp(store$cv_mars27$finalModel$fitted.values)), col='blue', lwd=2)
    } 
    else if(input$mars_prognoosid_plot_type == 3){ #32
      plot(data_train$RI,exp(store$mars3$fitted.values),xlab="Actual Value",ylab="Predicted Value")
      abline(0,1,lwd=2,col="red")
      lines(loess.smooth(data_train$RI,exp(store$mars3$fitted.values)), col='blue', lwd=2)
    } 
  }) %>% bindCache('mars_prognoosid',input$mars_prognoosid_plot_type)
  output$mars_visualiseerimine_title <- renderText({
    req(input$mars_visualiseerimine_plot_type);
    if(input$mars_visualiseerimine_plot_type == 1){
      return("Parameetrite tuunimise tulemuste visualiseerimine(RMSE)")
    }
    else if(input$mars_visualiseerimine_plot_type == 1){
      return("Parameetrite tuunimise tulemuste visualiseerimine(RMSE) Parameetrite tuunimise tulemuste visualiseerimine")
    } 
  })
  output$mars_visualiseerimine <- renderPlot({ #26,27
    req(input$mars_visualiseerimine_plot_type);
    if(input$mars_visualiseerimine_plot_type == 1){ #26
      
      hyper_grid <- expand.grid(degree=1:3, nprune=trunc(seq(2, 100, length.out = 10)))
      VeaSummary_exp<- function (data,lev=NULL,model=NULL){
        data$obs <- exp(data$obs)
        data$pred <- exp(data$pred)
        out <- c(RMSE=sqrt(mean((data$obs-data$pred)^2)),
          Rsquared=as.numeric(cor(data$obs,data$pred)^2))
        names(out) <- c("RMSE","Rsquared")
        out
      }
      gc();
      Mycluster = makeCluster(detectCores()-1)
      registerDoParallel(Mycluster)
      registerDoSEQ()
      system.time({
        RNGkind(sample.kind = "Rounding")
        set.seed(123)
        train.control <- trainControl(method = "repeatedcv",number = 5, repeats = 5,
                                      summaryFunction = VeaSummary_exp)
        cv_mars <- train(log(RI)~.,data=data_train,
                         method = "earth",
                         metric = "RMSE",
                         trControl = train.control,
                         tuneGrid = hyper_grid)
      })
      stopCluster(Mycluster)
      gc()
      p <- ggplot(cv_mars, aes(results$nprune, result$RMSE)) +
        coord_cartesian(xlim=input$mars_visual_slider_x,ylim=input$mars_visual_slider_y)
      p
      
    } 
    else if(input$mars_visualiseerimine_plot_type == 2){ #27
      p <- ggplot(cv_mars27(), aes(results$nprune, result$RMSE)) +
        coord_cartesian(xlim=input$mars_visual_slider_x,ylim=input$mars_visual_slider_y)
      p
    }
  }) %>% bindCache('mars_visualiseerimine',input$mars_visualiseerimine_plot_type)
  output$mars_kokkuvotte_title <- renderText({
    return("Võrdleme mudeli prognoose tunnuse Y tegelike väärtustega fitted/predicted vs actual diagrammil. Ideaalis punktid peavad paiknema y=x punasel joonel. Scatterploti siledav sinine joon näitab tegelikku tendentsi.")
  })
  output$mars_kokkuvotte <- renderUI({#34
    tags$div(
      tags$b('Parimate MARS mudelite Log(Y) teisendusega käitumine testandmetel:'),
      tags$ul(
        tags$ol("degree=1 teisendamata, 10 termi:",
                tags$b("RMSE=0.000891108, R2=88.59%")),
        tags$ol("degree=1 teisendatud, 10 termi:",
                tags$b("RMSE=0.0008910277, R2=88.60%")),
        tags$ol("degree=1, tuunitud parameetritega:",
                tags$b("RMSE=0.001178749, R2=80.73%")),
        tags$ol("degree=3, 17 term:",
                tags$b("RMSE=0.0004736428, R2=92.56%")),
        tags$ol("Parim MARS mudel on 17 termiga mudel degree=3:",
                tags$b("RMSE=0.0004736428, R2=92.56%"))
      ),
    )
  });
  output$mars_kokkuvotte_plot <- renderPlot({ #34
    op <- par(mar=c(5,8,4,2)) 
    plot(x = varImp(store$mars3)$Overall, y = reorder(factor(rownames(varImp(store$mars3))),varImp(store$mars3)$Overall), main = "Muutujate tähtsus", yaxt = "n", ylab = "", xlab = "",pch = 19, col = 4, cex=1.2)
    axis(2, at = 1:nrow(varImp(store$mars3)), labels = levels(reorder(factor(rownames(varImp(store$mars3))),varImp(store$mars3)$Overall)), las = 2,cex.axis=1.1,tck = 1,lty = 2, col = "gray")
  }) %>% bindCache('mars_kokkuvotte_plot');
  
  print("MARS OUTPUT END")
  print(timestamp())
  print(gc())
  gc()
  print(timestamp())  
  print("====== FINISH =======")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
