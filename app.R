library(data.table);library(shiny);library(shinydashboard);library(magrittr)
#DTT
pathdados="/var/www/dados-dtt.csv"
#pathdados="dados-dtt.csv"
if(!file.exists(pathdados)){
  dados=data.table(matrix(ncol = 190,nrow = 0))
  colnames(dados)=c("Participante", "Condicao", "Tarefa", "Analista", "tar1", "tar2", 
                    "tar3", "tar4", "tar5", "tar6.1", "tar6.2", "tar6.3", "tar6.4", 
                    "tar6.5", "tar6.6", "tar6.7", "tar6.8", "tar6.9", "tar6.10", 
                    "tar6.11", "tar6.12", "tar7.1", "tar7.2", "tar7.3", "tar7.4", 
                    "tar7.5", "tar7.6", "tar7.7", "tar7.8", "tar7.9", "tar7.10", 
                    "tar7.11", "tar7.12", "tar8.1", "tar8.2", "tar8.3", "tar8.4", 
                    "tar8.5", "tar8.6", "tar8.7", "tar8.8", "tar8.9", "tar8.10", 
                    "tar8.11", "tar8.12", "tar9.1", "tar9.2", "tar9.3", "tar9.4", 
                    "tar9.5", "tar9.6", "tar9.7", "tar9.8", "tar9.9", "tar9.10", 
                    "tar9.11", "tar9.12", "tar10.1", "tar10.2", "tar10.3", "tar10.4", 
                    "tar10.5", "tar10.6", "tar10.7", "tar10.8", "tar10.9", "tar10.10", 
                    "tar10.11", "tar10.12", "tar11.1", "tar11.2", "tar11.3", "tar11.4", 
                    "tar11.5", "tar11.6", "tar11.7", "tar11.8", "tar11.9", "tar11.10", 
                    "tar11.11", "tar11.12", "tar12.1", "tar12.2", "tar12.3", "tar12.4", 
                    "tar12.5", "tar12.6", "tar12.7", "tar12.8", "tar12.9", "tar12.10", 
                    "tar12.11", "tar12.12", "tar13.1", "tar13.2", "tar13.3", "tar13.4", 
                    "tar13.5", "tar13.6", "tar13.7", "tar13.8", "tar13.9", "tar13.10", 
                    "tar13.11", "tar13.12", "tar14.1", "tar14.2", "tar14.3", "tar14.4", 
                    "tar14.5", "tar14.6", "tar14.7", "tar14.8", "tar14.9", "tar14.10", 
                    "tar14.11", "tar14.12", "tar15.1", "tar15.2", "tar15.3", "tar15.4", 
                    "tar15.5", "tar15.6", "tar15.7", "tar15.8", "tar15.9", "tar15.10", 
                    "tar15.11", "tar15.12", "tar16.1", "tar16.2", "tar16.3", "tar16.4", 
                    "tar16.5", "tar16.6", "tar16.7", "tar16.8", "tar16.9", "tar16.10", 
                    "tar16.11", "tar16.12", "tar17.1", "tar17.2", "tar17.3", "tar17.4", 
                    "tar17.5", "tar17.6", "tar17.7", "tar17.8", "tar17.9", "tar17.10", 
                    "tar17.11", "tar17.12", "tar18.1", "tar18.2", "tar18.3", "tar18.4", 
                    "tar18.5", "tar18.6", "tar18.7", "tar18.8", "tar18.9", "tar18.10", 
                    "tar18.11", "tar18.12", "tar19.1", "tar19.2", "tar19.3", "tar19.4", 
                    "tar19.5", "tar19.6", "tar19.7", "tar19.8", "tar19.9", "tar19.10", 
                    "tar19.11", "tar19.12", "tar20.1", "tar20.2", "tar20.3", "tar20.4", 
                    "tar20.5", "tar20.6", "tar20.7", "tar20.8", "tar20.9", "tar20.10", 
                    "tar20.11", "tar20.12", "tar21")
  fwrite(dados,pathdados)
}else{
  dados=fread(pathdados)
}

construct=function(x){
  tar1=x[grep("1",x)[1]+1]
  tar2=x[grep("2",x)[1]+1]
  tar3=x[grep("3",x)[1]+1]
  tar4=x[grep("4",x)[1]+1]
  tar5=x[grep("5",x)[1]+1]
  tar6=x[(grep("6",x)[2]+1):(grep("6",x)[2]+12)];tar6[tar6==""]=NA
  tar7=x[(grep("7",x)[2]+1):(grep("7",x)[2]+12)];tar7[tar7==""]=NA
  tar8=x[(grep("8",x)[2]+1):(grep("8",x)[2]+12)];tar8[tar8==""]=NA
  tar9=x[(grep("9",x)[2]+1):(grep("9",x)[2]+12)];tar9[tar9==""]=NA
  tar10=x[(grep("10",x)[2]+1):(grep("10",x)[2]+12)];tar10[tar10==""]=NA
  tar11=x[(grep("11",x)[3]+1):(grep("11",x)[3]+12)];tar11[tar11==""]=NA
  tar12=x[(grep("12",x)[3]+1):(grep("12",x)[3]+12)];tar12[tar12==""]=NA
  tar13=x[(grep("13",x)[1]+1):(grep("13",x)[1]+12)];tar13[tar13==""]=NA
  tar14=x[(grep("14",x)[1]+1):(grep("14",x)[1]+12)];tar14[tar14==""]=NA
  tar15=x[(grep("15",x)[1]+1):(grep("15",x)[1]+12)];tar15[tar15==""]=NA
  tar16=x[(grep("16",x)[1]+1):(grep("16",x)[1]+12)];tar16[tar16==""]=NA
  tar17=x[(grep("17",x)[1]+1):(grep("17",x)[1]+12)];tar17[tar17==""]=NA
  tar18=x[(grep("18",x)[1]+1):(grep("18",x)[1]+12)];tar18[tar18==""]=NA
  tar19=x[(grep("19",x)[1]+1):(grep("19",x)[1]+12)];tar19[tar19==""]=NA
  tar20=x[(grep("20",x)[1]+1):(grep("20",x)[1]+12)];tar20[tar20==""]=NA
  tar21=x[grep("21",x)[1]+1]
  res=c(tar1,tar2,tar3,tar4,tar5,tar6,tar7,tar8,tar9,tar10,tar11,tar12,tar13,tar14,tar15,tar16,tar17,tar18,tar19,tar20,tar21)
  res[res%in%c("NA"," NA","NA "," NA ")]=NA
  nm=c()
  for(i in 6:20){
    for(j in 1:12){
      nm=c(nm,paste0("tar",i,".",j))
    }
  }
  names(res)=c(paste0("tar",1:5),nm,"tar21")
  return(res)
}

concordancia=function(x){
  x[x=="",]=NA
  x=as.data.frame(x)[,apply(is.na(x),2,sum)%>%{which(.!=2)}];x=x[,-c(1:4)]
  res=apply(x,2,function(x)ifelse(any(is.na(x)),F,x[1]==x[2]))%>%sum/dim(x)[2]
  return(res)
}

tabs=reactiveValues(dados=dados)

server = shinyServer(function(input, output,session) {
  session$onFlush(function() {
  })
  
  observeEvent(input$add,{
    if(!is.null(input$upload)){
      file=scan(input$upload$datapath,what=character(),sep=";")
      Encoding(file)="latin1"
      name=input$upload$name
      analista=ifelse(grepl("V",name),"Vinicius","Henrique")
      partc=substring(name,6,6)
      cond=ifelse(substring(name,8,8)==1,"PRE","POS")
      tarefa=ifelse(substring(name,10,10)==1,"imitar",ifelse(substring(name,10,10)==2,"apontar","combinar"))
      dt=as.list(c(partc,cond,tarefa,analista,construct(file)));names(dt)[1:4]=c("Participante","Condicao","Tarefa","Analista")
      if(dim(tabs$dados[Participante==partc&Condicao==cond&Tarefa==tarefa&Analista==analista])[1]<1){
        tabs$dados=rbind(tabs$dados,dt)
        fwrite(tabs$dados,pathdados)
      }
    }
  })
  
  calc=eventReactive(input$statCalc,{
    if(input$stat=="Concordância"){
      part=input$statPart
      cond=input$statCond
      tarefa=input$statTarefa
      conc=concordancia(tabs$dados[Participante==part&Condicao==cond&Tarefa==tarefa])
      print(paste0("Concordância de ",round(conc,4)*100,"%"))
    }
  })
  
  tabr=eventReactive(tabs$dados,{
    return(tabs$dados)
  })
  output$printTable <- renderDataTable(tabr(),options=list(pageLength=10,scrollX=T))
  output$statShow = renderPrint(calc())
})

ui = dashboardPage(skin="blue",
                   dashboardHeader(title="DTT"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Visualizar", tabName = "visualizar", icon = icon("commenting")),
                       menuItem("Adicionar", tabName = "adicionar", icon = icon("commenting")),
                       menuItem("Estatística", tabName = "stats", icon = icon("commenting"))
                     )
                   ),
                   dashboardBody(
                     tabItems(
                       # First tab content
                       tabItem(tabName="visualizar",
                               fluidRow(
                                 dataTableOutput("printTable")
                               )),
                       tabItem(tabName="adicionar",
                               fluidRow(tabsetPanel(
                                 tabBox(title="Adicionar database",
                                        box(
                                          fileInput("upload","Escolha o arquivo",multiple=F,accept=c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                          actionButton("add","Adicionar ao banco de dados")
                                        )))
                               )),
                       tabItem(tabName="stats",
                               fluidRow(tabsetPanel(
                                 tabBox(title="Adicionar database",
                                        box(
                                          selectInput("stat","Estatística",choices=c("Concordância")),
                                          selectInput("statPart","Participante",choices = c(2,3,4,6)),
                                          selectInput("statCond","Condição",choices = c("PRE","POS")),
                                          selectInput("statTarefa","Tarefa",choices = c("imitar","apontar","combinar")),
                                          actionButton("statCalc","Calcular"),
                                          verbatimTextOutput("statShow")
                                                           
                                        )))
                               ))
                     )
                   )
)


shinyApp(ui,server)