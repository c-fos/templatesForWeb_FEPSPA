library("RMySQL")
library("car")
library("robustbase")
library("pastecs")
library("fBasics")
library("stats")
library("gplots")
library("ggplot2")
library("cluster")
myfont="Helvetica"

dbPull<-function(queryName){
  con <- dbConnect(MySQL(), user="filteruser_local", password="filter123",dbname="filterdb", host="localhost")
  try(resultTable <- dbGetQuery(con, queryName))
  dbDisconnect(con)
  return(resultTable)
}

makeUserMask<-function(expList,maskMatrix){
  goodExp<-expList
  for (i in seq(length(dataBMatrix[1,]))){
    tmpQuery=getExpNameQuery(maskMatrix[2,i],maskMatrix[1,i])
    expName<-dbPull(tmpQuery)
    if (length(expName)>0){
      goodExp<-goodExp[!goodExp%in%expName[,1]]
    }
  }
  return(expList%in%goodExp)
}

myOpts<-opts(
  axis.text.x=theme_text(family = myfont,face = "bold",size=30),
  axis.text.y=theme_text(family = myfont, angle=90,face = "bold",size=30),
  axis.title.x=theme_text(family = myfont, face = "bold",size=35,vjust = 0.3),
  axis.title.y=theme_text(family = myfont, angle=90, face = "bold",size=35,vjust = 0.3),
  #legend.position="",
  legend.text=theme_text(family = myfont, face = "bold",size=25),
  legend.title=theme_text(family = myfont, face = "bold",size=25),
  legend.background = theme_rect(),
  panel.border=theme_rect(colour="black",size=0.5),
  panel.background=theme_rect(fill = "white"),
  #panel.grid.major = theme_line(colour = "black"),
  #panel.grid.minor = theme_line(colour = "black"),
  legend.position=c(.9,0.15)
  
  )

myLegend<-scale_shape(
  solid=TRUE,
  name="Группы",
  #breaks=c("control2","hight_bfa","without_tetan_5mkg"),
  labels=c("Control","Brefeldin A + LTP","Brefeldin A")
  )

myLabels<-labs(
    x="Time, min",
    y="p-spike amplitude, %"
    )
