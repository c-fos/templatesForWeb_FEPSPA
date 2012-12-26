## title: A test
## author:Ilya Malakhin
substanceTagName=c("'control'","'exo1'")#,"'control2'","'without_tetan_5mkg'","'exo1_2'")     #тэг экспериментов с вещ-вом
dataBMatrix=matrix(c(c("'without_tetan_5mkg'","'2012-05-05'"),c("'control'","'2011-03-30'"),c("'control'","'2011-07-11'"),c("'control'","'2011-08-07'"),c("'control'","'2012-02-15'"),c("'control'","'2012-04-09'"),c("'control'","'2011-07-04'"),c("'control'","'2012-02-20'"),c("'control'","'2011-02-02'"),c("'control'","'2012-02-16'"),c("'control'","'2012-03-28'"),c("'bfa_5'","'2012-02-02'"),c("'bfa_5'","'2012-03-21'"),c("'bfa_1'","'2011-07-08'"),c("'bfa_1'","'2011-07-06'"),c("'exo_1'","'2012-07-17'")),nrow=2)
mainTag="'vesTransp'"#"'bfa_new'"       #тэг всей экспериментальной серии
spikeNumber= 1       #какой по счету спайк смотрим: 0-волоконный 1-первый клеточный
filterVarNames="ampl"       #фильтация по характеристикам

library("RMySQL")

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

oneFullExp<-function(expName,charName,spikeNumber){return(sprintf("SELECT `experimentName`, `tagName`, `time`, `%s` AS `value` FROM `oneFullExpTmp` WHERE `experimentName` = %s AND `number` = %i",charName,expName,spikeNumber))}
expNames<-function(mainTag){return(sprintf("SELECT `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`tagTable` AS `tagTable` WHERE `experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `tagTable`.`tagName` = %s",mainTag))}
expStep<-function(stepName){return(sprintf("SELECT `experiment`.`experimentName` FROM `filterdb`.`record` AS `record`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`recordToTags` AS `recordToTags`, `filterdb`.`recordTags` AS `recordTags` WHERE `record`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `recordToTags`.`record_idrecord` = `record`.`idrecord` AND `recordToTags`.`recordTags_idrecordTags` = `recordTags`.`idrecordTags` AND `recordTags`.`tagName` = %s",stepName))}
tagQuery=sprintf("SELECT `tagTable`.`tagName`, `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS\
`experimentTags`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`tagTable` AS `tagTable` WHERE \
`experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experimentTags`.`tagTable_tagId`\
= `tagTable`.`tagId` AND `tagTable`.`tagName` = %s", substanceTagName)
getExpNameQuery<-function(date1,tag1){
   return (sprintf("SELECT DISTINCT `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`tagTable` AS `tagTable`, `filterdb`.`experiment` AS `experiment` WHERE `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experiment`.`date` = %s AND `tagTable`.`tagName` = %s", date1, tag1))}

createMainTable<-function(tag,comparisonTag){
  mainTable<-dbPull(expNames(tag))
  tagMask<-makeTagNamesTble(mainTable$experimentName,comparisonTag)[,2]
  expWithStep<-dbPull(expStep("'инкубация'"))
  incubMask<-mainTable$experimentName%in%expWithStep$experimentName
  expWithStep<-dbPull(expStep("'тетанизация'"))
  tetanMask<-mainTable$experimentName%in%expWithStep$experimentName
  userMask<-makeUserMask(mainTable[,1],dataBMatrix)
  mainTable<-cbind(mainTable,tagMask,incubMask,tetanMask,userMask)
  return(mainTable)
}

makeTagNamesTble<-function(expNames,tagList){  
  tagTable=cbind(as.vector(expNames),rep("empty",length(expNames)))
  for(i in tagList){
    comparsionNames<-dbPull(expNames(i))
    tmp1<-expNames%in%comparsionNames$experimentName
    tagTable[tmp1,2]<-i
  }
  return(tagTable)
}

mainTable<-createMainTable(mainTag,substanceTagName)
toPlot<-data.frame()
for(i in mainTable$experimentName[mainTable$userMask==T]){
  i2=sprintf("'%s'",i)
  fullExp<-dbPull(oneFullExp(i2,filterVarNames,spikeNumber))
  fullExp$time<-strptime(fullExp$time,"%H:%M:%S")-strptime(fullExp$time[1],"%H:%M:%S")
  print(i)
  if (min(fullExp$time)!=Inf){
    print("Полный эксперимент без обработки")
    plot(fullExp$time,fullExp$value)
    points(fullExp$time[fullExp$tagName=="инкубация"],fullExp$value[fullExp$tagName=="инкубация"],col="blue")
    points(fullExp$time[fullExp$tagName=="тетанизация"],fullExp$value[fullExp$tagName=="тетанизация"],col="green")}
  else{
    print("Мало точек")
  }
}