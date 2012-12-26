oneFullExp<-function(expName,charName,spikeNumber){return(sprintf("SELECT `experimentName`, `tagName`, `time`, `%s` AS `value` FROM `oneFullExpTmp` WHERE `experimentName` = %s AND `number` = %i",charName,expName,spikeNumber))}
expNames<-function(mainTag){return(sprintf("SELECT `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`tagTable` AS `tagTable` WHERE `experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `tagTable`.`tagName` = %s",mainTag))}
expStep<-function(stepName){return(sprintf("SELECT `experiment`.`experimentName` FROM `filterdb`.`record` AS `record`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`recordToTags` AS `recordToTags`, `filterdb`.`recordTags` AS `recordTags` WHERE `record`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `recordToTags`.`record_idrecord` = `record`.`idrecord` AND `recordToTags`.`recordTags_idrecordTags` = `recordTags`.`idrecordTags` AND `recordTags`.`tagName` = %s",stepName))}

query2=sprintf("SELECT `experiment`.`date`, `experiment`.`experimentName`, `recordTags`.`tagName` AS `recordTag`,\
`record`.`time`, `responses`.`numberofspikes`, `responses`.`length` AS `responseLength`, `responses`.`vpsp`,\
`responses`.`epspFront`,`responses`.`epspArea`,`responses`.`epileptStd`, `spikes`.`ampl`, `spikes`.`length` AS `spikeLength`, \
`spikes`.`maxDiff`, `spikes`.`area`, `spikes`.`angle1`, `spikes`.`angle2`, `spikes`.`delay`, `spikes`.`maxDiff`, `spikes`.\
`maxtomin` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`experiment` AS `experiment`,\
`filterdb`.`record` AS `record`, `filterdb`.`recordToTags` AS `recordToTags`, `filterdb`.`recordTags` AS `recordTags`,\
`filterdb`.`responses` AS `responses`, `filterdb`.`signalProperties` AS `signalProperties`, `filterdb`.`spikes` \
AS `spikes`, `filterdb`.`tagTable` AS `tagTable` WHERE `experimentTags`.`experiment_idexperiment` =\
`experiment`.`idexperiment` AND `record`.`experiment_idexperiment` = `experiment`.`idexperiment` AND\
`recordToTags`.`record_idrecord` = `record`.`idrecord` AND `recordToTags`.`recordTags_idrecordTags` = \
`recordTags`.`idrecordTags` AND `responses`.`record_idrecord` = `record`.`idrecord` AND `signalProperties`\
.`record_idrecord` = `record`.`idrecord` AND `spikes`.`responses_idresponses` = `responses`.`idresponses` AND\
`experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `tagTable`.`tagName` = %s AND `spikes`.\
`number` = %i AND `responses`.`number` = 1 ORDER BY `experiment`.`date` ASC, `record`.`time` ASC", mainTag,spikeNumber)

tagQuery=sprintf("SELECT `tagTable`.`tagName`, `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS\
`experimentTags`, `filterdb`.`experiment` AS `experiment`, `filterdb`.`tagTable` AS `tagTable` WHERE \
`experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experimentTags`.`tagTable_tagId`\
= `tagTable`.`tagId` AND `tagTable`.`tagName` = %s", substanceTagName)

expQuery=sprintf("SELECT DISTINCT `experiment`.`date`, `experiment`.`experimentName`, `recordTags`.`tagName` FROM\
                 `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`experiment` AS `experiment`,\
                 `filterdb`.`tagTable` AS `tagTable`, `filterdb`.`record` AS `record`, `filterdb`.`recordToTags`\
                 AS `recordToTags`, `filterdb`.`recordTags` AS `recordTags` WHERE `experimentTags`.`experiment_idexperiment`\
                 = `experiment`.`idexperiment` AND `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND\
                 `record`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `recordToTags`.`record_idrecord`\
                 = `record`.`idrecord` AND `recordToTags`.`recordTags_idrecordTags` = `recordTags`.`idrecordTags` AND\
                 `tagTable`.`tagName` = %s", mainTag)
getExpNameQuery<-function(date1,tag1){
  return (sprintf("SELECT DISTINCT `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`tagTable` AS `tagTable`, `filterdb`.`experiment` AS `experiment` WHERE `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `experiment`.`date` = %s AND `tagTable`.`tagName` = %s", date1, tag1))}

tagMaskQuery<-function(tag1){ return (sprintf("SELECT DISTINCT `experiment`.`experimentName` FROM `filterdb`.`experimentTags` AS `experimentTags`, `filterdb`.`tagTable` AS `tagTable`, `filterdb`.`experiment` AS `experiment` WHERE `experimentTags`.`tagTable_tagId` = `tagTable`.`tagId` AND `experimentTags`.`experiment_idexperiment` = `experiment`.`idexperiment` AND `tagTable`.`tagName` = %s", tag1))}