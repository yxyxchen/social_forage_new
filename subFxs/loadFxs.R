loadAllData = function(){
  library("gtools")
  # get the filename for each participant 
  fileNames = list.files(path= "data", pattern=('2[0-9][0-9].csv'))
  nSub = length(fileNames)
  if(any(duplicated(fileNames))){
    print("duplicated files!")
    break
  }else{
    sprintf("load data for %d participants", nSub) 
  }
  
  # load data 
  ids = vector(length = nSub)
  trialData = list()
  for(i in 1 : nSub){
    fileName = fileNames[i]
    id = substr(fileName, 1,3)
    ids[i] = id
    thisTrialData = read.csv(sprintf("%s/%s", "data", fileName), header = T)
    thisTrialData = thisTrialData[ , 1:9] # sometimes there is one extra comma at the end of each line
    thisTrialData$id = id
    trialData[[id]] = thisTrialData
  }
  outputs= list(
    ids = ids,
    trialData=trialData)
  return(outputs)
}