getPAcceptCurve = function(tGrid, thisTrialData){
  nT = length(tGrid) 
  thisAcceptMatrixOnGrid = matrix(NA, nrow = nUnqHt, ncol = nT)
  nChunk = max(thisTrialData$chunkIdx)
  endOfChunkTimes = sapply(1 : nChunk, function(i) max(thisTrialData$taskTime[thisTrialData$chunkIdx == i]))
  # NA on the end, assuming not observed actions the same as the one on the last chunk
  for(i in 1 : nT){
    t = tGrid[i]
    if(t <= max(endOfChunkTimes)){
      thisChunkIdx = min(which(endOfChunkTimes > t))
    }else{
      thisChunkIdx = nChunk
    }
    for(j in 1 : nUnqHt){
      tempt = thisTrialData %>% filter(thisTrialData$chunkIdx == thisChunkIdx & thisTrialData$scheduledHt == unqHts[j])
      thisAcceptMatrixOnGrid[j,i] = mean(tempt$action)
    }
  }
  return(thisAcceptMatrixOnGrid)
}
