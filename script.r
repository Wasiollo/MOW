rm(list=ls())

readDTrain = function(filepath) {
  con = file(filepath, "r")
  

  line = readLines(con, n = 1)
  if(length(line) != 1) {
    # is file correct
    close(con)
    return(NULL)
  }
  
  splitIndex = strtoi(line)
  
  if(is.na(splitIndex)) {
    close(con)
    return(NULL)
  }
  
  

  sequenceData = NULL
  sequeceLabels = NULL
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line)  == 0 ) {
      break
    } else if (nchar(line) == 1) {
      label = strtoi(line)
      if(is.na(label)){
        close(con)
        return(NULL)
      }
      sequeceLabels = rbind(sequeceLabels, label, deparse.level = 0)
      
    } else {
      seqLength = nchar(line)
      singleNuk = substring(line, seq(1, seqLength, 1), seq(1, seqLength, 1))
      sequenceData = rbind(sequenceData, singleNuk, deparse.level = 0)
    }
    
  }
  close(con)
  
  data = list("splitIndex" = splitIndex, "data" = sequenceData, "labels" = sequeceLabels)
  return (data)
}

constructiveInduction = function(data) {
  uniqueNukleons = unique(c(data))
  
  diNuks = expand.grid(uniqueNukleons, uniqueNukleons)
  triNuks = expand.grid(uniqueNukleons, uniqueNukleons, uniqueNukleons)
  
  newFeatures = NULL
  
  for(i in 1:ncol(data)) {
    for(n in uniqueNukleons) {
      newFeatures = cbind(newFeatures, data[, i] == n)
    }
    
    diResponse = matrix(FALSE, ncol = nrow(diNuks), nrow = nrow(data))
    if(i >= 2)  {
      for (j in 1:nrow(diNuks)) {
        for(k in 1:nrow(data)) {
          if (data[k, i - 1]  == diNuks[j,1] && data[k, i]  == diNuks[j,2]) {
            diResponse[k, j] = TRUE
          }
        }
      }
    }
    
    newFeatures = cbind(newFeatures, diResponse)
    
    triResponse = matrix(FALSE, ncol = nrow(triNuks), nrow = nrow(data))
    if(i >= 3)  {
      for (j in 1:nrow(diNuks)) {
        for(k in 1:nrow(data)) {
          if ((data[k, i - 2]  == triNuks[j,1] && data[k, i - 1]  == triNuks[j,2]) &&  data[k, i] == triNuks[j,3]) {
            triResponse[k, j] = TRUE
          }
        }
      }
    }
    
    newFeatures = cbind(newFeatures, triResponse)
    
  }
  
  return(newFeatures)
}

reduceSet = function(reducedSize, splitData) {
  rSetCount = nrow(splitData$data)
  
  tinyData = splitData$data[1:reducedSize, ]
  tinyData = rbind(tinyData, splitData$data[(rSetCount - reducedSize) : rSetCount ,])
  
  tinyLabels = splitData$labels[1:reducedSize]
  tinyLabels = c(tinyLabels, splitData$labels[(rSetCount - reducedSize) :  rSetCount])
  
  res = list("splitIndex" = splitData$splitIndex, "data" = tinyData, "labels" = tinyLabels)
  
}


dataSpliceD = readDTrain('data/spliceDTrainKIS.dat')
dataSpliceA = readDTrain('data/spliceATrainKIS.dat')


# Reduce set for testing

reducedD = reduceSet(25, dataSpliceD) 


# Constructive induction

# newFeaturesSpliceD = constructiveInduction(dataSpliceD$data)
# newFeaturesSpliceA = constructiveInduction(dataSpliceA$data)












