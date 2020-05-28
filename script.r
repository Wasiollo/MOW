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
      sequeceLabels = rbind(sequeceLabels, label)
      
    } else {
      seqLength = nchar(line)
      singleNuk = substring(line, seq(1, seqLength, 1), seq(1, seqLength, 1))
      sequenceData = rbind(sequenceData, singleNuk)
    }
    
  }
  close(con)
  
  data = list("splitIndex" = splitIndex, "data" = sequenceData, "labels" = sequeceLabels)
  return (data)
}


dataSpliceD = readDTrain('data/spliceDTrainKIS.dat')
dataSpliceA = readDTrain('data/spliceATrainKIS.dat')

