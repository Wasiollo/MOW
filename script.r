# install.packages('caret')

library("caret")

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
  diNuksColNames = paste(diNuks[,1], diNuks[,2], sep = "")
  
  triNuks = expand.grid(uniqueNukleons, uniqueNukleons, uniqueNukleons)
  triNuksColNames = paste(triNuks[,1], triNuks[,2], triNuks[,3], sep = "")
  
  newFeatures = NULL
  
  for(i in 1:ncol(data)) {

    for(n in uniqueNukleons) {
      newFeatures = cbind(newFeatures, data[, i] == n)
      colnames(newFeatures)[ncol(newFeatures)] = paste(n, i, sep = "")

    }
    
    diResponse = matrix(FALSE, ncol = nrow(diNuks), nrow = nrow(data))
    colnames(diResponse) = paste(diNuksColNames, i, sep = "")
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
    colnames(triResponse) = paste(triNuksColNames, i, sep = "")
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
  
  ret = newFeatures
  ret[newFeatures == TRUE] = 1
  ret[newFeatures == FALSE] = 0
  
  return(ret)
}

reduceSet = function(reducedSize, splitData) {
  rSetCount = nrow(splitData$data)
  
  tinyData = splitData$data[1:reducedSize, ]
  tinyData = rbind(tinyData, splitData$data[(rSetCount - reducedSize + 1) : rSetCount ,])
  
  tinyLabels = splitData$labels[1:reducedSize]
  tinyLabels = c(tinyLabels, splitData$labels[(rSetCount - reducedSize + 1) :  rSetCount])
  
  res = list("splitIndex" = splitData$splitIndex, "data" = tinyData, "labels" = tinyLabels)
  
}

write.plot.png = function(filename, plotSource) {
  png(filename)
  trellis.par.set(caretTheme())
  print(plot(plotSource,  type = c("g", "o")))
  dev.off()
} 


test.rfe.lm = function(testName, data, labels, cvNumber=10, sizes=c(1:10,seq(10, ncol(data), by=250))) {
  data = as.data.frame(data)
  ctrl = rfeControl(functions = lmFuncs, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = FALSE)
  
  profileRes = rfe(x = data, y = labels, rfeControl = ctrl, sizes = sizes)
  
  outputText = profileRes
  
  sink(paste(testName, "_rfe_lm_cv_", cvNumber,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_rfe_lm_cv_", cvNumber,"_plot.png", sep = ""), profileRes)
}

test.rfe.rf = function(testName, data, labels, cvNumber=10, sizes=c(1:10,seq(10, ncol(data), by=250))) {
  data = as.data.frame(data)
  ctrl = rfeControl(functions = rfFuncs, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = FALSE)
  
  profileRes = rfe(x = data, y = labels, rfeControl = ctrl, sizes = sizes)
  
  outputText = profileRes
  
  sink(paste(testName, "_rfe_rf_cv_", cvNumber,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_rfe_rf_cv_", cvNumber,"_plot.png", sep = ""), profileRes)
}

test.rfe.nb = function(testName, data, labels, cvNumber=10, sizes=c(1:10,seq(10, ncol(data), by=250))) {
  data = as.data.frame(data)
  labels = as.factor(labels)
  
  ctrl = rfeControl(functions = nbFuncs, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = FALSE)
  
  profileRes = rfe(x = data, y = labels, rfeControl = ctrl, sizes = sizes)
  
  outputText = profileRes
  
  sink(paste(testName, "_rfe_nb_cv_", cvNumber,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_rfe_nb_cv_", cvNumber,"_plot.png", sep = ""), profileRes)
  
}

test.rfe.treebag = function(testName, data, labels, cvNumber=10, sizes=c(1:10,seq(10, ncol(data), by=250))) {
  data = as.data.frame(data)
  data = as.factor(data)
  ctrl = rfeControl(functions = treebagFuncs, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = FALSE)
  
  profileRes = rfe(x = data, y = labels, rfeControl = ctrl, sizes = sizes)
  
  outputText = profileRes
  
  sink(paste(testName, "_rfe_treebag_cv_", cvNumber,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_rfe_treebag_cv_", cvNumber,"_plot.png", sep = ""), profileRes)
  
}

#   GAFS

test.gafs.rf = function(testName, data, labels, cvNumber=10, iterations=200) {
  data = as.data.frame(data)
  ctrl = gafsControl(functions = rfGA, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = FALSE, allowParallel = TRUE, genParallel = TRUE)
  
  profileRes = gafs(x = data, y = labels, gafsControl = ctrl, iters = iterations )
  
  outputText = profileRes
  
  sink(paste(testName, "_ga_tf_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_ga_tf_cv_", cvNumber,"_iters_", iterations,"_plot.png", sep = ""), profileRes)

}

test.gafs.treebag = function(testName, data, labels, cvNumber=10, iterations=200) {
  data = as.data.frame(data)
  ctrl = gafsControl(functions = treebagGA, method = "repeatedcv", number = cvNumber, repeats = 5, verbose = TRUE, allowParallel = TRUE, genParallel = TRUE)
  
  profileRes = gafs(x = data, y = labels, gafsControl = ctrl, iters = iterations )
  
  outputText = profileRes
  
  sink(paste(testName, "_ga_treebag_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_ga_treebag_cv_", cvNumber,"_iters_", iterations,"_plot.png", sep = ""), profileRes)
  
}

test.gafs.nb = function(testName, data, labels, cvNumber=10, iterations=200) {
  labels = as.factor(labels)
  data = as.data.frame(data)
  
  nb_ga_ctrl = gafsControl(functions = caretGA, method = "cv", number = cvNumber)
  
  profileRes = gafs(x =  data, y = labels, iters = iterations, gafsControl = nb_ga_ctrl, method = "nb", trControl= trainControl(method = "cv", allowParallel = TRUE))
  
  outputText = profileRes
  
  sink(paste(testName, "_ga_nb_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_ga_nb_cv_", cvNumber,"_iters_", iterations,"_plot.png", sep = ""), profileRes)
}

test.gafs.lm = function(testName, data, labels, cvNumber=10, iterations=200) {
  data = as.data.frame(data)
  
  lm_ga_ctrl = gafsControl(functions = caretGA, method = "cv", number = cvNumber)
  
  profileRes = gafs(x =  data, y = labels, iters = iterations, gafsControl = lm_ga_ctrl, method = "lm", trControl= trainControl(method = "cv", allowParallel = TRUE))
  
  outputText = profileRes
  
  sink(paste(testName, "_ga_lm_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_ga_lm_cv_", cvNumber,"_iters_", iterations,"_plot.png", sep = ""), profileRes)
}

#   SAFS

test.safs.rf = function(testName, data, labels, cvNumber=10, iterations=200, improve=5) {
  data = as.data.frame(data) 
  ctrl = safsControl(functions = rfSA, method = "repeatedcv", number = cvNumber, repeats = 5, improve = improve, verbose = FALSE, allowParallel = TRUE)
  
  profileRes = safs(x = data, y = labels, safsControl = ctrl, iters = iterations )
  
  outputText = profileRes
  
  sink(paste(testName, "_sa_tf_cv_", cvNumber,"_impr_", improve,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_sa_tf_cv_", cvNumber,"_impr_", improve,"_iters_", iterations,"_plot.png", sep = ""), profileRes)
  
}

test.safs.treebag = function(testName, data, labels, cvNumber=10, iterations=200, improve=5) {
  data = as.data.frame(data)
  ctrl = safsControl(functions = treebagSA, method = "repeatedcv", number = cvNumber, repeats = 5, improve = improve, verbose = FALSE, allowParallel = TRUE)
  
  profileRes = safs(x = data, y = labels, safsControl = ctrl, iters = iterations )
  
  outputText = profileRes
  
  sink(paste(testName, "_sa_treebag_cv_", cvNumber,"_impr_", improve, "_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_sa_treebag_cv_", cvNumber,"_impr_", improve, "_iters_", iterations,"_plot.png", sep = ""), profileRes)
  
}

test.safs.nb = function(testName, data, labels, cvNumber=10, iterations=200, improve=5) {
  labels = as.factor(labels)
  data = as.data.frame(data)
  
  nb_sa_ctrl = safsControl(functions = caretSA, method = "cv", repeats = 5, number = cvNumber, improve = improve)
  
  profileRes = safs(x = data, y = labels, iters = iterations, safsControl = nb_sa_ctrl, method = "nb", trControl= trainControl(method = "cv", allowParallel = TRUE))
  
  outputText = profileRes
  
  sink(paste(testName, "_sa_nb_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_sa_nb_cv_", cvNumber,"_impr_", improve, "_iters_", iterations,"_plot.png", sep = ""), profileRes)
}

test.safs.lm = function(testName, data, labels, cvNumber=10, iterations=200, improve=5) {
  data = data.frame(data)
  
  lm_sa_ctrl = safsControl(functions = caretSA, method = "cv", repeats = 5, number = cvNumber, improve = improve)
  
  profileRes = safs(x =  data, y = labels, iters = iterations, safsControl = lm_sa_ctrl, method = "lm", trControl= trainControl(method = "cv", allowParallel = TRUE))
  
  outputText = profileRes
  
  sink(paste(testName, "_sa_lm_cv_", cvNumber,"_iters_", iterations,"_output.txt", sep = ""))
  print(outputText)
  sink()
  
  write.plot.png(paste(testName, "_sa_lm_cv_", cvNumber,"_impr_", improve, "_iters_", iterations,"_plot.png", sep = ""), profileRes)
}

# Running tests

runTestsSplice = function(name, data, labels, cvNumers=c(5), itersVec=c(25, 50, 100, 150), imprVec=c(10, 25, 50)) {
  
  # rfe tests
  
  for(cvn in cvNumers) {
    #test.rfe.lm(testName = name, data = data, labels = labels, cvNumber = cvn)
    
    #test.rfe.nb(testName = name, data = data, labels = labels, cvNumber = cvn)
    
    #test.rfe.rf(testName = name, data = data, labels = labels, cvNumber = cvn)
    
    #test.rfe.treebag(testName = name, data = data, labels = labels, cvNumber = cvn)
  }
  
  # gafs
  
  for(cvn in cvNumers) {
    
    for(itr in itersVec) {
      #test.gafs.lm(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr)
      
      #test.gafs.nb(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr)
      
      #test.gafs.rf(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr)
      
      #test.gafs.treebag(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr)
    }
  }
  
  #safs

  for(cvn in cvNumers) {
    
    for(itr in itersVec) {
      
      for(impr in imprVec) {
        #test.safs.lm(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr, improve = impr)
        
        test.safs.nb(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr, improve = impr)
        
        test.safs.rf(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr, improve = impr)
        
        #test.safs.treebag(testName = name, data = data, labels = labels, cvNumber = cvn, iterations = itr, improve = impr)
        
      }
    }
  }
  
}


#Load test data D

dataSpliceD = readDTrain('data/spliceDTrainKIS.dat')

# Constructive induction D

newFeaturesSpliceD = constructiveInduction(dataSpliceD$data)

# Run tests D

runTestsSplice(name = "spliceD_results", data = newFeaturesSpliceD, labels = dataSpliceD$labels)

#Load test data D

dataSpliceA = readDTrain('data/spliceATrainKIS.dat')

# Constructive induction D

newFeaturesSpliceA = constructiveInduction(dataSpliceA$data)

# Run tests D

runTestsSplice(name = "spliceA_results", data = newFeaturesSpliceA, labels = dataSpliceA$labels)


# Reduce set for testing

reducedD = reduceSet(25, dataSpliceD) 
extRedD = constructiveInduction(reducedD$data)

#reducedD$labels = as.factor(reducedD$labels)

#extRedD = extRedD[,1:10]
#extLabels = reducedD$labels

#runTestsSplice(name = "test_tests_results1", data = extRedD, labels = extLabels)

