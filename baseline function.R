baseline<- function(dataset, target){
  #baseline
  baseline<- as.data.frame(prop.table(table(dataset[[target]])))
  dataset$Var1<-dataset[[target]]
  baseline<- merge(dataset, baseline, by= 'Var1', all.x= TRUE)
  return(mean(baseline$Freq))
}

baseline(LCF2014, 'econ_ref')