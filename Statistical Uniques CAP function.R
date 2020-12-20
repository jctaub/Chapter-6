# CAP function with statistical uniques with non-matches coded as 0

rm.6.m10<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                    syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]], 
                               dataset[[key5]], dataset[[key6]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[key5]],dataset[[key6]], dataset[[target]]))
  #return(table2)
  #break()
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7')
  table3<- table3[, vars]
  
  #return(table3)
  #break
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[key5]], syndata1[[key6]],
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]], syndata1[[key6]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]], syndata2[[key6]],
                                  syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]], syndata2[[key6]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[key6]],
                                  syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[key6]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[key6]],
                                  syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[key6]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[key6]],
                                  syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[key6]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]], syndata6[[key6]],
                                  syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]], syndata6[[key6]] ))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[key6]],
                                  syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[key6]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[key6]],
                                  syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[key6]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[key6]],
                                  syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[key6]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[key6]],
                                   syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[key6]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined10$com2x<-combined10$freq1x + combined10$freq2x
  combined10$com2y<-combined10$freq1y + combined10$freq2y
  combined10$com3x<-combined10$com2x + combined10$freq3x
  combined10$com3y<-combined10$com2y + combined10$freq3y
  combined10$com4x<-combined10$com3x + combined10$freq4x
  combined10$com4y<-combined10$com3y + combined10$freq4y
  combined10$com5x<-combined10$com4x + combined10$freq5x
  combined10$com5y<-combined10$com4y + combined10$freq5y
  combined10$com6x<-combined10$com5x + combined10$freq6x
  combined10$com6y<-combined10$com5y + combined10$freq6y
  combined10$com7x<-combined10$com6x + combined10$freq7x
  combined10$com7y<-combined10$com6y + combined10$freq7y
  combined10$com8x<-combined10$com7x + combined10$freq8x
  combined10$com8y<-combined10$com7y + combined10$freq8y
  combined10$com9x<-combined10$com8x + combined10$freq9x
  combined10$com9y<-combined10$com8y + combined10$freq9y
  combined10$com10x<-combined10$com9x + combined10$freq10x
  combined10$com10y<-combined10$com9y + combined10$freq10y
  
  #combined10$origpaa<-combined10$origx/combined10$origy
  combined10$paa1<- combined10$freq1x/combined10$freq1y
  combined10$paa2<- combined10$com2x/combined10$com2y
  combined10$paa3<- combined10$com3x/combined10$com3y
  combined10$paa4<- combined10$com4x/combined10$com4y
  combined10$paa5<- combined10$com5x/combined10$com5y
  combined10$paa6<- combined10$com6x/combined10$com6y
  combined10$paa7<- combined10$com7x/combined10$com7y
  combined10$paa8<- combined10$com8x/combined10$com8y
  combined10$paa9<- combined10$com9x/combined10$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  combined10$paa1[is.na(combined10$paa1)]<-0
  combined10$paa2[is.na(combined10$paa2)]<-0
  combined10$paa3[is.na(combined10$paa3)]<-0
  combined10$paa4[is.na(combined10$paa4)]<-0
  combined10$paa5[is.na(combined10$paa5)]<-0
  combined10$paa6[is.na(combined10$paa6)]<-0
  combined10$paa7[is.na(combined10$paa7)]<-0
  combined10$paa8[is.na(combined10$paa8)]<-0
  combined10$paa9[is.na(combined10$paa9)]<-0
  combined10$paa10[is.na(combined10$paa10)]<-0
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(6)
  paa= c(mean(combined10$paa1), mean(combined10$paa2), mean(combined10$paa3), mean(combined10$paa4),
         mean(combined10$paa5), mean(combined10$paa6), mean(combined10$paa7), mean(combined10$paa8),
         mean(combined10$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df)
}

rm.6.m10(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
         cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
         'internet', 'hhsize', 'econ_ref')
#######################################################

rm.5.m10<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                    syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]], 
                               dataset[[key5]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[key5]], dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[key5]],syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]],syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]],syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined10$com2x<-combined10$freq1x + combined10$freq2x
  combined10$com2y<-combined10$freq1y + combined10$freq2y
  combined10$com3x<-combined10$com2x + combined10$freq3x
  combined10$com3y<-combined10$com2y + combined10$freq3y
  combined10$com4x<-combined10$com3x + combined10$freq4x
  combined10$com4y<-combined10$com3y + combined10$freq4y
  combined10$com5x<-combined10$com4x + combined10$freq5x
  combined10$com5y<-combined10$com4y + combined10$freq5y
  combined10$com6x<-combined10$com5x + combined10$freq6x
  combined10$com6y<-combined10$com5y + combined10$freq6y
  combined10$com7x<-combined10$com6x + combined10$freq7x
  combined10$com7y<-combined10$com6y + combined10$freq7y
  combined10$com8x<-combined10$com7x + combined10$freq8x
  combined10$com8y<-combined10$com7y + combined10$freq8y
  combined10$com9x<-combined10$com8x + combined10$freq9x
  combined10$com9y<-combined10$com8y + combined10$freq9y
  combined10$com10x<-combined10$com9x + combined10$freq10x
  combined10$com10y<-combined10$com9y + combined10$freq10y
  
  #combined10$origpaa<-combined10$origx/combined10$origy
  combined10$paa1<- combined10$freq1x/combined10$freq1y
  combined10$paa2<- combined10$com2x/combined10$com2y
  combined10$paa3<- combined10$com3x/combined10$com3y
  combined10$paa4<- combined10$com4x/combined10$com4y
  combined10$paa5<- combined10$com5x/combined10$com5y
  combined10$paa6<- combined10$com6x/combined10$com6y
  combined10$paa7<- combined10$com7x/combined10$com7y
  combined10$paa8<- combined10$com8x/combined10$com8y
  combined10$paa9<- combined10$com9x/combined10$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  combined10$paa1[is.na(combined10$paa1)]<-0
  combined10$paa2[is.na(combined10$paa2)]<-0
  combined10$paa3[is.na(combined10$paa3)]<-0
  combined10$paa4[is.na(combined10$paa4)]<-0
  combined10$paa5[is.na(combined10$paa5)]<-0
  combined10$paa6[is.na(combined10$paa6)]<-0
  combined10$paa7[is.na(combined10$paa7)]<-0
  combined10$paa8[is.na(combined10$paa8)]<-0
  combined10$paa9[is.na(combined10$paa9)]<-0
  combined10$paa10[is.na(combined10$paa10)]<-0
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(5)
  paa= c(mean(combined10$paa1), mean(combined10$paa2), mean(combined10$paa3), mean(combined10$paa4),
         mean(combined10$paa5), mean(combined10$paa6), mean(combined10$paa7), mean(combined10$paa8),
         mean(combined10$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df)
}

###########################################################################

rm.4.m10<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                    syndata8, syndata9, syndata10,key1, key2, key3, key4, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]],syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]],syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined10$com2x<-combined10$freq1x + combined10$freq2x
  combined10$com2y<-combined10$freq1y + combined10$freq2y
  combined10$com3x<-combined10$com2x + combined10$freq3x
  combined10$com3y<-combined10$com2y + combined10$freq3y
  combined10$com4x<-combined10$com3x + combined10$freq4x
  combined10$com4y<-combined10$com3y + combined10$freq4y
  combined10$com5x<-combined10$com4x + combined10$freq5x
  combined10$com5y<-combined10$com4y + combined10$freq5y
  combined10$com6x<-combined10$com5x + combined10$freq6x
  combined10$com6y<-combined10$com5y + combined10$freq6y
  combined10$com7x<-combined10$com6x + combined10$freq7x
  combined10$com7y<-combined10$com6y + combined10$freq7y
  combined10$com8x<-combined10$com7x + combined10$freq8x
  combined10$com8y<-combined10$com7y + combined10$freq8y
  combined10$com9x<-combined10$com8x + combined10$freq9x
  combined10$com9y<-combined10$com8y + combined10$freq9y
  combined10$com10x<-combined10$com9x + combined10$freq10x
  combined10$com10y<-combined10$com9y + combined10$freq10y
  
  #combined10$origpaa<-combined10$origx/combined10$origy
  combined10$paa1<- combined10$freq1x/combined10$freq1y
  combined10$paa2<- combined10$com2x/combined10$com2y
  combined10$paa3<- combined10$com3x/combined10$com3y
  combined10$paa4<- combined10$com4x/combined10$com4y
  combined10$paa5<- combined10$com5x/combined10$com5y
  combined10$paa6<- combined10$com6x/combined10$com6y
  combined10$paa7<- combined10$com7x/combined10$com7y
  combined10$paa8<- combined10$com8x/combined10$com8y
  combined10$paa9<- combined10$com9x/combined10$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  combined10$paa1[is.na(combined10$paa1)]<-0
  combined10$paa2[is.na(combined10$paa2)]<-0
  combined10$paa3[is.na(combined10$paa3)]<-0
  combined10$paa4[is.na(combined10$paa4)]<-0
  combined10$paa5[is.na(combined10$paa5)]<-0
  combined10$paa6[is.na(combined10$paa6)]<-0
  combined10$paa7[is.na(combined10$paa7)]<-0
  combined10$paa8[is.na(combined10$paa8)]<-0
  combined10$paa9[is.na(combined10$paa9)]<-0
  combined10$paa10[is.na(combined10$paa10)]<-0
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(4)
  paa= c(mean(combined10$paa1), mean(combined10$paa2), mean(combined10$paa3), mean(combined10$paa4),
         mean(combined10$paa5), mean(combined10$paa6), mean(combined10$paa7), mean(combined10$paa8),
         mean(combined10$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df)
}

########################################################


rm.3.m10<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                    syndata8, syndata9, syndata10,key1, key2, key3, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined10$com2x<-combined10$freq1x + combined10$freq2x
  combined10$com2y<-combined10$freq1y + combined10$freq2y
  combined10$com3x<-combined10$com2x + combined10$freq3x
  combined10$com3y<-combined10$com2y + combined10$freq3y
  combined10$com4x<-combined10$com3x + combined10$freq4x
  combined10$com4y<-combined10$com3y + combined10$freq4y
  combined10$com5x<-combined10$com4x + combined10$freq5x
  combined10$com5y<-combined10$com4y + combined10$freq5y
  combined10$com6x<-combined10$com5x + combined10$freq6x
  combined10$com6y<-combined10$com5y + combined10$freq6y
  combined10$com7x<-combined10$com6x + combined10$freq7x
  combined10$com7y<-combined10$com6y + combined10$freq7y
  combined10$com8x<-combined10$com7x + combined10$freq8x
  combined10$com8y<-combined10$com7y + combined10$freq8y
  combined10$com9x<-combined10$com8x + combined10$freq9x
  combined10$com9y<-combined10$com8y + combined10$freq9y
  combined10$com10x<-combined10$com9x + combined10$freq10x
  combined10$com10y<-combined10$com9y + combined10$freq10y
  
  #combined10$origpaa<-combined10$origx/combined10$origy
  combined10$paa1<- combined10$freq1x/combined10$freq1y
  combined10$paa2<- combined10$com2x/combined10$com2y
  combined10$paa3<- combined10$com3x/combined10$com3y
  combined10$paa4<- combined10$com4x/combined10$com4y
  combined10$paa5<- combined10$com5x/combined10$com5y
  combined10$paa6<- combined10$com6x/combined10$com6y
  combined10$paa7<- combined10$com7x/combined10$com7y
  combined10$paa8<- combined10$com8x/combined10$com8y
  combined10$paa9<- combined10$com9x/combined10$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  combined10$paa1[is.na(combined10$paa1)]<-0
  combined10$paa2[is.na(combined10$paa2)]<-0
  combined10$paa3[is.na(combined10$paa3)]<-0
  combined10$paa4[is.na(combined10$paa4)]<-0
  combined10$paa5[is.na(combined10$paa5)]<-0
  combined10$paa6[is.na(combined10$paa6)]<-0
  combined10$paa7[is.na(combined10$paa7)]<-0
  combined10$paa8[is.na(combined10$paa8)]<-0
  combined10$paa9[is.na(combined10$paa9)]<-0
  combined10$paa10[is.na(combined10$paa10)]<-0
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(3)
  paa= c(mean(combined10$paa1), mean(combined10$paa2), mean(combined10$paa3), mean(combined10$paa4),
         mean(combined10$paa5), mean(combined10$paa6), mean(combined10$paa7), mean(combined10$paa8),
         mean(combined10$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df)
}

#######################################################


col.rm.6.m10<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                        syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target){
  
  set6<-rm.6.m10(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                 syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target)
  set5<- rm.5.m10(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                  syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, target)
  set4<- rm.4.m10(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                  syndata8, syndata9, syndata10,key1, key2, key3, key4, target)
  set3<- rm.3.m10(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                  syndata8, syndata9, syndata10,key1, key2, key3, target) 
  df<-rbind(set6, set5, set4, set3
  print(df)
}

col.rm.6.m10(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
             cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
             'internet', 'hhsize', 'econ_ref')


#############################################################################
# CAP function with statistical uniques with non-matches excluded

rm.6.m10.wo0<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                        syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]], 
                               dataset[[key5]], dataset[[key6]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[key5]],dataset[[key6]], dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[key5]], syndata1[[key6]],
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]], syndata1[[key6]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]], syndata2[[key6]],
                                  syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]], syndata2[[key6]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[key6]],
                                  syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[key6]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[key6]],
                                  syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[key6]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[key6]],
                                  syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[key6]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]], syndata6[[key6]],
                                  syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]], syndata6[[key6]] ))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[key6]],
                                  syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[key6]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[key6]],
                                  syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[key6]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[key6]],
                                  syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[key6]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[key6]],
                                   syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[key6]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined2$com2x<-combined2$freq1x + combined2$freq2x
  combined2$com2y<-combined2$freq1y + combined2$freq2y
  combined3$com3x<- combined3$freq1x + combined3$freq2x + combined3$freq3x
  combined3$com3y<- combined3$freq1y + combined3$freq2y + combined3$freq3y
  combined4$com4x<- combined4$freq1x + combined4$freq2x + combined4$freq3x + combined4$freq4x
  combined4$com4y<- combined4$freq1y + combined4$freq2y + combined4$freq3y + combined4$freq4y
  combined5$com5x<- combined5$freq1x + combined5$freq2x + combined5$freq3x + combined5$freq4x + combined5$freq5x
  combined5$com5y<- combined5$freq1y + combined5$freq2y + combined5$freq3y + combined5$freq4y + combined5$freq5y
  combined6$com6x<- combined6$freq1x + combined6$freq2x + combined6$freq3x + combined6$freq4x + combined6$freq5x + combined6$freq6x
  combined6$com6y<- combined6$freq1y + combined6$freq2y + combined6$freq3y + combined6$freq4y + combined6$freq5y + combined6$freq6y
  combined7$com7x<- combined7$freq1x + combined7$freq2x + combined7$freq3x + combined7$freq4x + combined7$freq5x + combined7$freq6x + combined7$freq7x
  combined7$com7y<- combined7$freq1y + combined7$freq2y + combined7$freq3y + combined7$freq4y + combined7$freq5y + combined7$freq6y + combined7$freq7y
  combined8$com8x<- combined8$freq1x + combined8$freq2x + combined8$freq3x + combined8$freq4x + combined8$freq5x + combined8$freq6x + combined8$freq7x + combined8$freq8x
  combined8$com8y<- combined8$freq1y + combined8$freq2y + combined8$freq3y + combined8$freq4y + combined8$freq5y + combined8$freq6y + combined8$freq7y + combined8$freq8y
  combined9$com9x<- combined9$freq1x + combined9$freq2x + combined9$freq3x + combined9$freq4x + combined9$freq5x + combined9$freq6x + combined9$freq7x + combined9$freq8x + combined9$freq9x
  combined9$com9y<- combined9$freq1y + combined9$freq2y + combined9$freq3y + combined9$freq4y + combined9$freq5y + combined9$freq6y + combined9$freq7y + combined9$freq8y + combined9$freq9y
  combined10$com10x<- combined10$freq1x + combined10$freq2x + combined10$freq3x + combined10$freq4x + combined10$freq5x + combined10$freq6x + combined10$freq7x + combined10$freq8x + combined10$freq9x + combined10$freq10x
  combined10$com10y<- combined10$freq1y + combined10$freq2y + combined10$freq3y + combined10$freq4y + combined10$freq5y + combined10$freq6y + combined10$freq7y + combined10$freq8y + combined10$freq9y + combined10$freq10y
  
  #removing 0s per dataset
  combined<- subset(combined, combined$freq1y > 0)
  combined2<- subset(combined2, combined2$com2y > 0)
  combined3<- subset(combined3, combined3$com3y > 0)
  combined4<- subset(combined4, combined4$com4y > 0)
  combined5<- subset(combined5, combined5$com5y > 0)
  combined6<- subset(combined6, combined6$com6y > 0)
  combined7<- subset(combined7, combined7$com7y > 0)
  combined8<- subset(combined8, combined8$com8y > 0)
  combined9<- subset(combined9, combined9$com9y > 0)
  combined10<- subset(combined10, combined10$com10y > 0)
  
  combined$paa1<- combined$freq1x/combined$freq1y
  combined2$paa2<- combined2$com2x/combined2$com2y
  combined3$paa3<- combined3$com3x/combined3$com3y
  combined4$paa4<- combined4$com4x/combined4$com4y
  combined5$paa5<- combined5$com5x/combined5$com5y
  combined6$paa6<- combined6$com6x/combined6$com6y
  combined7$paa7<- combined7$com7x/combined7$com7y
  combined8$paa8<- combined8$com8x/combined8$com8y
  combined9$paa9<- combined9$com9x/combined9$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(6)
  paa= c(mean(combined$paa1), mean(combined2$paa2), mean(combined3$paa3), mean(combined4$paa4),
         mean(combined5$paa5), mean(combined6$paa6), mean(combined7$paa7), mean(combined8$paa8),
         mean(combined9$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df) 
}



################################################################################################

rm.5.m10.wo0<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                        syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]], 
                               dataset[[key5]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[key5]], dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[key5]],syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]],syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[key5]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]], syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]], syndata3[[key5]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]], syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[key5]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]], syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[key5]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]],syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[key5]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]], syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[key5]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]], syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[key5]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]], syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]], syndata9[[key5]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]], syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[key5]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined2$com2x<-combined2$freq1x + combined2$freq2x
  combined2$com2y<-combined2$freq1y + combined2$freq2y
  combined3$com3x<- combined3$freq1x + combined3$freq2x + combined3$freq3x
  combined3$com3y<- combined3$freq1y + combined3$freq2y + combined3$freq3y
  combined4$com4x<- combined4$freq1x + combined4$freq2x + combined4$freq3x + combined4$freq4x
  combined4$com4y<- combined4$freq1y + combined4$freq2y + combined4$freq3y + combined4$freq4y
  combined5$com5x<- combined5$freq1x + combined5$freq2x + combined5$freq3x + combined5$freq4x + combined5$freq5x
  combined5$com5y<- combined5$freq1y + combined5$freq2y + combined5$freq3y + combined5$freq4y + combined5$freq5y
  combined6$com6x<- combined6$freq1x + combined6$freq2x + combined6$freq3x + combined6$freq4x + combined6$freq5x + combined6$freq6x
  combined6$com6y<- combined6$freq1y + combined6$freq2y + combined6$freq3y + combined6$freq4y + combined6$freq5y + combined6$freq6y
  combined7$com7x<- combined7$freq1x + combined7$freq2x + combined7$freq3x + combined7$freq4x + combined7$freq5x + combined7$freq6x + combined7$freq7x
  combined7$com7y<- combined7$freq1y + combined7$freq2y + combined7$freq3y + combined7$freq4y + combined7$freq5y + combined7$freq6y + combined7$freq7y
  combined8$com8x<- combined8$freq1x + combined8$freq2x + combined8$freq3x + combined8$freq4x + combined8$freq5x + combined8$freq6x + combined8$freq7x + combined8$freq8x
  combined8$com8y<- combined8$freq1y + combined8$freq2y + combined8$freq3y + combined8$freq4y + combined8$freq5y + combined8$freq6y + combined8$freq7y + combined8$freq8y
  combined9$com9x<- combined9$freq1x + combined9$freq2x + combined9$freq3x + combined9$freq4x + combined9$freq5x + combined9$freq6x + combined9$freq7x + combined9$freq8x + combined9$freq9x
  combined9$com9y<- combined9$freq1y + combined9$freq2y + combined9$freq3y + combined9$freq4y + combined9$freq5y + combined9$freq6y + combined9$freq7y + combined9$freq8y + combined9$freq9y
  combined10$com10x<- combined10$freq1x + combined10$freq2x + combined10$freq3x + combined10$freq4x + combined10$freq5x + combined10$freq6x + combined10$freq7x + combined10$freq8x + combined10$freq9x + combined10$freq10x
  combined10$com10y<- combined10$freq1y + combined10$freq2y + combined10$freq3y + combined10$freq4y + combined10$freq5y + combined10$freq6y + combined10$freq7y + combined10$freq8y + combined10$freq9y + combined10$freq10y
  
  #removing 0s per dataset
  combined<- subset(combined, combined$freq1y > 0)
  combined2<- subset(combined2, combined2$com2y > 0)
  combined3<- subset(combined3, combined3$com3y > 0)
  combined4<- subset(combined4, combined4$com4y > 0)
  combined5<- subset(combined5, combined5$com5y > 0)
  combined6<- subset(combined6, combined6$com6y > 0)
  combined7<- subset(combined7, combined7$com7y > 0)
  combined8<- subset(combined8, combined8$com8y > 0)
  combined9<- subset(combined9, combined9$com9y > 0)
  combined10<- subset(combined10, combined10$com10y > 0)
  
  combined$paa1<- combined$freq1x/combined$freq1y
  combined2$paa2<- combined2$com2x/combined2$com2y
  combined3$paa3<- combined3$com3x/combined3$com3y
  combined4$paa4<- combined4$com4x/combined4$com4y
  combined5$paa5<- combined5$com5x/combined5$com5y
  combined6$paa6<- combined6$com6x/combined6$com6y
  combined7$paa7<- combined7$com7x/combined7$com7y
  combined8$paa8<- combined8$com8x/combined8$com8y
  combined9$paa9<- combined9$com9x/combined9$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(5)
  paa= c(mean(combined$paa1), mean(combined2$paa2), mean(combined3$paa3), mean(combined4$paa4),
         mean(combined5$paa5), mean(combined6$paa6), mean(combined7$paa7), mean(combined8$paa8),
         mean(combined9$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df) 
}

##########################################################################################



rm.4.m10.wo0<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                        syndata8, syndata9, syndata10,key1, key2, key3, key4, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]], syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[key4]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]],syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[key4]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]], syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[key4]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]], syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[key4]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]], syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[key4]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]], syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[key4]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]], syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[key4]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]],syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[key4]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]], syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[key4]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined2$com2x<-combined2$freq1x + combined2$freq2x
  combined2$com2y<-combined2$freq1y + combined2$freq2y
  combined3$com3x<- combined3$freq1x + combined3$freq2x + combined3$freq3x
  combined3$com3y<- combined3$freq1y + combined3$freq2y + combined3$freq3y
  combined4$com4x<- combined4$freq1x + combined4$freq2x + combined4$freq3x + combined4$freq4x
  combined4$com4y<- combined4$freq1y + combined4$freq2y + combined4$freq3y + combined4$freq4y
  combined5$com5x<- combined5$freq1x + combined5$freq2x + combined5$freq3x + combined5$freq4x + combined5$freq5x
  combined5$com5y<- combined5$freq1y + combined5$freq2y + combined5$freq3y + combined5$freq4y + combined5$freq5y
  combined6$com6x<- combined6$freq1x + combined6$freq2x + combined6$freq3x + combined6$freq4x + combined6$freq5x + combined6$freq6x
  combined6$com6y<- combined6$freq1y + combined6$freq2y + combined6$freq3y + combined6$freq4y + combined6$freq5y + combined6$freq6y
  combined7$com7x<- combined7$freq1x + combined7$freq2x + combined7$freq3x + combined7$freq4x + combined7$freq5x + combined7$freq6x + combined7$freq7x
  combined7$com7y<- combined7$freq1y + combined7$freq2y + combined7$freq3y + combined7$freq4y + combined7$freq5y + combined7$freq6y + combined7$freq7y
  combined8$com8x<- combined8$freq1x + combined8$freq2x + combined8$freq3x + combined8$freq4x + combined8$freq5x + combined8$freq6x + combined8$freq7x + combined8$freq8x
  combined8$com8y<- combined8$freq1y + combined8$freq2y + combined8$freq3y + combined8$freq4y + combined8$freq5y + combined8$freq6y + combined8$freq7y + combined8$freq8y
  combined9$com9x<- combined9$freq1x + combined9$freq2x + combined9$freq3x + combined9$freq4x + combined9$freq5x + combined9$freq6x + combined9$freq7x + combined9$freq8x + combined9$freq9x
  combined9$com9y<- combined9$freq1y + combined9$freq2y + combined9$freq3y + combined9$freq4y + combined9$freq5y + combined9$freq6y + combined9$freq7y + combined9$freq8y + combined9$freq9y
  combined10$com10x<- combined10$freq1x + combined10$freq2x + combined10$freq3x + combined10$freq4x + combined10$freq5x + combined10$freq6x + combined10$freq7x + combined10$freq8x + combined10$freq9x + combined10$freq10x
  combined10$com10y<- combined10$freq1y + combined10$freq2y + combined10$freq3y + combined10$freq4y + combined10$freq5y + combined10$freq6y + combined10$freq7y + combined10$freq8y + combined10$freq9y + combined10$freq10y
  
  #removing 0s per dataset
  combined<- subset(combined, combined$freq1y > 0)
  combined2<- subset(combined2, combined2$com2y > 0)
  combined3<- subset(combined3, combined3$com3y > 0)
  combined4<- subset(combined4, combined4$com4y > 0)
  combined5<- subset(combined5, combined5$com5y > 0)
  combined6<- subset(combined6, combined6$com6y > 0)
  combined7<- subset(combined7, combined7$com7y > 0)
  combined8<- subset(combined8, combined8$com8y > 0)
  combined9<- subset(combined9, combined9$com9y > 0)
  combined10<- subset(combined10, combined10$com10y > 0)
  
  combined$paa1<- combined$freq1x/combined$freq1y
  combined2$paa2<- combined2$com2x/combined2$com2y
  combined3$paa3<- combined3$com3x/combined3$com3y
  combined4$paa4<- combined4$com4x/combined4$com4y
  combined5$paa5<- combined5$com5x/combined5$com5y
  combined6$paa6<- combined6$com6x/combined6$com6y
  combined7$paa7<- combined7$com7x/combined7$com7y
  combined8$paa8<- combined8$com8x/combined8$com8y
  combined9$paa9<- combined9$com9x/combined9$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(4)
  paa= c(mean(combined$paa1), mean(combined2$paa2), mean(combined3$paa3), mean(combined4$paa4),
         mean(combined5$paa5), mean(combined6$paa6), mean(combined7$paa7), mean(combined8$paa8),
         mean(combined9$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df) 
}


############################################################################################

rm.3.m10.wo0<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                        syndata8, syndata9, syndata10,key1, key2, key3, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]]))
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4')
  table3<- table3[, vars]
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  #pop 2
  syn2tab1<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]],
                                  syndata2[[target]]))
  syn2tab2<- as.data.frame(ftable(syndata2[[key1]], syndata2[[key2]], syndata2[[key3]]))
  combined2<-merge(combined, syn2tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined2<- merge(combined2, syn2tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined2$freq2x<- combined2$Freq.x
  combined2$freq2y<- combined2$Freq.y
  combined2$Freq.x<-NULL
  combined2$Freq.y<-NULL
  
  #pop 3
  syn3tab1<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]],
                                  syndata3[[target]]))
  syn3tab2<- as.data.frame(ftable(syndata3[[key1]], syndata3[[key2]], syndata3[[key3]]))
  combined3<-merge(combined2, syn3tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined3<- merge(combined3, syn3tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined3$freq3x<- combined3$Freq.x
  combined3$freq3y<- combined3$Freq.y
  combined3$Freq.x<-NULL
  combined3$Freq.y<-NULL
  
  #pop 4
  syn4tab1<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]],
                                  syndata4[[target]]))
  syn4tab2<- as.data.frame(ftable(syndata4[[key1]], syndata4[[key2]], syndata4[[key3]]))
  combined4<-merge(combined3, syn4tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined4<- merge(combined4, syn4tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined4$freq4x<- combined4$Freq.x
  combined4$freq4y<- combined4$Freq.y
  combined4$Freq.x<-NULL
  combined4$Freq.y<-NULL
  
  #pop 5
  syn5tab1<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]],
                                  syndata5[[target]]))
  syn5tab2<- as.data.frame(ftable(syndata5[[key1]], syndata5[[key2]], syndata5[[key3]]))
  combined5<-merge(combined4, syn5tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined5<- merge(combined5, syn5tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined5$freq5x<- combined5$Freq.x
  combined5$freq5y<- combined5$Freq.y
  combined5$Freq.x<-NULL
  combined5$Freq.y<-NULL
  
  #pop 6
  syn6tab1<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]],
                                  syndata6[[target]]))
  syn6tab2<- as.data.frame(ftable(syndata6[[key1]], syndata6[[key2]], syndata6[[key3]]))
  combined6<-merge(combined5, syn6tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined6<- merge(combined6, syn6tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined6$freq6x<- combined6$Freq.x
  combined6$freq6y<- combined6$Freq.y
  combined6$Freq.x<-NULL
  combined6$Freq.y<-NULL
  
  #pop 7
  syn7tab1<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]],
                                  syndata7[[target]]))
  syn7tab2<- as.data.frame(ftable(syndata7[[key1]], syndata7[[key2]], syndata7[[key3]]))
  combined7<-merge(combined6, syn7tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined7<- merge(combined7, syn7tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined7$freq7x<- combined7$Freq.x
  combined7$freq7y<- combined7$Freq.y
  combined7$Freq.x<-NULL
  combined7$Freq.y<-NULL
  
  #pop 8
  syn8tab1<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]],
                                  syndata8[[target]]))
  syn8tab2<- as.data.frame(ftable(syndata8[[key1]], syndata8[[key2]], syndata8[[key3]]))
  combined8<-merge(combined7, syn8tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined8<- merge(combined8, syn8tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined8$freq8x<- combined8$Freq.x
  combined8$freq8y<- combined8$Freq.y
  combined8$Freq.x<-NULL
  combined8$Freq.y<-NULL
  
  #pop 9
  syn9tab1<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]],
                                  syndata9[[target]]))
  syn9tab2<- as.data.frame(ftable(syndata9[[key1]], syndata9[[key2]], syndata9[[key3]]))
  combined9<-merge(combined8, syn9tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined9<- merge(combined9, syn9tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined9$freq9x<- combined9$Freq.x
  combined9$freq9y<- combined9$Freq.y
  combined9$Freq.x<-NULL
  combined9$Freq.y<-NULL
  
  #pop 10
  syn10tab1<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]],
                                   syndata10[[target]]))
  syn10tab2<- as.data.frame(ftable(syndata10[[key1]], syndata10[[key2]], syndata10[[key3]]))
  combined10<-merge(combined9, syn10tab1, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined10<- merge(combined10, syn10tab2, by= c('Var1', 'Var2', 'Var3'), all.x=TRUE)
  combined10$freq10x<- combined10$Freq.x
  combined10$freq10y<- combined10$Freq.y
  combined10$Freq.x<-NULL
  combined10$Freq.y<-NULL
  
  #Combining and PAA scores
  combined2$com2x<-combined2$freq1x + combined2$freq2x
  combined2$com2y<-combined2$freq1y + combined2$freq2y
  combined3$com3x<- combined3$freq1x + combined3$freq2x + combined3$freq3x
  combined3$com3y<- combined3$freq1y + combined3$freq2y + combined3$freq3y
  combined4$com4x<- combined4$freq1x + combined4$freq2x + combined4$freq3x + combined4$freq4x
  combined4$com4y<- combined4$freq1y + combined4$freq2y + combined4$freq3y + combined4$freq4y
  combined5$com5x<- combined5$freq1x + combined5$freq2x + combined5$freq3x + combined5$freq4x + combined5$freq5x
  combined5$com5y<- combined5$freq1y + combined5$freq2y + combined5$freq3y + combined5$freq4y + combined5$freq5y
  combined6$com6x<- combined6$freq1x + combined6$freq2x + combined6$freq3x + combined6$freq4x + combined6$freq5x + combined6$freq6x
  combined6$com6y<- combined6$freq1y + combined6$freq2y + combined6$freq3y + combined6$freq4y + combined6$freq5y + combined6$freq6y
  combined7$com7x<- combined7$freq1x + combined7$freq2x + combined7$freq3x + combined7$freq4x + combined7$freq5x + combined7$freq6x + combined7$freq7x
  combined7$com7y<- combined7$freq1y + combined7$freq2y + combined7$freq3y + combined7$freq4y + combined7$freq5y + combined7$freq6y + combined7$freq7y
  combined8$com8x<- combined8$freq1x + combined8$freq2x + combined8$freq3x + combined8$freq4x + combined8$freq5x + combined8$freq6x + combined8$freq7x + combined8$freq8x
  combined8$com8y<- combined8$freq1y + combined8$freq2y + combined8$freq3y + combined8$freq4y + combined8$freq5y + combined8$freq6y + combined8$freq7y + combined8$freq8y
  combined9$com9x<- combined9$freq1x + combined9$freq2x + combined9$freq3x + combined9$freq4x + combined9$freq5x + combined9$freq6x + combined9$freq7x + combined9$freq8x + combined9$freq9x
  combined9$com9y<- combined9$freq1y + combined9$freq2y + combined9$freq3y + combined9$freq4y + combined9$freq5y + combined9$freq6y + combined9$freq7y + combined9$freq8y + combined9$freq9y
  combined10$com10x<- combined10$freq1x + combined10$freq2x + combined10$freq3x + combined10$freq4x + combined10$freq5x + combined10$freq6x + combined10$freq7x + combined10$freq8x + combined10$freq9x + combined10$freq10x
  combined10$com10y<- combined10$freq1y + combined10$freq2y + combined10$freq3y + combined10$freq4y + combined10$freq5y + combined10$freq6y + combined10$freq7y + combined10$freq8y + combined10$freq9y + combined10$freq10y
  
  #removing 0s per dataset
  combined<- subset(combined, combined$freq1y > 0)
  combined2<- subset(combined2, combined2$com2y > 0)
  combined3<- subset(combined3, combined3$com3y > 0)
  combined4<- subset(combined4, combined4$com4y > 0)
  combined5<- subset(combined5, combined5$com5y > 0)
  combined6<- subset(combined6, combined6$com6y > 0)
  combined7<- subset(combined7, combined7$com7y > 0)
  combined8<- subset(combined8, combined8$com8y > 0)
  combined9<- subset(combined9, combined9$com9y > 0)
  combined10<- subset(combined10, combined10$com10y > 0)
  
  combined$paa1<- combined$freq1x/combined$freq1y
  combined2$paa2<- combined2$com2x/combined2$com2y
  combined3$paa3<- combined3$com3x/combined3$com3y
  combined4$paa4<- combined4$com4x/combined4$com4y
  combined5$paa5<- combined5$com5x/combined5$com5y
  combined6$paa6<- combined6$com6x/combined6$com6y
  combined7$paa7<- combined7$com7x/combined7$com7y
  combined8$paa8<- combined8$com8x/combined8$com8y
  combined9$paa9<- combined9$com9x/combined9$com9y
  combined10$paa10<- combined10$com10x/combined10$com10y
  
  m= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  key=c(3)
  paa= c(mean(combined$paa1), mean(combined2$paa2), mean(combined3$paa3), mean(combined4$paa4),
         mean(combined5$paa5), mean(combined6$paa6), mean(combined7$paa7), mean(combined8$paa8),
         mean(combined9$paa9), mean(combined10$paa10))
  df= data.frame(m, key, paa)
  return(df) 
}

#####################################################################################################

col.rm.6.m10.wo0<- function(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                            syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target){
  
  set6<-rm.6.m10.wo0(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                     syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, key6, target)
  set5<- rm.5.m10.wo0(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                      syndata8, syndata9, syndata10,key1, key2, key3, key4, key5, target)
  set4<- rm.4.m10.wo0(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                      syndata8, syndata9, syndata10,key1, key2, key3, key4, target)
  set3<- rm.3.m10.wo0(dataset, syndata1, syndata2, syndata3, syndata4, syndata5, syndata6, syndata7,
                      syndata8, syndata9, syndata10,key1, key2, key3, target) 
  df<-rbind(set6, set5, set4, set3)
  print(df)
}

col.rm.6.m10.wo0(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                 cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                 'internet', 'hhsize', 'econ_ref')

















































