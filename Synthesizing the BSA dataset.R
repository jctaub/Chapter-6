#synth for BSA

vars<- c('GOR2', 'MarStat6', 'HEdQual2', 'RAgecat3', 'Rsex', 'RClass', 'HHIncD')

BSA2014<- bsa14_final[, vars]


BSA2014$GOR2<- as.factor(BSA2014$GOR2)
BSA2014$MarStat6<- as.factor(BSA2014$MarStat6)
BSA2014$HEdQual2<- as.factor(BSA2014$HEdQual2)
BSA2014$RAgecat3<- as.factor(BSA2014$RAgecat3)
BSA2014$Rsex<- as.factor(BSA2014$Rsex)
BSA2014$RClass<- as.factor(BSA2014$RClass)
BSA2014$HHIncD<- as.factor(BSA2014$HHIncD)



cart_bsa<- syn(BSA2014, m=10, seed = 05301991, cont.na = list(MarStat6 = c(9),HEdQual2= c(9),
                                                              RAgecat3= c(9), RClass= c(-1, 8), HHIncD= c(-1, 97)))

para_bsa<- syn(BSA2014, m=10, method= 'parametric', seed = 05301991, cont.na = list(MarStat6 = c(9),HEdQual2= c(9),
                                                                                    RAgecat3= c(9), RClass= c(-1, 8), HHIncD= c(-1, 97)))
#this is written so that either script can be plugged in
#jen<- as.data.frame(cart_bsa$syn)
#jen<-as.data.frame(para_bsa$syn)

vars2<- c('GOR2.1', 'MarStat6.1', 'HEdQual2.1', 'RAgecat3.1', 'Rsex.1', 'RClass.1', 'HHIncD.1')
vars3<- c('GOR2.2', 'MarStat6.2', 'HEdQual2.2', 'RAgecat3.2', 'Rsex.2', 'RClass.2', 'HHIncD.2')
vars4<- c('GOR2.3', 'MarStat6.3', 'HEdQual2.3', 'RAgecat3.3', 'Rsex.3', 'RClass.3', 'HHIncD.3')
vars5<- c('GOR2.4', 'MarStat6.4', 'HEdQual2.4', 'RAgecat3.4', 'Rsex.4', 'RClass.4', 'HHIncD.4')
vars6<- c('GOR2.5', 'MarStat6.5', 'HEdQual2.5', 'RAgecat3.5', 'Rsex.5', 'RClass.5', 'HHIncD.5')
vars7<- c('GOR2.6', 'MarStat6.6', 'HEdQual2.6', 'RAgecat3.6', 'Rsex.6', 'RClass.6', 'HHIncD.6')
vars8<- c('GOR2.7', 'MarStat6.7', 'HEdQual2.7', 'RAgecat3.7', 'Rsex.7', 'RClass.7', 'HHIncD.7')
vars9<- c('GOR2.8', 'MarStat6.8', 'HEdQual2.8', 'RAgecat3.8', 'Rsex.8', 'RClass.8', 'HHIncD.8')
vars10<- c('GOR2.9', 'MarStat6.9', 'HEdQual2.9', 'RAgecat3.9', 'Rsex.9', 'RClass.9', 'HHIncD.9')

jen1<- jen[, vars]
jen2<- jen[, vars2]
jen3<- jen[, vars3]
jen4<- jen[, vars4]
jen5<- jen[, vars5]
jen6<- jen[, vars6]
jen7<- jen[, vars7]
jen8<- jen[, vars8]
jen9<- jen[, vars9]
jen10<- jen[, vars10]

jen2$GOR2<-     jen2$GOR2.1
jen2$MarStat6<- jen2$MarStat6.1
jen2$HEdQual2<- jen2$HEdQual2.1
jen2$RAgecat3<- jen2$RAgecat3.1
jen2$Rsex<-     jen2$Rsex.1
jen2$RClass<-   jen2$RClass.1
jen2$HHIncD<-   jen2$HHIncD.1


#run
jen3$GOR2<-     jen3$GOR2.2
jen3$MarStat6<- jen3$MarStat6.2
jen3$HEdQual2<- jen3$HEdQual2.2
jen3$RAgecat3<- jen3$RAgecat3.2
jen3$Rsex<-     jen3$Rsex.2
jen3$RClass<-   jen3$RClass.2
jen3$HHIncD<-   jen3$HHIncD.2

jen4$GOR2<-     jen4$GOR2.3
jen4$MarStat6<- jen4$MarStat6.3
jen4$HEdQual2<- jen4$HEdQual2.3
jen4$RAgecat3<- jen4$RAgecat3.3
jen4$Rsex<-     jen4$Rsex.3
jen4$RClass<-   jen4$RClass.3
jen4$HHIncD<-   jen4$HHIncD.3

jen5$GOR2<-     jen5$GOR2.4
jen5$MarStat6<- jen5$MarStat6.4
jen5$HEdQual2<- jen5$HEdQual2.4
jen5$RAgecat3<- jen5$RAgecat3.4
jen5$Rsex<-     jen5$Rsex.4
jen5$RClass<-   jen5$RClass.4
jen5$HHIncD<-   jen5$HHIncD.4

jen6$GOR2<-     jen6$GOR2.5
jen6$MarStat6<- jen6$MarStat6.5
jen6$HEdQual2<- jen6$HEdQual2.5
jen6$RAgecat3<- jen6$RAgecat3.5
jen6$Rsex<-     jen6$Rsex.5
jen6$RClass<-   jen6$RClass.5
jen6$HHIncD<-   jen6$HHIncD.5

jen7$GOR2<-     jen7$GOR2.6
jen7$MarStat6<- jen7$MarStat6.6
jen7$HEdQual2<- jen7$HEdQual2.6
jen7$RAgecat3<- jen7$RAgecat3.6
jen7$Rsex<-     jen7$Rsex.6
jen7$RClass<-   jen7$RClass.6
jen7$HHIncD<-   jen7$HHIncD.6

jen8$GOR2<-     jen8$GOR2.7
jen8$MarStat6<- jen8$MarStat6.7
jen8$HEdQual2<- jen8$HEdQual2.7
jen8$RAgecat3<- jen8$RAgecat3.7
jen8$Rsex<-     jen8$Rsex.7
jen8$RClass<-   jen8$RClass.7
jen8$HHIncD<-   jen8$HHIncD.7

jen9$GOR2<-     jen9$GOR2.8
jen9$MarStat6<- jen9$MarStat6.8
jen9$HEdQual2<- jen9$HEdQual2.8
jen9$RAgecat3<- jen9$RAgecat3.8
jen9$Rsex<-     jen9$Rsex.8
jen9$RClass<-   jen9$RClass.8
jen9$HHIncD<-   jen9$HHIncD.8

jen10$GOR2<-     jen10$GOR2.9
jen10$MarStat6<- jen10$MarStat6.9
jen10$HEdQual2<- jen10$HEdQual2.9
jen10$RAgecat3<- jen10$RAgecat3.9
jen10$Rsex<-     jen10$Rsex.9
jen10$RClass<-   jen10$RClass.9
jen10$HHIncD<-   jen10$HHIncD.9

jen2<- jen2[, vars]
jen3<- jen3[, vars]
jen4<- jen4[, vars]
jen5<- jen5[, vars]
jen6<- jen6[, vars]
jen7<- jen7[, vars]
jen8<- jen8[, vars]
jen9<- jen9[, vars]
jen10<- jen10[, vars]

# change into seperate datasets

#cart_bsa1<- jen1
#cart_bsa2<- jen2
#cart_bsa3<- jen3
#cart_bsa4<- jen4
#cart_bsa5<- jen5
#cart_bsa6<- jen6
#cart_bsa7<- jen7
#cart_bsa8<- jen8
#cart_bsa9<- jen9
#cart_bsa10<- jen10

#para_bsa1<- jen1
#para_bsa2<- jen2
#para_bsa3<- jen3
#para_bsa4<- jen4
#para_bsa5<- jen5
#para_bsa6<- jen6
#para_bsa7<- jen7
#para_bsa8<- jen8
#para_bsa9<- jen9
#para_bsa10<- jen10
























