#Differential Confidentiality

#split dataset in 2

split_lcf<- LCF2014

split_lcf$ID<- 1
split_lcf$ID<- (1:nrow(split_lcf))

split_lcf1<- split_lcf[sample(nrow(split_lcf), 2567),]

var_split<- split_lcf1

var_split$x<- 1

var_split1<- c('ID', 'x')

var_split<- var_split[ , var_split1]

split_lcf2<- merge(split_lcf, var_split, by= 'ID', all.x = TRUE)

split_lcf2$x[is.na(split_lcf2$x)]<- 0

split_lcf2<- subset(split_lcf2, split_lcf2$x!= 1)


split_lcf1<- split_lcf1[, ordered_vars]
split_lcf2<- split_lcf2[, ordered_vars]


split_cart_lcf<- syn(split_lcf1, m=10, seed = 05301991)

split_para_lcf<- syn(split_lcf1, method = 'parametric', m=10, seed=05301991)


#in put data set name so either can be subbed in

#jen<- as.data.frame(split_cart_lcf$syn) #change as needed
#jen<-as.data.frame(split_para_lcf$syn)

jen1<- jen[, ordered_vars]

vars2<- c('Gorx.1', 'A049.1', 'OAC1D.1', 'A121.1', 'A093.1', 'A116.1', 'A054.1', 'a160p.1', 'A172.1')
vars3<- c('Gorx.2', 'A049.2', 'OAC1D.2', 'A121.2', 'A093.2', 'A116.2', 'A054.2', 'a160p.2', 'A172.2')
vars4<- c('Gorx.3', 'A049.3', 'OAC1D.3', 'A121.3', 'A093.3', 'A116.3', 'A054.3', 'a160p.3', 'A172.3')
vars5<- c('Gorx.4', 'A049.4', 'OAC1D.4', 'A121.4', 'A093.4', 'A116.4', 'A054.4', 'a160p.4', 'A172.4')
vars6<- c('Gorx.5', 'A049.5', 'OAC1D.5', 'A121.5', 'A093.5', 'A116.5', 'A054.5', 'a160p.5', 'A172.5')
vars7<- c('Gorx.6', 'A049.6', 'OAC1D.6', 'A121.6', 'A093.6', 'A116.6', 'A054.6', 'a160p.6', 'A172.6')
vars8<- c('Gorx.7', 'A049.7', 'OAC1D.7', 'A121.7', 'A093.7', 'A116.7', 'A054.7', 'a160p.7', 'A172.7')
vars9<- c('Gorx.8', 'A049.8', 'OAC1D.8', 'A121.8', 'A093.8', 'A116.8', 'A054.8', 'a160p.8', 'A172.8')
vars10<- c('Gorx.9', 'A049.9', 'OAC1D.9', 'A121.9', 'A093.9', 'A116.9', 'A054.9', 'a160p.9', 'A172.9')

jen2<- jen[, vars2]
jen3<- jen[, vars3]
jen4<- jen[, vars4]
jen5<- jen[, vars5]
jen6<- jen[, vars6]
jen7<- jen[, vars7]
jen8<- jen[, vars8]
jen9<- jen[, vars9]
jen10<- jen[, vars10]

#jen1<-split_lcf1
jen1<- split_lcf2

jen1$gor<- jen1$Gorx
jen1$output_area<- jen1$OAC1D
jen1$tenure<- jen1$A121
jen1$dwelling<- jen1$A116
jen1$internet<- jen1$A172
jen1$hhsize<- jen1$A049
jen1$cars<- jen1$a160p
jen1$workers<- jen1$A054
jen1$econ_ref<- jen1$A093

nvars<- c('gor', 'output_area', 'tenure', 'dwelling', 'internet', 'hhsize', 'cars', 
          'workers', 'econ_ref')

jen1<- jen1[, nvars]


jen2$gor<-         jen2$Gorx.1
jen2$output_area<- jen2$OAC1D.1
jen2$tenure<-      jen2$A121.1
jen2$dwelling<-    jen2$A116.1
jen2$internet<-    jen2$A172.1
jen2$hhsize<-      jen2$A049.1
jen2$cars<-        jen2$a160p.1
jen2$workers<-     jen2$A054.1
jen2$econ_ref<-    jen2$A093.1

jen3$gor<-         jen3$Gorx.2
jen3$output_area<- jen3$OAC1D.2
jen3$tenure<-      jen3$A121.2
jen3$dwelling<-    jen3$A116.2
jen3$internet<-    jen3$A172.2
jen3$hhsize<-      jen3$A049.2
jen3$cars<-        jen3$a160p.2
jen3$workers<-     jen3$A054.2
jen3$econ_ref<-    jen3$A093.2

jen4$gor<-         jen4$Gorx.3
jen4$output_area<- jen4$OAC1D.3
jen4$tenure<-      jen4$A121.3
jen4$dwelling<-    jen4$A116.3
jen4$internet<-    jen4$A172.3
jen4$hhsize<-      jen4$A049.3
jen4$cars<-        jen4$a160p.3
jen4$workers<-     jen4$A054.3
jen4$econ_ref<-    jen4$A093.3

jen5$gor<-         jen5$Gorx.4
jen5$output_area<- jen5$OAC1D.4
jen5$tenure<-      jen5$A121.4
jen5$dwelling<-    jen5$A116.4
jen5$internet<-    jen5$A172.4
jen5$hhsize<-      jen5$A049.4
jen5$cars<-        jen5$a160p.4
jen5$workers<-     jen5$A054.4
jen5$econ_ref<-    jen5$A093.4

jen6$gor<-         jen6$Gorx.5
jen6$output_area<- jen6$OAC1D.5
jen6$tenure<-      jen6$A121.5
jen6$dwelling<-    jen6$A116.5
jen6$internet<-    jen6$A172.5
jen6$hhsize<-      jen6$A049.5
jen6$cars<-        jen6$a160p.5
jen6$workers<-     jen6$A054.5
jen6$econ_ref<-    jen6$A093.5

jen7$gor<-         jen7$Gorx.6
jen7$output_area<- jen7$OAC1D.6
jen7$tenure<-      jen7$A121.6
jen7$dwelling<-    jen7$A116.6
jen7$internet<-    jen7$A172.6
jen7$hhsize<-      jen7$A049.6
jen7$cars<-        jen7$a160p.6
jen7$workers<-     jen7$A054.6
jen7$econ_ref<-    jen7$A093.6

jen8$gor<-         jen8$Gorx.7
jen8$output_area<- jen8$OAC1D.7
jen8$tenure<-      jen8$A121.7
jen8$dwelling<-    jen8$A116.7
jen8$internet<-    jen8$A172.7
jen8$hhsize<-      jen8$A049.7
jen8$cars<-        jen8$a160p.7
jen8$workers<-     jen8$A054.7
jen8$econ_ref<-    jen8$A093.7

jen9$gor<-         jen9$Gorx.8
jen9$output_area<- jen9$OAC1D.8
jen9$tenure<-      jen9$A121.8
jen9$dwelling<-    jen9$A116.8
jen9$internet<-    jen9$A172.8
jen9$hhsize<-      jen9$A049.8
jen9$cars<-        jen9$a160p.8
jen9$workers<-     jen9$A054.8
jen9$econ_ref<-    jen9$A093.8

jen10$gor<-         jen10$Gorx.9
jen10$output_area<- jen10$OAC1D.9
jen10$tenure<-      jen10$A121.9
jen10$dwelling<-    jen10$A116.9
jen10$internet<-    jen10$A172.9
jen10$hhsize<-      jen10$A049.9
jen10$cars<-        jen10$a160p.9
jen10$workers<-     jen10$A054.9
jen10$econ_ref<-    jen10$A093.9

jen2<- jen2[, nvars]
jen3<- jen3[, nvars]
jen4<- jen4[, nvars]
jen5<- jen5[, nvars]
jen6<- jen6[, nvars]
jen7<- jen7[, nvars]
jen8<- jen8[, nvars]
jen9<- jen9[, nvars]
jen10<- jen10[, nvars]


#change into unique data sets

#spit_cart_lcf1<-jen1
#split_cart_lcf2<-jen2
#split_cart_lcf3<-jen3
#split_cart_lcf4<-jen4
#split_cart_lcf5<-jen5
#split_cart_lcf6<-jen6
#split_cart_lcf7<-jen7
#split_cart_lcf8<-jen8
#split_cart_lcf9<-jen9
#split_cart_lcf10<-jen10

#split_para_lcf1<- jen1
#split_para_lcf2<- jen2
#split_para_lcf3<- jen3
#split_para_lcf4<- jen4
#split_para_lcf5<- jen5
#split_para_lcf6<- jen6
#split_para_lcf7<- jen7
#split_para_lcf8<- jen8
#split_para_lcf9<- jen9
#split_para_lcf10<- jen10

#split_lcf1<-jen1
split_lcf2<- jen1





split1_lcf<-as.data.frame(paa.6.m10(split_lcf1, spit_cart_lcf1, split_cart_lcf2, split_cart_lcf3, split_cart_lcf4, split_cart_lcf5, split_cart_lcf6, split_cart_lcf7,
                                    split_cart_lcf8, split_cart_lcf9, split_cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                    'internet', 'hhsize','econ_ref'))
split2_lcf<-as.data.frame(paa.6.m10(split_lcf2, spit_cart_lcf1, split_cart_lcf2, split_cart_lcf3, split_cart_lcf4, split_cart_lcf5, split_cart_lcf6, split_cart_lcf7,
                                    split_cart_lcf8, split_cart_lcf9, split_cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                    'internet', 'hhsize','econ_ref'))

split1_lcf_para<-as.data.frame(paa.6.m10(split_lcf1, split_para_lcf1, split_para_lcf2, split_para_lcf3, split_para_lcf4, split_para_lcf5, split_para_lcf6, split_para_lcf7,
                                         split_para_lcf8, split_para_lcf9, split_para_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                         'internet', 'hhsize','econ_ref'))

split2_lcf_para<-as.data.frame(paa.6.m10(split_lcf2, split_para_lcf1, split_para_lcf2, split_para_lcf3, split_para_lcf4, split_para_lcf5, split_para_lcf6, split_para_lcf7,
                                         split_para_lcf8, split_para_lcf9, split_para_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                         'internet', 'hhsize','econ_ref'))

t.test(split1_lcf$paa1, split2_lcf$paa1)
t.test(split1_lcf$paa2, split2_lcf$paa2)
t.test(split1_lcf$paa3, split2_lcf$paa3)
t.test(split1_lcf$paa4, split2_lcf$paa4)
t.test(split1_lcf$paa5, split2_lcf$paa5)
t.test(split1_lcf$paa6, split2_lcf$paa6)
t.test(split1_lcf$paa7, split2_lcf$paa7)
t.test(split1_lcf$paa8, split2_lcf$paa8)
t.test(split1_lcf$paa9, split2_lcf$paa9)
t.test(split1_lcf$paa10, split2_lcf$paa10)

t.test(split1_lcf_para$paa1, split2_lcf_para$paa1)
t.test(split1_lcf_para$paa2, split2_lcf_para$paa2)
t.test(split1_lcf_para$paa3, split2_lcf_para$paa3)
t.test(split1_lcf_para$paa4, split2_lcf_para$paa4)
t.test(split1_lcf_para$paa5, split2_lcf_para$paa5)
t.test(split1_lcf_para$paa6, split2_lcf_para$paa6)
t.test(split1_lcf_para$paa7, split2_lcf_para$paa7)
t.test(split1_lcf_para$paa8, split2_lcf_para$paa8)
t.test(split1_lcf_para$paa9, split2_lcf_para$paa9)
t.test(split1_lcf_para$paa10, split2_lcf_para$paa10)




