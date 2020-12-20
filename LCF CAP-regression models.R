# Regression Models

#CART with non-matches as 0 regression
fs6_cart_econ<-data.frame(paa.6.m10(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                  cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                  'internet', 'hhsize', 'econ_ref'))

fs6_cart_model<- lm(paa~m, data=fs6_cart_econ)
summary(fs6_cart_model)

#Parametric with non-matches as 0 regression
fs6_para_econ<-data.frame(paa.6.m10(LCF2014, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                                  para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                                  'dwelling', 'internet', 'hhsize', 'econ_ref'))
fs6_para_model<- lm(paa~m, data = fs6_para_econ)
summary(fs6_cart_model)

#CART with non-matches excluded regression
fs6w_cart_econ<-data.frame(paa.6.m10.wo0(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                                    cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                    'internet', 'hhsize', 'econ_ref'))
fs6w_cart_model<-lm(paa~m, data = fs6w_cart_econ)
summary(fs6w_cart_model)

#Parametric with non-matches excluded regression
fs6w_para_econ<-data.frame(paa.6.m10.wo0(LCF2014, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                             para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                             'dwelling', 'internet', 'hhsize', 'econ_ref'))
fs6w_para_model<- lm(paa~m, data = fs6w_para_econ)
summary(fs6w_para_model)

#CART with non-matches as 0 regression-Statistical Uniques
rm6_cart_econ<-data.frame(rm.6.m10(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                                        cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                        'internet', 'hhsize', 'econ_ref'))
rm6_cart_model<- lm(paa~m, data = rm6_cart)
summary(rm6_cart_model)

#Parametric with non-matches as 0 regression-Statistical Uniques
rm6_para_econ<-data.frame(rm.6.m10(LCF2014, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                             para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                             'dwelling', 'internet', 'hhsize', 'econ_ref'))
rm6_para_model<-lm(paa~m, data = rm6_para)
summary(rm6_para_model)

#CART with non-matches excluded regression-Statistical Uniques
rm6w_cart_econ<-data.frame(rm.6.m10.wo0(LCF2014, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                                   cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                   'internet', 'hhsize', 'econ_ref'))
rm6w_cart_model<- lm(paa~m, data = rm6w_cart)
summary(rm6w_cart_model)

#Parametric with non-matches excluded regression-Statistical Uniques
fs6_para_econ<-data.frame(rm.6.m10.wo0(LCF2014, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                             para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                             'dwelling', 'internet', 'hhsize', 'econ_ref'))
rm6w_para_model<- lm(paa~m, data = rm6w_para)
summary(rm6w_para_model)

#CART with non-matches as 0 regression-Special Uniques
spu_cart_econ<- data.frame(paa.6.m10(spulcf, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                              cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                              'internet', 'hhsize', 'econ_ref'))
spu6_cart_model<- lm(paa~m, data = spu6_cart)
summary(spu6_cart_model)

#Parametric with non-matches as 0 regression-Special Uniques
spu_para_econ<-data.frame(paa.6.m10(spulcf, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6, para_lcf7,
                             para_lcf8, para_lcf9, para_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                             'internet', 'hhsize', 'econ_ref'))
spu6_para_model<- lm(paa~m, data = spu6_para)
summary(spu6_para_model)

#CART with non-matches excluded regression-Special Uniques
spuw_cart_econ<-data.frame(paa.6.m10.wo0(spulcf, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                                  cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                  'internet', 'hhsize', 'econ_ref'))
spu6w_cart_model<- lm(paa~m, data= spu6w_cart)
summary(spu6w_cart_model)

#parametric with non-matches excluded regression-Special Uniques
spuw_para_econ<-data.frame(paa.6.m10.wo0(spulcf, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                                  para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                                  'dwelling', 'internet', 'hhsize', 'econ_ref'))
spu6w_para_model<- lm(paa~m, data= spu6w_para)
summary(spu6w_para_model)






