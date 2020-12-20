#Regression CART CAP- with non-matches as 0
tot6_cart<-data.frame(paa.6.m10(BSA2014, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
              cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
              'RClass', 'HHIncD'))

tot6_cart_model<- lm(paa~m, data = tot6_cart)
summary(tot6_cart_model)

##################
#Regression Parametric CAP- with non-matches as 0
tot6_para<- data.frame(paa.6.m10(BSA2014, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                   para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                   'RClass', 'HHIncD'))

tot6_para_model<- lm(paa~m, data = tot6_para)
summary(tot6_para_model)

#####################
#Regression CART CAP- with non-matches excluded
wtot6_cart<-data.frame(paa.6.m10.wo0(BSA2014, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
                     cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                     'RClass', 'HHIncD'))

wtot6_cart_model<- lm(paa~m, data = wtot6_cart)
summary(wtot6_cart_model)

##################
#Regression Parametric CAP- with non-matches excluded
tot6_para<- data.frame(paa.6.m10.wo0(BSA2014, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                 para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                 'RClass', 'HHIncD'))

wtot6_para_model<- lm(paa~m, data = wtot6_para)
summary(wtot6_para_model)

############################
#Regression CART CAP- with non-matches as 0s-Statistical uniques
uniq6_cart<-data.frame(rm.6.m10(BSA2014, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
                     cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                     'RClass', 'HHIncD'))

uniq6_cart_model<- lm(paa~m, data = uniq6_cart)
summary(uniq6_cart_model)

#############################################
#Regression Parametric CAP- with non-matches as 0s-Statistical uniques
uniq6_para<- data.frame(rm.6.m10(BSA2014, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                 para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                 'RClass', 'HHIncD'))

uniq6_para_model<- lm(paa~m, data = uniq6_para)
summary(uniq6_para_model)

##############################
#Regression CART CAP- with non-matches excluded-Statistical uniques
wuniq6_cart<-data.frame(rm.6.m10.wo0(BSA2014, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
                     cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                     'RClass', 'HHIncD'))

wuniq6_cart_model<- lm(paa~m, data = wuniq6_cart)
summary(wuniq6_cart_model)

#############################
#Regression Parametric CAP- with non-matches excluded-Statistical uniques
wuniq6_para<- data.frame(rm.6.m10.wo0(BSA2014, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                 para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                 'RClass', 'HHIncD'))

wuniq6_para_model<- lm(paa~m, data = wuniq6_para)
summary(wuniq6_para_model)

#############################
#Regression CART CAP- with non-matches as 0s-Special uniques
sp6_cart<-data.frame(paa.6.m10(sp_6, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
                     cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                     'RClass', 'HHIncD'))

sp6_cart_model<- lm(paa~m, data = sp6_cart)
summary(sp6_cart_model)

##############################
#Regression Parametric CAP- with non-matches as 0s-Special uniques
sp6_para<- data.frame(paa.6.m10(sp_6, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                 para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                 'RClass', 'HHIncD'))

sp6_para_model<- lm(paa~m, data = sp6_para)
summary(sp6_para_model)

##########################
#Regression CART CAP- with non-matches excluded-Special uniques
sp6_cart<-data.frame(paa.6.m10.wo0(sp_6, cart_bsa1, cart_bsa2, cart_bsa3, cart_bsa4, cart_bsa5, cart_bsa6, cart_bsa7,
                    cart_bsa8, cart_bsa9, cart_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                    'RClass', 'HHIncD'))

wsp6_cart_model<- lm(paa~m, data = wsp6_cart)
summary(wsp6_cart_model)

##############################
#Regression Parametric CAP- with non-matches excluded-Special uniques
wsp6_para<- data.frame(paa.6.m10(sp_6, para_bsa1, para_bsa2, para_bsa3, para_bsa4, para_bsa5, para_bsa6, para_bsa7,
                                 para_bsa8, para_bsa9, para_bsa10, 'GOR2', 'HEdQual2', 'MarStat6', 'RAgecat3', 'Rsex', 
                                 'RClass', 'HHIncD'))

wsp6_para_model<- lm(paa~m, data = wsp6_para)
summary(wsp6_para_model)




