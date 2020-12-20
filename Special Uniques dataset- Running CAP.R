#the Special uniques file comes from the Special Unique detection SUDA) algorithm which is 
#a seperate software  

# creating a version of the LCF with an ID varaibles
suda6lcf<- LCF2014
suda6lcf$ID<- 1
suda6lcf$ID<- (1:nrow(suda6lcf))

#the suda_key_6 dataset comes fom the SUDA software
SUDA_6<-suda_key_6[, c('ID')]

suda6lcf<- merge(SUDA_6, suda6lcf, by = "ID")

#kept only the important variables from the SUDA file
vars_suda<- c('ID', 'SUDA_S', 'DIS_SS')

suda_key_6<- suda_key_6[vars_suda]


sp_alu<- merge(suda_key_6, suda6lcf, by = 'ID')

#keeping only the speical uniques whcih recieved a SUDA score in the top percentile
special_uniques<- subset(suda_key_6, suda_key_6$SUDA_S>=12)

spulcf<- LCF2014

spulcf$ID<- 1

spulcf$ID<- (1:nrow(spulcf))

spulcf<- merge(special_uniques, spulcf, by = "ID")




#running the CAP function with the Special unique datasets
#Special Uniques with non-matches as 0s-CART
spu_cart_econ<- col.paa.6.m10(spulcf, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                              cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                              'internet', 'hhsize', 'econ_ref')

#Special uniques with non-matches as 0s-Parametric
spu_para_econ<-col.paa.6.m10(spulcf, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6, para_lcf7,
                             para_lcf8, para_lcf9, para_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                             'internet', 'hhsize', 'econ_ref')


#special uniques with non matches undefined-CART
spuw_cart_econ<-col.paa.6.m10.wo0(spulcf, cart_lcf1, cart_lcf2, cart_lcf3, cart_lcf4, cart_lcf5, cart_lcf6, cart_lcf7,
                                  cart_lcf8, cart_lcf9, cart_lcf10, 'gor', 'output_area', 'tenure', 'dwelling', 
                                  'internet', 'hhsize', 'econ_ref')

#special uniques with non matches undefined-Parametric
spuw_para_econ<-col.paa.6.m10.wo0(spulcf, para_lcf1, para_lcf2, para_lcf3, para_lcf4, para_lcf5, para_lcf6,
                                  para_lcf7, para_lcf8, para_lcf9, para_lcf10,'gor', 'output_area', 'tenure',
                                  'dwelling', 'internet', 'hhsize', 'econ_ref')








