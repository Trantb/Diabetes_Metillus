lab <- df[,c(98:173)]


de <- as.data.frame(matrix(0, nrow = 130157, ncol = 38))
for (i in 1:length(lab)){
  if (i %% 2 == 1){
    de[,i-(i-1)/2] <- lab[i] - lab[i+1]
  }
}

lab <- de
rm(de)
rm(i)

colnames(lab)[1:38] <- paste('l', as.character(1:38), sep = '')
colnames(lab)[31:38] <- paste('lbg', as.character(1:8), sep = '')

lab$l3[is.na(lab$l3)] <- mean(lab$l3, na.rm = T)
lab$l4[is.na(lab$l4)] <- mean(lab$l4, na.rm = T)
lab$l5[is.na(lab$l5)] <- mean(lab$l5, na.rm = T)
lab$l6[is.na(lab$l6)] <- mean(lab$l6, na.rm = T)
lab$l7[is.na(lab$l7)] <- mean(lab$l7, na.rm = T)
lab$l8[is.na(lab$l8)] <- mean(lab$l8, na.rm = T)
lab$l9[is.na(lab$l9)] <- mean(lab$l9, na.rm = T)
lab$l12[is.na(lab$l12)] <- mean(lab$l12, na.rm = T)
lab$l13[is.na(lab$l13)] <- mean(lab$l13, na.rm = T)
lab$l14[is.na(lab$l14)] <- mean(lab$l14, na.rm = T)
lab$l15[is.na(lab$l15)] <- mean(lab$l15, na.rm = T)

lab <- lab[,-c(1,2,10,11,16,17:38)]






