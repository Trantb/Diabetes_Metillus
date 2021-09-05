vital <- df[,c(46:97)]

de <- as.data.frame(matrix(0, nrow = 130157, ncol = 26))
for (i in 1:length(vital)){
  if (i %% 2 == 1){
    de[,i-(i-1)/2] <- vital[i] - vital[i+1]
  }
}

vital <- de
rm(de)
rm(i)

vital$V2[is.na(vital$V2)] <- mean(vital$V2, na.rm = T)
vital$V3[is.na(vital$V3)] <- mean(vital$V3, na.rm = T)
vital$V4[is.na(vital$v4)] <- mean(vital$V4, na.rm = T)
vital$V6[is.na(vital$V6)] <- mean(vital$V6, na.rm = T)
vital$V7[is.na(vital$V7)] <- mean(vital$V7, na.rm = T)
vital$V8[is.na(vital$V8)] <- mean(vital$V8, na.rm = T)
vital$V9[is.na(vital$V9)] <- mean(vital$V9, na.rm = T)
vital$V11[is.na(vital$V11)] <- mean(vital$V11, na.rm = T)
vital$V12[is.na(vital$V12)] <- mean(vital$V12, na.rm = T)
vital$V13[is.na(vital$V13)] <- mean(vital$V13, na.rm = T)
vital$V15[is.na(vital$V15)] <- mean(vital$V15, na.rm = T)
vital$V16[is.na(vital$V16)] <- mean(vital$V16, na.rm = T)
vital$V17[is.na(vital$V17)] <- mean(vital$V17, na.rm = T)
vital$V19[is.na(vital$V19)] <- mean(vital$V19, na.rm = T)
vital$V20[is.na(vital$V20)] <- mean(vital$V20, na.rm = T)
vital$V21[is.na(vital$V21)] <- mean(vital$V21, na.rm = T)
vital$V22[is.na(vital$V22)] <- mean(vital$V22, na.rm = T)
vital$V24[is.na(vital$V24)] <- mean(vital$V24, na.rm = T)
vital$V25[is.na(vital$V25)] <- mean(vital$V25, na.rm = T)
vital$V26[is.na(vital$V26)] <- mean(vital$V26, na.rm = T)

vital <- vital[,-c(1,5,10,14,18,23)]


