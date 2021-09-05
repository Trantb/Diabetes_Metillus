
summary(df$age)

df[df$age == NA,] <- mean(df$age)

summary(df[,c(4:17)])

df$age[is.na(df$age)] <- mean(df$age, na.rm = T)
summary(df$bmi)

df$bmi[is.na(df$bmi)] <- mean(df$bmi, na.rm = T)
df$height[is.na(df$height)] <- mean(df$height, na.rm = T)
df$weight[is.na(df$weight)] <- mean(df$weight, na.rm = T)

demographic <- df[,c(4:17)]

demographic$ethnicity[is.na(demographic$ethnicity)] <- 'NA'
str(demographic)
demographic$gender[is.na(demographic$gender)] <- 'NA'

demographic$hospital_admit_source[is.na(demographic$hospital_admit_source)] <- 'NA'
demographic$icu_admit_source[is.na(demographic$icu_admit_source)] <- 'NA'

demographic$icu_type[is.na(demographic$icu_type)]
table(demographic$readmission_status)

string <- c('ethnicity', 'gender', 'hospital_admit_source',
            'icu_admit_source','icu_stay_type', 'icu_type')
demographic <- dummy_cols(demographic, string, remove_first_dummy = T,
                          ignore_na = T)
summary(demographic)
t(t(names(demographic)))

colnames(demographic)[15:20] <- c('ethasian','ethcaucasian','ethhispanic',
                                  'ethna','ethnative','ethother')
colnames(demographic)[21:22] <- c('genmale','genna')
colnames(demographic)[23:37] <- c('hpain','hadmit','hdepartment','hfloor',
                                  'hicu','hicutosdu','hna','hobservation',
                                  'hoperating','hother','hotherhos',
                                  'hothericu','hpacu', 'hrecover',
                                  'hsdu')
colnames(demographic)[38:42] <- c('iasfloor','iasna','iasrecover',
                                  'iashos','iasicu')
colnames(demographic)[43:44] <- c('istreadmit','isttranfer')
colnames(demographic)[45:51] <- c('itccucticu','itcsicu','itcticu',
                                  'itmedsurg','itmicu','itneuro','itsicu')
str(demographic)
t(t(names(demographic)))
demographic <- demographic[,-c(4,5,7,8:11,13)]
rm(a)
rm(string)

demographic <- as.factor(demographic[,-c(1,2,4:6)])

