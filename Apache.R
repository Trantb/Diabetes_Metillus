apache <- df[,c(18:45)]
summary(apache)

apache$apache_2_diagnosis[is.na(apache$apache_2_diagnosis)] <- 
  mean(apache$apache_2_diagnosis, na.rm = T)
apache$apache_3j_diagnosis[is.na(apache$apache_3j_diagnosis)] <-
  mean(apache$apache_3j_diagnosis, na.rm = T)
apache$bun_apache[is.na(apache$bun_apache)] <- mean(apache$bun_apache, na.rm = T)
apache$creatinine_apache[is.na(apache$creatinine_apache)] <-
  mean(apache$creatinine_apache, na.rm = T)
apache$gcs_eyes_apache[is.na(apache$gcs_eyes_apache)] <-
  mean(apache$gcs_eyes_apache, na.rm = T)
apache$gcs_motor_apache[is.na(apache$gcs_motor_apache)] <-
  mean(apache$gcs_motor_apache, na.rm = T)
apache$gcs_unable_apache[is.na(apache$gcs_unable_apache)] <-
  mean(apache$gcs_unable_apache, na.rm = T)
apache$gcs_verbal_apache[is.na(apache$gcs_verbal_apache)] <-
  mean(apache$gcs_verbal_apache, na.rm = T)
apache$glucose_apache[is.na(apache$glucose_apache)] <-
  mean(apache$glucose_apache, na.rm = T)
apache$heart_rate_apache[is.na(apache$heart_rate_apache)] <-
  mean(apache$heart_rate_apache, na.rm = T)
apache$hematocrit_apache[is.na(apache$hematocrit_apache)] <-
  mean(apache$hematocrit_apache, na.rm = T)
apache$map_apache[is.na(apache$map_apache)] <- mean(apache$map_apache, na.rm = T)
apache$resprate_apache[is.na(apache$resprate_apache)] <-
  mean(apache$resprate_apache, na.rm = T)
apache$sodium_apache[is.na(apache$sodium_apache)] <-
  mean(apache$sodium_apache, na.rm = T)
apache$temp_apache[is.na(apache$temp_apache)] <- mean(apache$temp_apache, na.rm = T)
apache$wbc_apache[is.na(apache$wbc_apache)] <- mean(apache$wbc_apache, na.rm = T)

summary(apache)
t(t(names(apache)))
apache <- apache[,-c(1,6,9,19:22,26)]
