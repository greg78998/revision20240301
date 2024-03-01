

setwd("C:/Users/cepe-s3-01/Desktop/greg_bdf/_data")


data_ori_X <- fread("engieX.csv",header = TRUE, sep = ";")
data_ori_Y <- fread("engieY.csv",header = TRUE, sep = ";")



data_ori <- cbind(data_ori_X %>% select(-ID),
                  data_ori_Y %>% select(-ID))



don_ori <- data_ori

rm(data_ori_X, data_ori_Y,data_ori)

