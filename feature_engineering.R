
don_0 <- don_ori %>% 
  rename(Y=TARGET) %>%
  filter(MAC_CODE == "WT1") %>%
  select(-MAC_CODE)

dim(don_0)
summary(don_0) ### pas évident puisque l'on a besoin de variable
# ce qui peut être interessant, c'est de comparer les données manquants vs données non manquants vav de la variable


don_1 <- don_0 %>% na.omit()

summary(don_1)

# Je retire toutes les variables avec un suffix suivant : max // min // std
don_2 <- don_1 %>% select(!ends_with(c("max", "min","std")))

nb <- 10

decou <- sample(rep(1:nb,length=nrow(don_2)))


don <- don_2[decou ==1,]

rm(don_0,don_1, don_ori)


summary(don)



