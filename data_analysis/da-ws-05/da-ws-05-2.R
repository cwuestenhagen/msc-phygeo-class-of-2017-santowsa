setwd("D:/mpg/daten/tab")
ff <- readRDS("feldfruechte_clean.rds")

a <- ff[[6]]
b <- ff[[8]]

reg_ff <- lm(a~b)
plot(a, b, main = "scatter plot with modeled relationship", 
     xlab = "(a) winter wheat", ylab = "(b) winter barley")
abline(reg_ff, col = "red", lwd = 2)

ss_observ <- sum((reg_ff$model$b - mean(reg_ff$model$b))**2)
ss_model <- sum((reg_ff$fitted.values - mean(reg_ff$model$b))**2)
ss_resid <- sum(reg_ff$fitted.values - reg_ff$model$b)

mss_observ <- ss_observ / (length(reg_ff$model$b)-1)
mss_resid <- ss_resid / (length(reg_ff$model$b)-2) 
mss_model <- ss_model / 1

f_value <- mss_model / mss_resid
f_value

anova(reg_ff)
summary(reg_ff)


plot(reg_ff, which = 1)
plot(reg_ff, which = 2)

#lapply

for (i in (1:100)){
  set.seed(i)
  s1 <-sample(nrow(subset(ff,(!is.na(ff[,6]))&
                            (!is.na(ff[,8])))),50)
  s2 <-sample(nrow(subset(ff,(!is.na(ff[,6]))&
                            (!is.na(ff[,8])))),100)
  lmod1 <- lm(a[s1] ~ b[s1])
  lmod2 <- lm(a[s2] ~ b[s2])
  p1<-shapiro.test(lmod1$residuals)$p.value
  p2<-shapiro.test(lmod2$residuals)$p.value
  pvalues1<-c(p1, pvalues1)
  pvalues2<-c(p2, pvalues2)
  num1<-sum(pvalues1<0.05)
  num2<-sum(pvalues2<0.05)
  df<-data.frame("50"=num1, "100"=num2)
}
print(df)       
?lm        

lista <- c(set.seed(1), sample(a, 100))
listb <- c(set.seed(1), sample(b, 100))

for (i in (1:100)){
  set.seed(i)
  s1 <-sample(nrow(subset(ff,(!is.na(ff[,6]))&
                            (!is.na(ff[,8])))),50)
  s2 <-sample(nrow(subset(ff,(!is.na(ff[,6]))&
                            (!is.na(ff[,8])))),100)
  
  lmod <- lm(lista~listb)
  p1<- shapiro.test(lmod$residuals)$p.value
  num1 <- sum(p1<0.05)
  df <- data.frame("100"=num1)
}
print(df)
?set.seed
lista = 1
lista
