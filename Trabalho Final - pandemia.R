library(pacman)
pacman::p_load(readr,survival,AdequacyModel,tidyverse,AICcmodavg,xtable)

setwd("~/Estudos/Análise de Sobrevivência/Trabalho final")

# Descritiva ----
banco <- readRDS("coracao_de_mae_sempre_cabe-mais_um_ano.rds")
banco_teste <- banco %>%  mutate(DIAS_PERM=DIAS_PERM+1,
                                 morte = case_when(MORTE == 0 ~ "Sobrevivência",
                                                   MORTE == 1 ~ "Morte"),
                                 pand = factor(case_when(anos_mes %in% c("1905","1906","1907","1908","1909",
                                                                  "1910","1911","1912","2001","2002") ~ "Pré-Pandemia",
                                                  anos_mes %in% c("2003","2004","2005","2006","2007",
                                                                  "2008","2009","2010","2011","2012") ~ "Pandemia"),
                                               levels=c("Pré-Pandemia","Pandemia")),
                                 sexo = factor(case_when(SEXO==1~"Masculino",
                                                         SEXO==3~"Feminino"),
                                               levels=c("Masculino","Feminino")),
                                 regiao = factor(case_when(estado %in% c("AC","AM","AP","PA","RO","RR","TO")~"Norte",
                                                           estado %in% c("AL","BA","CE","MA","PB","PE","PI","SE","RN")~"Nordeste",
                                                           estado %in% c("DF","GO","MT","MS")~"Centro-Oeste",
                                                           estado %in% c("SP","RJ","ES","MG")~"Sudeste",
                                                           estado %in% c("PR","SC","RS")~"Sul"),
                                                 levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")),
                                 complex = factor(case_when(COMPLEX=="02"~"Média complexidade",
                                                            COMPLEX=="03"~"Alta complexidade"),
                                                  levels=c("Média complexidade","Alta complexidade"))) %>% 
  filter(anos_mes %in% c("1910","1911","1912","2010","2011","2012"))

KM <- survfit(Surv(DIAS_PERM,MORTE)~1, data=banco_teste, conf.int=F)
plot(KM, conf.int=F, mark.time = T,col="#598883")
TTT(banco_teste$DIAS_PERM)
plot(KM,mark.time=T,conf.int=F,xlab="Tempo",ylab="H(t)",col=c("#598883"),fun="cumhaz")
lines(x = c(0,max(KM$time)),
      y = c(0,max(KM$cumhaz)))

# SEXO
KM_sexo <- survfit(Surv(DIAS_PERM,MORTE)~SEXO,data=banco_teste, conf.int=F)
summary(KM_sexo)
plot(KM_sexo,mark.time=T,conf.int=F,xlab="Tempo",ylab="S(t)",
     col= c("#598883","#E0B15C"))
legend(x = "topright",
       legend=c("Masculino","Feminino"), 
       fill = c("#598883","#E0B15C"))

(dif_1 <- survdiff(Surv(DIAS_PERM,MORTE)~SEXO, data=banco_teste,rho=0)) #logrank
(dif_2 <- survdiff(Surv(DIAS_PERM,MORTE)~SEXO, data=banco_teste,rho=1)) #wilcoxon

# REGIAO
KM_regiao <- survfit(Surv(DIAS_PERM,MORTE)~regiao,data=banco_teste, conf.int=F)
summary(KM_regiao)
plot(KM_regiao,mark.time=T,conf.int=F,xlab="Tempo",ylab="S(t)",
     col= c("#598883","#B87D61","#F77E45","#E0B15C","#78665D"))
legend(x = "topright",
       legend=c("Norte","Nordeste","Sudeste","Centro-Oeste","Sul"), 
       fill = c("#598883","#B87D61","#F77E45","#E0B15C","#78665D"))


(dif_1 <- survdiff(Surv(DIAS_PERM,MORTE)~regiao, data=banco_teste,rho=0)) #logrank
(dif_2 <- survdiff(Surv(DIAS_PERM,MORTE)~regiao, data=banco_teste,rho=1)) #wilcoxon

#2x2
banco_regiao <-data.frame(tempo=banco_teste$DIAS_PERM,
                          regiao=banco_teste$regiao,
                          MORTE=banco_teste$MORTE)
tempo_ne<-banco_regiao[banco_regiao$regiao == "Nordeste",]
tempo_no<-banco_regiao[banco_regiao$regiao == "Norte",]
tempo_su<-banco_regiao[banco_regiao$regiao == "Sul",]
tempo_se<-banco_regiao[banco_regiao$regiao == "Sudeste",]
tempo_co<-banco_regiao[banco_regiao$regiao == "Centro-Oeste",]

(dif_ne_no <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_co, tempo_ne),rho=1))

(dif_ne_su <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_su, tempo_ne),rho=1)) #não difere

(dif_ne_se <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_se, tempo_ne),rho=1))

(dif_ne_co <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_co, tempo_ne),rho=1))

(dif_no_su <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_no, tempo_su),rho=1))

(dif_no_se <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_no, tempo_se),rho=1))

(dif_no_co <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_co, tempo_no),rho=1)) #não difere

(dif_su_se <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_su, tempo_se),rho=1)) #não difere

(dif_su_co <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_co, tempo_su),rho=1)) 

(dif_se_co <- survdiff(Surv(tempo,MORTE)~regiao, 
                       data=rbind(tempo_co, tempo_se),rho=1))
# COMPLEXIDADE
KM_complex <- survfit(Surv(DIAS_PERM,MORTE)~complex,data=banco_teste, conf.int=F)
summary(KM_complex)
plot(KM_complex,mark.time=T,conf.int=F,xlab="Tempo",ylab="S(t)",
     col= c("#598883","#E0B15C"))
legend(x = "topright",
       legend=c("Média","Alta"), 
       fill = c("#598883","#E0B15C"))

(dif_1 <- survdiff(Surv(DIAS_PERM,MORTE)~complex, data=banco_teste,rho=0)) #logrank
(dif_2 <- survdiff(Surv(DIAS_PERM,MORTE)~complex, data=banco_teste,rho=1)) #wilcoxon

# PANDEMIA
KM_pand <- survfit(Surv(DIAS_PERM,MORTE)~pand,data=banco_teste, conf.int=F)
summary(KM_pand)
plot(KM_pand,mark.time=T,conf.int=F,xlab="Tempo",ylab="S(t)",
     col= c("#598883","#E0B15C"))
legend(x = "topright",
       legend=c("Pré-Pandemia","Pandemia"), 
       fill = c("#598883","#E0B15C"))

(dif_1 <- survdiff(Surv(DIAS_PERM,MORTE)~pand, data=banco_teste,rho=0)) #logrank
(dif_2 <- survdiff(Surv(DIAS_PERM,MORTE)~pand, data=banco_teste,rho=1)) #wilcoxon

# Distribuição ----
KM_exp <- survreg(Surv(DIAS_PERM, MORTE)~1, data=banco_teste, dist="exponential")
summary(KM_exp)

KM_weibull <- survreg(Surv(DIAS_PERM, MORTE)~1, data=banco_teste, dist="weibull")
summary(KM_weibull)

KM_lognormal <- survreg(Surv(DIAS_PERM, MORTE)~1, data=banco_teste, dist="lognormal")
summary(KM_lognormal)

KM_loglogistica <- survreg(Surv(DIAS_PERM, MORTE)~1, data=banco_teste, dist="loglogistic")
summary(KM_loglogistica)

exponencial_mu <- exp(as.numeric(summary(KM_exp)$coefficients))
st_exp <- exp(-(KM$time/exponencial_mu))

weibull_mu <- exp(as.numeric(summary(KM_weibull)$coefficients))
weibull_sigma <- 1/summary(KM_weibull)$scale
st_weibull <- exp(-(KM$time/weibull_mu)^weibull_sigma)

loglogistica_mu <- exp(as.numeric(summary(KM_loglogistica)$coefficients))
loglogistica_sigma <- 1/summary(KM_loglogistica)$scale
st_loglogistica <- 1/(1+(KM$time/loglogistica_mu)^loglogistica_sigma)

lognormal_mu <- as.numeric(summary(KM_lognormal)$coefficient)
lognormal_sigma <- summary(KM_lognormal)$scale
st_lognormal <- pnorm((-log(KM$time)+lognormal_mu)/lognormal_sigma)

plot(KM, conf.int=F, mark.time = F,xlab="DIAS_PERM",ylab="S(t)",col=c("black"))
lines(KM$time, st_exp, col="#598883")
lines(KM$time, st_weibull, col="#B87D61")
lines(KM$time, st_loglogistica, col="#F77E45")
lines(KM$time, st_lognormal, col="#E0B15C")
legend(x = "topright",
       fill = c("#598883","#B87D61","#F77E45","#E0B15C"),
       legend=c("Exponencial","Weibull","Log-logistica","Log-normal"))

# Teste da razão de verossimilhamça
#H0 = mod reduzido (Exp)
#H1 = mod completo (Weibull)
(TRV <- 2*(KM_weibull$loglik[1]-KM_exp$loglik[1]))
pchisq(TRV,1,lower.tail = F) #Rejeita H0, ent Weibull

# AIC, AICc, BIC
p<-1
(AIC_exponencial <- -2*KM_exp$loglik[1]+(p*2))
(AICc_exponencial <- AIC_exponencial+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_exponencial <- -2*KM_exp$loglik[1] + (p*log(length(banco_teste$DIAS_PERM))))

p<-2
(AIC_weibull <- -2*KM_weibull$loglik[1]+(p*2))
(AICc_weibull <- AIC_weibull+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_weibull <- -2*KM_weibull$loglik[1] + (2*log(length(banco_teste$DIAS_PERM))))

(AIC_log_normal <- -2*KM_lognormal$loglik[1]+(p*2))
(AICc_log_normal <- AIC_log_normal+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_log_normal <- -2*KM_lognormal$loglik[1] + (p*log(length(banco_teste$DIAS_PERM))))

(AIC_log_logistica <- -2*KM_loglogistica$loglik[1]+(p*2))
(AICc_log_logistica <- AIC_log_logistica+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_log_logistica <- -2*KM_loglogistica$loglik[1] + (p*log(length(banco_teste$DIAS_PERM))))


aic_bic <- data.frame(Distribuição = c("Exponencial","Weibull","Log-Normal","Log-Logística"),
                      AIC = c(AIC_exponencial,AIC_weibull,AIC_log_normal,AIC_log_logistica),
                      AICc = c(AICc_exponencial,AICc_weibull,AICc_log_normal,AICc_log_logistica),
                      BIC = c(BIC_exponencial,BIC_weibull,BIC_log_normal,BIC_log_logistica))
xtable(aic_bic)

# BURR XII ----
#Função de sobrevivência padrão
KM <- survfit(Surv(banco_teste$DIAS_PERM,banco_teste$MORTE)~1, conf.int=F)

#Modelos de Regressão
modelo0 <- survreg(Surv(DIAS_PERM,MORTE) ~ 1, dist = 'lognormal', data =  banco_teste) 
summary(modelo0)

# Burr-XII
vero <- function(par){
  alpha <- par[1]
  c_par <- par[2]
  k_par <- par[3]
  t <- banco_teste$DIAS_PERM
  censura <- banco_teste$MORTE
  f <- (((k_par*c_par/alpha)*(t/alpha)^(c_par-1))/((1+(t/alpha)^c_par)^(k_par+1)))
  F_t <- 1-(1/(1+(t/alpha)^c_par)^k_par)
  s <- 1-F_t
  if(alpha > 0 && c_par > 0 && k_par > 0){
    logL <- sum(censura * log(f) + (1-censura)*log(s))
    return(-logL)
  } else {
    return(-Inf)
  }
}

(par_loglogistica_alpha <- exp(as.numeric(summary(KM_loglogistica)$coefficients)))
(par_loglogistica_gama <- 1/as.numeric(summary(KM_loglogistica)$scale))

(vero_burrxii<-optim(c(par_loglogistica_alpha,par_loglogistica_gama,1),vero,hessian = T))
(vero_burrxii$convergence) #tem que ser 0
(vero_burrxii$par)
(logLM_burrxii <- -vero_burrxii$value)
(vari_burrxii <- diag(solve(vero_burrxii$hessian)))
(ep_burrxii <- sqrt(vari_burrxii))

alpha <- vero_burrxii$par[1]
c_par <- vero_burrxii$par[2]
k_par <- vero_burrxii$par[3]
t <-KM$time
Ft_burrxii <- 1-(1/(1+(t/alpha)^c_par)^k_par)
st_burrxii <- 1- Ft_burrxii

# Kumaraswamy Log-Logistica ----
#Função de sobrevivência padrão
KM <- survfit(Surv(banco_teste$DIAS_PERM,banco_teste$MORTE)~1, conf.int=F)

#Modelos de Regressão
modelo0 <- survreg(Surv(DIAS_PERM,MORTE) ~ 1, dist = 'lognormal', data =  banco_teste) 
summary(modelo0)

# Kumaraswamy-Loglogistica
vero <- function(par){
  a <- par[1]
  b <- par[2]
  alpha <- par[3]
  gama <- par[4]
  t <- banco_teste$DIAS_PERM
  censura <- banco_teste$MORTE
  g <- ((gama/alpha^gama)*t^(gama-1)*(1+(t/alpha)^gama)^(-2))
  SG_t <- 1/(1+(t/alpha)^gama)
  G <- 1- SG_t
  f <- (a*b*g*G^(a-1))*(1-G^a)^(b-1)
  F_t <- 1-(1-G^a)^b
  s <- 1-F_t
  if(alpha > 0 && gama>0 && a > 0 && b > 0){
    logL <- sum(censura * log(f) + (1-censura)*log(s))
    return(-logL)
  } else {
    return(-Inf)
  }
}

(par_loglogistica_alpha <- exp(as.numeric(summary(KM_loglogistica)$coefficients)))
(par_loglogistica_gama <- 1/as.numeric(summary(KM_loglogistica)$scale))

(vero_kum_logis<-optim(c(1.5,1.5,par_loglogistica_alpha,par_loglogistica_gama),vero,hessian = T))
(vero_kum_logis$convergence) #tem que ser 0
(vero_kum_logis$par)
(logLM_kum_logis <- -vero_kum_logis$value)
(vari_kum_logis <- diag(solve(vero_kum_logis$hessian)))
(ep_kum_logis <- sqrt(vari_kum_logis))

a <- vero_kum_logis$par[1]
b <- vero_kum_logis$par[2]
alpha <- vero_kum_logis$par[3]
gama <- vero_kum_logis$par[4]
t <-KM$time
g <- ((gama/alpha^gama)*t^(gama-1)*(1+(t/alpha)^gama)^(-2))
SG_t <- 1/(1+(t/alpha)^gama)
G <- 1- SG_t
f <- (a*b*g*G^(a-1))*(1-G^a)^(b-1)
Ft_kum_logis <- 1-(1-G^a)^b
st_kum_logis <- 1- Ft_kum_logis


# Kumaraswamy Log-Normal ----
#Função de sobrevivência padrão
KM <- survfit(Surv(banco_teste$DIAS_PERM,banco_teste$MORTE)~1, conf.int=F)

#Modelos de Regressão
modelo0 <- survreg(Surv(DIAS_PERM,MORTE) ~ 1, dist = 'lognormal', data =  banco_teste) 
summary(modelo0)

# Kumaraswamy-Log-Normal
vero <- function(par){
  a <- par[1]
  b <- par[2]
  mu <- par[3]
  sigma <- par[4]
  t <- banco_teste$DIAS_PERM
  censura <- banco_teste$MORTE
  g <- (exp((-1/2)*((log(t)-mu)/sigma)^2))/(t*sigma*sqrt(2*pi))
  SG_t <- pnorm((-log(t)+mu)/sigma)
  G <- 1- SG_t
  f <- (a*b*g*G^(a-1))*(1-G^a)^(b-1)
  F_t <- 1-(1-G^a)^b
  s <- 1-F_t
  if(mu > 0 && sigma > 0 && a > 0 && b > 0){
    logL <- sum(censura * log(f) + (1-censura)*log(s))
    return(-logL)
  } else {
    return(-Inf)
  }
}

(lognorm_mu <- as.numeric(summary(modelo0)$coefficients))
(lognorm_sigma <- as.numeric(summary(modelo0)$scale))

(vero_kum_lognorm<-optim(c(1,1,lognorm_mu,lognorm_sigma),vero,hessian = T))
(vero_kum_lognorm$convergence) #tem que ser 0
(vero_kum_lognorm$par)
(logLM_kum_lognorm <- -vero_kum_lognorm$value)
(vari_kum_lognorm <- diag(solve(vero_kum_lognorm$hessian)))
(ep_kum_lognorm <- sqrt(vari_kum_lognorm))

a <- vero_kum_lognorm$par[1]
b <- vero_kum_lognorm$par[2]
mu <- vero_kum_lognorm$par[3]
sigma <- vero_kum_lognorm$par[4]
t <-KM$time
g <- (exp((-1/2)*((log(t)-mu)/sigma)^2))/(t*sigma*sqrt(2*pi))
SG_t <- pnorm((-log(t)+mu)/sigma)
G <- 1- SG_t
f <- (a*b*g*G^(a-1))*(1-G^a)^(b-1)
Ft_kum_lognorm <- 1-(1-G^a)^b
st_kum_lognorm <- 1- Ft_kum_lognorm

plot(KM, conf.int = F, mark.time = F, col = "#242424", xlab = "Tempo (dias)", ylab = "S(t)", xlim = c(0, max(banco_teste$DIAS_PERM) + 5)) 
lines(KM$time, st_lognormal, col = "#598883")
lines(KM$time, st_loglogistica, col = "#B87D61")
lines(KM$time, st_burrxii,  col = "#F77E45")
lines(KM$time, st_kum_lognorm, col = "#E0B15C")
lines(KM$time, st_kum_logis, col = "darkgreen")
legend(x = "topright",  
       fill = c("#242424","#598883","#B87D61","#F77E45","#E0B15C","darkgreen"), 
       c("Kaplan-Meier","Log-Normal","Log-Logística","BURR-XII","Kumaraswamy-Log-Normal","Kumaraswamy-Log-Logistica"))


p <- 2
(AIC_log_normal <- -2*KM_lognormal$loglik[1]+(p*2))
(AICc_log_normal <- AIC_log_normal+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_log_normal <- -2*KM_lognormal$loglik[1] + (p*log(length(banco_teste$DIAS_PERM))))

(AIC_log_logistica <- -2*KM_loglogistica$loglik[1]+(p*2))
(AICc_log_logistica <- AIC_log_logistica+ (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_log_logistica <- -2*KM_loglogistica$loglik[1] + (p*log(length(banco_teste$DIAS_PERM))))

p<-3
(AIC_burrxii <- -2*logLM_burrxii + (p*2))
(AICc_burrxii <- AIC_burrxii + (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_burrxii <- -2*logLM_burrxii + (p*log(length(banco_teste$DIAS_PERM))))

p<-4
(AIC_kum_logis <- -2*logLM_kum_logis + (p*2))
(AICc_kum_logis <- AIC_kum_logis + (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_kum_logis <- -2*logLM_kum_logis + (p*log(length(banco_teste$DIAS_PERM))))

(AIC_kum_lognorm <- -2*logLM_kum_lognorm + (p*2))
(AICc_kum_lognorm <- AIC_kum_lognorm + (2*p*(p-1))/(length(banco_teste$DIAS_PERM)-p-1))
(BIC_kum_lognorm <- -2*logLM_kum_lognorm + (p*log(length(banco_teste$DIAS_PERM))))


(aic_bic_tds <- data.frame(Dist = c("Log-normal","Log-Logística","Burr-XII","Kumaraswamy-Log-Logistica","Kumaraswamy-Log-Normal"),
                           AIC = c(AIC_log_normal,AIC_log_logistica,AIC_burrxii,AIC_kum_lognorm,AIC_kum_lognorm),
                           AICc = c(AICc_log_normal,AICc_log_logistica,AICc_burrxii,AICc_kum_logis,AICc_kum_lognorm),
                           BIC = c(BIC_log_normal,BIC_log_logistica,BIC_burrxii,BIC_kum_logis,BIC_kum_lognorm)))


xtable(aic_bic_tds)
