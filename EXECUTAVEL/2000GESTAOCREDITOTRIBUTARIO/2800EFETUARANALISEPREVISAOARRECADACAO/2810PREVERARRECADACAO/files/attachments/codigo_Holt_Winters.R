require(forecast)
require(lubridate)

rm(list=ls())

#Lê a base de dados
dados.df <- read.table("S:/CRE/IGA/SAPR/ARQUIVOS/Mapeamento/Previsão/Exemplo usando R/ICMS_PR_entrada.csv", header=TRUE,sep=";",dec=",",row.names=1)

#Cria a série temporal
serie <- ts(dados.df$ICMS, start=c(1997,1), frequency=12)



# exemplo 1: salvar arquivo com previsao para os proximos 12 meses (60 amostras)
rm(list="prevlista")
nd = 60 # numero de amostras a utilizar
nper = 12 # periodos a prever
anoinicioprev = 2018 # inicio da previsao
mesinicioprev = 6 # inicio da previsao
datainicioprev <- as.Date(ISOdate(anoinicioprev,mesinicioprev,1))
datainiciodados <- datainicioprev
month(datainiciodados) <- month(datainiciodados) - nd
datafimdados <- datainicioprev
month(datafimdados) <- month(datafimdados) - 1
anoiniciodados = year(datainiciodados)
mesiniciodados = month(datainiciodados)
anofimdados = year(datafimdados)
mesfimdados = month(datafimdados)
serie_prev = window(serie, start=c(anoiniciodados,mesiniciodados), end=c(anofimdados,mesfimdados))
modHoltWinters <- HoltWinters(serie_prev, seasonal="additive")
modhw <- ets(serie_prev, model="AAA")
prevHoltWinters <- predict(modHoltWinters, nper)
prevhw <- predict(modhw, h = nper)$mean
if (exists("prevlista")) prevlista = cbind(prevlista, prevHoltWinters, prevhw) else prevlista = cbind(prevHoltWinters,prevhw)
write.table(prevlista, file = "S:/CRE/IGA/SAPR/ARQUIVOS/Mapeamento/Previsão/Exemplo usando R/1_saida_prev_unica.csv", sep=";", dec=",", col.names=NA)




# exemplo 2: rodar varias previsões para os próximos 12 meses e comparar com o realizado
anoinicio = 2011
anofim = 2018
for (anoprev in anoinicio:anofim){
  serie_prev = window(serie, start=c(anoprev-6,1), end=c(anoprev-1,12))
  modHoltWinters <- HoltWinters(serie_prev, seasonal="additive")
  modhw <- ets(serie_prev, model="AAA")
  prevAnoHoltWinters <- predict(modHoltWinters, 12)
  prevAnohw <- predict(modhw, h = 12)
  estAnoHoltWinters <- accuracy(window(serie,c(anoprev,1),c(anoprev,12)), prevAnoHoltWinters)
  estAnoHoltWinters <- cbind(estAnoHoltWinters, ano = anoprev)
  estAnohw <- accuracy(window(serie,c(anoprev,1),c(anoprev,12)), prevAnohw$mean)
  estAnohw <- cbind(estAnohw, ano = anoprev)
  if (anoprev == anoinicio) {
    prevHoltWinters <- prevAnoHoltWinters
    prevhw <- prevAnohw$mean
    estHoltWinters <- estAnoHoltWinters
    esthw <- estAnohw
  } else {
    prevHoltWinters <- c(prevHoltWinters,prevAnoHoltWinters)
    prevhw <- c(prevhw,prevAnohw$mean)
    estHoltWinters <- rbind(estHoltWinters,estAnoHoltWinters)
    esthw <- rbind(esthw,estAnohw)
  }
}
rm(list="modHoltWinters","modhw","prevAnoHoltWinters","prevAnohw","estAnoHoltWinters","estAnohw","anoprev","serie_prev")
prevHoltWinters <- ts(prevHoltWinters, start=c(anoinicio,1), frequency=12)
prevhw <- ts(prevhw, start=c(anoinicio,1), frequency=12)
ts.plot(window(serie, c(anoinicio,1), c(min(anofim,2018),12)),prevHoltWinters,prevhw, gpars = list(col = c("black", "blue","red"),xlab="ano",ylab="R$ milhoes"))
legend("topleft",c("real","funcao HoltWinters","funcao ETS"), col=c("black", "blue","red"), lty=1)
tudo=cbind(real=window(serie, c(anoinicio,1), c(min(anofim,2018),12)),prevHoltWinters,prevhw)
write.table(tudo, file = "S:/CRE/IGA/SAPR/ARQUIVOS/Mapeamento/Previsão/Exemplo usando R/2_saida_prev_12meses_vs_realizado.csv", sep=";", dec=",", col.names=NA)




# exemplo 3: rodar várias previsões para o próximo mês e comparar com realizado (usando 60 amostras)
anoinicio = 2011
anofim = 2018
nper = 60
nmeses = (anofim - anoinicio + 1) * 12
for (imes in 1:nmeses){
  anoiniciodados = anoinicio + floor((imes-1-nper)/12)
  mesiniciodados = ((imes - 1 - nper) %% 12) + 1
  anofimdados = anoinicio + floor((imes-1-1)/12)
  mesfimdados = ((imes - 1 - 1) %% 12) + 1
  anoprev = anoinicio + floor((imes-1)/12)
  mesprev = ((imes - 1) %% 12) + 1
  serie_prev = window(serie, start=c(anoiniciodados,mesiniciodados), end=c(anofimdados,mesfimdados))
  modHoltWinters <- HoltWinters(serie_prev, seasonal="additive")
  modhw <- ets(serie_prev, model="AAA")
  prevAnoHoltWinters <- predict(modHoltWinters, 1)
  prevAnohw <- predict(modhw, h = 1)
  estAnoHoltWinters <- accuracy(window(serie,c(anoprev,1),c(anoprev,12)), prevAnoHoltWinters)
  estAnoHoltWinters <- cbind(estAnoHoltWinters, ano = anoprev, mes = mesprev)
  estAnohw <- accuracy(window(serie,c(anoprev,1),c(anoprev,12)), prevAnohw$mean)
  estAnohw <- cbind(estAnohw, ano = anoprev, mes = mesprev)
  if (imes == 1) {
    prevHoltWinters <- prevAnoHoltWinters
    prevhw <- prevAnohw$mean
    estHoltWinters <- estAnoHoltWinters
    esthw <- estAnohw
  } else {
    prevHoltWinters <- c(prevHoltWinters,prevAnoHoltWinters)
    prevhw <- c(prevhw,prevAnohw$mean)
    estHoltWinters <- rbind(estHoltWinters,estAnoHoltWinters)
    esthw <- rbind(esthw,estAnohw)
  }
}
rm(list="modHoltWinters","modhw","prevAnoHoltWinters","prevAnohw","estAnoHoltWinters","estAnohw","anoprev","serie_prev")
prevHoltWinters <- ts(prevHoltWinters, start=c(anoinicio,1), frequency=12)
prevhw <- ts(prevhw, start=c(anoinicio,1), frequency=12)
ts.plot(window(serie, c(anoinicio,1), c(min(anofim,2018),12)),prevHoltWinters,prevhw, gpars = list(col = c("black", "blue","red"),xlab="ano",ylab="R$ milhoes"))
legend("topleft",c("real","funcao HoltWinters","funcao ETS"), col=c("black", "blue","red"), lty=1)
tudo=cbind(real=window(serie, c(anoinicio,1), c(min(anofim,2018),12)),prevHoltWinters,prevhw)
write.table(tudo, file = "S:/CRE/IGA/SAPR/ARQUIVOS/Mapeamento/Previsão/Exemplo usando R/3_saida_prev_1mes_vs_realizado.csv", sep=";", dec=",", col.names=NA)


