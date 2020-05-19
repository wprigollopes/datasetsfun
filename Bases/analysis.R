acidentes2009 <- read.csv("acidentes-2009.csv", header=TRUE, sep=";")
acidentes2010 <- read.csv("acidentes-2010.csv", header=TRUE, sep=";")
acidentes2011 <- read.csv("acidentes-2011.csv", header=TRUE, sep=";")
acidentes2012 <- read.csv("acidentes-2012.csv", header=TRUE, sep=";")
acidentes2013 <- read.csv("acidentes-2013.csv", header=TRUE, sep=";")
acidentes2014 <- read.csv("acidentes-2014.csv", header=TRUE, sep=";")

# Contando numero de acidentes
length(acidentes2009)
length(acidentes2010)
length(acidentes2011)
length(acidentes2012)
length(acidentes2013)
length(acidentes2014)

# Verificando acidentes fatais
summary(as.factor(acidentes2009$FATAIS))
summary(as.factor(acidentes2010$FATAIS))
summary(as.factor(acidentes2011$FATAIS))
summary(as.factor(acidentes2012$FATAIS))
summary(as.factor(acidentes2013$FATAIS))
summary(as.factor(acidentes2014$FATAIS))

# Somando número de mortes
sum(acidentes2009$MORTE_POST)+sum(acidentes2009$MORTES)
sum(acidentes2010$MORTE_POST)+sum(acidentes2010$MORTES)
sum(acidentes2011$MORTE_POST)+sum(acidentes2011$MORTES)
sum(acidentes2012$MORTE_POST)+sum(acidentes2012$MORTES)
sum(acidentes2013$MORTE_POST)+sum(acidentes2013$MORTES)
sum(acidentes2014$MORTE_POST)+sum(acidentes2014$MORTES)

# Desconsiderando valores vazios
na.rm=T

# Verificando valores mínimos e máximos
range(acidentes2009$MORTE_POST)
range(acidentes2010$MORTE_POST)
range(acidentes2011$MORTE_POST)
range(acidentes2012$MORTE_POST)
range(acidentes2013$MORTE_POST)
range(acidentes2014$MORTE_POST)

range(acidentes2009$MORTES)
range(acidentes2010$MORTES)
range(acidentes2011$MORTES)
range(acidentes2012$MORTES)
range(acidentes2013$MORTES)
range(acidentes2014$MORTES)

range(acidentes2009$FATAIS)
range(acidentes2010$FATAIS)
range(acidentes2011$FATAIS)
range(acidentes2012$FATAIS)
range(acidentes2013$FATAIS)
range(acidentes2014$FATAIS)

length(which(as.numeric(acidentes2009$FATAIS)>=1))
length(which(as.numeric(acidentes2010$FATAIS)>=1))
length(which(as.numeric(acidentes2011$FATAIS)>=1))
length(which(as.numeric(acidentes2012$FATAIS)>=1))
length(which(as.numeric(acidentes2013$FATAIS)>=1))
length(which(as.numeric(acidentes2014$FATAIS)>=1))

# Gerando histogramas de horários de acidentes

jpeg(filename="horas_acidentes.jpg", width=800, height=800, units="px", pointsize=12, quality=100, bg="white", res=NA);
par(mfrow=c(3,2))
hist(as.numeric(acidentes2014$FX_HORA), breaks=24, main="2014", xlab="Horário do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2013$FX_HORA), breaks=24, main="2013", xlab="Horário do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2012$FX_HORA), breaks=24, main="2012", xlab="Horário do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2011$FX_HORA), breaks=24, main="2011", xlab="Horário do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2010$FX_HORA), breaks=24, main="2010", xlab="Horário do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2009$FX_HORA), breaks=24, main="2009", xlab="Horário do acidente", ylab="Frequência", col="yellow");
dev.off()

jpeg(filename="meses_acidentes.jpg", width=800, height=800, units="px", pointsize=12, quality=100, bg="white", res=NA);
par(mfrow=c(3,2))
hist(as.numeric(acidentes2014$MES), breaks=12, main="2014", xlab="Mês do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2013$MES), breaks=12, main="2013", xlab="Mês do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2012$MES), breaks=12, main="2012", xlab="Mês do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2011$MES), breaks=12, main="2011", xlab="Mês do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2010$MES), breaks=12, main="2010", xlab="Mês do acidente", ylab="Frequência", col="yellow");
hist(as.numeric(acidentes2009$MES), breaks=12, main="2009", xlab="Mês do acidente", ylab="Frequência", col="yellow");
dev.off()

fatais2014 <- data.frame(cbind(AUTO = subset(acidentes2014, FATAIS != 0)$AUTO, TAXI = subset(acidentes2014, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2014, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2014, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2014, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2014, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2014, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2014, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2014, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2014, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2014, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2014, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2014, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2014, FATAIS != 0)$FERIDOS))

fatais2013 <- data.frame(cbind(AUTO = subset(acidentes2013, FATAIS != 0)$AUTO, TAXI = subset(acidentes2013, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2013, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2013, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2013, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2013, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2013, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2013, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2013, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2013, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2013, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2013, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2013, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2013, FATAIS != 0)$FERIDOS))

fatais2012 <- data.frame(cbind(AUTO = subset(acidentes2012, FATAIS != 0)$AUTO, TAXI = subset(acidentes2012, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2012, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2012, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2012, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2012, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2012, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2012, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2012, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2012, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2012, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2012, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2012, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2012, FATAIS != 0)$FERIDOS))

fatais2011 <- data.frame(cbind(AUTO = subset(acidentes2011, FATAIS != 0)$AUTO, TAXI = subset(acidentes2011, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2011, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2011, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2011, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2011, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2011, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2011, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2011, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2011, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2011, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2011, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2011, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2011, FATAIS != 0)$FERIDOS))

fatais2010 <- data.frame(cbind(AUTO = subset(acidentes2010, FATAIS != 0)$AUTO, TAXI = subset(acidentes2010, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2010, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2010, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2010, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2010, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2010, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2010, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2010, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2010, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2010, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2010, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2010, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2010, FATAIS != 0)$FERIDOS))

fatais2009 <- data.frame(cbind(AUTO = subset(acidentes2009, FATAIS != 0)$AUTO, TAXI = subset(acidentes2009, FATAIS != 0)$TAXI, LOTACAO = subset(acidentes2009, FATAIS != 0)$LOTACAO, ONIBUS_URB = subset(acidentes2009, FATAIS != 0)$ONIBUS_URB, ONIBUS_INT = subset(acidentes2009, FATAIS != 0)$ONIBUS_INT, CAMINHAO = subset(acidentes2009, FATAIS != 0)$CAMINHAO, MOTO = subset(acidentes2009, FATAIS != 0)$MOTO, CARROCA = subset(acidentes2009, FATAIS != 0)$CARROCA, BICICLETA = subset(acidentes2009, FATAIS != 0)$BICICLETA, OUTRO = subset(acidentes2009, FATAIS != 0)$OUTRO, FATAIS = subset(acidentes2009, FATAIS != 0)$FATAIS, MORTES = subset(acidentes2009, FATAIS != 0)$MORTES, MORTE_POST = subset(acidentes2009, FATAIS != 0)$MORTE_POST, FERIDOS = subset(acidentes2009, FATAIS != 0)$FERIDOS))


fatais <- rbind(fatais2014, fatais2013)
fatais <- rbind(fatais, fatais2012)
fatais <- rbind(fatais, fatais2011)
fatais <- rbind(fatais, fatais2010)
fatais <- rbind(fatais, fatais2009)

# Correlações
correlacoes = data.frame(rbind( 
  FATAIS = c(AUTO = cor(fatais$FATAIS, fatais$AUTO), TAXI = cor(fatais$FATAIS, fatais$TAXI),LOACAO = cor(fatais$FATAIS, fatais$LOTACAO),ONIBUS_URB = cor(fatais$FATAIS, fatais$ONIBUS_URB),ONIBUS_INT = cor(fatais$FATAIS, fatais$ONIBUS_INT),CAMINHAO = cor(fatais$FATAIS, fatais$CAMINHAO), MOTO = cor(fatais$FATAIS, fatais$MOTO),CARROCA = cor(fatais$FATAIS, fatais$CARROCA), BICICLETA = cor(fatais$FATAIS, fatais$BICICLETA), OUTRO = cor(fatais$FATAIS, fatais$OUTRO)), 
  MORTES = c(cor(fatais$MORTES, fatais$AUTO), cor(fatais$MORTES, fatais$TAXI), cor(fatais$MORTES, fatais$LOTACAO), cor(fatais$MORTES, fatais$ONIBUS_URB), cor(fatais$MORTES, fatais$ONIBUS_INT), cor(fatais$MORTES, fatais$CAMINHAO), cor(fatais$MORTES, fatais$MOTO), cor(fatais$MORTES, fatais$CARROCA), cor(fatais$MORTES, fatais$BICICLETA), cor(fatais$MORTES, fatais$OUTRO)), 
  MORTES_POST = c(cor(fatais$MORTE_POST, fatais$AUTO), cor(fatais$MORTE_POST, fatais$TAXI), cor(fatais$MORTE_POST, fatais$LOTACAO), cor(fatais$MORTE_POST, fatais$ONIBUS_URB), cor(fatais$MORTE_POST, fatais$ONIBUS_INT), cor(fatais$MORTE_POST, fatais$CAMINHAO), cor(fatais$MORTE_POST, fatais$MOTO), cor(fatais$MORTE_POST, fatais$CARROCA), cor(fatais$MORTE_POST, fatais$BICICLETA), cor(fatais$MORTE_POST, fatais$OUTRO)), 
  FERIDOS = c(cor(fatais$FERIDOS, fatais$AUTO), cor(fatais$FERIDOS, fatais$TAXI), cor(fatais$FERIDOS, fatais$LOTACAO), cor(fatais$FERIDOS, fatais$ONIBUS_URB), cor(fatais$FERIDOS, fatais$ONIBUS_INT), cor(fatais$FERIDOS, fatais$CAMINHAO), cor(fatais$FERIDOS, fatais$MOTO), cor(fatais$FERIDOS, fatais$CARROCA), cor(fatais$FERIDOS, fatais$BICICLETA), cor(fatais$FERIDOS, fatais$OUTRO))))

#
# APRIORI
#
fatais2014 <- data.frame(NOITEDIA = subset(acidentes2014, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2014, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2014, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2014, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2014, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2014, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2014, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2014, FATAIS != 0)$REGIAO)

fatais2013 <- data.frame(NOITEDIA = subset(acidentes2013, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2013, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2013, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2013, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2013, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2013, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2013, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2013, FATAIS != 0)$REGIAO)

fatais2012 <- data.frame(NOITEDIA = subset(acidentes2012, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2012, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2012, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2012, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2012, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2012, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2012, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2012, FATAIS != 0)$REGIAO)

fatais2011 <- data.frame(NOITEDIA = subset(acidentes2011, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2011, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2011, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2011, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2011, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2011, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2011, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2011, FATAIS != 0)$REGIAO)

fatais2010 <- data.frame(NOITEDIA = subset(acidentes2010, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2010, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2010, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2010, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2010, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2010, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2010, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2010, FATAIS != 0)$REGIAO)

fatais2009 <- data.frame(NOITEDIA = subset(acidentes2009, FATAIS != 0)$NOITE_DIA, LOCAL = subset(acidentes2009, FATAIS != 0)$LOCAL, DIASEM = subset(acidentes2009, FATAIS != 0)$DIA_SEM, LOG1 = subset(acidentes2009, FATAIS != 0)$LOG1, TIPO_ACID = subset(acidentes2009, FATAIS != 0)$TIPO_ACID, LOCAL = subset(acidentes2009, FATAIS != 0)$LOCAL, TEMPO = subset(acidentes2009, FATAIS != 0)$TEMPO, REGIAO = subset(acidentes2009, FATAIS != 0)$REGIAO)

fatais <- merge(fatais2014, fatais2013, all=TRUE)
fatais <- merge(fatais, fatais2012, all=TRUE)
fatais <- merge(fatais, fatais2011, all=TRUE)
fatais <- merge(fatais, fatais2010, all=TRUE)
fatais <- merge(fatais, fatais2009, all=TRUE)

library(arules)
rules <- apriori(fatais, parameter = list(minlen=2, sup=0.005, conf=0.8), appearance = list(rhs=c("NOITEDIA=NOITE","NOITEDIA=DIA"), default="lhs"), control=list(verbose=F))
inspect(rules)
rules <- apriori(fatais, parameter = list(minlen=2, sup=0.01, conf=0.8), appearance = list(rhs=c("TIPO_ACID=ATROPELAMENTO", "TIPO_ACID=CHOQUE", "TIPO_ACID=ALBAROAMENTO"), default="lhs"), control=list(verbose=F))
inspect(rules)

