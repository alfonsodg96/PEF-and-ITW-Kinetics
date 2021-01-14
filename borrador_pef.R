library(ggplot2)
require(readxl)

DATOS_PEF_NIÑOS <- read_excel("/Volumes/ALFONSOONCE/ODIN_EXTRACTION/DATOS_PEF_NIÑOS.xlsx",
sheet = "PEF (NIÑOS)", range = "A1:E35")

DATOS_ITW <- read_excel("/Volumes/ALFONSOONCE/ODIN_EXTRACTION/DATOS_ITW.xlsx",
                        sheet = "ITW", range = "A1:E37")

datos_pef <- as.data.frame(DATOS_PEF_NIÑOS)

datos_pef <- datos_pef[which(datos_pef$EDAD <= 16),]
datos_pef <- datos_pef[which(datos_pef$`GRUPO (Nº ciclos)` == 2),]

summary(datos_pef$EDAD)


datos_itw <- as.data.frame(DATOS_ITW)
datos_itw <- datos_itw[which(datos_itw$EDAD <= 16),]
datos_itw <- datos_itw[which(datos_itw$`GRUPO (Nº ciclos)` == 2),]

summary(datos_itw$EDAD)


count_M <- length(which(datos_pef$SEXO == "M"))
count_F <- length(which(datos_pef$SEXO == "F"))
v_M <- c("M", count_M)
v_F <- c("F", count_F)

data <- v_M
data <- as.data.frame(rbind(data, v_F))
data[] <- lapply(data, as.character)


e <- ggplot(data, aes(x = V1, y = as.integer(V2)))
e + geom_bar(stat ="identity") +
  labs(x= "Sex", y="Count", title="PEF sex distribution") +
  geom_text(aes(label=V2), vjust= -0.2, size=5) +
  ylim(0, max(as.integer(data$V2))*1.1)




count_M <- length(which(datos_itw$SEXO == "M"))
count_F <- length(which(datos_itw$SEXO == "F"))
v_M <- c("M", count_M)
v_F <- c("F", count_F)

data2 <- v_M
data2 <- as.data.frame(rbind(data2, v_F))
data2[] <- lapply(data2, as.character)


e <- ggplot(data2, aes(x = V1, y = as.integer(V2)))
e + geom_bar(stat ="identity") +
  labs(x= "Sex", y="Count", title="ITW sex distribution") +
  geom_text(aes(label=V2), vjust= -0.2, size=5) +
  ylim(0, max(as.integer(data2$V2))*1.1)




## AGE ##
age_pef <- datos_pef$EDAD
v_age <- seq(1:max(age_pef))
count <- vector(mode="integer", length=length(v_age))
for (i in min(v_age):max(v_age)){
  count[i] <- length(which(age_pef == i))
}

data_age_pef <- cbind(v_age, count)
ggplot(data.frame(data_age_pef),aes(x=as.factor(v_age), y=count)) + geom_bar(stat="identity") +
  labs(x="Age", y="Count", title="PEF age distribution")



age_itw <- datos_itw$EDAD
v_age <- seq(1:max(age_itw))
count <- vector(mode="integer", length=length(v_age))
for (i in min(v_age):max(v_age)){
  count[i] <- length(which(age_itw == i))
}

data_age_itw <- cbind(v_age, count)
ggplot(data.frame(data_age_itw),aes(x=as.factor(v_age), y=count)) + geom_bar(stat="identity")+
  labs(x="Age", y="Count", title="ITW age distribution")



## AGE with SEX ##

#Plot Nº1
age_sex_pef <- datos_pef[,c(2,3)]
v_age_pef <- rep(seq(1:max(age_pef)),each=2)
count_age_pef <- vector(mode="integer", length=length(v_age_pef))
sex <- rep(c("M","F"), length.out=length(v_age_pef))
j=1

for (i in min(v_age_pef):max(v_age_pef)){
  count_age_pef[j] <- length(which(age_sex_pef$EDAD == i & age_sex_pef$SEXO == "M"))
  count_age_pef[j+1] <- length(which(age_sex_pef$EDAD == i & age_sex_pef$SEXO == "F"))
  j=j+2
}

e <- as.data.frame(cbind(v_age_pef, sex, count_age_pef))
e[] <- lapply(e, as.character)

facet_plot <- ggplot(e, aes(x = as.integer(v_age_pef), y = as.integer(count_age_pef))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "\n Age", y = "Count \n",
       title = "PEF age distribution by gender \n") +
  facet_grid(. ~ as.factor(e$sex)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12),
        strip.background = element_rect(fill="lightgreen", colour="black", size=1),
        strip.text = element_text(face="bold", size=rel(1.2)))

facet_plot


#Plot Nº2
ggplot(e, aes(fill=sex, y=as.integer(count_age_pef), x=as.integer(v_age_pef))) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x= "\n Age", y= "Count \n", title ="PEF age distribution by gender \n")


