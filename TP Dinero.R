library(scales)
library(readxl)
library(tidyverse)
library(lubridate)
library(tidyr)
library(ggplot2)

#Importo el seriese

BM_original=read_excel(path = "C:/Users/Pedro/Documents/UBA/3. Dinero/TP Dinero/series.xls",
                       sheet = "BASE MONETARIA",
                       range = "A3217:O4521",
                       col_names = FALSE)


View(BM_original)

#Le saco las columnas vacias

BM_clean=BM_original %>% 
  select(-...2,-...14)

#Nombro las columnas

colnames(BM_clean)= c("fecha", "Variacion Total", "Compras Div Sector Priv","Compras Div al Tesoro Nac", "Adelantos Transitorios Tesoro","Transferencias de utilidades Tesoro","Resto Tesoro","Pases","LELIQ", "Redescuentos y adelantos", "Intereses", "LEBAC y NOBAC", "Otros")

View(BM_clean)

#Cambio el formato de la columna fecha a "Date"

BM_clean=BM_clean%>%
  mutate(
    fecha = as.Date(fecha)
  )
str(BM_clean)


#Pivoteo el data frame para que queden los datos ordenados

BM_tidy=BM_clean%>%
  pivot_longer(
    
    cols = -fecha,
    
    names_to="variable",
    values_to="valor"
    
  )

view(BM_tidy)

#3-A) Variación de la BM por operaciones de divisas

#Saco el importe mensual de la variacion diaria de Compras de divisas al sector publico
#y de la variacion diaria de Compras de divisas al Tesoro Nacional

Variacion_Mensual_Divisas=BM_tidy %>%
  filter(
    variable %in% c("Compras Div Sector Priv", "Compras Div al Tesoro Nac")  
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )

VariacionDivisas2=Variacion_Mensual_Divisas%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )

  colnames(VariacionDivisas2)=c("mes","Total")


  
View(VariacionDivisas2)


VariacionDivisas2_tidy=VariacionDivisas2%>%
  pivot_longer(
    
    cols = -mes,
    
    names_to="variable",
    values_to="valor"
    
  )

View(VariacionDivisas2_tidy)

View(Variacion_Mensual_Divisas)

tail(Variacion_Mensual_Divisas)

VV=rbind(Variacion_Mensual_Divisas,VariacionDivisas2_tidy)

VV=arrange(VV,mes)

#Grafico 1: 3.a Variacion de la BM de operaciones de divisas

View(VV)

ggplot(VV, aes(fill=variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable))+
  labs(
    title = "Variacion mensual de BM x operaciones de divisas" ,
    x = "Plazo en meses" , 
    y = "Valores"
  )
  

#3-B) Variación de la BM por operaciones con el sector público

Variacion_Mensual_SecPub=BM_tidy %>%
  filter(
    variable %in% c("Adelantos Transitorios Tesoro", "Transferencias de utilidades Tesoro", "Resto Tesoro")  #!= es para sacar el saldo y el fx    != es "no quiero"
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )

View(Variacion_Mensual_SecPub)

#Grafico 2: 3.b Variacion de la BM de operaciones del tesoro.

ggplot(Variacion_Mensual_SecPub, aes(fill=variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable))+
  labs(
    title = "Variacion de BM por operaciones del tesoro",
    x = "Plazo en meses" , 
    y = "Valores"
  )


#3-C) Variación de la BM por operaciones con el sistema financiero

Variacion_Mensual_SecFin=BM_tidy %>%
  filter(
    variable == c("Redescuentos y adelantos")  
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )


View(Variacion_Mensual_SecFin)

#Grafico 3: Variacion mensual de BM por operaciones del sector financiero

ggplot(Variacion_Mensual_SecFin,aes(x=mes, y=valor))+
  geom_col(colour="blue")+
  labs(
    title = "Variacion Mensual de BM x Adelantos y Redescuentos" ,
    x = "Plazo en meses" , 
    y = "Valores"
  ) + 
  geom_smooth(method = "loess")  #Para hacer la curva 



#3-D Oferta de BM ex-ante = suma de variables (a), (b) y (c).

View(Variacion_Mensual_Otros)

Variacion_Mensual_Otros=BM_tidy %>%
  filter(
    variable == c("Otros")  
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )  


Oferta_BM=rbind(Variacion_Mensual_Divisas,Variacion_Mensual_SecFin,Variacion_Mensual_Otros,Variacion_Mensual_SecPub)

View(Oferta_BM)

Oferta_BM=arrange(Oferta_BM,mes)

View(Oferta_BM)


Oferta_BM2=Oferta_BM%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )

colnames(Oferta_BM2)=c("mes","Oferta Total")



View(Oferta_BM2)


Oferta_BM2_tidy=Oferta_BM2%>%
  pivot_longer(
    
    cols = -mes,
    
    names_to="variable",
    values_to="valor"
    
  )

View(Oferta_BM2_tidy)

ggplot(Oferta_BM2_tidy, aes(x=mes,y=valor))+
  geom_col(colour= "blue")+
  labs(
    title = "Oferta de BM ex-ante" ,
    x = "Plazo de meses" ,
    y = "Montos"
  )


#3-E OMA y Ventanilla de liquidez


Variacion_Mensual_Operaciones_Politica_Discriminado=BM_tidy %>%
  filter(
    variable %in% c("Pases", "LELIQ", "Intereses","LEBAC y NOBAC")  #!= es para sacar el saldo y el fx    != es "no quiero"
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )

View(Variacion_Mensual_Operaciones_Politica_Discriminado)

Variacion_Mensual_OMA=BM_tidy %>%
  filter(
    variable %in% c("LELIQ", "LEBAC y NOBAC")  #!= es para sacar el saldo y el fx    != es "no quiero"
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )

Variacion_Mensual_Ventanilla=BM_tidy %>%
  filter(
    variable %in% c("Pases", "Intereses")  #!= es para sacar el saldo y el fx    != es "no quiero"
  )%>%
  mutate(
    mes=floor_date(fecha, "month")
  )%>%
  group_by(mes,variable)%>%
  summarise(
    valor=sum(valor)
  )

View(Variacion_Mensual_OMA)
View(Variacion_Mensual_Ventanilla)

Variacion_Mensual_OMA=Variacion_Mensual_OMA%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )

View(Variacion_Mensual_OMA)
colnames(Variacion_Mensual_OMA)=c("mes","OMA")

Variacion_Mensual_Ventanilla=Variacion_Mensual_Ventanilla%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )


colnames(Variacion_Mensual_Ventanilla)=c("mes","Ventanilla")
View(Variacion_Mensual_Ventanilla)


OMA_tidy=Variacion_Mensual_OMA%>%
  pivot_longer(
    
    cols = -mes,
    
    names_to="variable",
    values_to="valor"
    
  )

Ventanilla_tidy=Variacion_Mensual_Ventanilla%>%
  pivot_longer(
    
    cols = -mes,
    
    names_to="variable",
    values_to="valor"
    
  )


Variacion_Mensual_OMAyVentanilla=rbind(OMA_tidy,Ventanilla_tidy)


Variacion_Mensual_OMAyVentanilla=arrange(Variacion_Mensual_OMAyVentanilla,mes)
View(Variacion_Mensual_OMAyVentanilla)

Variacion_Mensual_Total_PM=Variacion_Mensual_OMAyVentanilla%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )
colnames(Variacion_Mensual_Total_PM)=c("mes","Total_PM")

View(Variacion_Mensual_Total_PM)



PM_tidy=Variacion_Mensual_Total_PM%>%
  pivot_longer(
    
    cols = -mes,
    
    names_to="variable",
    values_to="valor"
    
  )


View(PM_tidy)

View(Variacion_Mensual_OMAyVentanilla)  #A este le tengo que meter el total y cambiarle los nombres de las columnas

Variacion_Mensual_OMA_Ventanilla_Total2 = rbind(OMA_tidy, Ventanilla_tidy, PM_tidy)


ggplot(Variacion_Mensual_OMA_Ventanilla_Total2, aes(fill=variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable)) +
  labs(
    title = "Variacion Mensual de BM x OMA y Ventanilla" ,
    x = "Plazo en meses" ,
    y = "Montos"
  )


View(Variacion_Mensual_Ventanilla)

#Grafico de OMA por separado
ggplot(OMA_tidy, aes(fill = variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable)) +
  labs(
    title = "Variacion Mensual de BM x OMA" ,
    x = "Plazo en meses" ,
    y = "Montos"
  )

#Grafico de Ventanilla por separado
ggplot(Ventanilla_tidy, aes(fill= variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable)) +
  labs(
    title = "Variacion Mensual de BM x Ventanilla" ,
    x = "Plazo en meses" ,
    y = "Montos"
  )


#Demanda de BM = variación de la BM total

Variacion_BM_Total=rbind(PM_tidy,Oferta_BM2_tidy)%>%
  group_by(mes)%>%
  summarise(
    valor=sum(valor)
  )
  colnames(Variacion_BM_Total)=c("mes","Demanda Total")


View(Variacion_BM_Total)

Demanda_Total=Variacion_BM_Total%>%
pivot_longer(
  cols = -mes,
  
  names_to="variable",
  values_to="valor"
)

#Grafico 6: Variacion mensual de la BM por politica monetaria

View(Demanda_Total)

ggplot(Demanda_Total, aes(fill=variable,y=valor, x=mes))+
  geom_bar(position = "dodge", stat = "identity", aes(color=variable))+
  labs(
    title = "Variacion Total de la BM" ,
    x = "Plazo en meses" ,
    y = "Valores"
  )



#Oferta ex-ante y demanda real


#Importo los datos del Excel para el IPC

IPC=read_excel(path = "C:/Users/Pedro/Documents/UBA/3. Dinero/TP Dinero/SerieIPC.xls",
               sheet = "Variación mensual IPC Nacional",
               range = "A36:BA61",
               col_names = T)%>%
  setNames(., c('fecha', format(as.Date(as.numeric(names(.)[-1]),
                                        origin = '1899-12-30'), '%Y/%m/%d')))

View(IPC)


IPC_clean=na.omit(IPC)
IPC_clean
View(IPC_clean)



IPC_tidy=IPC_clean%>%
  pivot_longer(
    
    cols = -fecha,
    
    names_to="variable",
    values_to="valor"
    
  )

View(IPC_tidy)



IPC_tidy=IPC_tidy[,c(2,1,3)]
colnames(IPC_tidy)=c("fecha","variable","valor")

View(IPC_tidy)


IPC_tidy=IPC_tidy%>%
  mutate(
    fecha=as.Date(fecha)
  )

#Oferta real ex ante

View(Oferta_BM2_tidy)

#Tengo que borrar algunos datos para que me coincidan los dataframes

Oferta_BM3_tidy=Oferta_BM2_tidy[-c(1:12,65),]
colnames(Oferta_BM3_tidy)=c("fecha","variable","valor")


Oferta_Monetaria_Real=rbind(Oferta_BM3_tidy,IPC_tidy)
Oferta_Monetaria_Real=arrange(Oferta_Monetaria_Real,fecha)

View(Oferta_Monetaria_Real)

Oferta_Monetaria_Real_Total=Oferta_Monetaria_Real%>%
  group_by(fecha)%>%
  summarise(
    valor=valor[variable=="Oferta Total"]/valor[variable=="Nivel general"]
  )

View(Oferta_Monetaria_Real_Total)



AA=Oferta_BM3_tidy%>%
  select(-variable)
colnames(AA)=c("fecha","valor")

Oferta_Monetaria_Real2=rbind(Oferta_Monetaria_Real_Total,AA[12,])
Oferta_Monetaria_Real2=arrange(Oferta_Monetaria_Real_Total,fecha)

View(Oferta_Monetaria_Real2)

View(Oferta_Monetaria_Real)

colnames(Oferta_Monetaria_Real2)=c("fecha","Oferta Real")
Oferta_Monetaria_Real3=Oferta_Monetaria_Real2%>%
  pivot_longer(
    
    cols = -fecha,
    
    names_to="variable",
    values_to="valor"
    
  )

View(Oferta_Monetaria_Real3)


#Grafico 7: Grafico de todos las variables del IPC - No sirve porque son muy chicas todas respecto la del total.
#Me hubiese gustado poder sacar los datos de "total" sobre la serie para ver si hay algunos conceptos mas representativos que otros.

View(Oferta_Monetaria_Real)

ggplot(Oferta_Monetaria_Real, aes(fill=variable, y = valor, x = fecha))+
  geom_bar(position = "dodge" , stat = "identity", aes(color = variable))


#Demanda Real

View(Demanda_Total)
Demanda_Total2=Demanda_Total[-c(1:12,65),]
colnames(Demanda_Total2)=c("fecha","variable","valor")



Demanda_Real=rbind(Demanda_Total2,IPC_tidy)
Demanda_Real=arrange(Demanda_Real,fecha)

View(Demanda_Real)

Demanda_Real2=Demanda_Real%>%
  group_by(fecha)%>%
  summarise(
    valor=valor[variable=="Demanda Total"]/valor[variable=="Nivel general"]
  )

View(Demanda_Real2)

Demanda_Total2


DD=Demanda_Total2%>%
  select(-variable)
colnames(DD)=c("fecha","valor")


#Tengo que volver a ver los datos
View(DD)
Demanda_Real3=rbind(Demanda_Real2,DD[12,])


Demanda_Real3=arrange(Demanda_Real2,fecha)

colnames(Demanda_Real3)=c("fecha","Demanda Real")
Demanda_Real4=Demanda_Real3%>%
  pivot_longer(
    
    cols = -fecha,
    
    names_to="variable",
    values_to="valor"
    
  )


#Estos son los dos con los que tengo que trabajar
View(Demanda_Real4)
View(Oferta_Monetaria_Real3)

Oferta_Demanda=rbind(Oferta_Monetaria_Real3,Demanda_Real4)
Oferta_Demanda=arrange(Oferta_Demanda,fecha)

View(Oferta_Demanda)


ggplot(Oferta_Demanda, aes(fill=variable,y=valor, x=fecha))+
  geom_bar(position = "dodge", stat="identity")+
  labs(
    title = "Variacion Demanda y Oferta Real" ,
    x = "Plazo en meses" ,
    y = "Valores"
  )

