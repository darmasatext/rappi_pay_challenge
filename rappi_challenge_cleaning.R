#FIRMALO!!!



#cargamos nuestras librerias
library(data.table) 
library(tidyverse)
library(lubridate)
library(plyr)
library(R.utils)
library(readxl)


#funciones que ocuparemos 
'%!in%' <- function(x,y)!('%in%'(x,y))
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
#quitamos la notacion cientifica 
options(scipen = 999)

#creamos un directorio para nuestros outputs
mkdirs("outputs") 


#cargamos nuestra data con las clases necesarias
data = fread("raw_data\\BIQuiz_022021.csv",colClasses = c('ID' = 'character',
                                                          'UPDATE' = 'POSIXct',
                                                           'CP' ='character'))

# carguemos nuestra data y separemos el dia del mes, en nombre del dia, mes 
# y anio
data = data %>% 
       mutate(., "INTEREST_RATE" = INTEREST_RATE/100,
                 "fecha_measure" = as.Date(data$UPDATE),
                 "hora" = lubridate::hour(UPDATE),
                 "num_dia" = lubridate::day(UPDATE), 
                 "num_dia_sem" = lubridate::wday(UPDATE, week_start = 1),
                 "nom_dia"= gsub("[[:punct:]]","",lubridate::wday(UPDATE, label = T)),
                 "mes_nombre" = lubridate::month(UPDATE, label = T),
                 "mes" = lubridate::month(UPDATE),
                 "trimestre" = lubridate::quarter(UPDATE),
                 "anio" = lubridate::year(UPDATE)) 
#veamos cuantos usuarios unicos tenemos en nuestro archivo 
uniqueN(data$ID) ## 3341

# sacamos la fecha min y maxima que tenemos 
stDte = min(data$fecha_measure)
edDte =  max(data$fecha_measure)

#notemos que traemos valores mayores al dia de hoy; dejemos fuera del analisis 
#estos datos para evitar cualquier sesgo en el futuro 

data = data %>% 
       filter(., fecha_measure <= today())

#veamos si despues de este filtro tuvimos una cantidad significativa de perdida de 
#usuarios 
uniqueN(data$ID) ## 3335 

#tuvimos una reduccion de 6 usuarios; ie un %.17

#notemos que la distribuccion de los estatus esta un poco dispareja, tenemos
#que esta mas cargada hacia los estatus vacios 
tbl = table(data$STATUS, useNA = "always")
tbl = cbind(tbl,round(prop.table(tbl)*100,2))
colnames(tbl) = c("conteo","Porcentaje")
tbl

#Recordemos que en hoja se nos menciona que el proceso empieza cuando un cliente 
#responde una comunicacion; viendo la tabla anterior vemos que todo los que respondieron 
#a la comunicacion (2,292) son los mismos que entraron al modelo de riesgo (2,292) , y 
# de estos 1,501 fueron aprobados y 791 fueron rechazados (notemos que se siguen sumando 2,292 )
# de los cuales 1,043 ya se les fue enviado tu tarjeta. 
#la incognita aun es que esta pasando con nuestro estatus vacio


# veamos como se ve la distribuccion de nuestro estatus de vacio a lo largo de
#los dias 
status_vacio = data %>%  
               filter(., STATUS == "") %>% 
               group_by(UPDATE) %>% 
               dplyr::summarise("conteo" = dplyr::n()) %>% 
               #ungroup() %>% 
               arrange(desc(conteo)) %>% 
               head(status_vacio,n = 5)

status_vacio

#notemos que existe una gran cantidad de registros vacios en el primer dia de 
#nuestra informacion, sin perdida de generalidad (spg) y basandonos en el archivo 
#compartido supongamos que el dia 11de noviembre haya sido el lanzamiento de la 
#tarjeta; existe la posibilidad de que se haya tenido un error en sistemas para
#verificar esto veamos como los registros que vienen vacios en la columna de
#estatus se compartan en las otras columnas

nas_dias = as.data.frame(data) %>%
           filter(., STATUS == "") %>%
           group_by(UPDATE) %>%
           dplyr::summarise("nas_interest_rate" = sum(is.na(INTEREST_RATE)),
                     "nas_amount" = sum(is.na(AMOUNT)),
                     "nas_cat" = sum(is.na(CAT)),
                     "nas_txn" = sum(is.na(TXN)),
                     "nas_cp" = sum(is.na(CP)),
                     "nas_delivery_score" = sum(is.na(DELIVERY_SCORE)),
                     "total_nas" = sum(nas_interest_rate, nas_amount,nas_cat,
                                       nas_txn, nas_cp, nas_delivery_score)) %>%
           arrange(desc(total_nas)) %>% 
           head(status_vacio,n = 5)

sum(nas_dias$nas_txn)

#de esta tabla notemos que la fecha del 11 de noviembre de 2019 es la unica que 
#contiene valores "NA's" en la columna de txn; es decir podemos decir que 
#de nuestra base con registros vacios 3,722 registros corresponden a txns 
#(4765-1043) hemos encontrado un problema en el etiquetado de los datos, en los 
#siguientes pasos haremos el re-etiquetado.Por otro lado pareciera que los 1,043 
#registros no tienen sentido pero si recordamos nuestro universo y le restamos 
#el numero de personas que respondieron obtenemos 1,043 (3,335 - 2,292) es decir 
#estas 1043 personas no respondieron a nuestro acercamiento/comunicacion!!! de igual manera 
# hagamos un nuevo etiquetado 
data = data %>% 
         mutate(., STATUS = ifelse( STATUS == "" & is.na(data$TXN),"NO_RESPONSE",
                                    ifelse(STATUS == "" & !is.na(data$TXN),"TXN",STATUS))) 

#Propongamos una breve segmetacion para los clientes a los que 
#ya se les aprobo su tarjeta, esta segmetacion sera sobre el comportamiento de 
# del uso de la tarjeta, para esto chequemos a nivel cliente cuantas txns hacen
# y el monto total 

segmentacion = data %>%  
               filter(., STATUS == "TXN")  %>% 
               group_by(ID) %>% 
               dplyr::summarise(., "txns" = n(), 
                            "monto_total" = sum(TXN,na.rm = T),
                            "segmentacion" = case_when(monto_total <= 800 ~ "4.-Bronce", 
                                                       monto_total <= 6043 & monto_total > 800 ~ "3.-Plata", 
                                                       monto_total <= 15000 & monto_total > 6043 ~ "2.-Oro",
                                                       monto_total > 15000 ~ "1.-Diamante"))

table(segmentacion$segmentacion,segmentacion$txns)
#notemos que dada nuestra segmentacion parece existir una relacion entre el el segmento
#asignado y el numero de txns que realiza.

#guardaremos esta tabla para crear un modelo de datos dentro de nuestra herramienta de BI
fwrite(segmentacion,"outputs\\segmetacion.csv", row.names = F)

#de igual manera guardemos nuestros datos correctamente filtrados y etiquetado
fwrite(data,"outputs\\data_limpia.csv", row.names = F)
#creamos un agrupado a nivel fecha; esto para que esta columna sea con la que podamos hacer 
#calculos atrves del tiempo en nuestra herramienta de BI 
fechas = data %>%  
         filter(.,fecha_measure >= stDte & fecha_measure <= today()) %>% 
         group_by(fecha_measure) %>%  
         dplyr::summarise("num_intereciones" = n(), 
                   "monto_transaccionado" = sum(TXN, na.rm = T), 
                   "monto_lineas_liberadas" = sum(AMOUNT, na.rm = T))

#como queremos crear un rolling over year para nuestro dashbord necesitamos informacion 
#de todos los dias de nuestro primer dia hasta nuestro ultimo dia, al no tener informacion
#de todos los dias seguidos hagamos una simulacion de los dias atraves del tiempo para
#hacerle un left join y de esta manera conseguir fechas consecutivas. 

Dte = as.data.frame(seq(as.Date(stDte), as.Date(edDte), by = "day"))
colnames(Dte) = c("dia")

fechas_final = left_join(Dte,fechas, by = c("dia" = "fecha_measure")) %>%  
               filter(.,dia >= stDte & dia <= today()) %>%  
               mutate(., "num_dia" = lubridate::day(dia), 
                         "num_dia_sem" = lubridate::wday(dia, week_start = 1),
                         "nom_dia"= gsub("[[:punct:]]","",lubridate::wday(dia, label = T)),
                         "mes" = lubridate::month(dia),
                         "mes_nombre" = lubridate::month(dia, label = T),
                         "trimestre" = lubridate::quarter(dia),
                         "anio" = lubridate::year(dia))

#guardemos nuestras fechas para usarla en nuestras visualuzaciones
fwrite(fechas_final,"outputs\\fecha_measure.csv", row.names = F)

#archivo para el equipo de sistemas; la finalidad de este output es que el equipo 
#de IT nos ayude a ver porque tenemos registros con fechas que aun no llegan
fechas_it = fread("raw_data\\BIQuiz_022021.csv",colClasses = c('ID' = 'character',
                                                               'UPDATE' = 'POSIXct')) %>% 
            filter(.,UPDATE > today()) 
fwrite(fechas_final,"outputs\\feedback_it.csv", row.names = F)

#cargamos la informacion de la geolocalizacion de los cps
cps_geo = readxl::read_xlsx("external_data\\CP.xlsx") %>% 
                 mutate(., "CP" = as.character(right(Estado_CP,5)))

#filtramos el universo de aprobados
aprobados = data %>%  
     filter(., STATUS == "APPROVED") %>% 
     select(ID,AMOUNT) 
#filtramos el universo de enviados
enviados = data %>% 
           filter(., STATUS == "DELIVERED") %>% 
           select(ID,CP,DELIVERY_SCORE) 

cp = left_join(enviados,aprobados, by = "ID") %>% 
     group_by(CP) %>%  
     dplyr::summarise("Número_de_clientes" = n(),
                      "Monto_promedio" = mean(AMOUNT, na.rm = T),
                      "Score_promedio" = mean(DELIVERY_SCORE, na.rm = T)) %>% 
     left_join(.,cps_geo, by = "CP" )

fwrite(cp,"outputs\\cps_geo.csv", row.names = F)


