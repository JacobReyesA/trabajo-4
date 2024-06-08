
#Procesamiento de variables
#llamada de paquetes
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#selección de variables
proc_data <- elsoc_2021 %>% select(m0_edad, #Edad
                                   r09, #Grado de simpatía por extranjeros que viven en Chile
                                   r11, #Grado de ansiedad por interacción con Extranjeros
                                   r12_03, #Grado de acuerdo: Chile pierde su identidad con la llegada de extranjeros 
                                   r12_04, #Grado de acuerdo: La llegada de extranjeros aumenta el desempleo
                                   r16, #Grado de confianza en extranjeros
                                   r18_01, #Grado de acuerdo: fomentar migración calificada
                                   r18_02, #Acceso igualitario a salud para nacionales y extranjeros
                                   c37_05, #Grado de acuerdo: Restricciones ingreso de inmigrantes
                                   )
#Descriptivos de las variables escogidas.
frq(proc_data$m0_edad) 
frq(proc_data$r09) 
frq(proc_data$r11)
frq(proc_data$r12_03)
frq(proc_data$r12_04)
frq(proc_data$r16)
frq(proc_data$r18_01)
frq(proc_data$r18_02)
frq(proc_data$c37_05) #Al todas estar en un orden ascendente se decide no reordenar las categorías de las variables
##Se transformaran los -999, -888, -777 y -666 en NA
proc_data <- proc_data %>% set_na(., na = c(-999, -888, -777, -666))
###Cambio de nombres de las variables
proc_data <- proc_data %>% rename("edad" = m0_edad,
                                  "simpatia_extranjeros" = r09,
                                  "ansiedad_interaccion_inmg" = r11,
                                  "acuerdo_perdida_de_identidad" = r12_03,
                                  "acuerdo_aumento_desempleo" = r12_04,
                                  "confianza_en_extranjeros" = r16,
                                  "fomentar_migracion_calificada" = r18_01,
                                  "acceso_igualitario_salud" = r18_02,
                                  "acuerdo_restriccion_inmigrantes" = c37_05)
####Por último, se cambian las etiquetas de las variables
proc_data$Edad <- set_label(x = proc_data$Edad, label = "Edad")
proc_data$simpatia_extranjeros <- set_label(x = proc_data$simpatia_extranjeros, label = "Nivel de simpatía por inmigrantes")
proc_data$ansiedad_interaccion_inmg <- set_label(x = proc_data$ansiedad_interaccion_inmg, label = "Nivel de ansiedad por interactuar con inmigrantes")
proc_data$acuerdo_perdida_de_identidad <- set_label(x = proc_data$acuerdo_perdida_de_identidad, label = "Nivel de acuerdo con que la inmigración afecta la identidad nacional")
proc_data$acuerdo_aumento_desempleo <- set_label(x = proc_data$acuerdo_aumento_desempleo, label = "Nivel de acuerdo con que la inmigración genera desempleo")
proc_data$confianza_en_extranjeros <- set_label(x = proc_data$confianza_en_extranjeros, label = "Nivel de confianza en inmigrantes")
proc_data$fomentar_migracion_calificada <- set_label(x = proc_data$fomentar_migracion_calificada, label = "Nivel de acuerdo con fomentar la inmigración calificada")
proc_data$acceso_igualitario_salud <- set_label(x = proc_data$acceso_igualitario_salud, label = "Nivel de acuerdo con que exista un ingreso igualitario a la salud")
proc_data$acuerdo_restriccion_inmigrantes <- set_label(x = proc_data$acuerdo_restriccion_inmigrantes, label = "Nivel de acuerdo con restringir el acceso a inmigrantes")
get_label(proc_data)

#Guardando la base de datos con las variables procesadas. 
save(proc_data, file="ipo/input/data-proc/variables_operacionalizadas.RData")

#visualización de datos
##Cargando los paquetes necesario para la visualizacion de datos.
pacman::p_load(summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos
pacman::p_load(texreg, ggpubr)
###Tabla descriptiva
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
####Omisión de casos perdidos.
proc_data_original <- proc_data #Primero para evitar cualquier error crítico se respalda la base datos.
dim(proc_data)
sum(is.na(proc_data))
proc_data <-na.omit(proc_data) #Se eliminan los casos perdidos, los cuales eran 1376.
####Gráficos univariados.
graph1 <- proc_data %>% ggplot(aes(x = simpatia_extranjeros)) + 
  geom_bar(fill = "red")+
  labs(title = "Nivel de simpatía por inmigrantes",
       x = "Simpatia por inmigrantes",
       y = "Frecuencia") +
  theme_bw()
graph2 <- proc_data %>% ggplot(aes(x=acuerdo_restriccion_inmigrantes)) +
  geom_bar(fill = "red") +
  labs(title = "Nivel de acuerdo con restringir el ingreso de inmigrantes al país",
       x = "Nivel de acuerdo", #Creo que el título del gráfico es suficientemente claro para dejar esta etiqueta así de simple
       y = "Frecuencia") +
  theme_bw()
graph2
#Correlación de las variables.
colSums(is.na(proc_data))
sjPlot::tab_corr(proc_data, 
                 triangle = "lower")
sjPlot::tab_corr(proc_data, 
                 na.deletion = "pairwise", # espeficicamos tratamiento NA
                 triangle = "lower")

#Regresion lineal
pacman::p_load(texreg, ggpubr)
reg2 <- lm(simpatia_extranjeros ~ Edad, data = proc_data_original)
reg3 <- lm(acuerdo_restriccion_inmigrantes ~ Edad, data = proc_data_original)
reg4 <- lm(acuerdo_aumento_desempleo ~ Edad, data = proc_data_original)

knitreg(list(reg2, reg3, reg4),
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        caption = "Actitudes hacia los inmigrantes",
        caption.above = TRUE)
stargazer(reg2, type = "text")
stargazer(reg3, type = "text")
stargazer(reg4, type = "text")
#Gráfico de valores predichos
pacman::p_load(fastdummies, ggeffects)
ggeffects::ggpredict(reg2, terms = c("Edad")) %>%
  ggplot(aes(x=x, y=predicted)) +
  geom_bar(stat="identity", color="grey", fill="red")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.1) +
  labs(title="Simpatía por inmigrantes según edad", x = "", y = "") +
  theme_bw() +
  scale_x_discrete(name = "",
                   labels = c("Muy Poco/Nada", "Poco", "Algo", "Bastante", "Mucho")) +
  scale_y_continuous(limits = c(0,16), 
                     breaks = seq(0,16, by = 1))
frq(proc_data_original$ansiedad_interaccion_inmg)
frq(proc_data$simpatia_extranjeros)
frq(proc_data_original$acuerdo_restriccion_inmigrantes)
proc_data %>% ggplot(aes(x = simpatia_extranjeros)) + 
  geom_bar(fill = "red")+
  labs(title = "Nivel de simpatía por inmigrantes",
       x = "Simpatia por inmigrantes",
       y = "Frecuencia") +
  theme_bw()
