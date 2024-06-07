#Procesamiento de variables
#llamada de paquetes
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
#Cargar Base de Datos
lapopchi2023 <- read_dta("ipo/input/data-orig/lapopchile2023.dta", encoding = "UTF-8")
dim(lapopchi2023)
#Selección de variables
proc_data <- lapopchi2023 %>% select(b37, # Confianza en los medios de comunicación 
                                     immig1xa, #Actitud a Servicios Sociales a Venezolanos
                                     immig1xb, #Actitud a Servicios Sociales a Inmigrantes
                                     immig1xc, #Actitud a Servicios Sociales a Españoles
                                     comcon3xa, #Actitud a tener vecinos Venezolanos
                                     comcon3xb, #Actitud a tener vecinos Inmigrantes
                                     comcon3xc) #Actitud a tener vecinos Españoles
sjlabelled::get_label(proc_data)
#Procesamiento de variables
#a) Descriptivo general
frq(proc_data$conf_mcm)
frq(proc_data$immig1xa) #Mayor valor representa mayor desacuerdo, creo que sería mejor cambiarle el orden
frq(proc_data$immig1xb)
frq(proc_data$immig1xc)
frq(proc_data$comcon3xa) #Este grupo de variables también el mayor valor representa mayor desacuerdo
frq(proc_data$comcon3xb)
frq(proc_data$comcon3xc)
#b) Recodificacion 
proc_data$immig1xa <- recode(proc_data$immig1xa, "1=5;2=4;3=3;4=2;5=1")
proc_data$immig1xb <- recode(proc_data$immig1xb, "1=5;2=4;3=3;4=2;5=1")
proc_data$immig1xc <- recode(proc_data$immig1xc, "1=5;2=4;3=3;4=2;5=1")
proc_data$comcon3xa <- recode(proc_data$comcon3xa, "1=3; 2=2; 3=1; 4=0")
proc_data$comcon3xb <- recode(proc_data$comcon3xb, "1=3; 2=2; 3=1; 4=0")
proc_data$comcon3xc <- recode(proc_data$comcon3xc, "1=3; 2=2; 3=1; 4=0")
#Correción de etiquetas de los valores recodificados.
proc_data$immig1xa <- set_labels(proc_data$immig1xa,
                                 labels = c("Muy en desacuerdo" = 1,
                                            "Algo en desacuerdo" = 2,
                                            "Ni de acuerdo ni en desacuerdo" = 3,
                                            "Algo de acuerdo" = 4,
                                            "Muy de acuerdo" = 5))
proc_data$immig1xb <- set_labels(proc_data$immig1xb,
                                 labels = c("Muy en desacuerdo" = 1,
                                            "Algo en desacuerdo" = 2,
                                            "Ni de acuerdo ni en desacuerdo" = 3,
                                            "Algo de acuerdo" = 4,
                                            "Muy de acuerdo" = 5))
proc_data$immig1xc <- set_labels(proc_data$immig1xc,
                                 labels = c("Muy en desacuerdo" = 1,
                                            "Algo en desacuerdo" = 2,
                                            "Ni de acuerdo ni en desacuerdo" = 3,
                                            "Algo de acuerdo" = 4,
                                            "Muy de acuerdo" = 5))
proc_data$comcon3xa <- set_labels(proc_data$comcon3xa,
                                  labels=c("Nada" = 0,
                                            "Poco" = 1,
                                            "Algo" = 2,
                                            "Mucho" = 3))
proc_data$comcon3xb <- set_labels(proc_data$comcon3xb,
                                  labels=c("Nada" = 0,
                                            "Poco" = 1,
                                            "Algo" = 2,
                                            "Mucho" = 3))
proc_data$comcon3xc <- set_labels(proc_data$comcon3xc,
                                  labels=c("Nada" = 0,
                                            "Poco" = 1,
                                            "Algo" = 2,
                                            "Mucho" = 3))
#c) Etiquetado, se le cambiará el nombre a las variables
proc_data <- proc_data %>% rename("s_sociales_ven"=immig1xa,
                                  "s_sociales_inmg"=immig1xb,
                                  "s_sociales_esp"=immig1xc,
                                  "vecino_ven"=comcon3xa,
                                  "vecino_inmg"=comcon3xb,
                                  "vecino_esp"=comcon3xc,
                                  "conf_mcm"=b37)
##Ahora se le cambiará la etiqueta a las variables
proc_data$conf_mcm <- set_label(x = proc_data$conf_mcm, label = "Confianza: Medios de Comunicación")
get_label(proc_data$conf_mcm)

proc_data$s_sociales_ven <- set_label(x= proc_data$s_sociales_ven, label = "Nivel de acuerdo con que el gobierno ofrezca servicios sociales a Venezolanos")
get_label(proc_data$s_sociales_ven)

proc_data$s_sociales_inmg <- set_label(x= proc_data$s_sociales_inmg, label = "Nivel de acuerdo con que el gobierno ofrezca servicios sociales a Imnmigrantes")
get_label(proc_data$s_sociales_inmg)

proc_data$s_sociales_esp <- set_label(x= proc_data$s_sociales_esp, label = "Nivel de acuerdo con que el gobierno ofrezca servicios sociales a Españoles")
get_label(proc_data$s_sociales_esp)

proc_data$vecino_ven <- set_label(x = proc_data$vecino_ven, label = "Nivel de molestia: Tener un vecino venezolano")
get_label(proc_data$vecino_ven)

proc_data$vecino_inmg <- set_label(x = proc_data$vecino_inmg, label = "Nivel de molestia: Tener un vecino inmigrante")
get_label(proc_data$vecino_inmg)

proc_data$vecino_esp <- set_label(x = proc_data$vecino_ven, label = "Nivel de molestia: Tener un vecino español")
get_label(proc_data$vecino_esp)

#Guardando la base de datos con las variables procesadas. 
save(proc_data, file="ipo/input/data-proc/variables_operacionalizadas.RData")
