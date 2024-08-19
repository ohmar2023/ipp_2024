rm(list = ls())

library(tidyverse)
library(readxl)
library(reshape2)
library(janitor)
library(dplyr)
library(readxl)
library(xlsx)
library(openxlsx)
library(rio)

#-------------------------------------------------------------------------------
# LECTURA DEL DIRECTORIO 2009-2021: 20230321: 10306119 número de casos en el 
# directorio
#-------------------------------------------------------------------------------
directorio <- readRDS("insumos/01_directorio2022/reem_20240110.rds") %>% 
  filter(anio == 2022)

#-------------------------------------------------------------------------------
# Codigos productos y actividad princiapal canasta
#-------------------------------------------------------------------------------
canasta <- read_excel("insumos/02_listado_act/Muestreo_INPP_julio_2024_v1.xlsx") %>% 
  mutate(codigo_actividad_eco = str_replace(codigo_actividad_eco,"[.]",""))

table(nchar(canasta$codigo_actividad_eco))
#-------------------------------------------------------------------------------
# CREANDO MARCO IPP CON EL DIEE 2022
#-------------------------------------------------------------------------------

marco_canasta_08 <- directorio %>% 
  filter(codigo_actividad_eco %in% canasta$codigo_actividad_eco,
         tamanou_plazas!=1) %>% 
  filter(situacion == 1) %>%
  #filtro las empresas no ubicadas
  filter(is.na(empresas_noubicadas)) %>%
  mutate(
    dom_1 = codigo_seccion,
    dom_2 = paste0(tamanou_plazas, codigo_seccion),
    id_empresa = as.character(id_empresa)) %>% 
  group_by(dom_2) %>% 
  mutate(n_cod_act_eco = n_distinct(codigo_actividad_eco)) %>% 
  ungroup()
# 
# marco_canasta_08 %>% 
#   select(dom_2,n_cod_act_eco) %>% 
#   distinct(dom_2,n_cod_act_eco) %>% View("UNO")
# 
# marco_canasta_08 %>% filter(dom_2=="4A") %>% 
#   select(dom_2,n_cod_act_eco) %>% 
#   distinct(dom_2,n_cod_act_eco) %>% View()


#-------------------------------------------------------------------------------
# EXPORTANDO MARCO CANASTA 2021 
#-------------------------------------------------------------------------------
export(marco_canasta_08,
       "productos/01_marco/marco_canasta_08.rds")

