
rm(list = ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(janitor)
library(rio)

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------
tamanio <- readRDS("productos/02_tamanio_seleccion/tamanio.rds")

#-------------------------------------------------------------------------------
# Marco sin dominios de inclusion forozosa
#-------------------------------------------------------------------------------
marco <- readRDS("productos/01_marco/marco_canasta_08.rds")

marco_sin_inc_for <- marco %>% 
  mutate(inclusion_forzosa = ifelse(tamanou_plazas==5,1,0)) %>% 
  filter(inclusion_forzosa==0)

# ------------------------------------------------------------------------------
# PPT MINIMO 5 -----------------------------------------------------------------
# En caso de que no hayan 5 en el estrato, se toman todos y el resto del tamaño 
# muetsral se lo hace PPT entre los estratos que tengan empresas disponibles. 
# ------------------------------------------------------------------------------

marco_aux <- marco_sin_inc_for %>% 
  #filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_2) %>% 
  summarise(N=n(),H = n_distinct(codigo_actividad_eco)) 

aux <- marco_sin_inc_for %>% 
  #filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_2,codigo_actividad_eco) %>% 
  summarise(Nh=n()) %>% 
  left_join(select(tamanio,dom_2=dominio,n_pro),by="dom_2") %>% 
  left_join(marco_aux,by="dom_2") 

seleccion <- aux %>% mutate(a = if_else(Nh<5,Nh,5),
                            b = Nh-a)
aux_2 <-  seleccion %>%  
  group_by(dom_2) %>% 
  summarise(Nh_b=sum(b), #cantidad restante para elegir muestra
            n_b = unique(n_pro)-sum(a)) %>% #tamaño muestral faltante por llenar
  right_join(seleccion,by="dom_2") %>% 
  mutate(n_b = if_else(n_b<0,0,n_b),
         PPT_b =if_else(Nh_b==0,0,ceiling((n_b)*(b/Nh_b))),
         PPT_b = PPT_b+a,
         Control = Nh - PPT_b)

seleccion <- aux_2                      


# ------------------------------------------------------------------------------
# MUESTRA SIN INCLUSIÓN FOR Y SIN DOMINIOS C -----------------------------------
# ------------------------------------------------------------------------------

muestra <- marco_sin_inc_for %>%
  left_join(select(seleccion,dom_2,codigo_actividad_eco,PPT_b),
            by=c("dom_2","codigo_actividad_eco")) %>% 
  group_by(dom_2,codigo_actividad_eco) %>% 
  sample_n(unique(PPT_b)) %>% 
  select(id_empresa,dom_2,codigo_actividad_eco)

# control de muestra: en diff no deben aparecer negativos

muestra %>% 
  group_by(dom_2) %>% 
  summarise(n_muestra=n()) %>%
  left_join(select(tamanio,n_pro,dom_2 = dominio),by="dom_2") %>% 
  mutate(dif = n_muestra-n_pro) %>% adorn_totals() %>% View()

# ------------------------------------------------------------------------------
# MUESTRA SCON INCLUSIÓN FORSOZA ----------------------------------------------
# ------------------------------------------------------------------------------

muestra_inc_forzosa <- marco %>% 
  mutate(inclusion_forzosa = ifelse(tamanou_plazas==5,1,0)) %>% 
  filter(inclusion_forzosa==1) %>% 
  select(id_empresa,dom_2,codigo_actividad_eco)

muestra <- rbind(muestra,muestra_inc_forzosa)


# Control: La diferencia debe ser mayor o igual a cero ----------------------
marco %>% 
  mutate(seleccionado = ifelse(id_empresa %in% muestra$id_empresa,1,0)) %>% 
  group_by(codigo_actividad_eco) %>% 
  summarise(total = n(),
            seleccionado = sum(seleccionado), 
            dif = total - seleccionado) %>% 
  View()


# exportando --------------------------------------------------------
export(muestra,"productos/02_tamanio_seleccion/muestra.rds")

  


