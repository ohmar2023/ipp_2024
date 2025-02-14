
rm(list = ls())

source("rutinas/02_cobertura/999_librerias.R")

#--------------------------------------------------------------------------
# LECTURA DE BASES DE COBERTURA -------------------------------------------
#--------------------------------------------------------------------------

bdd_cober <- read_excel("intermedios/01_base_cobertura/bdd_cober.xlsx")

#--------------------------------------------------------------------------
# Exploratorio productos: novedades 
# bdd_cober_01 presenta los datos de manera vertical
#--------------------------------------------------------------------------

bdd_cober_01 <- bdd_cober %>% pivot_longer(cols = c(16:20),
                                           names_to = "productos",
                                           values_to = "productos_cod")


#--------------------------------------------------------------------------
# Novedades reune las observaciones en las que se encuentran duplicados
# los cod a 11 digitos. Según DECON esto no es erroneo, se puede dar, por lo que
# desde DINEM esto ya no constiuye una novedad de base. 
#--------------------------------------------------------------------------

novedades <- bdd_cober_01 %>% 
  group_by(id_empresa) %>%
  summarise(bdd_tot = n_distinct(productos_cod, na.rm = TRUE),
            cob_tot = unique(total_de_productos)) %>% 
  mutate(dif = bdd_tot - cob_tot) %>% 
  filter(dif != 0) 

#--------------------------------------------------------------------------
# Controlando caracteres en productos_cod: Todos deben tener 11
#--------------------------------------------------------------------------

table(nchar(bdd_cober_01$productos_cod))

#--------------------------------------------------------------------------
# Controlamos la variable "total_de_productos" verificando que la cantidad
# de prod sea el mismo que el aparece en base.
#--------------------------------------------------------------------------

bdd_cober_01 %>% 
  group_by(id_empresa) %>%
  summarise(bdd_tot = sum(!is.na(productos_cod)),
            cob_tot = unique(total_de_productos)) %>% 
  mutate(dif = bdd_tot - cob_tot) %>% 
  filter(dif != 0) %>% 
  View()


export(novedades, "intermedios/02_novedades/novedades_cobertura.xlsx")
