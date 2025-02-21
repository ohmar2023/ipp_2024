
rm(list = ls())

source("rutinas/02_cobertura/999_librerias.R")

#--------------------------------------------------------------------------
# LECTURA DE BASES DE COBERTURA -------------------------------------------
#--------------------------------------------------------------------------

bdd_cober <- read_excel("intermedios/01_base_cobertura/bdd_cober.xlsx")

#bdd_cober <- bdd_cober %>% filter(grepl(dom_2,pattern = "5"))

#--------------------------------------------------------------------------
# Exploratorio Empresas
#--------------------------------------------------------------------------

# todas las empresas de la muestra
emp_todas <-bdd_cober %>% 
  group_by(dom_2) %>% 
  summarise(n_muestra =n())

# empresas visitadas/encontradas
emp_encon <- bdd_cober %>% filter(establecimientos_investigados == "Investigado",
                                  empresa_ubicada_no_ubicada == "Ubicada") %>% 
  group_by(dom_2) %>% 
  summarise(n_ubi = n()) 

# empresas gestionadas verdaderamente no ubicadas con la info de la muestra: 72
emp_no_encon <- bdd_cober %>% filter(establecimientos_investigados == "Investigado",
                                     empresa_ubicada_no_ubicada == "No ubicada") %>% 
  group_by(dom_2) %>% 
  summarise(n_no_ubi = n()) 

# empresas que no fueron gestionadas/visitadas: 29
emp_no_inv <- bdd_cober %>% filter(establecimientos_investigados == "No investigado") %>%
  group_by(dom_2) %>% 
  summarise(n_no_gest = n())

# juntando todas los resutados de emp
f_1 <- function(x,y){x %>% left_join(y, by = "dom_2")}  
resumen_emp <- list(emp_todas,emp_encon,emp_no_encon,emp_no_inv) %>% reduce(f_1) %>% 
  adorn_totals(c("row"))  
  
# tabla 4 informe de cobertura

tabla_4 <- resumen_emp %>% select(dom_2, n_muestra, n_ubi) %>% 
  mutate(cobertura = round(n_ubi/n_muestra,4))

export(tabla_4, "productos/03_cobertura/01_tablas_informe/tabla_4.xlsx")

#--------------------------------------------------------------------------
# Exploratorio prouctos 
#--------------------------------------------------------------------------

bdd_cober_01 <- bdd_cober %>% pivot_longer(cols = c(16:20),
                                           names_to = "productos",
                                           values_to = "productos_cod") %>% 
  filter(total_de_productos != 0) %>% 
  filter(!is.na(productos_cod))
  
# efectividad de empresas por dominio: tabla 6

tabla_6 <- bdd_cober_01 %>% 
  filter(!duplicated(id_empresa)) %>% 
  group_by(dom_2) %>% 
  summarise(n_efect = n()) %>% 
  left_join(emp_todas) %>% 
  adorn_totals(c("row")) %>% 
  mutate(porcent = round(n_efect/n_muestra,4))

export(tabla_6, "productos/03_cobertura/01_tablas_informe/tabla_6.xlsx")

# cantidad de productos por dominio
emp_efect_prod <- bdd_cober_01 %>% 
  group_by(dom_2) %>% 
  summarise(n_efect_prod = n())

# Empresas que tienen mas de 5 productos por dominio

bdd_cober_01 %>% 
  filter(total_de_productos >= 5) %>% 
  group_by(dom_2) %>% 
  summarise(n_efect_prod = n())

# Frecuencia de productos
aux <- bdd_cober_01 %>% 
  filter(!is.na(productos_cod)) %>% 
  group_by(productos_cod) %>% 
  summarise(n_prod = n()) 

tabla_7 <- table(aux$n_prod) %>% 
  data.frame() %>% 
  mutate(porc = round(Freq/sum(Freq), 4),
         porc_acum = cumsum(porc)) %>% 
  select("Tomas" = Var1, 
         "Total de Productos" = Freq,
         "% Productos" = porc,
         "% Acumulado" = porc_acum) 

export(tabla_7, "productos/03_cobertura/01_tablas_informe/tabla_7.xlsx")

 







 