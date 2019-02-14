library(tidyverse)
library(quickcountmx)

# Casillas completas ------------------------------------------------------

completas <- list.files(
  path = here::here("data/muestras_gto/completas"),
  full.names = TRUE
)

completas_df <- map_df(
  completas, 
  function(x){
    cat("Generando estimación de: ", x, "\n")
    # Lee archivo
    df <- read_csv(x)
    # Estimación del modelo
    estimacion <- mrp_estimation_stan(
      data     = df, 
      party    = pri_pvem:otros,
      stratum  = distrito_loc_17,
      parallel = TRUE,
      n_cores  = 4,
      n_chains = 2
    )
    # Tabla resumen
    tabla <- estimacion$post_summary %>% 
      filter(party != "participacion") %>% 
      mutate(
        n_muestra = gsub(".*_([[:digit:]]+).csv", "\\1", x) %>% 
          as.numeric
      )

    return(tabla)
  }
)

write_csv(
  completas_df,
  path = here::here("results/estimaciones_gto/completas.csv")
)


# Faltantes estrato -------------------------------------------------------

estrato <- list.files(
  path = here::here("data/muestras_gto/faltantes_estrato"),
  full.names = TRUE
)

estrato_df <- map_df(
  estrato, 
  function(x){
    cat("Generando estimación de: ", x, "\n")
    
    df <- read_csv(x)
    
    estimacion <- mrp_estimation_stan(
      data     = df, 
      party    = pri_pvem:otros,
      stratum  = distrito_loc_17,
      parallel = TRUE,
      n_cores  = 4,
      n_chains = 2
    )
    
    tabla <- estimacion$post_summary %>% 
      filter(party != "participacion") %>% 
      mutate(
        n_muestra = gsub(".*_([[:digit:]]+).csv", "\\1", x) %>% 
          as.numeric
      )
    
    return(tabla)
  }
)

write_csv(
  estrato_df,
  path = here::here("results/estimaciones_gto/faltantes_estrato.csv")
)

# Faltantes casillas ------------------------------------------------------

casillas <- list.files(
  path = here::here("data/muestras_gto/faltantes_casilla"),
  full.names = TRUE
)

casillas_df <- map_df(
  casillas, 
  function(x){
    cat("Generando estimación de: ", x, "\n")
    
    df <- read_csv(x)
    
    estimacion <- mrp_estimation_stan(
      data     = df, 
      party    = pri_pvem:otros,
      stratum  = distrito_loc_17,
      parallel = TRUE,
      n_cores  = 4,
      n_chains = 2
    )
    
    tabla <- estimacion$post_summary %>% 
      filter(party != "participacion") %>% 
      mutate(
        n_muestra = gsub(".*_([[:digit:]]+).csv", "\\1", x) %>% 
          as.numeric
      )
    
    return(tabla)
  }
)

write_csv(
  casillas_df,
  path = here::here("results/estimaciones_gto/faltantes_casilla.csv")
)
