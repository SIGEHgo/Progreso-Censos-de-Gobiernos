#datos = readxl::read_excel("Datos/Copia de Listado de enlaces.xlsx", sheet = 2, skip = 1, col_names = T)
datos = readr::read_delim('https://docs.google.com/spreadsheets/d/e/2PACX-1vSMFur7lSgBuLOWTObLf6ZQtb8A-LSpt37OCv-o_M4DcLzaFPv6odPCWiYflnbK2A/pub?gid=1339770385&single=true&output=tsv',delim = "\t",skip = 1)

datos = datos |> 
  dplyr::rename(CENSO = `Integración de información preliminar`,
                TAREA = ...2,
                Proceso = `Porcentaje estimado`,
                Estatus = Estatus...4) |> 
  dplyr::select(CENSO, TAREA, Proceso, Estatus, `Semana 1`:`Semana 4`) 


datos = datos |> 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "N/A")))

datos = datos |> 
  dplyr::mutate(Observación = paste0("Semana 1: ", `Semana 1`, "; Semana 2:", `Semana 2`, "; Semana 3: ", `Semana 3`, "; Semana 4: ", `Semana 4`),
                Observación = dplyr::if_else(condition = Observación == "Semana 1: NA; Semana 2:NA; Semana 3: NA; Semana 4: NA", true = NA, false = Observación),
                Observación = dplyr::if_else(condition = is.na(Observación), true = Estatus, false = Observación)) |> 
  dplyr::select(-Estatus, -`Semana 1`, -`Semana 2`, -`Semana 3`, -`Semana 4`)
  
datos = datos |> 
  dplyr::mutate(Proceso = gsub(pattern = "%", replacement = "", x = Proceso),
                Proceso = as.numeric(Proceso)*100,
                Proceso = dplyr::if_else(condition = Proceso > 100, true = Proceso/100, false = Proceso),
                Proceso = paste0(Proceso, "%"), 
                Proceso = dplyr::if_else(condition = Proceso == "NA%", true = NA, false = Proceso)) 

datos = datos |> 
  dplyr::mutate(tier1 =  stringr::word(CENSO, 1),
                tier1 = dplyr::if_else(condition = tier1 == "Modulo", true = "Módulo", false = tier1),
                tier1 = dplyr::if_else(condition = tier1 == "Módulo", true = NA, false = CENSO))
 

datos = datos |> 
  tidyr::fill(tier1, .direction = "down") |> 
  tidyr::fill(CENSO, .direction = "down") |>
  dplyr::relocate(tier1, .before = CENSO)


datos = datos |> 
  dplyr::filter(!CENSO %in% c("Integración de información preliminar", 
                              "Revisón primaria y justes de información preliminar", 
                              "Revisión OC y liberación deinformación definitiva.", 
                              "Recuperación de firmas yformalización de cuestionarios.",
                              "Difusión de resultados"))

