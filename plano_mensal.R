
# DEPENDENCIES ---------------------------------------------------------

library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)
library(ggthemes)
library(robotoolbox)
library(dm)
library(haven)
library(gt)
library(glue)
library(openxlsx)
library(scales)

# KOBO CONNECT & PULL ---------------------------------------------------------

# Set Kobo project
kobo_project <- "DNSP PES"

# Define Kobo connection
Sys.setenv(KOBOTOOLBOX_URL = "https://eu.kobotoolbox.org")
Sys.setenv(KOBOTOOLBOX_TOKEN = Sys.getenv("KOBO_TOKEN"))

kobo_setup(
  url = Sys.getenv("KOBOTOOLBOX_URL"),
  token = Sys.getenv("KOBOTOOLBOX_TOKEN")
)

# fetch kobo assets
assets <- kobo_asset_list()

# pull asset uid
uid <- assets %>%
  filter(name == kobo_project) %>%
  pull(uid) %>%
  first()

# fetch asset list
asset_list <- kobo_asset(uid)

# pull submission df
asset_df <- kobo_submissions(asset_list)

# clean r environment
rm(assets, asset_list, uid)

# TIMESTAMPS & LIMITERS --------------------------------------------------------

# create timestamps and Excel output file name
dt <- now()
dt_pm <- dt %m+% months(1)
dt_pm_ano <- as.character(year(dt_pm)+ 1) # change to below when live!!!
#dt_pm_ano <- year(dt_pm)

# set period limiter
limit_period_floor <- floor_date(dt_pm, "month")
limit_period_ceiling  <- ceiling_date(dt_pm, "month") - days(1)


# REQUIRED VARS & LABELS -------------------------------------------

# variables to be presented as labels
vars_to_label <- c("responsavel_programa", 
                   "responsavel_pf_ma", 
                   "subactividade_tipo", 
                   "formacao_modalidade",
                   "objectivo_especifico", 
                   "objectivo_pess", 
                   "subactividade_local")

# variables to be kept from tbl_main
vars_pm <- c(
  "_index",
  "responsavel_programa",
  "objectivo_especifico",
  "subactividade_titulo",
  "subactividade_tipo",
  "formacao_modalidade"
)


# SUBACTIVITY CODES ----------------------------------------------------

df_codigos <- asset_df$main %>% 
  filter(ano_subactividade == dt_pm_ano) %>% 
  select(`_index`,
         responsavel_programa,
         objectivo_especifico) %>% 
  mutate(across(any_of(vars_to_label), as_factor)) %>% 
  rename(actividade_principal = objectivo_especifico) %>% 
  group_by(responsavel_programa) %>%
  mutate(
    atividade_id = dense_rank(actividade_principal),
    codigo_actividade = paste0("DNSP-", responsavel_programa, "-A", atividade_id)
  ) %>%
  ungroup() %>% 
  group_by(responsavel_programa, actividade_principal) %>%
  mutate(codigo_subactividade = paste0(codigo_actividade, ".", row_number())) %>%
  ungroup() %>% 
  select(`_index`,
         codigo_subactividade)


# SUBACTIVITY META INFO -----------------------------------------------------

# extract month from dt
int_month <- month(dt_pm)

# map portuguese months
months_pt <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
               "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

# set portuguese month value
value_month <- months_pt[int_month]

# create meta info dataframe
df_meta <- asset_df$main %>% 
  mutate(
    across(any_of(vars_to_label), as_factor)
  ) %>%
  select(any_of(vars_pm)) %>% 
  mutate(mes = value_month)


# PLANO MENSAL DATAFRAME --------------------------------------------------

output_plano_mensal <- asset_df$tbl_datas_impl %>% 
  
  # select required columns
  select(
    `_parent_index`,
    subactividade_data_inicio,
    subactividade_data_fim
  ) %>% 
  
  # filter rows where subactivity occurs during month
  filter(
    !is.na(subactividade_data_inicio),
    !is.na(subactividade_data_fim),
    subactividade_data_inicio <= limit_period_ceiling,
    subactividade_data_fim >= limit_period_floor
  ) %>% 
  
  mutate(
    n = row_number(),
    
    # code subactivities as "nova" or "continua"
    subactividade_classificacao = 
      if_else(month(subactividade_data_inicio) > int_month - 1, 
              "Nova (início neste mês no PESOE)",
              "Continua (iniciou em meses anteriores e continua neste mês)"),
    
    # clip subactivities to month window
    overlap_start = pmax(subactividade_data_inicio, limit_period_floor),
    overlap_end   = pmin(subactividade_data_fim, limit_period_ceiling),
    
    # count days inclusively within month; 0 if no overlap; NA if any date is NA
    duracao = case_when(
      is.na(subactividade_data_inicio) | is.na(subactividade_data_fim) ~ NA_integer_,
      overlap_end < overlap_start ~ 0L,
      TRUE ~ as.integer(overlap_end - overlap_start + 1L)
    )
  ) %>%
  
  # join subactivity codes and meta data
  left_join(df_meta,
            by = join_by(`_parent_index` == `_index`)) %>% 
  left_join(df_codigos,
            by = join_by(`_parent_index` == `_index`)) %>% glimpse() %>% 
  
  # define variable order
  select(
    n,
    mes,
    responsavel_programa,
    objectivo_especifico,
    subactividade_titulo,
    codigo_subactividade,
    subactividade_tipo,
    formacao_modalidade, # should have a different variable name
    subactividade_classificacao,
    subactividade_data_inicio,
    subactividade_data_fim,
    duracao
  )


# CREATE EXCEL PRODUCT -----------------------------------------------------------

# Create workbook and tabs
wb <- createWorkbook()
addWorksheet(wb, "Plano Mensal")

writeData(
  wb, 
  sheet = "Plano Mensal", 
  x = output_plano_mensal, 
  startCol = 1, 
  startRow = 1
)

# WRITE PRODUCT ----------------------------------------------------------

dt_filename <- format(dt, "%Y-%m")
filename_output <- paste0("Dataout/dnsp_pm_", dt_filename, "_", ".xlsx") 
saveWorkbook(wb, file = filename_output, overwrite = TRUE)
