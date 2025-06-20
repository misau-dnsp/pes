
# ATTACH DEPENDENCIES ---------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(robotoolbox)
library(glamr)
library(dm)
library(haven)
library(gt)
library(glue)


# DEFINE GLOBAL VARIABLES ---------------------------------------------------------

acct_kobo <- "kobo-jlara"
acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)

main_vars <- c(
  "_index",
  "responsavel_programa",
  "objectivo_pess", 
  "actividade_principal_descricao",
  "actividade_principal_indicador",
  "actividade_principal_meta",
  "subactividade_tipo",
  "subactividade_tipo_outro_espec",
  "subactividade_descricao",
  "subactividade_beneficiario",
  "subactividade_indicador",
  "subactividade_meta",
  "subactividade_local",
  "subactividade_local_inter_deta",
  "subactividade_local_reg_detalh",
  "subactividade_local_prov_detal",
  "financiamento_oe",
  "financiamento_prosaude",
  "financiamento_outro_total",
  "calc_financiamento_lacuna"
)

columns_to_label <- c("responsavel_programa", 
                      "responsavel_pf_ma", 
                      "subactividade_tipo", 
                      "objectivo_especifico", 
                      "objectivo_pess", 
                      "subactividade_local")


tbl_objectivos_esp <- tibble::tibble(
  actividade_principal = paste0("objectivo_especifico_", paste0("objective_esp_", 1:20)),
  label = c(
    "Reduzir a morbi-mortalidade através da expansão e melhoria da Qualidade dos Cuidados e Serviços de Saúde Materna",
    "Expandir e melhorar a Qualidade dos Cuidados e Serviços de Saúde Sexual e Reprodutiva",
    "Reduzir a mortalidade em crianças menores de 5 anos",
    "Contribuir para a redução da morbimortalidade infantil, por desnutrição",
    "Reduzir a prevalência e mortalidade das doenças preveníveis através de vacinas",
    "Aumentar o número de adultos e de crianças vivendo com HIV que beneficiam de TARV",
    "Reduzir a taxa de transmissão do HIV de mãe para filho",
    "Contribuir para a redução da incidência e da morbimortalidade por tuberculose",
    "Reduzir para metade a morbi-mortalidade por malária",
    "Reduzir o peso das DNT e mitigar o seu impacto socioeconómico",
    "Reduzir a prevalência das doenças tropicais negligenciadas na comunidade",
    "Melhorar o acesso e a qualidade dos Cuidados e Serviços de Saúde Sexual e Reprodutiva para Adolescentes e Jovens",
    "Contribuir para o estabelecimento de um ambiente escolar seguro, saudável e favorável à boa aprendizagem e ao desenvolvimento harmonioso do aluno",
    "Contribuir para a redução do peso da doença através da adopção de estilos de vida saudáveis e da redução dos comportamentos de risco para a saúde",
    "Prevenir e reduzir a morbilidade causada por perturbações mentais e de comportamento, doenças neuropsiquiátricas e distúrbios psicossociais, incluindo o consumo abusivo de drogas, sobretudo álcool e tabaco",
    "Contribuir para a redução da ocorrência/frequência de surtos/epidemias com ênfase na erradicação da Pólio e eliminação do Sarampo",
    "Contribuir para o fortalecimento da capacidade de gestão de emergências de saúde pública, deteção precoce e resposta atempada a eventos de saúde pública",
    "Contribuir para a redução da incidência e prevalência de doenças relacionadas com determinantes ambientais de saúde",
    "Contribuir para o bem-estar de saúde dos atletas e praticantes do exercício físico",
    "Estabelecer um programa de saúde da terceira idade com vista a melhorar a qualidade de vida dos idosos"
  )
)


# LOAD KOBO DATA ----------------------------------------------------------

# timestamp for indicating data of data pull
dt <- today()

# fetch kobo assets using account credentials
assets <- kobo_asset_list()

uid <- assets %>%
  filter(name == "DNSP PES 2026 Final") %>%
  pull(uid) %>%
  first()

asset_list <- kobo_asset(uid)

asset_df <- kobo_submissions(asset_list)

rm(assets, asset_list, uid)


# DRAW SCHEMA ------------------------------------------------------------

dm_draw(asset_df)


# MUNGE DF CODES ----------------------------------------------------

# Create dataframe for activity and subactivity codes
df_codigos <- asset_df$main %>% 
  select(`_index`,
         responsavel_programa,
         starts_with("objectivo_especifico_objective")) %>% 
  mutate(across(any_of(columns_to_label), as_factor)) %>% 
  pivot_longer(cols = !c(`_index`,
                         responsavel_programa),
               names_to = "actividade_principal",
               values_to = "value") %>% 
  mutate(actividade_principal = str_replace_all(actividade_principal, "actividade_principal", "")) %>% 
  filter(value == 1) %>% 
  group_by(`_index`) %>%
  slice_head(n = 1) %>%
  ungroup() %>% 
  left_join(tbl_objectivos_esp, by = "actividade_principal") %>% 
  group_by(responsavel_programa) %>%
  mutate(
    atividade_id = dense_rank(actividade_principal),
    codigo_actividade = paste0("DNSP-", responsavel_programa, "-A", atividade_id)
  ) %>%
  ungroup() %>% 
  group_by(responsavel_programa, actividade_principal) %>%
  mutate(codigo_subactividade = paste0(codigo_actividade, ".", row_number())) %>%
  ungroup() %>% 
  select('_index',
         codigo_actividade,
         codigo_subactividade)

rm(tbl_objectivos_esp)


# MUNGE DF GEOTARGET -----------------------------------------------------

df_metas <- asset_df$tbl_geografia_impl %>% 
  # Pivot geographic target table wide
  select(`_parent_index`, geo_nome_geo_label, meta_geo) %>% 
  mutate(
    localizacao = str_c(geo_nome_geo_label, " (", meta_geo, ")")
  ) %>% 
  pivot_wider(
    names_from = geo_nome_geo_label,
    values_from = localizacao,
    values_fn = list(localizacao = ~ paste(.x, collapse = ", "))
  ) %>% 
  select(-meta_geo) %>% 
  # Engineer geographic targets within single cell
  group_by(`_parent_index`) %>%
  summarise(
    across(
      .cols = everything(), 
      .fns = ~ {
        vals <- na.omit(unique(.x))
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
      }
    ),
    .groups = "drop"
  ) %>% 
  mutate(
    localizacao = pmap_chr(
      select(., -`_parent_index`),
      ~ c(...) %>%
        discard(is.na) %>%
        paste(collapse = ", ")
    )
  ) %>% 
  select(`_parent_index`,
         localizacao)


# MUNGE DF CALENDAR ------------------------------------------

# Create tibble of the entire year in case subactivities don't cover entire period
full_year <- tibble(
  dates = seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "day")
) %>%
  mutate(
    month = month(dates, label = TRUE, abbr = TRUE),
    week = isoweek(dates),
    month_week = paste0(month, "_", sprintf("%02d", week))
  ) %>%
  distinct(month_week, .keep_all = TRUE) %>%
  arrange(dates)

# Create sequenced vector based on calendar
month_week_levels <- full_year$month_week

# Identify calendar weeks where subactivity occurs
df_calendar_long <- asset_df$tbl_datas_impl %>%
  select(`_parent_index`, subactividade_data_inicio, subactividade_data_fim) %>%
  rowwise() %>%
  mutate(dates = list(seq(subactividade_data_inicio, subactividade_data_fim, by = "1 day"))) %>%
  unnest(dates) %>%
  mutate(
    month = month(dates, label = TRUE, abbr = TRUE),
    week = isoweek(dates),
    month_week = paste0(month, "_", sprintf("%02d", week))
  ) %>%
  ungroup() %>%
  distinct(`_parent_index`, month_week) %>%
  mutate(value = "x")

# Extract unique parent indexes from your data
parent_ids <- asset_df$tbl_datas_impl %>%
  distinct(`_parent_index`) %>%
  pull()

# Ensure calendar includes all combinations of _parent_index and month_week
full_grid <- tidyr::crossing(
  `_parent_index` = parent_ids,
  month_week = factor(month_week_levels, levels = month_week_levels)
)

# Finalize calendar
df_calendario <- full_grid %>%
  left_join(df_calendar_long, by = c("_parent_index", "month_week")) %>%
  mutate(value = replace_na(value, "")) %>%
  pivot_wider(
    names_from = month_week,
    values_from = value
  ) %>%
  arrange(`_parent_index`)

rm(full_year, month_week_levels, df_calendar_long, full_grid)


# JOIN DF & CLEAN--------------------------------------------------------------

df <- asset_df$main %>%
  # Subset variables and convert values to labels
  select(any_of(main_vars)) %>%
  mutate(
    across(any_of(columns_to_label), as_factor)
  ) %>% 
  # Join codigos and metas
  left_join(df_codigos) %>% 
  left_join(df_metas, by = join_by(`_index` == `_parent_index`)) %>% 
  arrange(codigo_subactividade) %>% 
  mutate(n = row_number()) %>% 
  rowwise() %>% 
  mutate(financiamento_total = sum(c_across(financiamento_oe:financiamento_outro_total))) %>% 
  ungroup() %>% 
  select(
    `_index`,
    n,
    codigo_actividade,
    actividade_principal_descricao,
    actividade_principal_indicador,
    actividade_principal_meta,
    responsavel_programa,
    codigo_subactividade,
    subactividade_descricao,
    subactividade_local,
    subactividade_meta,
    subactividade_indicador,
    localizacao,
    subactividade_beneficiario,
    financiamento_total,
    financiamento_oe,
    financiamento_prosaude,
    financiamento_outro_total,
    financiamento_lacuna = calc_financiamento_lacuna
  ) %>% 
  # Engineer localizacao variable for international and central subactivities
  mutate(
    subactividade_meta = as.character(subactividade_meta),
    localizacao = case_when(
      subactividade_local == "Nivel Internacional" ~ str_c(subactividade_local, " (", subactividade_meta, ")"),
      subactividade_local == "Nivel Central" ~ str_c(subactividade_local, " (", subactividade_meta, ")"),
      TRUE ~ localizacao
    )
  ) %>% 
  left_join(df_calendario, by = join_by(`_index` == `_parent_index`)) %>% 
  select(-c(`_index`, subactividade_local))


