
# PENDING ITEMS -----------------------------------------------------------
# See if there is another way to filter all data for pes year
# See if there is a way to avoid errors when certain tables are not present (for example when certain types of data have not been entered into kobo)
# Have to make it so when certain tables are not present that code still works

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

# PES year
ano_pes <- "2026"


# GLOBAL VARIABLES ---------------------------------------------------------



# Kobo connection
Sys.setenv(KOBOTOOLBOX_URL = "https://eu.kobotoolbox.org")
Sys.setenv(KOBOTOOLBOX_TOKEN = Sys.getenv("KOBO_TOKEN"))

kobo_setup(
  url = Sys.getenv("KOBOTOOLBOX_URL"),
  token = Sys.getenv("KOBOTOOLBOX_TOKEN")
)

# Create timestamps and Excel output file name
dt <- now()
dt_formatted_info <- format(dt, "%d/%m/%Y %H:%M")
dt_formatted_filename <- format(dt, "%Y-%m-%d")
filename_output <- paste0("Dataout/dnsp_pes_", dt_formatted_filename, ".xlsx") # not used, consider removing

# Variables required for PESOE
vars_pesoe <- c(
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

# Variables required for Plano de Formação
vars_pdf <- c(
  "_index",
  "responsavel_programa",
  "subactividade_descricao",
  "actividade_principal_descricao",
  "formacao_modalidade",
  "subactividade_beneficiario",
  "subactividade_meta",
  "subactividade_local",
  "financiamento_oe",
  "financiamento_prosaude",
  "financiamento_outro_total",
  "calc_custo_total"
)

# Variables to take kobo label
vars_to_label <- c("responsavel_programa", 
                   "responsavel_pf_ma", 
                   "subactividade_tipo", 
                   "formacao_modalidade",
                   "objectivo_especifico", 
                   "objectivo_pess", 
                   "subactividade_local")

# Financing source map for renaming
mapa_financiador <- c(
  "fonte_banco_mundial" = "Banco Mundial",
  "fonte_cdc" = "CDC/COAG",
  "fonte_fdc" = "FDC",
  "fonte_fnuap" = "FNUAP",
  "fonte_fundo_global" = "Fundo Global",
  "fonte_gavi" = "GAVI",
  "fonte_oim" = "OIM",
  "fonte_oms" = "OMS",
  "fonte_pepfar_1" = "PEPFAR",
  "fonte_prosaude" = "ProSaude",
  "fonte_rti" = "RTI",
  "fonte_unicef" = "UNICEF"
)

# PES activity map for recoding
# mapa_objectivos_esp <- tibble::tibble(
#   actividade_principal = paste0("objectivo_especifico_", paste0("objective_esp_", 1:20)),
#   label = c(
#     "Reduzir a morbi-mortalidade através da expansão e melhoria da Qualidade dos Cuidados e Serviços de Saúde Materna",
#     "Expandir e melhorar a Qualidade dos Cuidados e Serviços de Saúde Sexual e Reprodutiva",
#     "Reduzir a mortalidade em crianças menores de 5 anos",
#     "Contribuir para a redução da morbimortalidade infantil, por desnutrição",
#     "Reduzir a prevalência e mortalidade das doenças preveníveis através de vacinas",
#     "Aumentar o número de adultos e de crianças vivendo com HIV que beneficiam de TARV",
#     "Reduzir a taxa de transmissão do HIV de mãe para filho",
#     "Contribuir para a redução da incidência e da morbimortalidade por tuberculose",
#     "Reduzir para metade a morbi-mortalidade por malária",
#     "Reduzir o peso das DNT e mitigar o seu impacto socioeconómico",
#     "Reduzir a prevalência das doenças tropicais negligenciadas na comunidade",
#     "Melhorar o acesso e a qualidade dos Cuidados e Serviços de Saúde Sexual e Reprodutiva para Adolescentes e Jovens",
#     "Contribuir para o estabelecimento de um ambiente escolar seguro, saudável e favorável à boa aprendizagem e ao desenvolvimento harmonioso do aluno",
#     "Contribuir para a redução do peso da doença através da adopção de estilos de vida saudáveis e da redução dos comportamentos de risco para a saúde",
#     "Prevenir e reduzir a morbilidade causada por perturbações mentais e de comportamento, doenças neuropsiquiátricas e distúrbios psicossociais, incluindo o consumo abusivo de drogas, sobretudo álcool e tabaco",
#     "Contribuir para a redução da ocorrência/frequência de surtos/epidemias com ênfase na erradicação da Pólio e eliminação do Sarampo",
#     "Contribuir para o fortalecimento da capacidade de gestão de emergências de saúde pública, deteção precoce e resposta atempada a eventos de saúde pública",
#     "Contribuir para a redução da incidência e prevalência de doenças relacionadas com determinantes ambientais de saúde",
#     "Contribuir para o bem-estar de saúde dos atletas e praticantes do exercício físico",
#     "Estabelecer um programa de saúde da terceira idade com vista a melhorar a qualidade de vida dos idosos"
#   )
# )

mapa_objectivos_esp <- tibble::tibble(
  actividade_principal = paste0("objectivo_especifico_", paste0("objective_esp_", 1:34)),
  label = c(
    "Formar médicos especialistas, técnicos médios e especializados com qualidade",
    "Fortalecer a formação contínua de profissionais de saúde para uma melhor oferta de cuidados",
    "Melhorar o rácio de profissionais de regime especial através de provisão de mais profissionais de saúde qualificados",
    "Operacionalizar o sub-sistema de saúde comunitária tendo em conta abordagem de género",
    "Expandir e equipar unidades sanitárias com  enfoque de atenção primária",
    "Reforçar o sistema de referência e contra referência",
    "Reforçar as capacidades técnicas e operacionais em Água, Saneamento e Higiene (WASH) nas unidades sanitárias, incluindo a gestão de resíduos biomédicos e o fortalecimento dos laboratórios de controlo de qualidade de água e alimentos",
    "Reforçar as capacidades de alerta precoce/vigilância, diagnóstico, intervenção e coordenação intersectorial para responder a eventos que ameaçam a saúde pública",
    "Consolidar a implementação das acções de prevenção dos principais problemas de saúde pública do país como Malária, TB, HIV, doenças preveníveis e outras não transmissíveis",
    "Expandir o acesso e a retenção em tratamento antirretroviral (TARV) para todas as pessoas vivendo com HIV, com enfoque na aceleração da cobertura entre adultos e crianças",
    "Expandir a cobertura da identificação e tratamento da desnutrição aguda em crianças menores de 2 anos, com implementação do PIN",
    "Expandir e suster programas de promoção de saúde, estilos de vida saudáveis e prevenção de doenças para reduzir a incidência de doenças",
    "Desenvolver acções de mitigação dos principais factores de risco para a saúde,  incluindo trauma",
    "Consolidar parcerias com a medicina tradicional e liderança comunitária",
    "Implementar sistemas de monitoria em tempo real para identificar rapidamente surtos e tendências de doenças para preparar e responder adequadamente",
    "Treinar continuamente a equipe de emergência em estratégias de prevenção, controlo de doenças e resposta rápida",
    "Garantiar de Acesso Universal e Integrado a Serviços Essenciais de Saúde Sexual, Reprodutiva, Materna e Infantil",
    "Fortalecer a qualidade assistencial e humanizada nas US",
    "Adoptar padrões de segurança do paciente e dos profissionais",
    "Definir e arquitectar os sistemas interoperáveis para gestão e logística de medicamentos e produtos médicos (assegurar a interoperabilidade dos sistemas de informação para a gestão, logística de medicamentos e produtos de saúde)",
    "Fortalecer os sistemas de previsão, aquisição e distribuição (via CMAM)",
    "Elaborar normas e regulamentos que facilitem investimentos PPP para impulsionar a produção local",
    "Interoperar o sistema de informação para a gestão, planificação, monitoria e avaliação",
    "Implementar os sistemas de alerta e resposta precoce, sistema integrado de vigilância e monitorização, gestão integrada de vectores",
    "Reforcar a capacidade de diagnóstico para laboratórios de análise toxicológica",
    "Reforçar a planificação integrada entre o Sector da Saúde e Parceiros para alinhamento das intervenções e do financiamento",
    "Reforçar as competências sobre gestão e liderança dos gestores dos Sub -Sistemas Público e Comunitário de Saúde",
    "Implementar tecnologias de telemedicina e/ou saúde digital para alcançar comunidades remotas",
    "Garantir a digitalização dos processos de gestão das US",
    "Introduzir um sistema de contabilidade analítica para a categorização detalhada dos custos por cuidados prestados",
    "Institucionalizar as Contas Satélites de Saúde",
    "Melhorar o rácio de profissionais de regime especial através de provisão de mais profissionais de saúde qualificados",
    "Reforçar a capacidade técnica em gestão de Finanças Públicas no sector da saúde, fortalecendo a programação, execução e a monitoria orçamental",
    "Introduzir o mapeamento dos fundos (externos) e rastreio das despesas (RMET)"
  )
)


# LOAD KOBO ASSETS ----------------------------------------------------------

# fetch kobo assets using account credentials
assets <- kobo_asset_list()

uid <- assets %>%
  #filter(name == "DNSP PES 2026 Final v3") %>%
  filter(name == "DNSP PES 2026 Final v4") %>%
  pull(uid) %>%
  first()

asset_list <- kobo_asset(uid)

asset_df <- kobo_submissions(asset_list)

rm(assets, asset_list, uid)


# DRAW SCHEMA ------------------------------------------------------------

# dm_draw(asset_df)


# CURATE ACTIVITY & SUBACTIVITY CODES ----------------------------------------------------

# Create dataframe for activity and subactivity codes
df_codigos <- asset_df$main %>% 
  filter(ano_subactividade == ano_pes) %>% 
  select(`_index`,
         responsavel_programa,
         starts_with("objectivo_especifico_objective")) %>% 
  mutate(across(any_of(vars_to_label), as_factor)) %>% 
  pivot_longer(cols = !c(`_index`,
                         responsavel_programa),
               names_to = "actividade_principal",
               values_to = "value") %>% 
  mutate(actividade_principal = str_replace_all(actividade_principal, "actividade_principal", "")) %>% 
  filter(value == 1) %>% 
  group_by(`_index`) %>%
  slice_head(n = 1) %>%
  ungroup() %>% 
  left_join(mapa_objectivos_esp, by = "actividade_principal") %>% 
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

vec_subactividades <- df_codigos %>% 
  distinct(`_index`) %>% 
  pull()

rm(mapa_objectivos_esp)


# CURATE GEOTARGETS -----------------------------------------------------

df_metas <- asset_df$tbl_geografia_impl %>% 
  # Filter table for parameterized pes years
  filter(`_parent_index` %in% vec_subactividades) %>% 
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
      function(...) {
        vals <- c(...)
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
      }
    )
  ) %>% 
  select(`_parent_index`,
         localizacao)


# CURATE IMPL. CALENDAR/DURATION ------------------------------------------

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

# Create sequenced calendar vector
month_week_levels <- full_year$month_week

# Identify calendar weeks where subactivity occurs
df_calendar_long <- asset_df$tbl_datas_impl %>%
  # Filter table for parameterized pes years
  filter(`_parent_index` %in% vec_subactividades) %>% 
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

# Extract unique parent indexes from kobo data
parent_ids <- asset_df$tbl_datas_impl %>%
  filter(`_parent_index` %in% vec_subactividades) %>% 
  distinct(`_parent_index`) %>%
  pull()

# Ensure calendar includes all combinations of _parent_index and month_week
full_grid <- tidyr::crossing(
  `_parent_index` = parent_ids,
  month_week = factor(month_week_levels, levels = month_week_levels)
)

# Finalize implementation calendar
df_calendario <- full_grid %>%
  left_join(df_calendar_long, by = c("_parent_index", "month_week")) %>%
  mutate(value = replace_na(value, "")) %>%
  pivot_wider(
    names_from = month_week,
    values_from = value
  ) %>%
  arrange(`_parent_index`)

# Create subactivity duration used in PdF
df_calendar_duracao <- asset_df$tbl_datas_impl %>%
  filter(`_parent_index` %in% vec_subactividades) %>% 
  mutate(duracao_dias = interval(ymd(subactividade_data_inicio), ymd(subactividade_data_fim)) %/% days(1)) %>% 
  select(`_parent_index`, duracao_dias) %>% 
  group_by(`_parent_index`) %>% 
  summarize(duracao_dias = sum(duracao_dias, na.rm = TRUE), .groups = "drop")

rm(full_year, month_week_levels, df_calendar_long, full_grid, parent_ids)


# CURATE FINANCIADOR ---------------------------------------------

df_financiador_outro <- asset_df$tbl_financiamento_outro %>% 
  filter(`_parent_index` %in% vec_subactividades) %>% 
  mutate(
    financiador = pmap_chr(
      list(financiamento_outro_especificacao, financiamento_outro_especificacao_),
      function(codes_str, outros_val) {
        codes <- str_split(codes_str, "\\s+")[[1]]
        
        # Map known codes
        mapped <- mapa_financiador[codes[codes %in% names(mapa_financiador)]]
        
        # Add 'outros' text if present
        if ("fonte_outros" %in% codes) {
          mapped <- c(mapped, outros_val)
        }
        
        # Collapse to comma-separated string
        paste(mapped, collapse = ", ")
      }
    )
  ) %>%
  group_by(`_parent_index`) %>%
  summarise(
    financiador = paste(unique(financiador), collapse = ", "),
    .groups = "drop"
  )

df_financiador <- asset_df$main %>%
  filter(`_index` %in% vec_subactividades) %>% 
  # Subset variables and convert values to labels
  mutate(
    across(any_of(vars_to_label), as_factor),
    n = row_number(),
    financiamento_oe = if_else(financiamento_oe == "0", NA_character_, "OE"),
    financiamento_prosaude = if_else(financiamento_prosaude == "0", NA_character_, "ProSaude")
  ) %>% 
  left_join(df_financiador_outro, by = join_by(`_index` == `_parent_index`)) %>% 
  mutate(
    financiador = pmap_chr(
      list(financiador, financiamento_oe, financiamento_prosaude),
      function(f, oe, prosaude) {
        parts <- c(f, oe, prosaude)
        parts <- parts[!is.na(parts) & parts != ""]
        if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
      }
    ),
    n_financiador = str_count(financiador, ",") + 1
  ) %>% 
  select(
    `_index`,
    financiador,
    n_financiador
  )

rm(df_financiador_outro)

# FINALIZE PESOE --------------------------------------------------------------

df_pes <- asset_df$main %>%
  filter(`_index` %in% vec_subactividades) %>% 
  # Subset variables and convert values to labels
  select(any_of(vars_pesoe)) %>%
  mutate(
    across(any_of(vars_to_label), as_factor)
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
  )

output_pesoe <- df_pes %>% 
  left_join(df_calendario, by = join_by(`_index` == `_parent_index`)) %>% 
  select(-c(`_index`, subactividade_local)) %>% 
  # Format numbers
  mutate(
    across(
      c(financiamento_total, financiamento_oe, financiamento_prosaude,
        financiamento_outro_total, financiamento_lacuna),
      ~ number(as.numeric(.x), big.mark = ",", decimal.mark = ".", accuracy = 0.01)
    ),
    subactividade_meta = as.numeric(subactividade_meta)
  )



# FINALIZE PDF -------------------------------------------------------------

output_pdf <- asset_df$main %>%
  filter(`_index` %in% vec_subactividades) %>% 
  filter(subactividade_tipo == "formacao_capacitacao") %>% 
  # Subset variables and convert values to labels
  select(any_of(vars_pdf)) %>%
  arrange(responsavel_programa) %>% 
  mutate(
    across(any_of(vars_to_label), as_factor),
    n = row_number()
  ) %>% 
  left_join(df_metas, by = join_by(`_index` == `_parent_index`)) %>% 
  left_join(df_calendar_duracao, by = join_by(`_index` == `_parent_index`)) %>% 
  left_join(df_calendario, by = join_by(`_index` == `_parent_index`)) %>% 
  left_join(df_financiador, by = join_by(`_index` == `_index`)) %>% 
  select(
    n,
    subactividade_descricao,
    actividade_principal_descricao,
    formacao_modalidade,
    responsavel_programa,
    subactividade_beneficiario,
    duracao_dias,
    subactividade_meta,
    localizacao,
    matches("_[0-9]{2}$"),
    calc_custo_total,
    financiador
  )


# FINALIZE TTD -----------------------------------------------------------

df_ttd <- asset_df$main %>% 
  filter(`_index` %in% vec_subactividades) %>% 
  select(c(`_index`,
           subactividade_local,
           calc_custo_viagem_ajudas,
           calc_sum_viagem_passagens,
           calc_custo_viagem_terrestre,
           calc_custo_contratacao_recarga,
           calc_custo_contratacao_evento,
           calc_custo_outro)) %>% 
  left_join(df_financiador, by = join_by(`_index` == `_index`)) %>% 
  glimpse()


# functionalize this here...
if ("calc_custo_viagem_ajudas" %in% names(df_ttd)) {
  df_ttd <- df_ttd %>%
    mutate(
      # ajudas domesticas
      ttd_ajudas_domesticas_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      ttd_ajudas_domesticas_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      ttd_ajudas_domesticas_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      # ajudas internacionais
      ttd_ajudas_internacionais_oe = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      ttd_ajudas_internacionais_prosaude = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      ttd_ajudas_internacionais_outro = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_custo_viagem_ajudas,
        TRUE ~ NA_real_
      ),
      
      # outras despesas (MISSING!)
      ttd_outras_despesas_pessoal_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_outras_despesas_pessoal_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_outras_despesas_pessoal_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      # combustivel
      ttd_bens_combustivel_oe = case_when(
        n_financiador == 1 &
          financiador == 'OE' ~ calc_custo_viagem_terrestre,
        TRUE ~ NA_real_
      ),
      
      ttd_bens_combustivel_prosaude = case_when(
        n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_custo_viagem_terrestre,
        TRUE ~ NA_real_
      ),
      
      ttd_bens_combustivel_outro = case_when(
        n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_custo_viagem_terrestre,
        TRUE ~ NA_real_
      ),
      
      # bens gerais (MISSING!)
      ttd_bens_gerais_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_bens_gerais_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_bens_gerais_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      # comunicacao
      ttd_servicos_coms_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ calc_custo_contratacao_recarga,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_coms_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_custo_contratacao_recarga,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_coms_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_custo_contratacao_recarga,
        TRUE ~ NA_real_
      ),
      
      # passagens domesticas
      ttd_servicos_passagens_domesticas_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_passagens_domesticas_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_passagens_domesticas_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      # passagens internacionais
      ttd_servicos_passagens_internacionais_oe = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_passagens_internacionais_prosaude = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_passagens_internacionais_outro = case_when(
        subactividade_local == 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ calc_sum_viagem_passagens,
        TRUE ~ NA_real_
      ),
      
      # servicos gerais (MISSING!)
      ttd_servicos_gerais_oe = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'OE' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_gerais_prosaude = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          financiador == 'ProSaude' ~ NA_real_,
        TRUE ~ NA_real_
      ),
      
      ttd_servicos_gerais_outro = case_when(
        subactividade_local != 'nivel_internacional' &
          n_financiador == 1 &
          !(financiador %in% c('OE', 'ProSaude')) ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
}

df_ttd <- df_ttd %>% 
  select(
    `_index`,
    starts_with("ttd_"),
    n_financiador,
    financiador
  )
  
output_ttd <- df_pes %>% 
  select(`_index`,
         n,
         codigo_actividade,
         actividade_principal_descricao,
         actividade_principal_indicador,
         actividade_principal_meta,
         responsavel_programa,
         codigo_subactividade,
         subactividade_descricao,
         subactividade_meta,
         subactividade_indicador,
         subactividade_local,
         subactividade_beneficiario,
         financiamento_total,
         financiamento_oe,
         financiamento_prosaude,
         financiamento_outro_total,
         financiamento_lacuna
         ) %>% 
  left_join(df_ttd, by = join_by(`_index` == `_index`)) %>%
  relocate(contains('financiador'), .after = n)

# CREATE EXCEL PRODUCT -----------------------------------------------------------

# Create workbook and tabs
wb <- createWorkbook()
addWorksheet(wb, "Info")
addWorksheet(wb, "PESOE")
addWorksheet(wb, "PDF")
addWorksheet(wb, "TTD")

# Write data
writeData(
  wb,
  sheet = "Info",
  x = as.character(glue("Dados Retirados em: {dt_formatted_info}")),
  startCol = 1,
  startRow = 1
)

writeData(
  wb, 
  sheet = "Info", 
  x = "Acções necessárias após a criação do Excel", 
  startCol = 1, 
  startRow = 3
  )

writeData(
  wb, 
  sheet = "Info", 
  x = "Para as subactividades com mais de um financiador, distribuir as despesas por fonte na folha TTD (usar coluna “n_financiador” para detectar estes casos)", 
  startCol = 1, 
  startRow = 4
  )

writeData(
  wb, 
  sheet = "Info", 
  x = "Após a elaboração da proposta final de Excel, distribuir para revisão e correcções", 
  startCol = 1, 
  startRow = 5
  )

writeData(
  wb, 
  sheet = "PDF", 
  x = output_pdf, 
  startCol = 1, 
  startRow = 1
  )

writeData(
  wb, 
  sheet = "TTD", 
  x = output_ttd, 
  startCol = 1, 
  startRow = 1
  )

writeData(
  wb, 
  sheet = "PESOE", 
  x = output_pesoe, 
  startCol = 1, 
  startRow = 1
  )

# customize column widths
setColWidths(
  wb, sheet = "Info",
  cols = 1:2,
  widths = "auto"
)

setColWidths(
  wb, sheet = "PESOE",
  cols = 1:ncol(output_pesoe),
  widths = "auto"
)

setColWidths(
  wb, sheet = "PDF",
  cols = 1:ncol(output_pdf),
  widths = "auto"
)

# Create styles
style_background_white <- createStyle(fgFill = "#FFFFFF")

style_text_bold_red <- createStyle(
  textDecoration = "bold",
  fontColour = "#FF0000"
)

style_text_bold <- createStyle(
  textDecoration = "bold"
)

style_text_wrap <- createStyle(
  wrapText = TRUE
)

style_text_centered <- createStyle(
  halign = "center",
  valign = "center"
)

style_calendar <- createStyle(
  fgFill = "#92D050",
  halign = "center",
  valign = "center",
  textDecoration = NULL
)

# apply styles
addStyle(
  wb,
  sheet = "Info",
  style = style_background_white,
  rows = 1:1000,
  cols = 1:50,
  gridExpand = TRUE,
  stack = TRUE
)

addStyle(
  wb,
  sheet = "Info",
  style = style_text_bold_red,
  cols = 1,
  rows = 1,
  gridExpand = TRUE
)

addStyle(
  wb,
  sheet = "Info",
  style = style_text_bold,
  cols = 1,
  rows = 3,
  gridExpand = TRUE
)

addStyle(
  wb, 
  sheet = "PESOE", 
  style = style_text_centered,
  rows = 1:(nrow(output_pesoe) + 1),  # +1 to include header
  cols = 1,                           # Column 2 = "n"
  gridExpand = TRUE
)

addStyle(
  wb, 
  sheet = "PDF", 
  style = style_text_centered,
  rows = 1:(nrow(output_pdf) + 1),  # +1 to include header
  cols = 1,                           # Column 2 = "n"
  gridExpand = TRUE
)

addStyle(
  wb, 
  sheet = "PESOE", 
  style = style_text_wrap,
  rows = 1:(nrow(output_pesoe) + 1),  # Includes header row
  cols = c(2, 4),
  gridExpand = TRUE
)

apply_calendar_style <- function(wb, sheet, data) {
  data_no_label <- data[, -1]
  for (col_index in seq_along(data_no_label)) {
    rows <- which(data_no_label[[col_index]] == "x") + 1
    if (length(rows) > 0) {
      addStyle(
        wb, sheet = sheet, style = style_calendar,
        rows = rows, cols = col_index + 1,
        gridExpand = FALSE, stack = TRUE
      )
    }
  }
}

apply_calendar_style(
  wb, 
  "PESOE", 
  output_pesoe
)

apply_calendar_style(
  wb, 
  "PDF", 
  output_pdf
)


# WRITE PRODUCT ----------------------------------------------------------

saveWorkbook(wb, file = filename_output, overwrite = TRUE)

