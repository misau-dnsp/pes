calc_budget_vars <- function(df) {
  df <- df %>% 
    rowwise() %>%
    mutate(calc_custo_total = sum(c(c_across(starts_with("custo_")), calc_custo_contratacao), na.rm = TRUE),
           calc_financiamento_lacuna = calc_custo_total - calc_financiamento) %>% 
    ungroup()
}



# variable vector for kobo label substitution
columns_to_label <- c("responsavel_programa", 
                      "responsavel_pf_ma", 
                      "subactividade_tipo", 
                      "objectivo_especifico", 
                      "objectivo_pess", 
                      "subactividade_local")

# variables to remove due to calculation NaN
columns_to_remove <- c("calc_custo_instrumentos",
                       "calc_custo_estudo",
                       "calc_custo_deslocacao",
                       "calc_custo_total",
                       "calc_financiamento_lacuna")



cores_programa <- c(
  "Nutrição"           = "#007A33",  # health-green
  "PAV"                = "#2780e3",  # primary-blue
  "COESP"              = "#C8102E",  # primary-red
  "CSP"                = "#00ADEF",  # sky-blue
  "CSP-APS"            = "#FFD100",  # sun-yellow
  "DVS"                = "#FFB81C",  # corn-gold
  "DNT"                = "#8B5E3C",  # burnt-brown
  "DPCD"               = "#1C2826",  # midnight-black
  "DSA"                = "#005F73",  # custom: dark teal
  "DSF"                = "#0A9396",  # custom: aqua steel
  "LNHAA"              = "#94D2BD",  # custom: light mint
  "MD"                 = "#F4A261",  # custom: desert orange
  "PNC ITS-HIV/SIDA"   = "#E76F51",  # custom: coral red
  "PNCM"               = "#6A4C93",  # custom: royal purple
  "PNCT"               = "#BC4749",  # custom: warm crimson
  "PNSEAJ"             = "#E9C46A",  # custom: soft gold
  "Saúde Mental"       = "#264653"   # custom: deep slate
)

add_missing_ttd_vars <- function(df) {
  
  # List of required budget variables with "ttd_" prefix
  required_cols <- c(
    "ttd_ajudas_domesticas_oe",
    "ttd_ajudas_domesticas_prosaude",
    "ttd_ajudas_domesticas_outro",
    "ttd_ajudas_internacionais_oe",
    "ttd_ajudas_internacionais_prosaude",
    "ttd_ajudas_internacionais_outro",
    "ttd_outras_despesas_pessoal_oe",
    "ttd_outras_despesas_pessoal_prosaude",
    "ttd_outras_despesas_pessoal_outro",
    "ttd_bens_combustivel_oe",
    "ttd_bens_combustivel_prosaude",
    "ttd_bens_combustivel_outro",
    "ttd_bens_gerais_oe",
    "ttd_bens_gerais_prosaude",
    "ttd_bens_gerais_outro",
    "ttd_servicos_coms_oe",
    "ttd_servicos_coms_prosaude",
    "ttd_servicos_coms_outro",
    "ttd_servicos_passagens_domesticas_oe",
    "ttd_servicos_passagens_domesticas_prosaude",
    "ttd_servicos_passagens_domesticas_outro",
    "ttd_servicos_passagens_internacionais_oe",
    "ttd_servicos_passagens_internacionais_prosaude",
    "ttd_servicos_passagens_internacionais_outro",
    "ttd_servicos_gerais_oe",
    "ttd_servicos_gerais_prosaude",
    "ttd_servicos_gerais_outro"
  )
  
  # Identify which variables are missing from the dataframe
  missing_cols <- setdiff(required_cols, names(df))
  
  # Add missing columns with NA_real_
  df <- df %>%
    bind_cols(
      tibble::tibble(!!!setNames(
        lapply(missing_cols, function(col) NA_real_),
        missing_cols
      ))
    )
  
  return(df)
}
