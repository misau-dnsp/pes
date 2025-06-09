calc_budget_vars <- function(df) {
  df <- df %>% 
    rowwise() %>%
    mutate(calc_custo_total = sum(c_across(starts_with("custo_")), na.rm = TRUE),
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
                       "calc_custo_contratacao",
                       "calc_custo_deslocacao",
                       "calc_custo_total",
                       "calc_financiamento_lacuna")