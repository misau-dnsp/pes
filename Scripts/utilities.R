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



library(dplyr)

gen_tbl_activities <- function(df, breakdown = "subactividade_tipo") {
  # Use tidy evaluation for grouping
  group_var <- sym(breakdown)
  
  df_summary <- df %>%
    group_by(!!group_var) %>%
    summarise(
      n = n(),
      across(
        c(calc_custo_total, financiamento_oe, financiamento_prosaude,
          financiamento_outro_total, calc_financiamento_lacuna),
        ~ sum(as.numeric(.x), na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    )
  
  df_total <- df %>%
    summarise(
      !!group_var := "Total",
      n = n(),
      calc_custo_total = sum(as.numeric(calc_custo_total), na.rm = TRUE),
      financiamento_oe = sum(as.numeric(financiamento_oe), na.rm = TRUE),
      financiamento_prosaude = sum(as.numeric(financiamento_prosaude), na.rm = TRUE),
      financiamento_outro_total = sum(as.numeric(financiamento_outro_total), na.rm = TRUE),
      calc_financiamento_lacuna = sum(as.numeric(calc_financiamento_lacuna), na.rm = TRUE)
    )
  
  bind_rows(df_summary, df_total)
}



gen_tbl_financiador <- function(df, breakdown = financiador_detalhe) {
  
  df %>%
    group_by({{ breakdown }}) %>%
    summarise(
      n = n(),
      financiamento = sum(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(financiamento)) %>% 
    bind_rows(
      df %>%
        summarise(
          {{ breakdown }} := "Total",
          n = n(),
          financiamento = sum(valor, na.rm = TRUE)
        )
    )
}
