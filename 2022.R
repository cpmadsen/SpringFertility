df_2022 <- test %>%
  filter(year(treatment_start_date) == 2022)

table(df_2022$treatment)

df_filtered <- df_2022 %>%
  filter(treatment %in% c("ICSI", "IVF"))

table(df_filtered$pregnant_rev)
table(df_filtered$birth_rev)


infert <- df_2022%>%
  count(secondary_infertility_diagnosis)

test %>%
  count(year(treatment_start_date))


df_2022 %>%
distinct(pat_id)
