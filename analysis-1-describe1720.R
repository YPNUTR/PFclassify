pacman::p_load(haven, dplyr, tidyr, purrr, labelled, ggplot2, stringr, survey, tibble, openxlsx)

path_base <- "/Users/yongyipan/Library/CloudStorage/Box-Box/"
# path_base <- "C:/Users/ypan05/Box/"

path_dat <- paste0(path_base, "_working_pork/data/")
path_out <- paste0(path_base, "_working_pork/result/")

####################################### STEP 1: Read in demo and intake data -----
raw_demo_1720 <-
  read_xpt(paste0(path_dat, "DEMO_PP.xpt")) %>% 
  # build incoh
  mutate(incoh = if_else(RIDAGEYR >= 2, 1, 0)) %>% 
  # build demo categories
  mutate(
    age_grp = case_when(RIDAGEYR <  2                  ~ 0,
                        RIDAGEYR >= 2  & RIDAGEYR < 20 ~ 1,
                        RIDAGEYR >= 20 & RIDAGEYR < 45 ~ 2,
                        RIDAGEYR >= 45 & RIDAGEYR < 55 ~ 3,
                        RIDAGEYR >= 55 & RIDAGEYR < 65 ~ 4,
                        RIDAGEYR >= 65                 ~ 5),
    sex_grp = RIAGENDR,
    race_grp = case_when(RIDRETH3 %in% c(3)   ~ 1, # NHW
                         RIDRETH3 %in% c(4)   ~ 2, # NHB
                         RIDRETH3 %in% c(1,2) ~ 3, # H
                         RIDRETH3 %in% c(6)   ~ 4, # NHA
                         RIDRETH3 %in% c(7)   ~ 5  # Other
                         ),
    edu_grp = case_when(is.na(DMDEDUC2)      ~ 1, # no edu so age < 20
                        DMDEDUC2 %in% c(1,2) ~ 2, # < high school
                        DMDEDUC2 %in% c(3)   ~ 3, # high school eq
                        DMDEDUC2 %in% c(4)   ~ 4, # some college
                        DMDEDUC2 %in% c(5)   ~ 5, # college +
                        DMDEDUC2 %in% c(7,9) ~ 6  # no response
                        ),
    pir_grp = case_when(INDFMPIR < 1.3                 ~ 1,
                        INDFMPIR >= 1.3 & INDFMPIR < 3 ~ 2,
                        INDFMPIR >= 3   & INDFMPIR < 5 ~ 3,
                        INDFMPIR >= 5                  ~ 4,
                        is.na(INDFMPIR)                ~ 5)
  ) %>% 
  select(SEQN, incoh, SDDSRVYR, SDMVPSU, SDMVSTRA,
         ends_with("_grp")
         )
cbind(lapply(lapply(raw_demo_1720, is.na),sum)) # no missing

raw_intake_1720 <- 
  read_xpt(paste0(path_dat, "intake/DR1IFF_PP.xpt")) %>% 
  rename("DRXIFDCD" = "DR1IFDCD",
         "DRXIGRMS" = "DR1IGRMS") %>% 
  mutate(DAY = 1) %>% 
  select(SEQN, DAY, WTDR2DPP, DRXIFDCD, DRXIGRMS) %>% 
  union(read_xpt(paste0(path_dat, "intake/DR2IFF_PP.xpt")) %>% 
          rename("DRXIFDCD" = "DR2IFDCD",
                 "DRXIGRMS" = "DR2IGRMS") %>% 
          mutate(DAY = 2) %>% 
          select(SEQN, DAY, WTDR2DPP, DRXIFDCD, DRXIGRMS))

# only human milk has missing weight, ok
cbind(lapply(lapply(raw_intake_1720, is.na),sum))

####################################### STEP 2: Merge data -----
df_1720 <- raw_intake_1720 %>% 
  left_join(raw_demo_1720,
            by = "SEQN")

# only human milk has missing weight, ok
cbind(lapply(lapply(df_1720, is.na),sum))
####################################### STEP 3: Install and use PFclassify -----
# install.packages(paste0(path_base, "_working_pork/PFclassify_0.1.0.tar.gz"),
#                  repos = NULL, type = "source")# .rs.restartR()
library(PFclassify)

df_1720_fmt <- ReFormatDF(df_1720,
                          var_cycle = "SDDSRVYR",
                          var_Food_code = "DRXIFDCD",
                          var_weight = "DRXIGRMS",
                          your_cycle = c(66),
                          out_cycle = c("1920"))



df_1720_break <- CalPF(df_1720_fmt,
                       var_id = "SEQN",
                       fdcd_description = TRUE,
                       option_subtotal = c("meat", "poult", "curedmeat"),
                       option_meat = c("beef", "pork", "other"),
                       option_poult = c("chick", "turkey", "other"),
                       option_curedmeat = c("beef", "pork", "chick", "turkey", "other"))

df_1720_use <- df_1720_break %>% 
  mutate(across(starts_with("PF_"), ~ tidyr::replace_na(.x, 0))) %>% 
  # filter(!is.na(Food_description)) %>% 
  group_by(SEQN, DAY) %>% 
  summarise(
    across(starts_with("PF_"), \(x) sum(x, na.rm = TRUE)),
    WTDR2DPP = first(WTDR2DPP),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    cycle = first(cycle),
    incoh = first(incoh),
    across(ends_with("_grp"), first),
    .groups = "drop"
  ) %>% 
  group_by(SEQN) %>% 
  summarise(
    across(starts_with("PF_"), \(x) mean(x, na.rm = TRUE)),
    WTDR2DPP = first(WTDR2DPP),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    incoh = first(incoh),
    cycle = first(cycle),
    across(ends_with("_grp"), first),
    .groups = "drop"
  ) %>% 
  relocate(SEQN, cycle, incoh, WTDR2DPP, SDMVPSU, SDMVSTRA,
           ends_with("grp")) %>% 
  mutate(
    PF_TOTAL_all_sum = PF_MEAT_sum + PF_POULT_sum + PF_CUREDMEAT_sum,
    PF_TOTAL_meat_sum = PF_MEAT_sum + PF_CUREDMEAT_beef_sum + PF_CUREDMEAT_pork_sum,
    PF_TOTAL_poult_sum = PF_POULT_sum + PF_CUREDMEAT_chick_sum  + PF_CUREDMEAT_turkey_sum
  )

cbind(lapply(lapply(df_1720_use, is.na),sum))
####################################### STEP 4: Add PF intake 0/1 for prevalence
df_1720_use <- df_1720_use %>% 
  mutate(
    across(
      starts_with("PF_"),
      ~ as.numeric(.x > 0),
      .names = "{.col}_pos"
    )
  )

cbind(lapply(lapply(df_1720_use, is.na),sum))

####################################### STEP 4: Define survey design and list of vars-----
nhanes_design <- subset(
  svydesign(id = ~SDMVPSU,
            strata = ~SDMVSTRA,
            weights = ~WTDR2DPP,
            nest = TRUE,
            data = df_1720_use),
  incoh == 1
)

PF_var <- colnames(df_1720_use %>% select(ends_with("_sum")))
PF_pos_var <- colnames(df_1720_use %>% select(ends_with("_pos")))
POP_var <- colnames(df_1720_use %>% select(ends_with("_grp")))

####################################### Output 1: Intake Mean by sub-population  -----
for (i in 1:length(PF_var)) {
  for (j in 1:length(POP_var)) {
    if(j == 1){
      assign(paste0("temp_", i),
             svyby(
               as.formula(paste0("~", PF_var[i])),
               as.formula(paste0("~", POP_var[j])),
               design = nhanes_design,
               FUN = svymean,
               na.rm = TRUE
             ) %>% 
               as.data.frame() %>% 
               rename(grp_sub = POP_var[j],
                      mean    = PF_var[i]) %>%
               mutate(grp_var = POP_var[j],
                      !!PF_var[i] := sprintf("%.3f (%.3f, %.3f)", mean, mean-(1.96*se), mean+(1.96*se))
                      #!!PF_var[i] := sprintf("%.3f (%.3f)", mean, se)
                      ) %>% 
               select(grp_var, grp_sub, PF_var[i]) %>% 
               remove_rownames())
    }
    else {
      assign(paste0("temp_", i),
             get(paste0("temp_", i)) %>% 
               union(svyby(
                 as.formula(paste0("~", PF_var[i])),
                 as.formula(paste0("~", POP_var[j])),
                 design = nhanes_design,
                 FUN = svymean,
                 na.rm = TRUE
               ) %>% 
                 as.data.frame() %>% 
                 rename(grp_sub = POP_var[j],
                        mean    = PF_var[i]) %>%
                 mutate(grp_var = POP_var[j],
                        !!PF_var[i] := sprintf("%.3f (%.3f, %.3f)", mean, mean-(1.96*se), mean+(1.96*se))) %>% 
                 select(grp_var, grp_sub, PF_var[i]) %>% 
                 remove_rownames()))
    }
  }
}

temp_list <- mget(paste0("temp_", 1:17))

out_tab_1_mean_intake <- 
  reduce(
    temp_list,
    left_join,
    by = c("grp_var", "grp_sub")
  )

rm(list = ls(pattern = "^temp_"))


for (i in 1:length(PF_var)) {
  for (j in 1:length(POP_var)) {
    if(j == 1){
      assign(paste0("temp_", i),
             summary(svyglm(as.formula(paste0(PF_var[i], "~", POP_var[j])),
                            design = nhanes_design))$coefficients %>% 
               as.data.frame() %>% 
               mutate(varname = rownames(.)) %>% 
               filter(varname == POP_var[j]) %>% 
               mutate(!!PF_var[i] := case_when(`Pr(>|t|)`>=0.01 ~ sprintf("%.2f", `Pr(>|t|)`),
                                         `Pr(>|t|)`< 0.01 ~ "<0.01")) %>% 
               select(varname, PF_var[i]) %>% 
               remove_rownames()
             )
    }
    else {
      assign(paste0("temp_", i),
             get(paste0("temp_", i)) %>% 
               union(
                 summary(svyglm(as.formula(paste0(PF_var[i], "~", POP_var[j])),
                                design = nhanes_design))$coefficients %>% 
                   as.data.frame() %>% 
                   mutate(varname = rownames(.)) %>% 
                   filter(varname == POP_var[j]) %>% 
                   mutate(!!PF_var[i] := case_when(`Pr(>|t|)`>=0.01 ~ sprintf("%.2f", `Pr(>|t|)`),
                                             `Pr(>|t|)`< 0.01 ~ "<0.01")) %>% 
                   select(varname, PF_var[i]) %>% 
                   remove_rownames()
               )
             )
    }
  }
}


temp_list <- mget(paste0("temp_", 1:17))

out_tab_1_pvalue <- 
  reduce(
    temp_list,
    left_join,
    by = c("varname")
  )

rm(list = ls(pattern = "^temp_"))

####################################### Output 2: Prevalence by sub-population  -----
for (i in 1:length(PF_pos_var)) {
  for (j in 1:length(POP_var)) {
    if(j == 1){
      assign(paste0("temp_", i),
             svyby(
               as.formula(paste0("~", PF_pos_var[i])),
               as.formula(paste0("~", POP_var[j])),
               design = nhanes_design,
               FUN = svymean,
               na.rm = TRUE
             ) %>% 
               as.data.frame() %>% 
               rename(grp_sub = POP_var[j],
                      mean    = PF_pos_var[i]) %>%
               mutate(grp_var = POP_var[j],
                      !!PF_pos_var[i] := sprintf("%.1f%% (%.1f%%, %.1f%%)",
                                                 mean*100, 
                                                 (mean-(1.96*se))*100, 
                                                 (mean+(1.96*se))*100)
                      #!!PF_pos_var[i] := sprintf("%.1f%% (%.1f%%)", mean* 100, se*100)
                      ) %>% 
               select(grp_var, grp_sub, PF_pos_var[i]) %>% 
               remove_rownames())
    }
    else {
      assign(paste0("temp_", i),
             get(paste0("temp_", i)) %>% 
               union(svyby(
                 as.formula(paste0("~", PF_pos_var[i])),
                 as.formula(paste0("~", POP_var[j])),
                 design = nhanes_design,
                 FUN = svymean,
                 na.rm = TRUE
               ) %>% 
                 as.data.frame() %>% 
                 rename(grp_sub = POP_var[j],
                        mean    = PF_pos_var[i]) %>%
                 mutate(grp_var = POP_var[j],
                        !!PF_pos_var[i] := sprintf("%.1f%% (%.1f%%, %.1f%%)",
                                                   mean*100, 
                                                   (mean-(1.96*se))*100, 
                                                   (mean+(1.96*se))*100)) %>% 
                 select(grp_var, grp_sub, PF_pos_var[i]) %>% 
                 remove_rownames()))
    }
  }
}

temp_list <- mget(paste0("temp_", 1:17))

out_tab_2_prevalence <- 
  reduce(
    temp_list,
    left_join,
    by = c("grp_var", "grp_sub")
  )

rm(list = ls(pattern = "^temp_"))



for (i in 1:length(PF_var)) {
  for (j in 1:length(POP_var)) {
    if(j == 1){
      assign(paste0("temp_", i),
             summary(svyglm(as.formula(paste0(PF_pos_var[i], "~", POP_var[j])),
                            design = nhanes_design))$coefficients %>% 
               as.data.frame() %>% 
               mutate(varname = rownames(.)) %>% 
               filter(varname == POP_var[j]) %>% 
               mutate(!!PF_pos_var[i] := case_when(`Pr(>|t|)`>=0.01 ~ sprintf("%.2f", `Pr(>|t|)`),
                                               `Pr(>|t|)`< 0.01 ~ "<0.01")) %>% 
               select(varname, PF_pos_var[i]) %>% 
               remove_rownames()
      )
    }
    else {
      assign(paste0("temp_", i),
             get(paste0("temp_", i)) %>% 
               union(
                 summary(svyglm(as.formula(paste0(PF_pos_var[i], "~", POP_var[j])),
                                design = nhanes_design))$coefficients %>% 
                   as.data.frame() %>% 
                   mutate(varname = rownames(.)) %>% 
                   filter(varname == POP_var[j]) %>% 
                   mutate(!!PF_pos_var[i] := case_when(`Pr(>|t|)`>=0.01 ~ sprintf("%.2f", `Pr(>|t|)`),
                                                   `Pr(>|t|)`< 0.01 ~ "<0.01")) %>% 
                   select(varname, PF_pos_var[i]) %>% 
                   remove_rownames()
               )
      )
    }
  }
}

temp_list <- mget(paste0("temp_", 1:17))

out_tab_2_pvalue <- 
  reduce(
    temp_list,
    left_join,
    by = c("varname")
  )

rm(list = ls(pattern = "^temp_"))

####################################### Save output 1 & 2 to xlsx  -----
# wb <- createWorkbook()
# 
# addWorksheet(wb, "1-mean-intake")
# writeData(wb, "1-mean-intake", out_tab_1_mean_intake)
# 
# addWorksheet(wb, "1-pvalue")
# writeData(wb, "1-pvalue", out_tab_1_pvalue)
# 
# addWorksheet(wb, "2-prevalence")
# writeData(wb, "2-prevalence", out_tab_2_prevalence)
# 
# addWorksheet(wb, "2-pvalue")
# writeData(wb, "2-pvalue", out_tab_2_pvalue)
# 
# saveWorkbook(wb, paste0(path_out, "manuscript_tab_1_and_2_RAW.xlsx"), overwrite = TRUE)
# 
# rm(wb)

####################################### Output 3: All population consumption -----

# intake
for (i in 1:length(PF_var)) {
  assign(paste0("temp_", i),
         svymean(as.formula(paste0("~", PF_var[i])),
                 nhanes_design) %>% 
           as.data.frame() %>% 
           rename(se = PF_var[i]) %>%
           mutate(!!PF_var[i] := sprintf("%.3f (%.3f, %.3f)", mean, mean-(1.96*se), mean+(1.96*se))
                  #!!PF_var[i] := sprintf("%.3f (%.3f)", mean, se)
           ) %>% 
           select(PF_var[i]) %>% 
           remove_rownames())
}

temp_list <- mget(paste0("temp_", 1:17))

out_tab_ALL_mean_intake <- 
  reduce(
    temp_list,
    cbind
  )

rm(list = ls(pattern = "^temp_"))

# prevalence
for (i in 1:length(PF_pos_var)) {
  assign(paste0("temp_", i),
         svymean(as.formula(paste0("~", PF_pos_var[i])),
                 nhanes_design) %>% 
           as.data.frame() %>% 
           rename(se    = PF_pos_var[i]) %>%
           mutate(!!PF_pos_var[i] := sprintf("%.1f%% (%.1f%%, %.1f%%)",
                                             mean*100, 
                                             (mean-(1.96*se))*100, 
                                             (mean+(1.96*se))*100)
                  #!!PF_pos_var[i] := sprintf("%.1f%% (%.1f%%)", mean* 100, se*100)
           ) %>% 
           select(PF_pos_var[i]) %>% 
           remove_rownames())
}

temp_list <- mget(paste0("temp_", 1:17))

out_tab_ALL_prevalence <- 
  reduce(
    temp_list,
    cbind
  )

rm(list = ls(pattern = "^temp_"))

# test before combine
colnames(out_tab_ALL_prevalence) == paste0(colnames(out_tab_ALL_mean_intake), "_pos")
colnames(out_tab_ALL_prevalence) = colnames(out_tab_ALL_mean_intake)

out_tab_ALL <- rbind(out_tab_ALL_prevalence,
                     out_tab_ALL_mean_intake)


# wb <- createWorkbook()
# 
# addWorksheet(wb, "Formatted")
# writeData(wb, "Formatted", out_tab_ALL %>% select(PF_TOTAL_meat_sum,
#                                                   PF_MEAT_beef_sum,
#                                                   PF_MEAT_pork_sum,
#                                                   PF_MEAT_other_sum,
#                                                   PF_CUREDMEAT_beef_sum,
#                                                   PF_CUREDMEAT_pork_sum,
#                                                   PF_TOTAL_poult_sum,
#                                                   PF_POULT_chick_sum,
#                                                   PF_POULT_turkey_sum,
#                                                   PF_POULT_other_sum,
#                                                   PF_CUREDMEAT_chick_sum,
#                                                   PF_CUREDMEAT_turkey_sum))
# 
# addWorksheet(wb, "RAW")
# writeData(wb, "RAW", out_tab_ALL)
# 
# saveWorkbook(wb, paste0(path_out, "manuscript_tab_1_and_2_TOTAL.xlsx"), overwrite = TRUE)
# 
# rm(wb)

####################################### Output 4: Describe study population  -----

# All sample 12634, exclude 818 for age < 2

df_1720_temp <- df_1720_use %>% filter(incoh == 1)

for (i in 1:length(POP_var)) {
  assign(paste0("temp_", i),
         merge(
           # raw count
           table(df_1720_temp[POP_var[i]]) %>% 
             as.data.frame() %>% 
             mutate(temp_var = rownames(.)) %>% 
             mutate(grp_var = POP_var[i],
                    grp_sub = str_sub(temp_var, -1),
                    r_count = Freq) %>% 
             select(grp_var, grp_sub, r_count) %>% 
             remove_rownames(), 
           # weighted perc
           svymean(~factor(get(POP_var[i])), nhanes_design) %>% 
             as.data.frame() %>% 
             mutate(temp_var = rownames(.)) %>% 
             mutate(grp_var = POP_var[i],
                    grp_sub = str_sub(temp_var, -1),
                    w_perc = sprintf("%.1f%%", mean*100)) %>% 
             select(grp_var, grp_sub, w_perc) %>% 
             remove_rownames(), 
           # match main and sub group indicator
           by = c("grp_var", "grp_sub"))
  )
}


temp_list <- mget(paste0("temp_", 1:5))

out_tab_0_demo_describe <- reduce(temp_list, rbind)

rm(list = ls(pattern = "^temp_"))
rm(df_1720_temp)

writexl::write_xlsx(out_tab_0_demo_describe,
                    paste0(path_out, "manuscript_tab_0_RAW.xlsx"))

####################################### STEP :  -----
####################################### STEP :  -----
####################################### STEP :  -----
####################################### STEP :  -----
####################################### STEP :  -----
####################################### STEP :  -----

# ####################################### EXPIRE: simple visual of pop overall intake by subtype -----
# for (i in 1:length(PF_var)) {
#   if(i==1){
#     result_1720 <- 
#       svymean(as.formula(paste0("~", PF_var[i])), 
#               design = nhanes_design, na.rm = TRUE) %>% 
#       as.data.frame() %>% 
#       rename("SE" = PF_var[i])
#   }
#   else{
#     result_1720 <- result_1720 %>% 
#       union(svymean(as.formula(paste0("~", PF_var[i])), 
#                     design = nhanes_design, na.rm = TRUE) %>% 
#               as.data.frame() %>% 
#               rename("SE" = PF_var[i]))
#   }
# }
# 
# result_plt_1720 <- result_1720 %>% 
#   rownames_to_column(var = "PF_var") %>%
#   mutate(grp_main = case_when(str_starts(PF_var, "PF_MEAT") ~ "Red Meat",
#                               str_starts(PF_var, "PF_POULT") ~ "Poultry",
#                               str_starts(PF_var, "PF_CURED") ~ "Cured Meat",
#                               TRUE ~ NA),
#          grp_sub = case_when(str_detect(PF_var, "beef") ~ "Beef",
#                              str_detect(PF_var, "pork") ~ "Pork",
#                              str_detect(PF_var, "chick") ~ "Chicken",
#                              str_detect(PF_var, "turkey") ~ "Turkey",
#                              str_detect(PF_var, "other") ~ "Other",
#                              TRUE ~ "All")) %>% 
#   mutate(grp_main = factor(grp_main,
#                            levels = c("Red Meat", "Poultry", "Cured Meat")),
#          grp_sub = factor(grp_sub,
#                           levels = c("All", "Beef", "Pork", "Chicken", "Turkey", "Other"))) %>% 
#   select(-PF_var) %>% 
#   arrange(grp_main, grp_sub)
# 
# 
# 
# plt_1720 <- ggplot(result_plt_1720 %>% 
#                      mutate(fill_group = case_when(grp_sub == "All" ~ paste(grp_main, "(All)"),
#                                                    TRUE             ~ paste(grp_main, "(Subgroup)")),
#                             fill_group = factor(fill_group, 
#                                                 levels = c("Red Meat (All)", "Red Meat (Subgroup)",
#                                                            "Poultry (All)", "Poultry (Subgroup)",
#                                                            "Cured Meat (All)", "Cured Meat (Subgroup)"))),
#                    aes(x = grp_sub, y = mean, fill = fill_group)) +
#   geom_col(width = 0.75) +
#   geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.25, linewidth = 0.8, color = "#474747") +
#   facet_grid(. ~ grp_main, scales = "free_x", space = "free_x") +
#   labs(x = "Protein Food Subgroup",
#        y = "Mean ± SE (oz.eq/day)") +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 1.8),
#                      breaks = seq(0,1.8,0.2)) +
#   scale_fill_manual(values = c("Red Meat (All)"      = "#8B2E16",
#                                "Red Meat (Subgroup)" = "#D98867",
#                                "Poultry (All)"       = "#D4A017",
#                                "Poultry (Subgroup)"  = "#F2D28B",
#                                "Cured Meat (All)"    = "#7A4E7A",
#                                "Cured Meat (Subgroup)" = "#C2A2C2")) +
#   guides(fill = guide_legend(title = NULL, nrow = 2, byrow = FALSE)) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         strip.text = element_text(size = 19),
#         strip.background = element_rect(colour = "#BBBBBB"),
#         axis.title.x = element_text(size = 17),
#         axis.title.y = element_text(size = 17, vjust = +2),
#         axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15, 
#                                    angle = 45, hjust = 1),
#         legend.text = element_text(size = 15)
#   )
# 
# ggsave(plot = plt_1720,
#        paste0(path_out, "plt_1_description_1720.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)
#   












