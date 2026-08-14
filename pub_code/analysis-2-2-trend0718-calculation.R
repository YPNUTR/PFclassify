pacman::p_load(haven, dplyr, tidyr, labelled, ggplot2, stringr, survey, tibble, patchwork, showtext)

path_base <- "/Users/yongyipan/Library/CloudStorage/Box-Box/"

# path_base <- "C:/Users/ypan05/Box/"
# font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf") # windows font change
# showtext_auto()

path_dat <- paste0(path_base, "_working_pork/data/")
path_out <- paste0(path_base, "_working_pork/result/")

####################################### STEP 1: Read in demo and intake data -----
cyc_char <- c("E", "F", "G", "H", "I", "J")

for(i in 1:length(cyc_char)){
  if(i == 1){
    raw_demo_0718 <-
      read_xpt(paste0(path_dat, "DEMO_", cyc_char[i], ".xpt")) %>% 
      select(SEQN, SDDSRVYR, SDMVPSU, SDMVSTRA,
             RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, INDFMPIR)
  } else{
    raw_demo_0718 <- raw_demo_0718 %>% 
      union(read_xpt(paste0(path_dat, "DEMO_", cyc_char[i], ".xpt")) %>% 
              select(SEQN, SDDSRVYR, SDMVPSU, SDMVSTRA,
                     RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, INDFMPIR))
  }
}
raw_demo_0718 <- raw_demo_0718 %>% 
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
    race_grp = case_when(RIDRETH1 %in% c(3)   ~ 1, # NHW
                         RIDRETH1 %in% c(4)   ~ 2, # NHB
                         RIDRETH1 %in% c(1,2) ~ 3, # H
                         RIDRETH1 %in% c(5)   ~ 4  # Other Race - Including Multi-Racial
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

cbind(lapply(lapply(raw_demo_0718, is.na),sum)) # no missing

for(i in 1:length(cyc_char)){
  if(i == 1){
    raw_intake_0718 <-
      read_xpt(paste0(path_dat, "intake/DR1IFF_", cyc_char[i],".xpt")) %>% 
      rename("DRXIFDCD" = "DR1IFDCD",
             "DRXIGRMS" = "DR1IGRMS") %>% 
      mutate(DAY = 1) %>% 
      select(SEQN, DAY, WTDR2D, DRXIFDCD, DRXIGRMS) %>% 
      union(read_xpt(paste0(path_dat, "intake/DR2IFF_", cyc_char[i],".xpt")) %>% 
              rename("DRXIFDCD" = "DR2IFDCD",
                     "DRXIGRMS" = "DR2IGRMS") %>% 
              mutate(DAY = 2) %>% 
              select(SEQN, DAY, WTDR2D, DRXIFDCD, DRXIGRMS))
  } else{
    raw_intake_0718 <- raw_intake_0718 %>% 
      union(read_xpt(paste0(path_dat, "intake/DR1IFF_", cyc_char[i],".xpt")) %>% 
              rename("DRXIFDCD" = "DR1IFDCD",
                     "DRXIGRMS" = "DR1IGRMS") %>% 
              mutate(DAY = 1) %>% 
              select(SEQN, DAY, WTDR2D, DRXIFDCD, DRXIGRMS) %>% 
              union(read_xpt(paste0(path_dat, "intake/DR2IFF_", cyc_char[i],".xpt")) %>% 
                      rename("DRXIFDCD" = "DR2IFDCD",
                             "DRXIGRMS" = "DR2IGRMS") %>% 
                      mutate(DAY = 2) %>% 
                      select(SEQN, DAY, WTDR2D, DRXIFDCD, DRXIGRMS)))
  }
}

cbind(lapply(lapply(raw_intake_0718, is.na),sum))
# missing weights are all from human milk -- ok
# for cycle 0708, 0910, and 1112, some missing weights ppl who only have 1 day record
# --> will build a incoh binary next step to address the design

####################################### STEP 2: Merge data -----
df_0718 <- raw_intake_0718 %>% 
  left_join(raw_demo_0718, by = "SEQN")

# same missing situation as above, incoh built for weight missing case
cbind(lapply(lapply(df_0718, is.na),sum))

####################################### STEP 3: Install and use PFclassify -----
# install.packages(paste0(path_base, "_working_pork/PFclassify_0.1.0.tar.gz"),
#                  repos = NULL, type = "source",
#                  INSTALL_opts = "--no-multiarch")# .rs.restartR()
library(PFclassify)

df_0718_fmt <- ReFormatDF(df_0718,
                          var_cycle = "SDDSRVYR",
                          var_Food_code = "DRXIFDCD",
                          var_weight = "DRXIGRMS",
                          your_cycle = c(5,6,7,8,9,10),
                          out_cycle = c("0708", "0910", "1112", "1314", "1516", "1718"))




df_0718_break <- CalPF(df_0718_fmt,
                       var_id = "SEQN",
                       fdcd_description = TRUE,
                       option_subtotal = c("meat", "poult", "curedmeat"),
                       option_meat = c("beef", "pork", "other"),
                       option_poult = c("chick", "turkey", "other"),
                       option_curedmeat = c("beef", "pork", "chick", "turkey", "other"))

df_0718_use <- df_0718_break %>% 
  mutate(across(starts_with("PF_"), ~ tidyr::replace_na(.x, 0))) %>% 
  #filter(!is.na(Food_description)) %>% 
  group_by(SEQN, DAY, age_grp, sex_grp, race_grp, edu_grp, pir_grp) %>% 
  summarise(
    across(starts_with("PF_"), \(x) sum(x, na.rm = TRUE)),
    incoh = first(incoh),
    WTDR2D = first(WTDR2D),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    cycle = first(cycle),
    .groups = "drop"
  ) %>% 
  group_by(SEQN, age_grp, sex_grp, race_grp, edu_grp, pir_grp) %>% 
  summarise(
    across(starts_with("PF_"), \(x) mean(x, na.rm = TRUE)),
    incoh = first(incoh),
    WTDR2D = first(WTDR2D),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    cycle = first(cycle),
    .groups = "drop"
  ) %>% 
  mutate(WTDR2D_noNA = ifelse(is.na(WTDR2D), 0, WTDR2D)) %>% 
  mutate(WTDR2D_noNA_pool = WTDR2D_noNA / 6) %>% 
  mutate(cycle_num = case_when(cycle == "0708" ~ 1,
                               cycle == "0910" ~ 2,
                               cycle == "1112" ~ 3,
                               cycle == "1314" ~ 4,
                               cycle == "1516" ~ 5,
                               cycle == "1718" ~ 6)) %>% 
  mutate(PF_TOTAL_all_sum = PF_MEAT_sum + PF_POULT_sum + PF_CUREDMEAT_sum,
         PF_TOTAL_meat_sum = PF_MEAT_sum + PF_CUREDMEAT_beef_sum + PF_CUREDMEAT_pork_sum,
         PF_TOTAL_poult_sum = PF_POULT_sum + PF_CUREDMEAT_chick_sum  + PF_CUREDMEAT_turkey_sum) %>% 
  select(SEQN, cycle, cycle_num, incoh, WTDR2D_noNA, WTDR2D_noNA_pool, SDMVPSU, SDMVSTRA, 
         ends_with("grp"), starts_with("PF")) %>% 
  mutate(
    across(
      starts_with("PF_"),
      ~ as.numeric(.x > 0),
      .names = "{.col}_pos"
    )
  )


cbind(lapply(lapply(df_0718_use, is.na),sum))  

####################################### STEP 4: Define survey design and list of vars-----
nhanes_design <- subset(
  svydesign(id = ~SDMVPSU,
            strata = ~SDMVSTRA,
            weights = ~WTDR2D_noNA,
            nest = TRUE,
            data = df_0718_use),
  incoh == 1
)

nhanes_design_trend <- subset(
  svydesign(id = ~SDMVPSU,
            strata = ~SDMVSTRA,
            weights = ~WTDR2D_noNA_pool,
            nest = TRUE,
            data = df_0718_use),
  incoh == 1
)

PF_var <- colnames(df_0718_use %>% select(ends_with("_sum")))
PF_pos_var <- colnames(df_0718_use %>% select(ends_with("_pos")))
POP_var <- colnames(df_0718_use %>% select(ends_with("_grp")))
cyc <- c("0708", "0910", "1112", "1314", "1516", "1718")

####################################### RUN MODEL 1: Intake Mean trend  -----
for (k in 1:length(cyc)) {
  nhanes_design <- subset(svydesign(id = ~SDMVPSU,
                                    strata = ~SDMVSTRA,
                                    weights = ~WTDR2D_noNA,
                                    nest = TRUE,
                                    data = df_0718_use %>% filter(cycle == cyc[k])),
                          incoh == 1)
  
  for (i in 1:length(PF_var)) {
    if(i==1){
      result_temp <- 
        svymean(as.formula(paste0("~", PF_var[i])), 
                design = nhanes_design, na.rm = TRUE) %>% 
        as.data.frame() %>% 
        rename("SE" = PF_var[i])
    }
    else{
      result_temp <- result_temp %>% 
        union(svymean(as.formula(paste0("~", PF_var[i])), 
                      design = nhanes_design, na.rm = TRUE) %>% 
                as.data.frame() %>% 
                rename("SE" = PF_var[i]))
    }
  }
  
  assign(paste0("result_temp_",cyc[k]), result_temp %>% mutate(cycle = cyc[k]))
  rm(result_temp)
}

result_intake_0718 <- do.call(rbind, mget(paste0("result_temp_", cyc))) %>% 
  mutate(PF_var = str_remove(rownames(.), "^.*\\.")) %>% 
  remove_rownames()
rm(list = paste0("result_temp_", cyc))

####################################### RUN TREND 1: Intake Mean trend -----
nhanes_design_trend <- 
  subset(svydesign(id = ~SDMVPSU,
                   strata = ~SDMVSTRA,
                   weights = ~WTDR2D_noNA_pool,
                   nest = TRUE,
                   data = df_0718_use),
         incoh == 1)

for (i in 1:length(PF_var)) {
  if(i==1){
    temp_reg <- summary(svyglm(as.formula(paste0(PF_var[i], "~ cycle_num")), 
                               design = nhanes_design_trend))
    result_p_value <- 
      data.frame(PF_var = PF_var[i],
                 p4trend = as.data.frame(temp_reg$coefficients) %>% 
                   rownames_to_column(var = "var") %>% 
                   filter(var == "cycle_num") %>% 
                   select(`Pr(>|t|)`) %>% 
                   pull())
  }
  else{
    temp_reg <- summary(svyglm(as.formula(paste0(PF_var[i], "~ cycle_num")), 
                               design = nhanes_design_trend))
    result_p_value <- result_p_value %>% 
      union(data.frame(PF_var = PF_var[i],
                       p4trend = as.data.frame(temp_reg$coefficients) %>% 
                         rownames_to_column(var = "var") %>% 
                         filter(var == "cycle_num") %>% 
                         select(`Pr(>|t|)`) %>% 
                         pull()))
  }
}

rm(temp_reg)
result_intake_p <- result_p_value %>% 
  mutate(p4trend_fmt = case_when(p4trend < 0.001 ~ "<0.001",
                                 TRUE ~ as.character(format(round(p4trend, 3), nsmall = 3))))



####################################### RUN MODEL 2: Prevalence trend  -----
for (k in 1:length(cyc)) {
  nhanes_design <- subset(svydesign(id = ~SDMVPSU,
                                    strata = ~SDMVSTRA,
                                    weights = ~WTDR2D_noNA,
                                    nest = TRUE,
                                    data = df_0718_use %>% filter(cycle == cyc[k])),
                          incoh == 1)
  
  for (i in 1:length(PF_pos_var)) {
    if(i==1){
      result_temp <- 
        svymean(as.formula(paste0("~", PF_pos_var[i])), 
                design = nhanes_design, na.rm = TRUE) %>% 
        as.data.frame() %>% 
        rename("SE" = PF_pos_var[i])
    }
    else{
      result_temp <- result_temp %>% 
        union(svymean(as.formula(paste0("~", PF_pos_var[i])), 
                      design = nhanes_design, na.rm = TRUE) %>% 
                as.data.frame() %>% 
                rename("SE" = PF_pos_var[i]))
    }
  }
  
  assign(paste0("result_temp_",cyc[k]), result_temp %>% mutate(cycle = cyc[k]))
  rm(result_temp)
}

result_pre_0718 <- do.call(rbind, mget(paste0("result_temp_", cyc))) %>% 
  mutate(PF_var = str_extract(rownames(.), "(?<=\\.).*(?=_pos)")) %>% 
  remove_rownames()
rm(list = paste0("result_temp_", cyc))

####################################### RUN TREND 2: Prevalence trend -----
for (i in 1:length(PF_pos_var)) {
  if(i==1){
    temp_reg <- summary(svyglm(as.formula(paste0(PF_pos_var[i], "~ cycle_num")), 
                               design = nhanes_design_trend))
    result_p_value <- 
      data.frame(PF_pos_var = PF_pos_var[i],
                 p4trend = as.data.frame(temp_reg$coefficients) %>% 
                   rownames_to_column(var = "var") %>% 
                   filter(var == "cycle_num") %>% 
                   select(`Pr(>|t|)`) %>% 
                   pull())
  }
  else{
    temp_reg <- summary(svyglm(as.formula(paste0(PF_pos_var[i], "~ cycle_num")), 
                               design = nhanes_design_trend))
    result_p_value <- result_p_value %>% 
      union(data.frame(PF_pos_var = PF_pos_var[i],
                       p4trend = as.data.frame(temp_reg$coefficients) %>% 
                         rownames_to_column(var = "var") %>% 
                         filter(var == "cycle_num") %>% 
                         select(`Pr(>|t|)`) %>% 
                         pull()))
  }
}

rm(temp_reg)
result_pre_p <- result_p_value %>% 
  mutate(PF_var = str_remove(PF_pos_var, "_pos$")) %>% 
  select(PF_var, p4trend) %>% 
  mutate(p4trend_fmt = case_when(p4trend < 0.001 ~ "<0.001",
                                 TRUE ~ as.character(format(round(p4trend, 3), nsmall = 3))))

rm(result_p_value)


# save(list = ls()[!grepl("^path_", ls())],
#      file = paste0(path_dat, "process/trend_figure_numbers.RData"))





#######################################  -----
#######################################  -----
#######################################  -----
#######################################  -----
#######################################  -----
#######################################  -----
#######################################  -----
result_plt_0718 <- result_0718 %>% 
  rownames_to_column(var = "PF_var") %>%
  mutate(grp_main = case_when(str_detect(PF_var, "PF_MEAT") ~ "Red Meat",
                              str_detect(PF_var, "PF_POULT") ~ "Poultry",
                              str_detect(PF_var, "PF_CURED") ~ "Cured Meat",
                              TRUE ~ NA),
         grp_sub = case_when(str_detect(PF_var, "beef") ~ "Beef",
                             str_detect(PF_var, "pork") ~ "Pork",
                             str_detect(PF_var, "chick") ~ "Chicken",
                             str_detect(PF_var, "turkey") ~ "Turkey",
                             str_detect(PF_var, "other") ~ "Other",
                             TRUE ~ "All")) %>% 
  left_join(result_p_value, by = c("grp_main", "grp_sub")) %>% 
  mutate(grp_main = factor(grp_main,
                           levels = c("Red Meat", "Poultry", "Cured Meat")),
         grp_sub = factor(grp_sub,
                          levels = c("All", "Beef", "Pork", "Chicken", "Turkey", "Other"))) %>% 
  select(-PF_var) %>% 
  arrange(grp_main, grp_sub)

####################################### VISUAL 2-1: 3 total trends bar chart by cycle -----

plt_0718_1 <- ggplot(result_plt_0718 %>% 
                       filter(grp_sub == "All") %>% 
                       mutate(cycle = factor(cycle,
                                             levels = c("0708", "0910", "1112", "1314", "1516", "1718"),
                                             labels = c("2007-2008", "2009-2010", "2011-2012", 
                                                        "2013-2014", "2015-2016", "2017-2018"))),
                     aes(x = grp_main, y = mean, fill = grp_main)) +
  geom_col(width = 0.75) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.25, linewidth = 0.8, color = "#474747") +
  facet_grid(. ~ cycle, scales = "free_x", space = "free_x", switch = "x") +
  labs(x = "NHANES Cycle",
       y = "Mean ± SE (oz.eq/day)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1.8),
                     breaks = seq(0,1.8,0.2)) +
  scale_fill_manual(values = c("Red Meat"      = "#8B2E16",
                               "Poultry"       = "#D4A017",
                               "Cured Meat"    = "#7A4E7A")) +
  guides(fill = guide_legend(title = "Protein Food Group", nrow = 1, byrow = FALSE)) +
  theme_classic() +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        strip.background = element_rect(colour = "#FFFFFF"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17, vjust = +2),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(size = 15, 
        #                            angle = 45, hjust = 1),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15)
  )

# ggsave(plot = plt_0718_1,
#        paste0(path_out, "plt_2-1_total_trend_by_cycle_0718.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)

####################################### VISUAL 2-2 [with p-value]: 3 total trends bar chat by main PF group-----

plt_0718_2 <- result_plt_0718 %>% 
  filter(grp_sub == "All") %>% 
  mutate(cycle = factor(cycle,
                        levels = c("0708", "0910", "1112", "1314", "1516", "1718"),
                        labels = c("2007-2008", "2009-2010", "2011-2012", 
                                   "2013-2014", "2015-2016", "2017-2018"))) %>% 
  ggplot(.,
         aes(x = cycle, y = mean, fill = grp_main)) +
  geom_col(width = 0.75) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.25, linewidth = 0.8, color = "#474747") +
  geom_text(
    data = . %>%
      group_by(grp_main) %>%
      summarise(p4trend_fmt = unique(p4trend_fmt), .groups = "drop") %>%
      mutate(x = 5,
             y = max(result_plt_0718$mean + result_plt_0718$SE, na.rm = TRUE) * 1.05,
             label = paste0("Trend p = ", p4trend_fmt)),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold") +
  facet_grid(. ~ grp_main, scales = "free_x", space = "free_x", switch = "y") +
  labs(x = "NHANES Cycle",
       y = "Mean ± SE (oz.eq/day)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1.9),
                     breaks = seq(0,1.8,0.2)) +
  scale_fill_manual(values = c("Red Meat"      = "#8B2E16",
                               "Poultry"       = "#D4A017",
                               "Cured Meat"    = "#7A4E7A")) +
  guides(fill = "none") +
  theme_classic() +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        strip.background = element_rect(colour = "#969696"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17, vjust = +2),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,
                                   angle = 45, hjust = 1),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15)
  )


# ggsave(plot = plt_0718_2,
#        paste0(path_out, "plt_2-2_total_trend_by_main_0718.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)


####################################### VISUAL 2-3: 3 sub trends stack chart -----
meat_palette <- list(c("Beef"    = "#731F0D",  
                       "Pork"    = "#9B4732",  
                       "Other"   = "#C98C7A"),
                     c("Chicken" = "#B88E14", 
                       "Turkey"  = "#D1B14E",
                       "Other"   = "#E8D48F"),
                     c("Beef"    = "#3A233A",
                       "Pork"    = "#653D65",
                       "Chicken" = "#845D84",  
                       "Turkey"  = "#A97EA9",  
                       "Other"   = "#CEA5CE"))
pf_list <- c("Red Meat", "Poultry", "Cured Meat")

for (i in 1:3) {
  assign(paste0("plt_temp_3_",i),
         
         ggplot(result_plt_0718 %>% 
                  filter(grp_main == pf_list[i] & grp_sub != "All") %>% 
                  mutate(cycle = factor(cycle,
                                        levels = c("0708", "0910", "1112", "1314", "1516", "1718"),
                                        labels = c("2007-2008", "2009-2010", "2011-2012", 
                                                   "2013-2014", "2015-2016", "2017-2018"))),
                aes(x = cycle, y = mean, fill = grp_sub)) + 
           geom_bar(position="stack", stat = "identity") +
           labs(x = "NHANES Cycle",
                y = "Mean ± SE (oz.eq/day)") +
           scale_y_continuous(expand = c(0, 0), 
                              limits = c(0, 1.8),
                              breaks = seq(0,1.8,0.2)) +
           scale_fill_manual(values = meat_palette[[i]]) +
           guides(fill = guide_legend(title = paste0(pf_list[i], " Sub-Group"), 
                                      nrow = 3, byrow = TRUE)) +
           theme_classic() +
           theme(legend.position = "top",
                 legend.title.position = "top",
                 legend.title = element_text(size = 17),
                 legend.text = element_text(size = 15),
                 axis.title.x = element_text(size = 17),
                 axis.title.y = element_text(size = 17, vjust = +2),
                 axis.text.y = element_text(size = 15),
                 #axis.text.x = element_blank(),
                 axis.text.x = element_text(size = 15,
                                            angle = 45, hjust = 1)
                 )
         )
}


plt_0718_3 <- (plt_temp_3_1 + plt_temp_3_2 + plt_temp_3_3) +
  plot_layout(nrow = 1,
              axes = "collect_y",
              axis_titles = "collect",
              guides = "keep") &
  theme(legend.position = "top")

rm(list = paste0("plt_temp_3_", c(1,2,3)))

# ggsave(plot = plt_0718_3,
#        paste0(path_out, "plt_2-3_subgroup_trend_stack_0718.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)

####################################### VISUAL 2-4: 3 sub trends stack percentage chart -----
for (i in 1:3) {
  assign(paste0("plt_temp_4_",i),
         
         ggplot(result_plt_0718 %>% 
                  filter(grp_main == pf_list[i] & grp_sub != "All") %>% 
                  mutate(cycle = factor(cycle,
                                        levels = c("0708", "0910", "1112", "1314", "1516", "1718"),
                                        labels = c("2007-2008", "2009-2010", "2011-2012", 
                                                   "2013-2014", "2015-2016", "2017-2018"))),
                aes(x = cycle, y = mean, fill = grp_sub)) + 
           geom_bar(position="fill", stat = "identity") +
           labs(x = "NHANES Cycle",
                y = "Percentage")+
           scale_y_continuous(expand = c(0, 0),
                              breaks = seq(0,1,0.2),
                              labels = scales::percent) +
           scale_fill_manual(values = meat_palette[[i]]) +
           guides(fill = guide_legend(title = paste0(pf_list[i], " Sub-Group"), 
                                      nrow = 3, byrow = TRUE)) +
           theme_classic() +
           theme(legend.position = "top",
                 legend.title.position = "top",
                 legend.title = element_text(size = 17),
                 legend.text = element_text(size = 15),
                 axis.title.x = element_text(size = 17),
                 axis.title.y = element_text(size = 17, vjust = +2),
                 axis.text.y = element_text(size = 15),
                 #axis.text.x = element_blank(),
                 axis.text.x = element_text(size = 15,
                                            angle = 45, hjust = 1)
           )
  )
}


plt_0718_4 <- (plt_temp_4_1 + plt_temp_4_2 + plt_temp_4_3) +
  plot_layout(nrow = 1,
              axes = "collect_y",
              axis_titles = "collect",
              guides = "keep") &
  theme(legend.position = "top")

rm(list = paste0("plt_temp_4_", c(1,2,3)))

# ggsave(plot = plt_0718_4,
#        paste0(path_out, "plt_2-4_subgroup_trend_stack_perc_0718.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)


####################################### VISUAL 2-5: trend by sub group -----
ylim_list <- c(1.7, 1.9, 1.1)

plot_list <- vector("list", length = 3)

for (i in 1:3) {
  
  plot_list[[i]] <- local({  # <- store return value of local()
    
    ii <- i
    
    # Prepare data
    data_temp <- result_plt_0718 %>% 
      filter(grp_main == pf_list[ii], grp_sub != "All") %>% 
      mutate(cycle = factor(cycle,
                            levels = c("0708", "0910", "1112", "1314", "1516", "1718"),
                            labels = c("2007-2008", "2009-2010", "2011-2012", 
                                       "2013-2014", "2015-2016", "2017-2018")))
    
    # Precompute y position
    y_pos <- ylim_list[ii] * 0.95
    
    label_temp <- data_temp %>%
      group_by(grp_main, grp_sub) %>%
      summarise(p4trend_fmt = unique(p4trend_fmt), .groups = "drop") %>%
      mutate(
        x = 5,
        y = y_pos,
        label = paste0("Trend p ", p4trend_fmt)
      )
    
    # Return ggplot object from local()
    ggplot(data_temp, aes(x = cycle, y = mean, fill = grp_sub)) +
      geom_col(width = 0.75) +
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                    width = 0.25, linewidth = 0.8, color = "#474747") +
      geom_text(
        data = label_temp,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        size = 4,
        fontface = "bold"
      ) +
      facet_grid(. ~ grp_sub, scales = "free_x", space = "free_x", switch = "y") +
      labs(x = "NHANES Cycle", y = "Mean ± SE (oz.eq/day)") +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, ylim_list[ii]),
        breaks = seq(0, ylim_list[ii], 0.2)
      ) +
      scale_fill_manual(values = meat_palette[[ii]]) +
      guides(fill = "none") +
      theme_classic() +
      theme(
        legend.position = "top",
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        strip.background = element_rect(colour = "#969696"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17, vjust = +2),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 30, hjust = 1),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15)
      )
    
  })  # <- end of local()
}


plt_0718_5 <- (plot_list[[1]] + plot_list[[2]] + plot_list[[3]]) +
  plot_layout(ncol = 1,
              axes = "collect_y",
              axis_titles = "collect",
              guides = "collect") &
  theme(legend.position = "top")
# 
# ggsave(plot = plt_0718_5,
#        paste0(path_out, "plt_2-5_subgroup_trend_panel_0718.png"),
#        width = 4000, height = 3000, units = "px",
#        dpi = 300)


