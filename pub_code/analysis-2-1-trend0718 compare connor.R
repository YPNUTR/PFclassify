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
      select(SEQN, SDDSRVYR, SDMVPSU, SDMVSTRA, RIDAGEYR)
  } else{
    raw_demo_0718 <- raw_demo_0718 %>% 
      union(read_xpt(paste0(path_dat, "DEMO_", cyc_char[i], ".xpt")) %>% 
              select(SEQN, SDDSRVYR, SDMVPSU, SDMVSTRA, RIDAGEYR))
  }
}
cbind(lapply(lapply(raw_demo_0718, is.na),sum)) # no missing

for(i in 1:length(cyc_char)){
  if(i == 1){
    raw_intake_0718 <-
      read_xpt(paste0(path_dat, "intake/DR1IFF_", cyc_char[i],".xpt")) %>% 
      rename("DRXIFDCD" = "DR1IFDCD",
             "DRXIGRMS" = "DR1IGRMS") %>% 
      mutate(DAY = 1) %>% 
      select(SEQN, DAY, WTDRD1, WTDR2D, DRXIFDCD, DRXIGRMS) %>% 
      union(read_xpt(paste0(path_dat, "intake/DR2IFF_", cyc_char[i],".xpt")) %>% 
              rename("DRXIFDCD" = "DR2IFDCD",
                     "DRXIGRMS" = "DR2IGRMS") %>% 
              mutate(DAY = 2) %>% 
              select(SEQN, DAY, WTDRD1, WTDR2D, DRXIFDCD, DRXIGRMS))
  } else{
    raw_intake_0718 <- raw_intake_0718 %>% 
      union(read_xpt(paste0(path_dat, "intake/DR1IFF_", cyc_char[i],".xpt")) %>% 
              rename("DRXIFDCD" = "DR1IFDCD",
                     "DRXIGRMS" = "DR1IGRMS") %>% 
              mutate(DAY = 1) %>% 
              select(SEQN, DAY, WTDRD1, WTDR2D, DRXIFDCD, DRXIGRMS) %>% 
              union(read_xpt(paste0(path_dat, "intake/DR2IFF_", cyc_char[i],".xpt")) %>% 
                      rename("DRXIFDCD" = "DR2IFDCD",
                             "DRXIGRMS" = "DR2IGRMS") %>% 
                      mutate(DAY = 2) %>% 
                      select(SEQN, DAY, WTDRD1, WTDR2D, DRXIFDCD, DRXIGRMS)))
  }
}

cbind(lapply(lapply(raw_intake_0718, is.na),sum))
# missing weights are all from human milk -- ok
# for cycle 0708, 0910, and 1112, some missing weights ppl who only have 1 day record
# --> will build a incoh binary next step to address the design

####################################### STEP 2: Merge data -----
df_0718 <- raw_intake_0718 %>% 
  left_join(raw_demo_0718, by = "SEQN") %>% 
  mutate(incoh = ifelse(!is.na(WTDR2D), 1, 0)) %>% 
  mutate(incoh_connor = case_when(!is.na(WTDRD1) & RIDAGEYR >= 2 ~ 1,
                                  TRUE ~ 0))

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
  group_by(SEQN, DAY) %>% 
  summarise(
    across(starts_with("PF_"), \(x) sum(x, na.rm = TRUE)),
    incoh = first(incoh),
    incoh_connor = first(incoh_connor),
    WTDR2D = first(WTDR2D),
    WTDRD1 = first(WTDRD1),
    RIDAGEYR = first(RIDAGEYR),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    cycle = first(cycle),
    .groups = "drop"
  ) %>% 
  # Here only keep day 1 to copy connor, 44274, 93792 only have DAY 2 recall
  filter(DAY == 1 | SEQN %in% c(44274, 93792)) %>% 
  relocate(SEQN, cycle, incoh_connor, WTDRD1, SDMVPSU, SDMVSTRA, RIDAGEYR) %>% 
  # mutate(WTDR2D_noNA = ifelse(is.na(WTDR2D), 0, WTDR2D)) %>% 
  # mutate(WTDR2D_noNA_2cyc = WTDR2D_noNA / 2)
  mutate(WTDRD1_noNA = ifelse(is.na(WTDRD1), 0, WTDRD1)) %>% 
  mutate(WTDRD1_noNA_2cyc = WTDRD1_noNA / 2)

####################################### STEP 4: weight calculation -----
PF_var <- colnames(df_0718_use %>% select(starts_with("PF_")))
cyc_list <- list(c("0708", "0910"),
                 c("1112", "1314"),
                 c("1516", "1718"))

for (k in 1:length(cyc_list)) {
  
  nhanes_design <- 
    subset(svydesign(id = ~SDMVPSU,
                     strata = ~SDMVSTRA,
                     #weights = ~WTDR2D_noNA,
                     weights = ~WTDRD1_noNA,
                     nest = TRUE,
                     data = df_0718_use %>% filter(cycle %in% cyc_list[[k]])),
           #incoh == 1,
           incoh_connor == 1)
  
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
  
  assign(paste0("result_temp_",paste0(cyc_list[[k]], collapse = "")),
         result_temp %>% mutate(cycle = paste0(cyc_list[[k]], collapse = "")))
  rm(result_temp)
}
result_0718 <- do.call(rbind, mget(paste0("result_temp_", sapply(cyc_list, paste0, collapse = ""))))
rm(list = paste0("result_temp_", sapply(cyc_list, paste0, collapse = "")))

####################################### STEP 5: visual data prep -----
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
  mutate(grp_main = factor(grp_main,
                           levels = c("Red Meat", "Poultry", "Cured Meat")),
         grp_sub = factor(grp_sub,
                          levels = c("All", "Beef", "Pork", "Chicken", "Turkey", "Other"))) %>% 
  select(-PF_var) %>% 
  arrange(grp_main, grp_sub)

result_plt_conner <- data.frame(
  mean = c(1.03, 0.97, 0.94,
           0.82, 0.72, 0.70,
           0.21, 0.25, 0.24),
  cycle = rep(c("07080910", "11121314", "15161718"), 3),
  grp_main = c(rep("Cured Meat", 9)),
  grp_sub = c(rep("All Processed Meat", 3),
              rep("Processed Red Meat", 3),
              rep("Processed Poultry", 3))
)

####################################### VISUAL 2-1: 3 total trends bar chart by cycle -----

plt_connor_1 <- result_plt_0718 %>% 
  filter(grp_main == "Cured Meat") %>% 
  mutate(grp_sub_co = case_when(grp_sub == "All" ~ "All Processed Meat",
                                grp_sub %in% c("Beef", "Pork") ~ "Processed Red Meat",
                                grp_sub %in% c("Chicken", "Turkey") ~ "Processed Poultry",
                                TRUE ~ "Processed Other")) %>% 
  group_by(cycle, grp_main, grp_sub_co) %>% 
  summarise(mean = sum(mean), .groups = "drop") %>% 
  filter(grp_sub_co != "Processed Other") %>% 
  rename("grp_sub" = "grp_sub_co") %>% 
  mutate(source = "Results using the PFclassify package") %>% 
  union(result_plt_conner %>% mutate(source = "Results from O'Connor et al. (2022)")) %>% 
  mutate(cycle = factor(cycle,
                        levels = c("07080910", "11121314", "15161718"),
                        labels = c("2007-2010", "2011-2014", "2015-2018")),
         grp_sub = factor(grp_sub,
                          levels = c("All Processed Meat", "Processed Red Meat", "Processed Poultry"))) %>% 
  ggplot(aes(x = cycle, y = mean, color = grp_sub, group = grp_sub)) +
  geom_line(linewidth = .8) +
  geom_point(shape = 20, size = 4) +
  geom_text(aes(label = sprintf("%.2f", mean)), 
            vjust = -1, size = 5,
            family = "Times New Roman",
            show.legend = FALSE) +
  facet_grid(. ~ source, scales = "free_x", space = "free_x", switch = "y") +
  labs(x = "NHANES Cycle",
       y = "Mean Intake (oz.eq/day)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1.25),
                     breaks = seq(0,1.25,0.25)) +
  scale_color_manual(values = c(
    "All Processed Meat" = "#2B5C8A",
    "Processed Poultry"  = "#E6842A",
    "Processed Red Meat" = "#B33C2E"
  )) +
  guides(color = guide_legend(title = "", nrow = 1, byrow = FALSE)) +
  theme(text = element_text(family = "Times New Roman"),
        
        legend.position = "top",
        
        panel.background = element_rect(fill = "#FFFFFF", colour = "#d1d1d1"),
        panel.grid.major.y = element_line(colour = "#d1d1d1"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        strip.placement = "outside",
        strip.text = element_text(size = 15, colour = "black"),
        strip.background = element_rect(colour = "#d1d1d1", fill = "#FFFFFF"),
        
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17, vjust = +2),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15, hjust = 0.5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.key  = element_blank(),
        legend.key.width = unit(2, "lines"),
        legend.key.spacing = unit(1, "lines"))

####################################### VISUAL 2-2: 3 total trends bar chat by main PF group-----

df_plt_stack <- data.frame(
  grp_main = c(rep("Total Processed Meat", 2),
               rep("Total Red Meat", 2),
               rep("Total Poultry", 2),
               rep("Total Red Meat and Poultry", 4)),
  grp_sub = c("Processed Red Meat", "Processed Poultry",
              "Processed Red Meat", "Lean Red Meat",
              "Processed Poultry", "Lean Poultry",
              "Processed Red Meat", "Processed Poultry",
              "Lean Red Meat", "Lean Poultry")) %>% 
  mutate(intake = case_when(grp_sub == "Processed Red Meat" ~ sum(result_plt_0718 %>% 
                                                                    filter(cycle == "15161718" &
                                                                             grp_main == "Cured Meat" &
                                                                             grp_sub %in% c("Beef", "Pork")) %>% 
                                                                    select(mean) %>% 
                                                                    pull()),
                            grp_sub == "Lean Red Meat" ~ sum(result_plt_0718 %>% 
                                                               filter(cycle == "15161718" &
                                                                        grp_main == "Red Meat" &
                                                                        grp_sub %in% c("All")) %>% 
                                                               select(mean) %>% 
                                                               pull()),
                            grp_sub == "Processed Poultry" ~ sum(result_plt_0718 %>% 
                                                                   filter(cycle == "15161718" &
                                                                            grp_main == "Cured Meat" &
                                                                            grp_sub %in% c("Chicken", "Turkey")) %>% 
                                                                   select(mean) %>% 
                                                                   pull()),
                            grp_sub == "Lean Poultry" ~ sum(result_plt_0718 %>% 
                                                              filter(cycle == "15161718" &
                                                                       grp_main == "Poultry" &
                                                                       grp_sub %in% c("All")) %>% 
                                                              select(mean) %>% 
                                                              pull())
  )) %>% 
  mutate(source = "Results using the PFclassify package") %>% 
  union(data.frame(
    grp_main = c(rep("Total Processed Meat", 2),
                 rep("Total Red Meat", 2),
                 rep("Total Poultry", 2),
                 rep("Total Red Meat and Poultry", 4)),
    grp_sub = c("Processed Red Meat", "Processed Poultry",
                "Processed Red Meat", "Lean Red Meat",
                "Processed Poultry", "Lean Poultry",
                "Processed Red Meat", "Processed Poultry",
                "Lean Red Meat", "Lean Poultry"),
    intake = c(74, 26,
               32, 68,
               14, 86,
               18, 6, 37, 39),
    source = rep("Results from O'Connor et al. (2022)", 10))
    ) %>% 
  mutate(grp_main = factor(grp_main,
                          levels = c("Total Processed Meat", "Total Red Meat", 
                                     "Total Poultry", "Total Red Meat and Poultry")),
         grp_sub = factor(grp_sub,
                          levels = c("Processed Red Meat", "Processed Poultry", 
                                     "Lean Red Meat", "Lean Poultry"))) %>% 
  group_by(grp_main, source) %>%
  arrange(grp_main, source, grp_sub) %>%  
  mutate(
    prop = intake / sum(intake),              # percent within each bar
    
    percent_label = scales::percent(prop, accuracy = 1)
  )


plt_connor_2 <- ggplot(df_plt_stack,
                       aes(x = grp_main, y = intake, fill = grp_sub)) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = percent_label),
            position = position_fill(vjust = 0.5, reverse = TRUE),
            size = 7,  family = "Times New Roman",
            color = "white") +
  facet_grid(. ~ source, scales = "free_x", space = "free_x", switch = "y") +
  labs(x = " ",
       y = "Category Composition (%)") +
  scale_y_continuous(expand = c(0.01, 0), 
                     labels = scales::percent,
                     breaks = seq(0,1,0.2)) + 
  scale_fill_manual(values = c(
    "Processed Red Meat"   = "#B33C2E",  # Red
    "Processed Poultry"    = "#E6842A",  # Orange
    "Lean Red Meat"    = "#2B5C8A",  # Blue
    "Lean Poultry"     = "#7A4E9B"   # Purple
  )) +
  guides(fill = guide_legend(title = "", nrow = 2, byrow = FALSE)) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "top",
        strip.placement = "outside",
        strip.text = element_text(size = 15, colour = "black"),
        strip.background = element_rect(colour = "#d1d1d1", fill = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#d1d1d1"),
        panel.grid.major.y = element_line(colour = "#d1d1d1"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17, vjust = +2),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15, hjust = 1, angle = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.key  = element_blank(),
        legend.key.width = unit(2, "lines"),
        legend.key.spacing = unit(0.5, "lines"))

plt_out <- (plt_connor_1 | plt_connor_2) &
  plot_layout(guides = "keep")

ggsave(plot = plt_out,
       paste0(path_out, "plt_connor_comb.png"),
       width = 4500, height = 3000, units = "px",
       dpi = 300)































