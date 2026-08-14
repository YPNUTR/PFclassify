pacman::p_load(haven, dplyr, tidyr, labelled, ggplot2, stringr, survey, tibble, patchwork, showtext)

path_base <- "/Users/yongyipan/Library/CloudStorage/Box-Box/"

# path_base <- "C:/Users/ypan05/Box/"
# font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf") # windows font change
# showtext_auto()

path_dat <- paste0(path_base, "_working_pork/data/")
path_out <- paste0(path_base, "_working_pork/result/")

load(paste0(path_dat, "process/trend_figure_numbers.RData"))
####################################### Visual PREP -----
visual_meat <- c("PF_TOTAL_meat_sum",
                 "PF_MEAT_beef_sum", "PF_CUREDMEAT_beef_sum", 
                 "PF_MEAT_pork_sum", "PF_CUREDMEAT_pork_sum",
                 "PF_MEAT_other_sum")
visual_poultry <- c("PF_TOTAL_poult_sum",
                    "PF_POULT_chick_sum", "PF_CUREDMEAT_chick_sum",
                    "PF_POULT_turkey_sum", "PF_CUREDMEAT_turkey_sum",
                    "PF_POULT_other_sum")

visual_color = c("grey20", 
                 "#B33C2E", "#B33C2E", #beef
                 "#2B5C8A", "#2B5C8A", #pork
                 "#E6842A")

####################################### Visual 1: Red meat intake -----
p4plt_intake <- result_intake_p %>% 
  filter(PF_var %in% visual_meat) %>% 
  left_join(result_intake_0718 %>% 
              filter(cycle %in% c("1516", "1718")) %>%
              group_by(PF_var) %>% 
              summarise(y_pos = mean(mean)),
            by = "PF_var") %>% 
  mutate(PF_var = factor(PF_var,
                         levels = visual_meat,
                         labels = c("Total Red Meat",
                                    "Lean Beef", "Processed Beef",
                                    "Lean Pork", "Processed Pork",
                                    "Other Lean Red Meat")),
         p_label = ifelse(str_detect(p4trend_fmt, "<"),
                          paste0("p", p4trend_fmt),
                          paste0("p=", p4trend_fmt))) %>% 
  select(PF_var, y_pos, p_label)


plt_out_meat <- result_intake_0718 %>% 
  filter(PF_var %in% visual_meat) %>% 
  mutate(cycle = factor(cycle,
                        levels = cyc,
                        labels = c("2007-2008","2009-2010","2011-2012",
                                   "2013-2014","2015-2016","2017-2018")),
         PF_var = factor(PF_var,
                         levels = visual_meat,
                         labels = c("Total Red Meat",
                                    "Lean Beef", "Processed Beef",
                                    "Lean Pork", "Processed Pork",
                                    "Other Lean Red Meat"))) %>% 
  ggplot(aes(x = cycle, y = mean, group = PF_var))+
  geom_line(aes(color = PF_var, linetype = PF_var),
            linewidth = 0.7) +
  geom_ribbon(aes(ymin = mean - 1.96*SE, ymax = mean + 1.96*SE, 
                  fill = PF_var),
              alpha = .15) +
  geom_text(data = p4plt_intake,
            aes(x = 6, y = y_pos, label = p_label, color = PF_var),
            hjust = -0.1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = visual_color)+
  scale_fill_manual(values = visual_color)+
  scale_linetype_manual(values = c("solid",
                                   rep(c("dashed", "dotted"), 2),
                                   "dashed")) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.8))) +
  scale_y_continuous(limits = c(0, 2.4),
                     breaks = seq(0, 2.4, 0.4),
                     minor_breaks = seq(0, 2.4, 0.2),
                     expand = expansion(add = c(0.05, 0.01))) +
  labs(x = "NHANES Cycle",
       y = "Estimated Intake (oz. eq/day)") +
  guides(fill = guide_legend(title = "", nrow = 2, byrow = TRUE),
         color = guide_legend(title = "", nrow = 2, byrow = TRUE),
         linetype = guide_legend(title = "", nrow = 2, byrow = TRUE)) +
  theme(text = element_text(family = "Times New Roman"),
        
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "lines"),
        legend.key.height = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.box.margin = margin(t = 0, b = -10),
        
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey95", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90", linetype = "dashed"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        
        axis.ticks = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9))


# ggsave(plot = plt_out_meat,
#        paste0(path_out, "plot_trend_meat_intake.png"),
#        width = 2000, height = 1500, units = "px",
#        dpi = 300)

####################################### Visual 2: Poultry intake -----
p4plt_intake <- result_intake_p %>% 
  filter(PF_var %in% visual_poultry) %>% 
  left_join(result_intake_0718 %>% 
              filter(cycle %in% c( "1718")) %>%
              group_by(PF_var) %>% 
              summarise(y_pos = ifelse(mean(mean)>0.02 | mean(mean)<0.01,
                                       mean(mean),
                                       0.04)),
            by = "PF_var") %>% 
  mutate(PF_var = factor(PF_var,
                         levels = visual_poultry,
                         labels = c("Total Poultry",
                                    "Lean Chicken", "Processed Chicken",
                                    "Lean Turkey", "Processed Turkey",
                                    "Other Lean Poultry")),
         p_label = ifelse(str_detect(p4trend_fmt, "<"),
                          paste0("p", p4trend_fmt),
                          paste0("p=", p4trend_fmt))) %>% 
  select(PF_var, y_pos, p_label)


plt_out_poultry <- result_intake_0718 %>% 
  filter(PF_var %in% visual_poultry) %>% 
  mutate(cycle = factor(cycle,
                        levels = cyc,
                        labels = c("2007-2008","2009-2010","2011-2012",
                                   "2013-2014","2015-2016","2017-2018")),
         PF_var = factor(PF_var,
                         levels = visual_poultry,
                         labels = c("Total Poultry",
                                    "Lean Chicken", "Processed Chicken",
                                    "Lean Turkey", "Processed Turkey",
                                    "Other Lean Poultry"))) %>% 
  ggplot(aes(x = cycle, y = mean, group = PF_var))+
  geom_line(aes(color = PF_var, linetype = PF_var),
            linewidth = 0.7) +
  geom_ribbon(aes(ymin = mean - 1.96*SE, ymax = mean + 1.96*SE, 
                  fill = PF_var),
              alpha = .15) +
  geom_text(data = p4plt_intake,
            aes(x = 6, y = y_pos, label = p_label, color = PF_var),
            hjust = -0.1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = visual_color)+
  scale_fill_manual(values = visual_color)+
  scale_linetype_manual(values = c("solid",
                                   rep(c("dashed", "dotted"), 2),
                                   "dashed")) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.8))) +
  scale_y_continuous(limits = c(0, 1.8),
                     breaks = seq(0, 1.8, 0.2),
                     minor_breaks = seq(0, 1.8, 0.1),
                     expand = expansion(add = c(0.05, 0.01))) +
  labs(x = "NHANES Cycle",
       y = "Estimated Intake (oz. eq/day)") +
  guides(fill = guide_legend(title = "", nrow = 2, byrow = TRUE),
         color = guide_legend(title = "", nrow = 2, byrow = TRUE),
         linetype = guide_legend(title = "", nrow = 2, byrow = TRUE)) +
  theme(text = element_text(family = "Times New Roman"),
        
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "lines"),
        legend.key.height = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.box.margin = margin(t = 0, b = -10),
        
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey95", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90", linetype = "dashed"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        
        axis.ticks = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9))


# ggsave(plot = plt_out_poultry,
#        paste0(path_out, "plot_trend_poultry_intake.png"),
#        width = 2000, height = 1500, units = "px",
#        dpi = 300)



####################################### Visual 3: Red meat prevalence -----
p4plt_pre <- result_pre_p %>% 
  filter(PF_var %in% visual_meat) %>% 
  left_join(result_pre_0718 %>% 
              filter(cycle %in% c("1718")) %>%
              group_by(PF_var) %>% 
              summarise(y_pos = mean(mean)),
            by = "PF_var") %>% 
  mutate(PF_var = factor(PF_var,
                         levels = visual_meat,
                         labels = c("Total Red Meat",
                                    "Lean Beef", "Processed Beef",
                                    "Lean Pork", "Processed Pork",
                                    "Other Lean Red Meat")),
         p_label = ifelse(str_detect(p4trend_fmt, "<"),
                          paste0("p", p4trend_fmt),
                          paste0("p=", p4trend_fmt))) %>% 
  select(PF_var, y_pos, p_label)


plt_out_meat <- result_pre_0718 %>% 
  filter(PF_var %in% visual_meat) %>% 
  mutate(cycle = factor(cycle,
                        levels = cyc,
                        labels = c("2007-2008","2009-2010","2011-2012",
                                   "2013-2014","2015-2016","2017-2018")),
         PF_var = factor(PF_var,
                         levels = visual_meat,
                         labels = c("Total Red Meat",
                                    "Lean Beef", "Processed Beef",
                                    "Lean Pork", "Processed Pork",
                                    "Other Lean Red Meat"))) %>% 
  ggplot(aes(x = cycle, y = mean, group = PF_var))+
  geom_line(aes(color = PF_var, linetype = PF_var),
            linewidth = 0.7) +
  geom_ribbon(aes(ymin = mean - 1.96*SE, ymax = mean + 1.96*SE, 
                  fill = PF_var),
              alpha = .15) +
  geom_text(data = p4plt_pre,
            aes(x = 6, y = y_pos, label = p_label, color = PF_var),
            hjust = -0.1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = visual_color)+
  scale_fill_manual(values = visual_color)+
  scale_linetype_manual(values = c("solid",
                                   rep(c("dashed", "dotted"), 2),
                                   "dashed")) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.8))) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent,
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     expand = expansion(add = c(0.05, 0.01))) +
  labs(x = "NHANES Cycle",
       y = "Estimated Prevalence (%)") +
  guides(fill = guide_legend(title = "", nrow = 2, byrow = TRUE),
         color = guide_legend(title = "", nrow = 2, byrow = TRUE),
         linetype = guide_legend(title = "", nrow = 2, byrow = TRUE)) +
  theme(text = element_text(family = "Times New Roman"),
        
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "lines"),
        legend.key.height = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.box.margin = margin(t = 0, b = -10),
        
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey95", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90", linetype = "dashed"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        
        axis.ticks = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9))


# ggsave(plot = plt_out_meat,
#        paste0(path_out, "plot_trend_meat_pre.png"),
#        width = 2000, height = 1500, units = "px",
#        dpi = 300)

####################################### Visual 4: Poultry prevalence -----
p4plt_pre <- result_pre_p %>% 
  filter(PF_var %in% visual_poultry) %>% 
  left_join(result_pre_0718 %>% 
              filter(cycle %in% c("1718")) %>%
              group_by(PF_var) %>% 
              summarise(y_pos = ifelse(mean(mean)<.65,
                                       mean(mean),
                                       mean(mean)*1.02)),# adjust label y position
            by = "PF_var") %>% 
  mutate(PF_var = factor(PF_var,
                         levels = visual_poultry,
                         labels = c("Total Poultry",
                                    "Lean Chicken", "Processed Chicken",
                                    "Lean Turkey", "Processed Turkey",
                                    "Other Lean Poultry")),
         p_label = ifelse(str_detect(p4trend_fmt, "<"),
                          paste0("p", p4trend_fmt),
                          paste0("p=", p4trend_fmt))) %>% 
  select(PF_var, y_pos, p_label)


plt_out_poultry <- result_pre_0718 %>% 
  filter(PF_var %in% visual_poultry) %>% 
  mutate(cycle = factor(cycle,
                        levels = cyc,
                        labels = c("2007-2008","2009-2010","2011-2012",
                                   "2013-2014","2015-2016","2017-2018")),
         PF_var = factor(PF_var,
                         levels = visual_poultry,
                         labels = c("Total Poultry",
                                    "Lean Chicken", "Processed Chicken",
                                    "Lean Turkey", "Processed Turkey",
                                    "Other Lean Poultry"))) %>% 
  ggplot(aes(x = cycle, y = mean, group = PF_var))+
  geom_line(aes(color = PF_var, linetype = PF_var),
            linewidth = 0.7) +
  geom_ribbon(aes(ymin = mean - 1.96*SE, ymax = mean + 1.96*SE, 
                  fill = PF_var),
              alpha = .15) +
  geom_text(data = p4plt_pre,
            aes(x = 6, y = y_pos, label = p_label, color = PF_var),
            hjust = -0.1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = visual_color)+
  scale_fill_manual(values = visual_color)+
  scale_linetype_manual(values = c("solid",
                                   rep(c("dashed", "dotted"), 2),
                                   "dashed")) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.8))) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent,
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     expand = expansion(add = c(0.05, 0.01))) +
  labs(x = "NHANES Cycle",
       y = "Estimated Prevalence (%)") +
  guides(fill = guide_legend(title = "", nrow = 2, byrow = TRUE),
         color = guide_legend(title = "", nrow = 2, byrow = TRUE),
         linetype = guide_legend(title = "", nrow = 2, byrow = TRUE)) +
  theme(text = element_text(family = "Times New Roman"),
        
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "lines"),
        legend.key.height = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.box.margin = margin(t = 0, b = -10),
        
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey95", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90", linetype = "dashed"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        
        axis.ticks = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9))


# ggsave(plot = plt_out_poultry,
#        paste0(path_out, "plot_trend_poultry_pre.png"),
#        width = 2000, height = 1500, units = "px",
#        dpi = 300)
