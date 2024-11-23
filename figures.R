#fig2 for main analysis
result_com <- main_analyis %>% mutate(early_ci=early) %>% mutate(late_ci=late)
result_long <- result_com %>%
  pivot_longer(
    cols = c(early, early_ci, early_l, early_u, late, late_ci, late_l, late_u),
    names_to = c("intervention", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE) %>%
  mutate(intervention = recode(intervention, 
                               "early" = "Early", 
                               "late" = "Late"))

fig2 <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2,color=NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 16), 
    legend.title = element_text(size = 16),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16)
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama() +
  scale_fill_jama() 

#figs1 risk difference for main analysis
figs1 <- ggplot(main_analyis, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
     labs(x = "Days since Referral",y = "Risk Difference") +
     theme_bw() +  
     theme(
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),  
         panel.border = element_blank(),  
         axis.line.x = element_line(color = "black"),  
         axis.line.y = element_line(color = "black"),
         axis.text.x = element_text(size = 14), 
         axis.text.y = element_text(size = 14),
         axis.title.x = element_text(size = 16), 
         axis.title.y = element_text(size = 16) 
       ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
     scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
     geom_hline(yintercept = 0, linetype = "dotted", color = "black") 


#figs2-s3:sensitivity analysis#1
result_com <- sensitivity_analyis1 %>% mutate(early_ci=early) %>% mutate(late_ci=late)
result_long <- result_com %>%
  pivot_longer(
    cols = c(early, early_ci, early_l, early_u, late, late_ci, late_l, late_u),
    names_to = c("intervention", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE) %>%
  mutate(intervention = recode(intervention, 
                               "early" = "Early", 
                               "late" = "Late"))

figs2 <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 16), 
    legend.title = element_text(size = 16),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16)
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()

figs3 <- ggplot(sensitivity_analyis1, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference") +
  theme_bw() +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16) 
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

#figs4-s5:sensitivity analysis#2 
result_com <- sensitivity_analyis2 %>% mutate(early_ci=early) %>% mutate(late_ci=late)
result_long <- result_com %>%
  pivot_longer(
    cols = c(early, early_ci, early_l, early_u, late, late_ci, late_l, late_u),
    names_to = c("intervention", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE) %>%
  mutate(intervention = recode(intervention, 
                               "early" = "Early", 
                               "late" = "Late"))

figs4 <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 16), 
    legend.title = element_text(size = 16),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16)
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()
figs4

figs5 <- ggplot(sensitivity_analyis2, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference") +
  theme_bw() +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16) 
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 
figs5

#figs6-s7:sensitivity analysis#3
result_com <- sensitivity_analysis3 %>% mutate(early_ci=early) %>% mutate(late_ci=late)
result_long <- result_com %>%
  pivot_longer(
    cols = c(early, early_ci, early_l, early_u, late, late_ci, late_l, late_u),
    names_to = c("intervention", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE) %>%
  mutate(intervention = recode(intervention, 
                               "early" = "Younger", 
                               "late" = "Older"))

figs6 <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 16), 
    legend.title = element_text(size = 16),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16)
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()

figs7 <- ggplot(sensitivity_analysis3, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference") +
  theme_bw() +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16) 
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 
