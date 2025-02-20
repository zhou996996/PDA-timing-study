#fig2a-b for main analysis
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

fig2a <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2,color=NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention",tag = "A") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1), 
    legend.background = element_rect(fill = "white", color = NA), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama() +
  scale_fill_jama() 

fig2b <- ggplot(main_analyis, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
     labs(x = "Days since Referral",y = "Risk Difference",tag = "B") +
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
         axis.title.y = element_text(size = 16),
         plot.tag = element_text(size = 18, face = "bold")
       ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
     scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
     geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

fig_main <- grid.arrange(fig2a, fig2b, ncol = 2)


#fig3a-b:sensitivity analysis after removal of death cases
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

fig3a <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention",tag = "A") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1), 
    legend.background = element_rect(fill = "white", color = NA), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()


fig3b <- ggplot(sensitivity_analyis1, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference",tag = "B") +
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
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

fig_s1 <- grid.arrange(fig3a, fig3b, ncol = 2)

#fig4a-b:sensitivity analysis after adding referral centers in the treatment models
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

fig4a <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention",tag = "A") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1), 
    legend.background = element_rect(fill = "white", color = NA), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()

fig4b <- ggplot(sensitivity_analyis2, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference",tag = "B") +
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
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

fig_s2 <- grid.arrange(fig4a, fig4b, ncol = 2)

#fig5a-b:sensitivity analysis for neonates whose actual age <= 21 days
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

fig5a <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention",tag = "A") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1), 
    legend.background = element_rect(fill = "white", color = NA), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()

fig5b <- ggplot(sensitivity_analysis3, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference",tag = "B") +
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
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

fig_s3 <- grid.arrange(fig5a, fig5b, ncol = 2)

#fig8a-b:sensitivity analysis for surgical ligation vs. device closure
result_com <- sensitivity_analysis6 %>% mutate(sl_ci=sl) %>% mutate(dc_ci=dc)
result_long <- result_com %>%
  pivot_longer(
    cols = c(sl, sl_ci, sl_l, sl_u, dc, dc_ci, dc_l, dc_u),
    names_to = c("intervention", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE) %>%
  mutate(intervention = recode(intervention, 
                               "sl" = "Surgical ligation", 
                               "dc" = "Device closure"))

fig8a <- ggplot(result_long, aes(x = day, y = ci, color = intervention)) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = intervention), alpha = 0.2, color = NA) + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral", y = "Cumulative incidence", color = "Intervention",tag = "A") +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1), 
    legend.background = element_rect(fill = "white", color = NA), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  guides(
    color = guide_legend(title = "Group"),
    fill = "none"  
  ) + 
  scale_color_jama()+
  scale_fill_jama()

fig8b <- ggplot(sensitivity_analysis6, aes(x = day, y = rd)) +
  geom_ribbon(aes(ymin = rd_l, ymax = rd_u), alpha = 0.2, fill = "lightblue") + 
  geom_line(size = 0.8) +
  labs(x = "Days since Referral",y = "Risk Difference",tag = "B") +
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
    axis.title.y = element_text(size = 16),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 45, by = 5))  +  
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") 

fig_s6 <- grid.arrange(fig8a, fig8b, ncol = 2)
