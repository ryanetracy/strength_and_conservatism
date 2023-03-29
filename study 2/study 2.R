###############################################
# strength and conservatism
# study 2
# body composition and politics, including ses
###############################################

pckgs <- c('lme4', 'lmerTest', 'rstatix', 'haven', 'psych', 'car', 'tidyverse', 
           'ggcorrplot', 'effectsize', 'ggpubr')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

df <- read_sav('study 2/Strength Politics SES.sav')

colnames(df)


# demographics
df %>%
  get_summary_stats(Age, type = 'mean_sd')

df %>%
  count(Race) %>%
  mutate(prop = 100* (n / sum(n)))

df %>%
  count(Sex) %>%
  mutate(prop = 100 * (n / sum(n)))

df %>%
  get_summary_stats(Political, type = 'mean_sd')


# rename columns
df_cols <- df %>%
  select(id,
         SR1Fiscal:WP2Wealth)

# fiscal
# social
# strong
# status
# wealth

name_vec <- paste0(
  rep('stim'),
  rep(1:8, each = 5),
  rep(c('strg', 'weak'), each = 20),
  rep(c('rich', 'poor'), each = 10),
  rep(c('fiscal', 'social', 'strong', 'status', 'wealth'))
)

names(df_cols)[2:41] <- name_vec


# reshape
df_long <- df_cols %>%
  pivot_longer(
    cols = stim1strgrichfiscal:stim8weakpoorwealth,
    names_to = 'stim_id',
    values_to = 'rating'
  ) %>%
  separate(col = 'stim_id', into = c('stim_id', 'item'), sep = -6) %>%
  separate(col = 'stim_id', into = c('stim_id', 'ses'), sep = -4) %>%
  separate(col = 'stim_id', into = c('stim_id', 'strength'), sep = -4) %>%
  pivot_wider(names_from = 'item', values_from = 'rating') %>%
  mutate(str_c = ifelse(strength == 'strg', 1, -1),
         ses_c = ifelse(ses == 'rich', 1, -1))


# correlation analysis
df_corrs <- df_long %>%
  select(
    id,
    strength,
    ses,
    fiscal:wealth
  ) %>%
  group_by(id) %>%
  summarize(
    fiscal = mean(fiscal, na.rm = T),
    social = mean(social, na.rm = T),
    strong = mean(strong, na.rm = T),
    status = mean(status, na.rm = T),
    wealth = mean(wealth, na.rm = T)
  )

corrs <- corr.test(df_corrs[,-1])

ggcorrplot(corrs$r,
           type = 'lower',
           p.mat = corrs$p)


# basic analyses
strong_mod <- lmer(strong ~ str_c * ses_c + (1|id) + (1|stim_id), data = df_long)
summary(strong_mod)

status_mod <- lmer(status ~ str_c * ses_c + (1|id) + (1|stim_id), data = df_long)
summary(status_mod)

wealth_mod <- lmer(wealth ~ str_c * ses_c + (1|id) + (1|stim_id), data = df_long)
summary(wealth_mod)


# focus only on fiscal vs social conservatism
df_main <- df_long %>%
  pivot_longer(
    cols = fiscal:social,
    names_to = 'conservatism_type',
    values_to = 'rating'
  ) %>%
  mutate(con_c = ifelse(conservatism_type == 'fiscal', 1, -1)) %>%
  select(id,
         stim_id,
         strength,
         ses,
         conservatism_type,
         str_c,
         ses_c,
         con_c,
         rating
  )


mod_1 <- lmer(rating ~ str_c * ses_c * con_c + (1|id) + (1|stim_id), data = df_main)
summary(mod_1)
standardize_parameters(mod_1)

df_main %>%
  group_by(strength) %>%
  get_summary_stats(rating, type = 'mean_sd')

df_main %>%
  group_by(ses) %>%
  get_summary_stats(rating, type = 'mean_sd')

df_main %>%
  group_by(conservatism_type) %>%
  get_summary_stats(rating, type = 'mean_sd')


plot_summ <- df_main %>%
  group_by(strength, ses, conservatism_type) %>%
  get_summary_stats(rating, type = 'mean_ci')

ses_labs <- c('Low-SES Targets', 'High-SES Targets')
names(ses_labs) <- c('poor', 'rich')

df_main %>%
  ggplot(aes(strength, rating, fill = conservatism_type)) +
  geom_point(alpha = .25, size = .5, position = position_jitterdodge(.15, .15, .9), shape = 4) +
  geom_violin(alpha = .75, color = 'black', position = position_dodge(.9)) +
  geom_point(data = plot_summ, aes(strength, mean),
             alpha = .8, shape = 7, size = 3, position = position_dodge(.9), color = '#ffffff') +
  geom_errorbar(data = plot_summ, aes(x = strength, y = mean,
                                      ymin = mean - ci, ymax = mean + ci),
                alpha = .7, width = .15, position = position_dodge(.9), color = '#ffffff') +
  theme_classic() +
  facet_wrap(~ ses, labeller = labeller(ses = ses_labs)) +
  labs(x = '',
       y = 'Conservatism Perception',
       fill = '',
       color = '') +
  scale_color_manual(values = c('#0b2265', '#a71930'),
                     labels = c('Fiscal\nConservatism', 'Social\nConservatism')) +
  scale_fill_manual(values = c('#0b2265', '#a71930'),
                    labels = c('Fiscal\nConservatism', 'Social\nConservatism')) +
  scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  theme(legend.position = 'bottom')


ggsave('study 2 conservatism plot.jpg', device = 'jpeg', path = 'study 2', units = 'cm')





