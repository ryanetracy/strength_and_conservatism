#################################
# strength and conservatism
# study 1
# body composition and politics
#################################

pckgs <- c(
  'lme4',
  'lmerTest',
  'rstatix',
  'haven',
  'psych',
  'car',
  'tidyverse',
  'ggcorrplot',
  'effectsize',
  'ggpubr'
)

for (p in pckgs) {
  if (!(p %in% installed.packages())) {
    install.packages(p)
  }
  lapply(p, library, character.only = T)
}


df <- read_sav('study 1/Strength Politics.sav')

colnames(df)

# stims:
# strong1
# weak1
# strong2
# weak2
# strong3
# weak3
# strong4
# weak4

# items:
# fiscal conservatism
# social conservatism
# 1 = liberal; 7 = conservative

# physical strength
# 1 = not at all; 7 = very

# high tax opposition
# welfare opposition
# wealth redistribution opposition
# traditional value support
# abortion opposition
# immigration restriction
# 1 = strongly disgree; 7 = strongly agree

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


# grab columns
df_cols <- df %>%
  select(
    Number,
    StrongFiscal1:SES6,
    Political
)

# apply name vectors to clean up names
name_vec <- paste0(
  rep('stim'),
  rep(1:8, each = 9),
  rep(c('strg', 'weak'), each = 9),
  rep(paste0('_', 1:9))
)

names(df_cols)[2:73] <- name_vec

df_long <- df_cols %>%
  pivot_longer(
    cols = stim1strg_1:stim8weak_9,
    names_to = 'stim_id',
    values_to = 'response'
  ) %>%
  separate(col = 'stim_id', into = c('stim_id', 'item'), sep = -2) %>%
  separate(col = 'stim_id', into = c('stim_id', 'strength'), sep = -4) %>%
  mutate(strength = ifelse(strength == 'strg', 'strong', 'weak'),
         item = case_when(
           item == '_1' ~ 'fiscal_conservatism',
           item == '_2' ~ 'social_conservatism',
           item == '_3' ~ 'physical_strength',
           item == '_4' ~ 'higher_tax_opp',
           item == '_5' ~ 'welfare_opp',
           item == '_6' ~ 'wealth_redist_opp',
           item == '_7' ~ 'traditional_vals_supp',
           item == '_8' ~ 'abortion_opp',
           item == '_9' ~ 'immigration_restriction_supp'
         ),
         str_c = ifelse(strength == 'strong', 1, -1),
       ) %>%
  mutate_at(
    .vars = c('Number', 'stim_id', 'strength'),
    .funs = as.factor) %>%
  pivot_wider(names_from = 'item',
              values_from = 'response')


# correlation analyses
df_corrs <- df_long %>%
  select(
    Number,
    strength,
    fiscal_conservatism:immigration_restriction_supp
  ) %>%
  group_by(Number) %>%
  summarize(
    fiscal_conservatism = mean(fiscal_conservatism, na.rm = T),
    social_conservatism = mean(social_conservatism, na.rm = T),
    physical_strength = mean(physical_strength, na.rm = T),
    higher_tax_opp = mean(higher_tax_opp, na.rm = T),
    welfare_opp = mean(welfare_opp, na.rm = T),
    wealth_redist_opp = mean(wealth_redist_opp, na.rm = T),
    traditional_vals_supp = mean(traditional_vals_supp, na.rm = T),
    abortion_opp = mean(abortion_opp, na.rm = T),
    immigration_restriction_supp = mean(immigration_restriction_supp, na.rm = T)
  )

corrs <- corr.test(df_corrs[,-1])

ggcorrplot(corrs$r,
           type = 'lower',
           p.mat = corrs$p)


# check perceptions of strength (manipulation check)
check_1 <- lmer(physical_strength ~ str_c + (1|Number), data = df_long)
summary(check_1)
standardize_parameters(check_1)



# grab only conservatism columns
df_main <- df_long %>%
  select(Number,
         stim_id,
         strength,
         str_c,
         fiscal_conservatism,
         social_conservatism) %>%
  pivot_longer(
    cols = fiscal_conservatism:social_conservatism,
    names_to = 'conservatism_type',
    values_to = 'rating'
  ) %>%
  mutate(
    conservatism_type = ifelse(
      conservatism_type == 'fiscal_conservatism', 'fiscal', 'social'),
    con_c = ifelse(conservatism_type == 'fiscal', 1, -1)
  )

mod_1 <- lmer(rating ~ str_c * con_c 
              + (str_c|Number) + (1|stim_id),
              data = df_main)
summary(mod_1)
standardize_parameters(mod_1)

df_main %>%
  group_by(strength) %>%
  get_summary_stats(rating, type = 'mean_sd')


plot_summ <- df_main %>%
  group_by(strength, conservatism_type) %>%
  get_summary_stats(rating, type = 'mean_ci')


df_main %>%
  ggplot(aes(strength, rating, fill = conservatism_type)) +
  geom_point(alpha = .25,
             size = .5,
             position = position_jitterdodge(.15, .15, .9),
             shape = 4) +
  geom_violin(alpha = .75,
              color = 'black',
              position = position_dodge(.9)) +
  geom_point(data = plot_summ,
             aes(strength, mean),
             alpha = .8,
             shape = 7,
             size = 3,
             position = position_dodge(.9),
             color = '#ffffff') +
  geom_errorbar(data = plot_summ,
                aes(x = strength,
                    y = mean,
                    ymin = mean - ci,
                    ymax = mean + ci),
                alpha = .7,
                width = .15,
                position = position_dodge(.9),
                color = '#ffffff') +
  theme_classic() +
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

# ggsave('study 1 conservatism plot 1.jpg', device = 'jpeg', path = 'study 1', units = 'cm')



# additional analyses (looking at conservatism separately and other measures)

# conservatism
fisc_con_mod <- lmer(fiscal_conservatism ~ str_c 
                     + (1|Number) + (1|stim_id),
                     data = df_long)
summary(fisc_con_mod)
standardize_parameters(fisc_con_mod)

soc_con_mod <- lmer(social_conservatism ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(soc_con_mod)
standardize_parameters(soc_con_mod)

# additional measures
tax_mod <- lmer(higher_tax_opp ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(tax_mod)
standardize_parameters(tax_mod)

welfare_mod <- lmer(welfare_opp ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(welfare_mod)
standardize_parameters(welfare_mod)

redist_mod <- lmer(wealth_redist_opp ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(redist_mod)
standardize_parameters(redist_mod)

vals_mod <- lmer(traditional_vals_supp ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(vals_mod)
standardize_parameters(vals_mod)

abortion_mod <- lmer(abortion_opp ~ str_c + (1|Number) + (1|stim_id), data = df_long)
summary(abortion_mod)
standardize_parameters(abortion_mod)

imm_mod <- lmer(immigration_restriction_supp ~ str_c + (1|Number) + (1|stim_id), data = df_long)  
summary(imm_mod)
standardize_parameters(imm_mod)


# plot out conservatism differences
cons_summ_fisc <- df_long %>%
  group_by(strength) %>%
  get_summary_stats(fiscal_conservatism, type = 'mean_ci')

cons_summ_soc <- df_long %>%
  group_by(strength) %>%
  get_summary_stats(social_conservatism, type = 'mean_ci')


fisc_con <- df_long %>%
  ggplot(aes(strength, fiscal_conservatism)) +
  geom_point(alpha = .25, size = .5, position = position_jitter(.15, .15), shape = 4) +
  geom_violin(alpha = .75, color = 'black', fill = '#0080c6') +
  geom_point(data = cons_summ_fisc, aes(strength, mean),
             alpha = .8, shape = 7, size = 3, color = '#ffc20e') +
  geom_errorbar(data = cons_summ_fisc, aes(x = strength, y = mean,
                                           ymin = mean - ci, ymax = mean + ci),
                alpha = .7, width = .15, color = '#ffc20e') +
  theme_classic() +
  labs(x = '',
       y = 'Fiscal Conservatism') +
  scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
  scale_y_continuous(breaks = seq(1, 7, 1))


soc_con <- df_long %>%
  ggplot(aes(strength, social_conservatism)) +
  geom_point(alpha = .25, size = .5, position = position_jitter(.15, .15), shape = 4) +
  geom_violin(alpha = .75, color = 'black', fill = '#0080c6') +
  geom_point(data = cons_summ_soc, aes(strength, mean),
             alpha = .8, shape = 7, size = 3, color = '#ffc20e') +
  geom_errorbar(data = cons_summ_soc, aes(x = strength, y = mean,
                                           ymin = mean - ci, ymax = mean + ci),
                alpha = .7, width = .15, color = '#ffc20e') +
  theme_classic() +
  labs(x = '',
       y = 'Social Conservatism') +
  scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
  scale_y_continuous(breaks = seq(1, 7, 1))


ggarrange(fisc_con, soc_con,
          nrow = 1)


ggsave('study 1 conservatism plot 2.jpg', device = 'jpeg', path = 'study 1', units = 'cm')
