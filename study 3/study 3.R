#######################################
# strength and conservatism
# study 3
# body composition and mfq endorsement
#######################################

pckgs <- c(
  'lme4',
  'lmerTest',
  'rstatix',
  'haven',
  'psych',
  'car',
  'emmeans',
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


df <- read_sav('study 3/Strength MFQ Perception.sav')

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
  select(
    id,
    StrengthStrong1:Weak4Lib4
  )

name_vec <- paste0(
  rep('stim'),
  rep(1:8, each = 13),
  rep(c('strg', 'weak'), each = 13),
  rep(c(
    'stren',
    'trad1', 'trad2', 'trad3', 'trad4',
    'comn1', 'comn2', 'comn3', 'comn4',
    'libr1', 'libr2', 'libr3', 'libr4'))
)

names(df_cols)[2:105] <- name_vec

# reshape
df_long <- df_cols %>%
  pivot_longer(
    cols = stim1strgstren:stim8weaklibr4,
    names_to = 'stim_id',
    values_to = 'response'
  ) %>%
  separate(col = 'stim_id', into = c('stim_id', 'item'), sep = -5) %>%
  separate(col = 'stim_id', into = c('stim_id', 'strength'), sep = -4) %>%
  pivot_wider(names_from = 'item', values_from = 'response')

df_long <- df_long %>%
  mutate(
    tradition = rowMeans(df_long[, c('trad1',
                                     'trad2',
                                     'trad3',
                                     'trad4')],
                         na.rm = T),
    compassion = rowMeans(df_long[, c('comn1',
                                      'comn2',
                                      'comn3',
                                      'comn4')],
                          na.rm = T),
    liberty = rowMeans(df_long[, c('libr1',
                                   'libr2',
                                   'libr3',
                                   'libr4')],
                       na.rm = T)) %>%
  select(
    id,
    stim_id,
    stren,
    strength,
    tradition,
    compassion,
    liberty
  ) %>%
  mutate(str_c = ifelse(strength == 'strg', 1, -1))


df_corrs <- df_long %>%
  select(
    id,
    strength,
    stren,
    tradition,
    compassion,
    liberty
  ) %>%
  group_by(id) %>%
  summarize(
    stren = mean(stren),
    tradition = mean(tradition),
    compassion = mean(compassion),
    liberty = mean(liberty)
  )

corrs <- corr.test(df_corrs[,-1])

ggcorrplot(corrs$r,
           type = 'lower',
           p.mat = corrs$p)


str_mod <- lmer(stren ~ str_c + (1|id) + (1|stim_id), data = df_long)
summary(str_mod)


# main analyses
df_main <- df_long %>%
  pivot_longer(
    cols = tradition:liberty,
    names_to = 'mfq_item',
    values_to = 'rating'
  )


mod_1_int <- lmer(rating ~ strength * mfq_item 
                  + (1|id) + (1|stim_id),
                  data = df_main)
mod_1 <- lmer(rating ~ strength * mfq_item 
              + (strength|id) + (1|stim_id), data = df_main)
anova(mod_1_int, mod_1)
anova(mod_1)
eta_squared(mod_1)

# post-hoc for mfq main effect
mfq_p <- summary(
  pairs(
    emmeans(
      mod_1, ~ mfq_item, lmer.df = 'satterthwaite', lmerTest.limit = 5000
    )
  )
)
mfq_p

t_to_d(t = mfq_p$t.ratio,
       df_error = mfq_p$df,
       paired = T)

df_main %>%
  group_by(mfq_item) %>%
  get_summary_stats(rating, type = 'mean_sd')

# strong targets
mod_str <- lmer(rating ~ mfq_item + (1|id) + (1|stim_id), data = filter(df_main, strength == 'strg'))
anova(mod_str)
eta_squared(mod_str)

p_str <- summary(
  pairs(
    emmeans(
      mod_str, ~ mfq_item, lmer.df = 'satterthwaite', lmerTest.limit = 5000
    )
  )
)
p_str

t_to_d(t = p_str$t.ratio,
       df_error = p_str$df,
       paired = T)


# weak targets
mod_weak <- lmer(rating ~ mfq_item + (1|id) + (1|stim_id), data = filter(df_main, strength == 'weak'))
anova(mod_weak)
eta_squared(mod_weak)

p_weak <- summary(
  pairs(
    emmeans(
      mod_weak, ~ mfq_item, lmer.df = 'satterthwaite', lmerTest.limit = 5000
    )
  )
)
p_weak

t_to_d(t = p_weak$t.ratio,
       df_error = p_weak$df,
       paired = T)


# view based on target
trad <- lmer(rating ~ strength + (1|id) + (1|stim_id), data = filter(df_main, mfq_item == 'tradition'))
anova(trad)
eta_squared(trad)
pairs(emmeans(trad, ~ strength, lmer.df = 'satterthwaite'))
df_main %>%
  filter(mfq_item == 'tradition') %>%
  group_by(strength) %>%
  get_summary_stats(rating, type = 'mean_sd')

comp <- lmer(rating ~ strength + (1|id) + (1|stim_id), data = filter(df_main, mfq_item == 'compassion'))
anova(comp)
eta_squared(comp)
pairs(emmeans(comp, ~ strength, lmer.df = 'satterthwaite'))
df_main %>%
  filter(mfq_item == 'compassion') %>%
  group_by(strength) %>%
  get_summary_stats(rating, type = 'mean_sd')


libr <- lmer(rating ~ strength + (1|id) + (1|stim_id), data = filter(df_main, mfq_item == 'liberty'))
anova(libr)
eta_squared(libr)
pairs(emmeans(libr, ~ strength, lmer.df = 'satterthwaite'))
df_main %>%
  filter(mfq_item == 'liberty') %>%
  group_by(strength) %>%
  get_summary_stats(rating, type = 'mean_sd')



plot_summ <- df_main %>%
  group_by(strength, mfq_item) %>%
  get_summary_stats(rating, type = 'mean_ci')


df_main %>%
  ggplot(aes(strength, rating, fill = mfq_item)) +
  geom_point(alpha = .25, size = .5, position = position_jitterdodge(.15, .15, .9), shape = 4) +
  geom_violin(alpha = .75, color = 'black', position = position_dodge(.9)) +
  geom_point(data = plot_summ, aes(strength, mean),
             alpha = .8, shape = 7, size = 3, position = position_dodge(.9), color = '#ffffff') +
  geom_errorbar(data = plot_summ, aes(x = strength, y = mean,
                                      ymin = mean - ci, ymax = mean + ci),
                alpha = .7, width = .15, position = position_dodge(.9), color = '#ffffff') +
  theme_classic() +
  labs(x = '',
       y = 'MFQ Item Endorsement Perception',
       fill = '',
       color = '') +
  scale_color_manual(values = c('#0b2265', '#a71930', '#a5acaf'),
                     labels = c('Compassion', 'Liberty', 'Tradition')) +
  scale_fill_manual(values = c('#0b2265', '#a71930', '#a5acaf'),
                    labels = c('Compassion', 'Liberty', 'Tradition')) +
  scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  theme(legend.position = 'bottom')


ggsave('study 3 mfq plot.jpg', device = 'jpeg', path = 'study 3', units = 'cm')
