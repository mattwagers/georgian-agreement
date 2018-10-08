# Grab Experiment:AGR
agr_cleaner %>% filter(Experiment == "agr") %>% droplevels -> agr_data
agr_cleaner %>% filter(Experiment == "tam") %>% drop -> tam_data

# Rename Some Columns  
agr_data %<>% rename(!!c(judgment = "Gram.x", grammaticality = "Gram.y"))
levels(agr_data$judgment) <- c("yes", "no")
agr_data$correct <- paste(agr_data$grammaticality,agr_data$judgment,sep="-") %>% factor
levels(agr_data$correct) <- c("incorrect", "correct", "correct", "incorrect")

# Basic p correct
agr_data %>% group_by(Conj, LocArg, grammaticality, correct) %>%
  summarize(n.resp=n()) %>%
  mutate(p = n.resp/sum(n.resp)) -> agr_p.table

# Simple ME Logit Analysis
## Contrast coding set to sum +/- 1/2
contrasts(agr_data$Conj) %<>% contr.sum(2)/2     # direct=positive
contrasts(agr_data$LocArg) %<>% -contr.sum(2)/2  # s=positive
contrasts(agr_data$grammaticality) %<>% contr.sum(2)/2  # gram=positive
## Intercepts-only model [first-look]
glmer(correct ~ Conj*LocArg*grammaticality + (1|ParticipantID) + (1|Group), data=agr_data, family=binomial) -> agr_glm.a0b0.model 
summary(agr_glm.a0b0.model)

# Confidence
agr_data %>% group_by(Conj, LocArg, grammaticality, correct, Conf) %>%
  summarize(n.resp=n()) -> agr_conf.table

# dprime analysis
agr_p.table %>% filter(grammaticality=="gram" & correct=="correct") -> hits
agr_p.table %>% filter(grammaticality=="ungram" & correct=="incorrect") -> fas
full_join(hits, fas, by=c("Conj", "LocArg"), suffix=c(".h",".f")) %>%
  mutate(dprm = qnorm(p.h)-qnorm(p.f)) -> agr_dprime.table


glmer(correct ~ Conj*LocArg*grammaticality + (1|ParticipantID), data=agr_data, family=binomial) %>% summary

agr_cleaner %>% subset(experimen)
  group_by(cond,resp) %>%
  summarize(n.resp=n()) %>%
  mutate(p=n.resp/sum(n.resp))

table(cond,resp) %>% prop.table()
