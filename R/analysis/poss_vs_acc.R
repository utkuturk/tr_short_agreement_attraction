# load the neccassary packages
library(udpipe)
library(readxl)
library(dplyr)
library(magrittr)

#load the conllu file from the directory
tb <- udpipe_read_conllu("./data/all_ud_tr.conllu")

# select relevant columns
# doc_id is for treebank identification, 
# token_id, sentence and sentence_id is just to be able to check it afterwards
# feats is morphological features of a token
tb %<>% dplyr::select(doc_id, sentence, sentence_id, token_id, feats)

# create a lagged condition
# prev_feats is the features of the previous word
tb <- within(tb, prev_feats <- c(NA, head(as.character(feats), -1)))

# create vector with search conditions, 
# and grep rows that contain these markings
# acc stands for accusative marking
# since ud does not encode possessive as a case marking
## we need to use [psor] tag.
search_for <- c("Acc", "psor")
tb_slim<- tb %>% subset(grepl(paste(search_for, collapse = "|"), feats))

# only select accusative marked ones
tb_gen_acc <- tb_slim %>% subset(grepl("Acc", feats))
# with the genitive marking in the previous word
tb_gen_acc %<>% subset(grepl("Gen", prev_feats))
# clear false positives, for example mood = gen
tb_gen_acc %<>% subset(!grepl("Mood=Gen", prev_feats))
# tourism treebank is horrendous
tb_gen_acc %<>% subset(!grepl("tourism", doc_id)) 
# count
c_acc_given_gen <- nrow(tb_gen_acc)

tb_poss_acc <- tb_slim %>% subset(grepl("psor", feats))
# discarded both acc and psor marking, 
# because in those cases the last marking is actually accusative, 
# so they should be included with acc markinds.
tb_poss_acc <- tb_poss_acc %>% subset(!grepl("Acc", feats)) 
tb_poss_acc %<>% subset(grepl("Gen", prev_feats))
tb_poss_acc %<>% subset(!grepl("Mood=Gen", prev_feats))
tb_poss_acc %<>% subset(!grepl("tourism", doc_id)) # tourism treebank is horrendous
c_poss_given_gen <- nrow(tb_poss_acc)

p_acc_given_gen <- c_acc_given_gen / (c_acc_given_gen + c_poss_given_gen)
p_acc_given_gen <- round(p_acc_given_gen, 2)
#p_poss_given_gen
