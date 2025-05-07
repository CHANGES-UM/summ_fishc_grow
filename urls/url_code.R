#### script for reading in subject urls, cleaning them, and removing subjects that were removed from workflows 
## written by Katelyn King 
#these urls can be used across workflows (note that grow cards have their own url set)

urls<-read.csv("urls/afdmf_subjects_august.csv") %>%
  select(subject_id, locations) %>% 
  separate(col=locations, c("trash1", "trash2", "trash3", "front", "trash4", "trash5", "trash6", "back"), sep = '"') %>%
  select(subject_id, front, back) %>% 
  distinct(subject_id, .keep_all = TRUE) 

bad_urls<-read.csv("urls/oldbfirst_subjects_to_remove.csv") 

#return all rows from x without a match in y - remove the subjects that were removed from workflows and no longer have urls
new_urls<-anti_join(urls, bad_urls)

#write new urls to be matched to datasets prior to cleaning 
write.csv(new_urls, "urls/new_urls.csv", row.names = FALSE)

# code to pull out information from metadata #
grow_cross<-read.csv("GROW_general/missing_grow_subjects.csv") %>%
  select(subject_id, workflow_id, metadata) %>% 
  mutate(orig_subject_id = sub(".*?Original Subject ID.*?(\\d+).*", "\\1", metadata)
  ) %>% 
  distinct(orig_subject_id, workflow_id, .keep_all = TRUE) %>% # first remove where an original subject went through the same workflow twice
  select(subject_id, orig_subject_id) %>% 
  distinct(subject_id, .keep_all = TRUE) #then keep distinct subjects

write.csv(grow_cross, "GROW_general/grow_subject_xref.csv", row.names = FALSE)

