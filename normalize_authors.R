library(dplyr)

build_authors_lookup <- function(authors, email_provider_domains) {

  authors <- authors %>% 
    group_by(email) %>%
    mutate(name_freq=n()) %>%
    group_by(name) %>%
    mutate(email_freq=n())
  
  # Single Authors
  authors_single <- authors %>%
    ungroup() %>%
    filter(name_freq==1 & email_freq==1) %>%
    mutate(author=name, email_id=email)
  
  # Multi Authors
  authors_multi <- authors %>%
    filter(email_freq > 1 & name_freq > 1)
  
  # One Email to Many Names - pick an authoritative name based on max commits
  authors_name_single <- authors %>% filter(email_freq == 1 & name_freq > 1)

  authors_name_single_lookup <- authors_name_single %>%
    arrange(email, name, desc(num_commits)) %>%
    group_by(email) %>%
    mutate(author=name[which.max(num_commits)], 
           email_author=email[which.max(num_commits)]) %>%
    ungroup() %>%
    select(author, email_author, num_commits)
  
  authors_name_single_join <- merge(authors_name_single %>% select(name, email), 
                                    authors_name_single_lookup,
                                    by.x="email", by.y="email_author")
  
  authors_name_multi_join <- merge(authors_multi %>% select(name, email),
                                   authors_name_single_join %>% select(author, email, num_commits),
                                   by=c("email"))
  
  authors_names <- bind_rows(authors_name_multi_join, authors_name_single_join)
  
  # One Name to Many Emails - find emails associated with the same author name
  authors_email_single <- authors %>% filter(email_freq > 1 & name_freq == 1)
  
  # set email_id by latest commit or if they ever used a non-provider email
  authors_email_single_lookup <- authors_email_single %>%
    group_by(name) %>%
    arrange(desc(last_commit)) %>%
    mutate(email_id=email[which.max(last_commit)], author_email=name[which.max(last_commit)]) %>%
    separate(email, c("email_user", "email_host"), "@", remove=FALSE) %>%
    separate(email_id, c("email_id_user", "email_id_host"), "@", remove=FALSE) %>%
    mutate(email_id_alt = email[first(which(!email_host %in% email_provider_domains))],
           email_id=ifelse(!is.na(email_id_alt) & email_id != email_id_alt, email_id_alt, email_id)
    ) %>%
    ungroup() %>%
    select(author_email, email_id)
  
  authors_email_single_join <- merge(authors_email_single %>% select(name, email), 
                                     authors_email_single_lookup,
                                     by.x="name", by.y="author_email", all=TRUE)
  
  authors_email_multi_join <- merge(authors_multi %>% select(name, email),
                                    authors_email_single_join %>% select(name, email_id),
                                    by=c("name"))
  
  authors_emails <- bind_rows(authors_email_multi_join, authors_email_single_join)
  
  # Find authors with entries in both tables
  authors_author <- merge(authors_names %>% select(author, name), 
                          authors_emails,
                          by="name")
  
  authors_email_id <- merge(authors_names, 
                            authors_emails %>% select(email_id, email),
                            by="email")
  
  # create lookup for authors that are in both lists or occur once
  authors_cx <- bind_rows(authors_author, authors_email_id, 
                          authors_single %>% select(name, email, author, email_id))
  
  # merge with authors that only occur in the emails table (>1 email, 1 name)
  authors_email_merge <- merge(authors_cx, authors_emails, by=c("name", "email_id", "email"), all=TRUE)
  # na's just have one name, so set author to that name
  authors_email_merge <- authors_email_merge %>% mutate(author=ifelse(is.na(author), name, author))
  
  # merge with authors that only occur in the names table (1 email, >1 name)
  authors_name_merge <- merge(authors_cx, authors_names, by=c("email", "author", "name"), all=TRUE)
  # na's just have one email, so set email_id to that email
  authors_name_merge <- authors_name_merge %>% mutate(email_id=ifelse(is.na(email_id), email, email_id))
  
  # combine the rows
  authors_merge <- bind_rows(authors_email_merge, authors_name_merge)
  
  # dedupe
  authors_lookup <- unique(authors_merge)
  
  # extract email hosts
  authors_lookup <- authors_lookup %>%
    separate(email, c("email_user", "email_host"), "@", remove=FALSE) %>%
    separate(email_id, c("email_id_user", "email_id_host"), "@", remove=FALSE) %>%
    select(author, email_id, email, email_id_host)
  
  authors_lookup <- unique(authors_lookup)
  
  return(authors_lookup)
}

