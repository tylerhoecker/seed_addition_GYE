this_version <- 'data/seedling_data_complete.csv'

checkA <- read_csv(this_version) %>% 
  select(-starts_with('height_header'),-starts_with('basal_header'), 
         -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
  gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
  separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
  unite(x_cell, y_cell, col = 'cell', remove = F) %>% 
  filter(Fire == 'maple', Aspect == 'north') %>% 
  distinct(frameID) %>% 
  unlist() %>% 
  as.numeric() %>% 
  sort()
  
  
checkB <- clipr::read_clip() %>% 
  as.numeric() %>% 
  sort()

setdiff(checkA,checkB)


repeat_measures <- read_csv(this_version) %>% 
  select(-starts_with('height_header'),-starts_with('basal_header'), 
         -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
  gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
  separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
  unite(x_cell, y_cell, col = 'cell', remove = F) %>% 
  filter(variable == 'height', !is.na(value)) %>% 
  group_by(Fire, Aspect) %>% 
  summarise(max(date))
  #tally()
  summarise(count = n(),
            minheight = min(value),
            maxheight = max(value),
            first = date[[1]],
            second = date[2],
            third = date[3],
            fourth = date[4],
            fifth = date[5],
            sixth = date[6]
            ) 










