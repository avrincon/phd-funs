# avr

### Calculate duration
List of functions used when calculating the duration of a behaviour:

- duration_calculator()

_helper functions:_

- ap_sb_gr_duration_calculator()  
- prot_oos_duration_calculator()  
- sec_to_hms()  

### CSI functions
Functions that are usefull when calculating the CSI:  

- obs_time_calculator()
- obs_time_matrix()
- dyad_obs_total()
- get_obs_time_s()
- freq_matrix()
- transp_sum_matrix()
- sum_duration()
- dur_matrix()


### Exclude double focal interactions
List of functions used when double focal interactions.
i.e. when two focal animals interact with each other:  

- check_part()  
- get_p_dyad_count()  
- decider_same_count()  
- decider_diff_count()  
- decider()  
- get_int_decision()  
- excl_dbl_int()  

### highlight si/ei of da/pa
Functions that are useful to calculate da/pa interaction chunks

- check_da_ei()
- add_trackers()
- highlight_sibc()
- highlight_si()
- highlight_si_rev()
- map_nest()

### classify da_pa_ids
- classify_da_id()
_helper functions:_
- class_da_id()
- class_agg_action_intensity()
