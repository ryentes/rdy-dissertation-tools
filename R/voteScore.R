  #' Scores the votes of different methods of determining careless responders
  #' 
  #' Takes a dataframe of votes as a parameter and returns metrics. for each
  #' method of scoring in my dissertation.
  #' 
  #' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
  #' @param v a dataframe on which to test the classifier
  
  #' @export
  
  voteScore = function(v) {
  
    n = nrow(v)
    truth = v$truth
    
    totalCareless = sum(truth)
    totalCareful = n-totalCareless
    
    
    # Longstring
    ls_simul = confusionMatrix(v$ls_simul_preds, truth)
    ls_cleaned =  v %>% filter(ls_simul_preds == 0)
    ls_rCareless = sum(ls_cleaned$truth)
    ls_rCareful = nrow(ls_cleaned) - ls_rCareless
    ls_info = ls_simul$informedness
    ls_sensitivity = ls_simul$sensitivity
    ls_specificity = ls_simul$specificity
    
    # Even-odd (simultaneous)
    eo_simul = confusionMatrix(v$eo_simul_preds, truth)
    eo_simul_cleaned = v %>% filter(eo_simul_preds == 0)
    eo_simul_rCareless = sum(eo_simul_cleaned$truth)
    eo_simul_rCareful = nrow(eo_simul_cleaned) - eo_simul_rCareless
    eo_simul_info = eo_simul$informedness
    eo_simul_sensitivity = eo_simul$sensitivity
    eo_simul_specificity = eo_simul$specificity
    
    
    # Outlier (simultaneous)
    out_simul = confusionMatrix(v$out_simul_preds, truth)
    out_simul_cleaned = v %>% filter(out_simul_preds == 0)
    out_simul_rCareless = sum(out_simul_cleaned$truth)
    out_simul_rCareful = nrow(out_simul_cleaned) - out_simul_rCareless
    out_simul_info = out_simul$informedness
    out_simul_sensitivity = out_simul$sensitivity
    out_simul_specificity = out_simul$specificity
    
    # Any flag - LS first
    v = v %>% mutate(any_flag_lsf_preds= case_when(ls_simul_preds == 1 | eo_lsf_preds == 1 | out_lsf_preds == 1 ~ 1,
                                                         TRUE ~ 0))
    
    any_flag_lsf = confusionMatrix(v$any_flag_lsf_preds, truth)
    any_flag_lsf_cleaned = v %>% filter(any_flag_lsf_preds == 0)
    any_flag_lsf_rCareless = sum(any_flag_lsf_cleaned$truth)
    any_flag_lsf_rCareful = nrow(any_flag_lsf_cleaned) - any_flag_lsf_rCareless
    any_flag_lsf_info = any_flag_lsf$informedness
    any_flag_lsf_sensitivity = any_flag_lsf$sensitivity
    any_flag_lsf_specificity = any_flag_lsf$specificity
    
    # Any flag - Simultaneous
    v = v %>% mutate(any_flag_simul_preds = case_when(ls_simul_preds == 1 | eo_simul_preds == 1 | out_simul_preds == 1 ~ 1,
                                                         TRUE ~ 0))
    any_flag_simul = confusionMatrix(v$any_flag_simul_preds, truth)
    any_flag_simul_cleaned = v %>% filter(any_flag_simul_preds == 0)
    any_flag_simul_rCareless = sum(any_flag_simul_cleaned$truth)
    any_flag_simul_rCareful = nrow(any_flag_simul_cleaned) - any_flag_simul_rCareless
    any_flag_simul_info = any_flag_simul$informedness
    any_flag_simul_sensitivity = any_flag_simul$sensitivity
    any_flag_simul_specificity = any_flag_simul$specificity
    
    
    # All flags
    v = v %>% mutate(all_flags_preds = case_when(ls_simul_preds == 1 & eo_simul_preds == 1 & out_simul_preds == 1 ~ 1,
                                                 TRUE ~ 0))
    
    all_flags = confusionMatrix(v$all_flags_preds, truth)
    all_flags_cleaned = v %>% filter(v$all_flags_preds == 0)
    all_flags_rCareless = sum(all_flags_cleaned$truth)
    all_flags_rCareful = nrow(all_flags_cleaned) - all_flags_rCareless
    all_flags_info = all_flags$informedness
    all_flags_sensitivity = all_flags$sensitivity
    all_flags_specificity = all_flags$specificity
    
    # ls or agree (ls first)
    v = v %>% mutate(ls_or_agree_lsf_preds = case_when(ls_simul_preds == 1 | (eo_lsf_preds == 1 & out_lsf_preds == 1) ~ 1,
                                                        TRUE ~ 0))
    
    ls_or_agree_lsf = confusionMatrix(v$ls_or_agree_lsf_preds, truth)
    ls_or_agree_lsf_cleaned = v %>% filter(v$ls_or_agree_lsf_preds == 0)
    ls_or_agree_lsf_rCareless = sum(ls_or_agree_lsf_cleaned$truth)
    ls_or_agree_lsf_rCareful = nrow(ls_or_agree_lsf_cleaned) - ls_or_agree_lsf_rCareless
    ls_or_agree_lsf_info = ls_or_agree_lsf$informedness
    ls_or_agree_lsf_sensitivity = ls_or_agree_lsf$sensitivity
    ls_or_agree_lsf_specificity = ls_or_agree_lsf$specificity
    
    # ls or agree (simul)
    v = v %>% mutate(ls_or_agree_simul_preds = case_when(ls_simul_preds == 1 | (eo_simul_preds == 1 & out_simul_preds == 1) ~ 1,
                                                   TRUE ~ 0))
    
    ls_or_agree_simul = confusionMatrix(v$ls_or_agree_simul_preds, truth)
    ls_or_agree_simul_cleaned = v %>% filter(ls_or_agree_simul_preds == 0)
    ls_or_agree_simul_rCareless = sum(ls_or_agree_simul_cleaned$truth)
    ls_or_agree_simul_rCareful = nrow(ls_or_agree_simul_cleaned) - ls_or_agree_simul_rCareless
    ls_or_agree_simul_info = ls_or_agree_simul$informedness
    ls_or_agree_simul_sensitivity = ls_or_agree_simul$sensitivity
    ls_or_agree_simul_specificity = ls_or_agree_simul$specificity
    
    # ls or outlier
    v = v %>% mutate(ls_or_out_lsf_preds = case_when(ls_simul_preds == 1 | out_lsf_preds == 1 ~ 1,
                                                     TRUE ~ 0))
    
    ls_or_out_lsf = confusionMatrix(v$ls_or_out_lsf_preds, truth)
    ls_or_out_lsf_cleaned = v %>% filter(ls_or_out_lsf_preds == 0)
    ls_or_out_lsf_rCareless = sum(ls_or_out_lsf_cleaned$truth)
    ls_or_out_lsf_rCareful = nrow(ls_or_out_lsf_cleaned) - ls_or_out_lsf_rCareless
    ls_or_out_lsf_info = ls_or_out_lsf$informedness
    ls_or_out_lsf_sensitivity = ls_or_out_lsf$sensitivity
    ls_or_out_lsf_specificity = ls_or_out_lsf$specificity
    
    # ls or even-odd
    v = v %>% mutate(ls_or_eo_simul_preds = case_when(ls_simul_preds == 1 | eo_simul_preds == 1 ~ 1,
                                                      TRUE ~ 0))
    
    ls_or_eo_simul = confusionMatrix(v$ls_or_eo_simul_preds, truth)
    ls_or_eo_simul_cleaned = v %>% filter(ls_or_eo_simul_preds == 0)
    ls_or_eo_simul_rCareless = sum(ls_or_eo_simul_cleaned$truth)
    ls_or_eo_simul_rCareful = nrow(ls_or_agree_lsf_cleaned) - ls_or_eo_simul_rCareless
    ls_or_eo_simul_info = ls_or_eo_simul$informedness
    ls_or_eo_simul_sensitivity = ls_or_eo_simul$sensitivity
    ls_or_eo_simul_specificity = ls_or_eo_simul$specificity
    
    # ls or outsq
    v = v %>% mutate(ls_or_outsq_preds = case_when(ls_simul_preds == 1 | outsq_lsf_preds == 1 ~ 1,
                                                   TRUE ~ 0))
                     
    ls_or_outsq = confusionMatrix(v$ls_simul_preds, truth)
    ls_or_outsq_cleaned = v %>% filter(ls_or_outsq_preds == 0)
    ls_or_outsq_rCareless = sum(ls_or_outsq_cleaned$truth)
    ls_or_outsq_rCareful = nrow(ls_or_outsq_cleaned) - ls_or_outsq_rCareless
    ls_or_outsq_info = ls_or_outsq$informedness
    ls_or_outsq_sensitivity = ls_or_outsq$sensitivity
    ls_or_outsq_specificity = ls_or_outsq$specificity
    
    return(
            cbind(
                    totalCareless, totalCareful,
                    ls_info, ls_rCareless, ls_rCareful, ls_sensitivity, ls_specificity, 
                    eo_simul_info, eo_simul_rCareless, eo_simul_rCareful, eo_simul_sensitivity, eo_simul_specificity,
                    out_simul_info, out_simul_rCareless, out_simul_rCareful, out_simul_sensitivity, out_simul_specificity,
                    any_flag_lsf_info, any_flag_lsf_rCareless, any_flag_lsf_rCareful, any_flag_lsf_sensitivity, any_flag_lsf_specificity,
                    any_flag_simul_info, any_flag_simul_rCareless, any_flag_simul_rCareful, any_flag_simul_sensitivity, any_flag_simul_specificity,
                    all_flags_info, all_flags_rCareless, all_flags_rCareful, all_flags_sensitivity, all_flags_specificity,
                    ls_or_agree_lsf_info, ls_or_agree_lsf_rCareless, ls_or_agree_lsf_rCareful, ls_or_agree_lsf_sensitivity, ls_or_agree_lsf_specificity,
                    ls_or_agree_simul_info, ls_or_agree_simul_rCareless, ls_or_agree_simul_rCareful, ls_or_agree_simul_sensitivity, ls_or_agree_simul_specificity,
                    ls_or_out_lsf_info, ls_or_out_lsf_rCareless, ls_or_out_lsf_rCareful, ls_or_out_lsf_sensitivity, ls_or_out_lsf_specificity,
                    ls_or_eo_simul_info, ls_or_eo_simul_rCareless, ls_or_eo_simul_rCareful, ls_or_eo_simul_sensitivity,  ls_or_eo_simul_specificity,
                    ls_or_outsq_info, ls_or_outsq_rCareless, ls_or_outsq_rCareful, ls_or_outsq_sensitivity, ls_or_outsq_specificity
                  )
          )
  }