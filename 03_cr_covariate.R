#fully format covariates, and make one row per individual

#taste
cr_cov_formatted_taste <- func_formatting(input_data=cr_taste_base, cr_inclusion =  cr_inclusion_taste)

#validate
cr_cov_formatted_validate <- func_formatting(input_data=cr_validate_base, cr_inclusion =  cr_inclusion_validate)

rm(cr_taste_base, cr_validate_base, func_formatting)
