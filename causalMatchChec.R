# treatment
treatexample <- treatment_factory$new()
treatexample$effectLinear <- '1.5 *-x1- + 4'

# Test data
Mystery <- location_factory$new()
Mystery$create_sample(10, 2, 'mystery', 123)
Mystery$createY0()
Mystery$createY1()
Mystery$show_sample_distribution()

Known <- location_factory$new()
Known$create_sample(10, -2, 'known', 123)
Known$createY0()
Known$createY1()
Known$show_sample_distribution()

# Assign treatments
Known$assignTreatment(123)
Mystery$assignTreatment(123)
Mystery$ate

out_t <- match_to_k(Mystery, Known, t = 1, 'x1')

save_minima_t <- get_outcomes(out_t)

out_c <- match_to_k(Mystery, Known, t = 0, 'x1')

save_minima_c <- get_outcomes(out_c)
attr(save_minima_t,'index.list')
mean(save_minima_t) - mean(save_minima_c)

Mystery$ate


# Check
Mystery$dfTarget[rownames(Mystery$dfTarget) == '3', ]
Known$dfObserved[rownames(Known$dfObserved) == '3', ]

