# $find_counterfactuals returns meaningful error if x_interest does not contain all columns of predictor$data$X

    Assertion on 'names(x_interest)' failed: Must include the elements {cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb}.

# $find_counterfactuals returns meaningful error if x_interest has unexpected column types

    Columns that appear in `x_interest` and `predictor$data$X` must have the same types.

# $find_counterfactuals returns meaningful error if x_interest already has desired properties

    `x_interested` is already predicted with `desired_outcome`.

