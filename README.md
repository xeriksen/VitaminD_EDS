# VitaminD_EDS
Vitamin D and Quality of Life in Ehlers Danlos Syndrome

For this project, we used the National Institutes of Health’s \textit{All of Us} database. Our research focuses on the genetic connective tissue disorder Ehlers-Danlos syndrome (EDS), examining the overlap between EDS and Vitamin D deficiency (VDD) by comparing the quality of life between those with a deficiency and those without. 

In All of Us, we had two primary cohorts, starting with an initial parameter of an EDS diagnosis. We then refined the participants into two distinct groups: 
    Individuals with EDS and a diagnosis of vitamin D deficiency (n=446)
    Individuals without a VDD (n=787), but with EDS. 

Next, we applied a dataset that utilizes the All of Us Quality of Life rating to both groups. We recorded the frequency of each answer (excellent, very good, good, fair, poor) for each group. To ensure meaningful statistical comparisons, those who responded with "very good" or "excellent" were combined into a singular category, resulting in a four-level ordinal scale.

The frequency of each QoL survey question response provided the basis for our statistical analysis and graphing. Statistical modeling and data visualization were done using R, with statistical significance defined by an alpha level of 0.05. To evaluate the data we used three statistical tests:

    -- Mann-Whitney U Test: A non-parametric test used to compare the overall shift in median QoL ratings between the deficient and non-deficient study groups.
    
    -- Cochran-Armitage Test: Used to identify a linear trend in proportions of Vitamin D deficiencies and QoL ratings.
    
    -- Ordinal Logistical Regression: A Proportional Odds Model used for calculating an Odds Ratio and 95\% Confidence Intervals was fit to the data to quantify the likelihood of a patient reporting a higher quality of life based on their Vitamin D status.
