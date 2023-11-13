Thanks again for working on this.

(I) So for control variables you already have:

1) Being male (or female)
2) Academic grades
3) Parental smoking
4) Sibling smoking
5) One more to add is number of rooms in the household which is

t1_i10; t2_i10; & t3_i10 (we don’t have it in wave 4 so carry forward)

(II) Exposure Variables

1) Percent direct ties to smokers

1a) (Count of direct tie smokers)

2) Perceived use, variables

   t1_a7_f1, t1_a7_f2, t1_a7_f3, t1_a7_f4, t1_a7_f5, t1_a7_f6, t1_a7_f7

   t2_a7_f1, t2_a7_f2, t2_a7_f3, t2_a7_f4, t2_a7_f5, t2_a7_f6, t2_a7_f7

   t3_a7_f1, t3_a7_f2, t3_a7_f3, t3_a7_f4, t3_a7_f5, t3_a7_f6, t3_a7_f7

   t4_a7_f1, t4_a7_f2, t4_a7_f3, t4_a7_f4, t4_a7_f5, t4_a7_f6, t4_a7_f7

These are currently coded as 1=Yes 2=No. so they will need to be recoded to 1=Yes 0=No and perceived use calculated as both a percent and count.

3) Exposure weighted by tie strength which is variables:

   t1_a12_f1, t1_a12_f2, t1_a12_f3, t1_a12_f4, t1_a12_f5, t1_a12_f6, t1_a12_f7

t2_a12_f1, t2_a12_f2, t2_a12_f3, t2_a12_f4, t2_a12_f5, t2_a12_f6, t2_a12_f7

t3_a12_f1, t3_a12_f2, t3_a12_f3, t3_a12_f4, t3_a12_f5, t3_a12_f6, t3_a12_f7

which vary from 1 to 5 with 5 being the most close.

4) Indirect ties by including 2-step ties, these can be included as is, or divided by 2 (the inverse of the path distance).
5) Structural equivalence
6) Simmelian Ties (not sure you can program this one): each alter’s smoking behavior is multiplied by the number ties they share with ego. So in figure 1 B’s smoking would be multiplied by 1 (both ego and B are connected to A) whereas A’s smoking behavior is multiplied by 2 as it shares ties to B and F with Ego.
7) Density weighted, exposure is multiplied by ego density.
8) Weighted by each alter’s in-degree centrality score
9) Weighted on sex homophily, i.e., exposure calculated only on same-sex ties
10) Joint participation on the same team we’ll use the 3 most popular sports, basketball, soccer, and running/track:

    t1_e1_2 t1_e1_10 t1_e1_14

t2_e1_2 t2_e1_10 t2_e1_14

t3_e1_2 t3_e1_10 t3_e1_14

t4_e1_2 t4_e1_10 t4_e1_14

(III) Regression

It would be great to write out the data (as you have done) and also estimate the regression models in R so there is one file to accompany the submission. I’ve generally included dummy variables for the schools omitting school 111 to act as the reference. LMK if this is too much and run the regressions in STATA.

Again, thanks very much, please reach out with questions if you have them. I’ll be teaching from 10:00 to 12:30 on Monday.
