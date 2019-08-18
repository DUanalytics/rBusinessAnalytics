#Factor Manipulation 
#https://r4ds.had.co.nz/factors.html
#(https://cran.r-project.org/web/packages/forcats/forcats.pdf)

library(forcats)


programID = c('BTECH','BBA','MBA', 'MTECH', 'PHD')
set.seed(1234)
programs = sample(programID, size=100, replace=T, prob=c(.3, .2, .2, .2, .1))
programs

(programsF = as_factor(programs))

fct_anon(programsF, prefix ='')
fct_anon(programsF, prefix ='C')

#concatenate Factors
(fa <- factor(c('MBA', 'MBA')))
(fb <- factor(c('BBA','BBA','BBA')))
(fab <- factor(c('MBA', 'MBA', 'BBA','BBA')))
c(fa, fb, fab)  #see the problem
fct_c(fa, fb, fab)
# You can also pass a list of factors with !!!
fs <- list(fa, fb, fab)
fct_c(!!!fs)

#collapse

fct_collapse(programsF, MGT=c('MBA','BBA'), ENGG=c('BTECH', 'MTECH'))



fct_count(programsF)
fct_inorder(programsF, ordered = NA)
fct_inorder(programsF, ordered = T)


programsFna = factor(c(programsF, rep(NA,5)))
fct_explicit_na(programsFna)

fct_inorder(programsF, ordered = NA)
fct_infreq(programsF, ordered = NA)

fct_lump(programsF, n=2)
fct_lump(f, n, prop, w = NULL, other_level = "Other",
         ties.method = c("min", "average", "first", "last", "random", "max"))

fct_other(programsF, keep = c("BBA", "MBA"))
fct_other(programsF, drop = c("BBA", "MBA"))

fct_recode(programsF, MBA2 = "MBA", BBA2 = "BBA")

fct_relevel(programsF)
fct_relevel(programsF,'MBA')
fct_relevel(programsF,c('MBA','BBA'))
fct_relevel(programsF,'MBA', after=2)

fct_rev(programsF)

fct_shift(programsF, n = 1L)
fct_shift(programsF, n = 2L)
programsF

fct_shuffle(programsF)

fct_unify(list(factor(programsF[1:10]), factor(programsF[11:21])))


fct_unique(programsF)

lvls_reorder(programsF, 5:1)
lvls_revalue(programsF, c("BBA", "BTECH", "MBA", 'MTECH', 'PHD'))
lvls_expand(programsF, c("BBA", "BTECH", 'PHD', "MBA", 'MTECH'))

lvls_union(list(programsF))

lvls_union(list(factor(programsF[1:10]), factor(programsF[11:21])))

fct_unify(list(factor(programsF[1:10]), factor(programsF[11:21])))
