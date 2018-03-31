library("covr")

cov <- package_coverage()

function_coverage(fun =  sec_to_hms, code = sec_to_hms("l"))

# shine(cov) - shine() is deprecated, use report()
report(cov)


!is.numeric("a")
