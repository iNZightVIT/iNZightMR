R := R --vanilla --slave
document:
	@$(R) -e "devtools::document()"

check:
	@$(R) -e "devtools::check()"

revcheck:
	@$(R) -e "devtools::use_revdep()"
	@$(R) -f "revdep/check.R"

crancheck:
	@R CMD build .
	@R CMD check *.tar.gz


install:
	@R CMD INSTALL ./

test:
	@$(R) -e "devtools::test()"

clean:
	@rm -rf *.tar.gz *.Rcheck revdep

README.md: README.Rmd
	$(R) -e "rmarkdown::render('$^')"
	rm README.html

BRANCH := $(shell git branch --show-current | sed 's/[a-z]*\///')
releasePRs:
	@echo Creating PR to master
	@gh pr create -a "@me" -b "" -B master -l "release" -p "Tom" -t "Release $(BRANCH)"
	@echo Creating PR to dev
	@gh pr create -a "@me" -b "" -B dev -l "release" -p "Tom" -t "Release $(BRANCH) into dev"

site: document install
	@cp NEWS.Md NEWS.md
	@$(R) -e "pkgdown::build_site()"
