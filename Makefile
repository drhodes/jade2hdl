
build: ## build
	stack build

app: FORCE ## run the app
	stack exec -- jade2hdl --infile ${USER}.json --enthusiasm 10

test: ## test
	echo test

clean: ## clean all the things
	echo implement clean makefile rule

work: ## open all files in editor
	$(shell emacs -nw `find src app test -type f -name '*.hs' -o -name '*.yaml'`)

setup:
	touch battle-plan.org
	mkdir -p design

add: clean ## add files to the git repo
	git add -A :/

commit: ## git commit -a
	git commit -a


# http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

FORCE:

