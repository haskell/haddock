deps: ## Install the dependencies of the backend
	@cabal build all --only-dependencies

build: ## Build the project in fast mode
	@cabal build all -O1

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl haddock-library

test: ## Run the test suite
	@cabal test all

lint: ## Run the code linter (HLint)
	@find "haddock-library" "haddock-api" "haddock-test" "hoogle-test" "html-test" "hypsrc-test" "latex-test" \( -name "*.hs" -not -name "InterfaceFile.hs" \)  | xargs -P 12 -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	# @fourmolu -q --mode inplace test src ## The world is not ready yet
	@cabal-fmt -i *.cabal

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
