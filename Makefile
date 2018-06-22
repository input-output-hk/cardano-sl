help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

lint: ## Run hlint for the project
	./scripts/haskell/lint.sh

stylish: ## Run stylish-haskell on the entire project
	./scripts/haskell/stylish.sh

build-all: ## Build everything, including benchmarks and tests, but don't run them
	stack build --fast --bench --no-run-benchmarks --test --no-run-tests

ghcid: ## Pass DIR=package-directory to run that directory's ghcid command.
ifeq ($(DIR),)
	echo "You must specify the package directory for this command."
else
	cd $(DIR) && make ghcid
endif

ghcid-test: ## Pass DIR=package-directory to run that directory's ghcid command.
ifeq ($(DIR),)
	echo "You must specify the package directory for this command."
else
	cd $(DIR) && make ghcid-test
endif

.PHONY: help stylish lint ghcid ghcid-test build-all
