help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the cardano-sl-node package
	ghcid \
	    --command "stack ghci cardano-sl-node --ghci-options=-fno-code"

.PHONY: ghcid help
