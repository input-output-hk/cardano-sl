help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the cardano-sl-chain package
	ghcid \
	    --command "stack ghci cardano-sl-chain --ghci-options=-fno-code"

ghcid-test: ## Have ghcid run the test suite for the wallet-new-specs on successful recompile
	ghcid \
	    --command "stack ghci cardano-sl-chain:lib cardano-sl-chain:test:test --ghci-options=-fobject-code" \
	    --test "Main.main"

.PHONY: ghcid ghcid-test help
