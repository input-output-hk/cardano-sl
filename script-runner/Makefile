dist/build/poc/poc: AutomatedTestRunner.hs BlockParser.hs Poc.hs BrickUI.hs NodeControl.hs
	runhaskell Setup.hs build -j10 poc

poc-test: dist/build/poc/poc
	mkdir -p poc-state
	./dist/build/poc/poc --configuration-file ../lib/configuration.yaml --log-console-off --db-path poc-state/db --keyfile poc-state/secret.key --log-config log-config.yaml --logs-prefix poc-state/logs/ --topology topology-local.yaml

.PHONY: poc-test
