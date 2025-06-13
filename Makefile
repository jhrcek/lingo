.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-gild --io lingo.cabal

.PHONY: install
install:
	cabal install --overwrite-policy=always

