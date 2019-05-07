build:
	yarn install
	stack setup
	stack install --only-dependencies
	stack build
	stack exec -- d12frosted-io build
	cp -r assets/.well-known public/.well-known

clean: build
	stack exec -- d12frosted-io clean

watch: clean
	stack exec -- d12frosted-io watch
