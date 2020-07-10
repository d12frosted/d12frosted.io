build:
	yarn install
	stack setup
	stack install --only-dependencies
	stack build
	stack exec -- d12frosted-io clean
	stack exec -- d12frosted-io build
	cp -r assets/.well-known public/.well-known
	cp -r assets/CNAME public/CNAME
	cp -r assets/site.webmanifest public/site.webmanifest

watch: build
	stack exec -- d12frosted-io watch
