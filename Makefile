build:
	yarn install
	cabal build
	cabal run d12frosted-io -- clean
	cabal run d12frosted-io -- build
	cp -r assets/.well-known public/.well-known
	cp -r assets/CNAME public/CNAME
	cp -r assets/site.webmanifest public/site.webmanifest

watch: build
	cabal run d12frosted-io -- watch
