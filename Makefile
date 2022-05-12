build:
	yarn install
	stack build
	stack run d12frosted-io -- clean
	stack run d12frosted-io -- build
	cp -r assets/.well-known public/.well-known
	cp -r assets/CNAME public/CNAME
	cp -r assets/site.webmanifest public/site.webmanifest

watch: build
	stack run d12frosted-io -- watch
