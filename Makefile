notes:
	eldev exec '(porg-run "d12frosted.io")'

build:
	yarn install
	stack build
	stack run d12frosted-io -- clean
	stack run d12frosted-io -- build
	cp -r assets/.well-known _site/.well-known
	cp -r assets/CNAME _site/CNAME
	cp -r assets/site.webmanifest _site/site.webmanifest

watch: build
	stack run d12frosted-io -- watch
