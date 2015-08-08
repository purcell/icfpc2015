default:
	cabal build

archive:
	git archive master | bzip2 > dist/sanityinc.tar.bz2
