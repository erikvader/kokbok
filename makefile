.PHONY: all clean
all:
	cd generator && lein run ".." > ../kokbok-middle.tex
	cat kokbok-pre.tex kokbok-middle.tex kokbok-post.tex > kokbok.tex
	latexmk -pdf -interaction=nonstopmode kokbok

clean:
	latexmk -C
	rm -f kokbok-middle.tex kokbok.tex
	cd generator && lein clean

stripimages:
	find pictures -type f -print0 | xargs -0 exiv2 rm
