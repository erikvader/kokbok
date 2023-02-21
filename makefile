.PHONY: all
all: pdf

.PHONY: pdf
pdf: kokbok.tex versions
	latexmk -pdf -interaction=nonstopmode kokbok

.PHONY: kokbok.tex
kokbok.tex: middle
	cat kokbok-pre.tex kokbok-middle.tex kokbok-post.tex > kokbok.tex

.PHONY: middle
middle:
	cd generator && lein run ".." > ../kokbok-middle.tex

.PHONY: versions
versions:
	echo "\\newcommand{\\shortVersion}{$$(git describe --tags --abbrev=0 | cut -c 2-)}" > versions.tex
	if git describe --exact-match --tags HEAD >/dev/null 2>&1 && git diff --quiet; then \
		echo -E "\\newcommand{\\longVersion}{}" >> versions.tex; \
	else \
		echo -E "\\newcommand{\\longVersion}{$$(git describe --tags --dirty --long)}" >> versions.tex; \
	fi

.PHONY: clean
clean:
	latexmk -C
	rm -f kokbok-middle.tex kokbok.tex versions.tex
	cd generator && lein clean

.PHONY: stripimages
stripimages:
	find pictures -type f -print0 | xargs -0 exiv2 rm
