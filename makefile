TOMLS := $(shell find recipes -type f) books.toml ingredients.toml
CLJS := $(shell find generator/src -type f)

MIDDLE_TEX := kokbok-middle.tex
KOKBOK_PARTS := kokbok-pre.tex $(MIDDLE_TEX) kokbok-post.tex

KOKBOK_TEX := kokbok.tex
VERSIONS_TEX := versions.tex

PDF := kokbok.pdf

.PHONY: all
all: $(PDF)

$(PDF): $(KOKBOK_TEX) $(VERSIONS_TEX)
	latexmk -pdf -interaction=nonstopmode kokbok

$(KOKBOK_TEX): $(KOKBOK_PARTS)
	cat $^ > $@

$(MIDDLE_TEX): $(CLJS) $(TOMLS)
	cd generator && lein run ".." > ../$@

#NOTE: I don't think there is a good set of files to set as a dependency for this one
.PHONY: $(VERSIONS_TEX)
$(VERSIONS_TEX):
	printf '\\newcommand{\\shortVersion}{%s}\n' "$$(git describe --tags --abbrev=0 | cut -c 2-)" > $@
	if git describe --exact-match --tags HEAD >/dev/null 2>&1 && git diff --quiet; then \
		printf '\\newcommand{\\longVersion}{}' >> $@; \
	else \
		printf '\\newcommand{\\longVersion}{%s}\n' "$$(git describe --tags --dirty --long)" >> $@; \
	fi

.PHONY: clean
clean:
	latexmk -C
	rm -f $(MIDDLE_TEX) $(KOKBOK_TEX) $(VERSIONS_TEX)
	cd generator && lein clean

.PHONY: stripimages
stripimages:
	find pictures -type f -print0 | xargs -0 exiv2 rm
