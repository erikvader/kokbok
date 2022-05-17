.PHONY: all clean
all:
	latexmk -pdf -interaction=nonstopmode kokbok

clean:
	latexmk -C
	find recipes -name '*.aux' -type f -delete

stripimages:
	find pictures -type f -print0 | xargs -0 exiv2 rm
