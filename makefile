.PHONY: all clean
all:
	latexmk -pdf -interaction=nonstopmode kokbok

clean:
	latexmk -C
	find recipes -name '*.aux' -type f -delete
