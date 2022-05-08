.PHONY: all clean
all:
	latexmk -pdf -interaction=nonstopmode kokbok

clean:
	latexmk -C
