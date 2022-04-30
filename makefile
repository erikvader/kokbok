.PHONY: all clean
all:
	latexmk -pdf kokbok

clean:
	latexmk -C
