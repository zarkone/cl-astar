all: build run
run: 
	okular build/lab.pdf 

build: *.tex
	mkdir -p build

	xelatex --output-directory=build/ \
	-interaction nonstopmode \
	-halt-on-error	 \
	-file-line-error \
	-shell-escape lab.tex

clean:
	rm -rf build/*

