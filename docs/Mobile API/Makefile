all: spec-html spec-pdf

clean:
	rm -rf build

make-build-dir:
	mkdir -p build

spec-html: make-build-dir
	pandoc -s -f markdown Spec.md -o build/Spec.html

spec-pdf: make-build-dir
	pandoc -s --pdf-engine=xelatex --include-in-header=fontoptions.tex \
		-V geometry:margin=0.5in -f markdown Spec.md -o build/Spec.pdf
