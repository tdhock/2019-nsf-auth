figure-histogram-features-labels.pdf: figure-histogram-features-labels.tex figure-histogram-features-labels.png 
	pdflatex $<
figure-histogram-features-labels.png: figure-histogram-features-labels.R
	R --vanilla < $<
figure-histogram-features/index.html: figure-histogram-features.R
	R --vanilla < $<
