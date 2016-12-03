

INPUT=unoread.md
OUTPUT=unoread

GENERIC_OPTIONS=-N --toc ${INPUT}
CMD_PDF=${GENERIC_OPTIONS} -o ${OUTPUT}.pdf
CMD_HTML=${GENERIC_OPTIONS} --mathjax -s --highlight-style pygments -o ${OUTPUT}.html

PANDOC_WATCH=pandoc-watch/pandocwatch.py

all: pdf

pdf:
	pandoc ${CMD_PDF}

html:
	pandoc ${CMD_HTML}

watch:
	${PANDOC_WATCH} ${CMD_PDF}

watch-html:
	${PANDOC_WATCH} ${CMD_HTML}
