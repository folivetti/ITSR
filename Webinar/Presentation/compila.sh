#!/bin/bash
pandoc -t beamer --highlight-style=pygments --template=template.tex -f markdown-markdown_in_html_blocks+raw_html -o $1.pdf $1.md
#pandoc -t beamer --latex-engine=xelatex --highlight-style=pygments --template=template.tex -o $1.pdf $1.md
