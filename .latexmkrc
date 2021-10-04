#!/usr/bin/env perl

$latex = 'uplatex %O %S -synctex=1 -interaction=nonstopmode';
$pdflatex = 'pdflatex %O %S -synctex=1 -interaction=nonstopmode';
$lualatex = 'lualatex %O %S -synctex=1 -halt-on-error -interaction=nonstopmode';
$xelatex = 'xelatex %O %S -synctex=1 -halt-on-error -interaction=nonstopmode';
$biber = 'biber %O %S --bblencoding=utf8 -u -U --output_safechars';
$bibtex = 'pbibtex';
$makeindex = 'upmendex %O -o %D %S';
$dvipdf = 'dvipdfmx %O -o %D %S';
$dvips = 'dvips %O -z -f %S | convbkmk -u > %D';
$ps2pdf = 'ps2pdf %O %S %D';
$pdf_mode = 3;
$pvc_view_file_via_temporary = 0;
$pdf_previewer = 'okular';