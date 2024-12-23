#!/usr/bin/env bash
#
# Generate HTML from Markdown
#
# Inputs:
# - ./source_md
# Outputs:
# - ../docs
#
# Note: this script must be run from its parent directory (as `./generate.sh`)
#

set -euo pipefail

IN=source_md
OUT=../docs
TMP=generated_md
mkdir -p $OUT
mkdir -p $TMP

PANDOC="pandoc -d config/pandoc-defaults.yml --template=config/template.html"

mapfile -t filename <config/file-list.txt

chapterfile="$TMP"/chapters.md
titlesuffix=" - Learn You a Haskell for Great Good!"

cp source_md/chapters_head.md $chapterfile

# Generate temporary TOC file ($chapterfile)
for i in "${!filename[@]}"
do
<<<<<<< HEAD
    sourcemd=$IN/${filename[$i]}.md
    
    title[i]=$(sed -n '/^# /s/# //p;' "$sourcemd" | sed 's/{.*//' | sed 's/ *$//g')
||||||| parent of c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
    sourcemd=source_md/${filename[$i]}.md
    
    title[$i]=$(sed -n '/^# /s/# //p;' $sourcemd | sed 's/{.*//' | sed 's/ *$//g')
=======
    sourcemd=source_md/${filename[$i]}.md

    title[$i]=$(sed -n '/^# /s/# //p;' $sourcemd | sed 's/{.*//; s/ *$//g')
>>>>>>> c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)

    chnum=$((i + 1))
    if [[ $chnum -ge 10 ]];
    then
        sp=" "
    else
        sp="  "
    fi

<<<<<<< HEAD
    sed -n '/^#\{1,2\} /p' "$sourcemd" \
        | sed "s/^#  *\(.*[^ ]\) *{.*/$chnum.${sp}[\1](${filename[$i]}.html)/" \
        | sed "s/^#  *\(.*[^ ]\) */$chnum.${sp}[\1](${filename[$i]}.html)/" \
        | sed "s/^##  *\(.*[^ ]\) *{ *#\(.*\)}/    * [\1](${filename[$i]}.html\#\2)/" \
        >>$chapterfile
||||||| parent of c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
    sed -n '/^#\{1,2\} /p' $sourcemd \
        | sed "s/^#  *\(.*[^ ]\) *{.*/$chnum.$sp[\1](${filename[$i]}.html)/" \
        | sed "s/^#  *\(.*[^ ]\) */$chnum.$sp[\1](${filename[$i]}.html)/" \
        | sed "s/^##  *\(.*[^ ]\) *{ *#\(.*\)}/    * [\1](${filename[$i]}.html\#\2)/" \
        >>$chapterfile
=======
    grep '^#\{1,2\} ' $sourcemd \
        | sed "s/^#  *\(.*[^ ]\) *{.*/$chnum.$sp[\1](${filename[$i]}.html)/;
               s/^#  *\(.*[^ ]\) */$chnum.$sp[\1](${filename[$i]}.html)/;
               s/^##  *\(.*[^ ]\) *{ *#\(.*\)}/    * [\1](${filename[$i]}.html\#\2)/" \
        >> $chapterfile
>>>>>>> c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
done

# For every input MD file produce and HTML file
for i in "${!filename[@]}"
do
    # back/next-links business
    if ((i <= 0))
    then
        prev_title=
        prev_filename=
    else
        prev=$((i - 1))
        prev_title="${title[$prev]}"
        prev_filename=${filename[$prev]}
    fi
    if ((i >= ${#filename[@]} - 1))
    then
        next_title=
        next_filename=
    else
        next=$((i + 1))
        next_title="${title[$next]}"
        next_filename=${filename[$next]}
    fi
<<<<<<< HEAD
    
    $PANDOC \
||||||| parent of c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
    
    pandoc -d config/pandoc-defaults.yml --template=config/template.html \
=======

    pandoc -d config/pandoc-defaults.yml --template=config/template.html \
>>>>>>> c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
        -V footdiv=true -V title="${title[$i]}" \
        --metadata title="${title[$i]}$titlesuffix" \
<<<<<<< HEAD
        -V prev_title="$prev_title" -V prev_filename="$prev_filename" \
        -V next_title="$next_title" -V next_filename="$next_filename" \
        -o "$OUT"/"${filename[$i]}".html "$IN"/"${filename[$i]}".md

   sed '/<p><img/ { N; N; N; s#<p>\(<img[^>]*\) /></p>#\1># }
       s# />#>#' -i "$OUT"/"${filename[$i]}".html
||||||| parent of c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
        -V prev_title="$prev_title" -V prev_filename=$prev_filename \
        -V next_title="$next_title" -V next_filename=$next_filename \
        -o generated_html/${filename[$i]}.html source_md/${filename[$i]}.md   
=======
        -V prev_title="$prev_title" -V prev_filename=$prev_filename \
        -V next_title="$next_title" -V next_filename=$next_filename \
        -o generated_html/${filename[$i]}.html source_md/${filename[$i]}.md
>>>>>>> c1fa241 (re-generate html, concatenate sed commands, remove trailing whitespace)
done

cat "$IN"/chapters_foot.md >>$chapterfile

$PANDOC \
    -V title="Chapters" --metadata title="${title[$i]}$titlesuffix" \
    -o "$OUT"/chapters.html $chapterfile

sed 's/<ol/<ol class="chapters"/' -i "$OUT"/chapters.html

$PANDOC \
    -V faq=true -V title="FAQ" --metadata title="${title[$i]}$titlesuffix" \
    -o "$OUT"/faq.html source_md/faq.md

   sed '/<p><img/ { N; N; N; s#<p>\(<img[^>]*\) /></p>#\1># }
       s# />#>#' -i "$OUT"/faq.html

rm -rf $TMP
