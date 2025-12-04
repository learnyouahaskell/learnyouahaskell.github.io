#!/usr/bin/env bash
set -e

mapfile -t filename <config/file-list.txt

chapterfile=generated_md/chapters.md
titlesuffix=" - Learn You a Haskell for Great Good!"

cp source_md/chapters_head.md $chapterfile

for i in ${!filename[@]}
do
    sourcemd=source_md/${filename[$i]}.md
    
    title[$i]=$(sed -n '/^# /s/# //p;' $sourcemd | sed 's/{.*//' | sed 's/ *$//g')

    chnum=$(($i + 1))
    if [[ $chnum -ge 10 ]];
    then
        sp=" "
    else
        sp="  "
    fi

    sed -n '/^#\{1,2\} /p' $sourcemd \
        | sed "s/^#  *\(.*[^ ]\) *{.*/$chnum.$sp[\1](${filename[$i]}.html)/" \
        | sed "s/^#  *\(.*[^ ]\) */$chnum.$sp[\1](${filename[$i]}.html)/" \
        | sed "s/^##  *\(.*[^ ]\) *{ *#\(.*\)}/    * [\1](${filename[$i]}.html\#\2)/" \
        >>$chapterfile
done

for i in ${!filename[@]}
do
    if (($i <= 0))
    then
        prev_title=
        prev_filename=
    else
        prev=$(($i - 1))
        prev_title="${title[$prev]}"
        prev_filename=${filename[$prev]}
    fi
    if (($i >= ${#filename[@]} - 1))
    then
        next_title=
        next_filename=
    else
        next=$(($i + 1))
        next_title="${title[$next]}"
        next_filename=${filename[$next]}
    fi
    
    pandoc -d config/pandoc-defaults.yml --template=config/template.html \
        -V footdiv=true -V title="${title[$i]}" \
        --metadata title="${title[$i]}$titlesuffix" \
        -V prev_title="$prev_title" -V prev_filename=$prev_filename \
        -V next_title="$next_title" -V next_filename=$next_filename \
        -o generated_html/${filename[$i]}.html source_md/${filename[$i]}.md   
done

cat source_md/chapters_foot.md >>$chapterfile

pandoc -d config/pandoc-defaults.yml --template=config/template.html \
    -V title="Chapters" --metadata title="${title[$i]}$titlesuffix" \
    -o generated_html/chapters.html $chapterfile

pandoc -d config/pandoc-defaults.yml --template=config/template.html \
    -V faq=true -V title="FAQ" --metadata title="${title[$i]}$titlesuffix" \
    -o generated_html/faq.html source_md/faq.md
