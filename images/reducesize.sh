name=$(echo "$1" | cut -f 1 -d '.')
convert $name.jpg -resize 125 reduced/$name.jpg
