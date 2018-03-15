while IFS='' read -r line || [[ -n "$line" ]]; do
    egrep -i "$line" noiseResults.out > "$line.ppc"
done < "$1"
