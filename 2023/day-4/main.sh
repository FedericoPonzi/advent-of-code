#!/bin/env bash
set -e

get_points() {
    line=$1
    line=${line#*: }
    #echo $line;
    IFS="|" read -a cards <<<"$line"
    IFS=" " read -a winning_numbers <<<"${cards[0]}"
    IFS=" " read -a owned_numbers <<<"${cards[1]}"
    #echo "Winning numbers: ${winning_numbers[*]}"
    #echo "Owned numbers: ${owned_numbers[*]}"
    # check how many owned numbers are in winning numbers
    total=-1
    for i in "${owned_numbers[@]}"; do
        for j in "${winning_numbers[@]}"; do
            if [ "$i" = "$j" ]; then

                total=$((total+1))
                break
            fi
        done
    done
    #check if total is -1, if so, then the card is not won
    if [ $total = -1 ]; then
        echo 0;
    else 
        # 2 to the power of total:
        echo $((2**total))
    fi
}

solve() {
    filename=$1
    total=0
    while read -r line; do 
        line_points=$(get_points "$line")
        #echo $line_points;
        total=$((total+line_points))
    done < $filename
    echo $total;
}
example=$(solve "example.txt")
if [ $example != 13 ]; then 
    echo "Example failed: received $example, expected: 13";
    exit 1;
fi

input=$(solve "input.txt")
if [ $input != 20107 ]; then 
    echo "part 1 failed: received $example, expected: 20107";
    exit 1;
fi

