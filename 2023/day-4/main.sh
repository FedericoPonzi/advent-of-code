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

solve1() {
    filename=$1
    total=0
    while read -r line; do 
        line_points=$(get_points "$line")
        #echo $line_points;
        total=$((total+line_points))
    done < $filename
    echo $total;
}
example=$(solve1 "example.txt")
if [ $example != 13 ]; then 
    echo "Example failed: received $example, expected: 13";
    exit 1;
fi

input=$(solve1 "input.txt")
if [ $input != 20107 ]; then 
    echo "part 1 failed: received $example, expected: 20107";
    exit 1;
fi

# associative array with the card copies
declare -A copies

get_points2() {
    line_number=$1
    line=$2
    line=${line#*: }
    IFS="|" read -a cards <<<"$line"
    IFS=" " read -a winning_numbers <<<"${cards[0]}"
    IFS=" " read -a owned_numbers <<<"${cards[1]}"
    #echo "Winning numbers: ${winning_numbers[*]}"
    #echo "Owned numbers: ${owned_numbers[*]}"
    # check how many owned numbers are in winning numbers
    total=0
    for i in "${owned_numbers[@]}"; do
        for j in "${winning_numbers[@]}"; do
            if [ "$i" = "$j" ]; then
                    total=$((total+1))
                break
            fi
        done
    done
    #check if total is -1, if so, then the card is not won
    if [ $total = 0 ]; then
        return;
    fi

    my_copies=${copies[$line_number]}

    for ((i=$line_number+1; i<=$line_number+$total;i++)); do
        if [ "${copies[$i]}" = "" ]; then
            copies[$i]=1;
        fi
        copies[$i]=$(($my_copies+copies[$i]));
        done
}

solve2() {
    filename=$1
    line_number=1
    copies[1]=1
    lines=$(wc -l $filename | awk '{print $1}')
    for ((i=1;i<=$lines;i++)); do
        copies[$i]=1;
    done
    while read -r line; do 
        get_points2 $line_number "$line"
        line_number=$((line_number+1))
    done < $filename

    # sum all values of copies
    total=0
    for i in "${!copies[@]}"; do
        total=$((total+copies[$i]))
    done
    echo $total;
}
example=$(solve2 "example.txt")
if [ $example != 30 ]; then
    echo "Example failed: received $example, expected: 30";
    exit 1;
fi

input=$(solve2 "input.txt")
if [ $input != 30 ]; then
    echo "Input failed: received $input, expected: 30";
    exit 1;
fi

