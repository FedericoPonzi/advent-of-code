<?php

function load_map($filePath) {
    if (!file_exists($filePath)) {
        echo 'File not found.' . PHP_EOL;
        exit(1);
    }
    $ret = array();

    $fileContent = file($filePath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    if (empty($fileContent)) {
        echo 'The file is empty.' . PHP_EOL;
        exit(1);
    }
    foreach ($fileContent as $row) {
        $ret[] = str_split($row);
    }
    return $ret;
}

function find_empty_and_galaxies($map){
    $row = array_fill(0, count($map), true);
    $col = array_fill(0, count($map[0]), true);
    //iterate on all rows and cols:
    $galaxies = array();
    for ($i = 0; $i < count($map); $i++) {
        for ($j = 0; $j < count($map[$i]); $j++) {
            if ($map[$i][$j] == '#') {
                $row[$i] = false;
                $col[$j] = false;
                $galaxies[] = [$i, $j];
            }
        }
    }
    $empty_rows = array();
    for ($i = 0; $i < count($row); $i++) {
        if ($row[$i]) {
            $empty_rows[] = $i;
        }
    }
    $empty_cols = array();
    for ($i = 0; $i < count($col); $i++) {
        if ($col[$i]) {
            $empty_cols[] = $i;
        }
    }
    return [$empty_rows, $empty_cols, $galaxies];

}

function adjust_galaxy_positions($empty_rows, $empty_cols, $galaxies, $expansion) {
    // copy galaxies because position updates should be based on the
    // original position of the galaxy.
    $new_galaxies = $galaxies;
    for ($i = 0; $i < count($galaxies); $i++) {
        // adjust rows:
        for ($j = 0; $j < count($empty_rows); $j++) {
            if ($galaxies[$i][0] > $empty_rows[$j]) {
                $new_galaxies[$i][0]+=$expansion;
            }
        }
        // adjust cols:
        for ($j = 0; $j < count($empty_cols); $j++) {
            if ($galaxies[$i][1] > $empty_cols[$j]) {
                $new_galaxies[$i][1]+=$expansion;
            }
        }
    }
    return $new_galaxies;
}
function compact_array_print($arr) {
    echo '[';
    for ($i = 0; $i < count($arr); $i++) {
        if ($i > 0) {
            echo ', ';
        }
        if (is_array($arr[$i])) {
            compact_array_print($arr[$i]);
            continue;
        }
        echo $arr[$i];
    }
    echo ']';
}
function find_distance_between_galaxies($point1, $point2): int
{
    return abs($point1[0] - $point2[0]) + abs($point1[1] - $point2[1]);
}

function find_distance_sum($galaxies) {
    $distances = find_all_distances($galaxies);
    $sum = 0;
    for ($i = 0; $i < count($distances); $i++) {
        $sum += $distances[$i];
    }
    return $sum;
}
function find_all_distances($galaxies): array
{
    $distances = array();
    for ($i = 0; $i < count($galaxies); $i++) {
        for ($j = $i + 1; $j < count($galaxies); $j++) {
            $distances[] = find_distance_between_galaxies($galaxies[$i], $galaxies[$j]);
        }
    }
    return $distances;
}


function solve($filePath, $expansion) {
    $expansion = $expansion - 1;
    $map = load_map($filePath);
    [$empty_row, $empty_col, $galaxies] = find_empty_and_galaxies($map);
    $galaxies = adjust_galaxy_positions($empty_row, $empty_col, $galaxies, $expansion);
    return find_distance_sum($galaxies);
}

function main()
{
    $res = solve("./example.txt", 2);
    echo "Received: ".  $res . ". Expected: 374.". PHP_EOL;
    assert($res == 374);
    $res = solve("./input.txt", 2);
    echo "Received: ".  $res . ". Expected: 10165598.". PHP_EOL;
    assert($res == 10165598);

    $res = solve("./example.txt", 10);
    echo "Received: ".  $res . ". Expected: 1030.". PHP_EOL;
    assert($res == 1030);

    $res = solve("./example.txt", 100);
    echo "Received: ".  $res . ". Expected: 8410.". PHP_EOL;
    assert($res == 8410);

    $res = solve("./input.txt", 1000000);
    echo "Received: ".  $res . ". Expected: 678728808158.". PHP_EOL;
    assert($res == 678729486878);
    /*
     * Received: 374. Expected: 374.
     * Received: 10165598. Expected: 10165598.
     * Received: 1030. Expected: 1030.
     * Received: 8410. Expected: 8410.
     * Received: 678728808158. Expected: 678729486878.
     */
}

main();



